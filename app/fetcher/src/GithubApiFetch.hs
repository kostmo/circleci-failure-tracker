{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module GithubApiFetch (
    getBuildStatuses
  , getCommits
  , findAncestor
  , GitHubApiSupport (..)
  , fetchUser
  ) where


import           Control.Lens               hiding ((<.>))
import           Control.Monad
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Aeson.Lens            (key, _Array, _Integral)
import           Data.Aeson.Types           (FromJSON, Value, parseEither,
                                             parseJSON)
import           Data.Bifunctor
import qualified Data.ByteString.Char8      as BSU
import           Data.Either.Utils          (maybeToEither)
import           Data.List                  (intercalate)
import qualified Data.Maybe                 as Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Vector                as V
import           Network.HTTP.Conduit       hiding (Request)
import qualified Network.OAuth.OAuth2       as OAuth2
import           Prelude
import qualified Safe
import           URI.ByteString             (parseURI, strictURIParserOptions)

import qualified Builds
import qualified DbHelpers
import qualified Github
import qualified GitHubRecords
import           SillyMonoids               ()
import qualified StatusEventQuery
import           Types


maxGitHubCommitFetchCount :: Int
maxGitHubCommitFetchCount = 2000


-- | Sometimes what we're looking for is within the first few items
-- of the result set, so we can optimistically fetch a small number
-- of results to reduce bandwidth.
expressPerPageCount :: Int
expressPerPageCount = 10


perPageCount :: Int
perPageCount = 100


githubCommitsApiPrefix :: DbHelpers.OwnerAndRepo -> String
githubCommitsApiPrefix repo_owner_name = intercalate "/" [
    DbHelpers.githubRepoApiPrefix repo_owner_name
  , "commits"
  ]


data GitHubApiSupport = GitHubApiSupport {
    tls_manager  :: Manager
  , access_token :: OAuth2.AccessToken
  }


recursePaginated :: FromJSON a =>
     GitHubApiSupport
  -> String
  -> T.Text
  -> Int
  -> [a]
  -> IO (Either TL.Text [a])
recursePaginated
    ghsupport@(GitHubApiSupport mgr token)
    uri_prefix
    field_accessor
    page_offset
    old_retrieved_items = do

  putStrLn $ "Querying URL for build statuses: " ++ uri_string

  runExceptT $ do

    uri <- except $ first (const $ "Bad URL: " <> TL.pack uri_string) either_uri

    r <- ExceptT (first displayOAuth2Error <$> OAuth2.authGetJSON mgr token uri :: IO (Either TL.Text Value))

    let subval = V.toList $ r ^. key field_accessor . _Array

    newly_retrieved_items <- except $ first TL.pack $ mapM (parseEither parseJSON) subval

    let expected_count = r ^. key "total_count" . _Integral
        combined_list = old_retrieved_items ++ newly_retrieved_items

    if length combined_list < expected_count
      then ExceptT $ recursePaginated
        ghsupport
        uri_prefix
        field_accessor
        (page_offset + 1)
        combined_list
      else return combined_list

  where
    either_uri = parseURI strictURIParserOptions $ BSU.pack uri_string
    uri_string = uri_prefix
      <> "?per_page=" <> show perPageCount
      <> "&page=" <> show page_offset


-- | Returns an error if the commit chain is not linear,
-- otherwise returns a tuple ()
getCommitsRecurse ::
     GitHubApiSupport
  -> Int -- ^ commit fetch limit
  -> String -- ^ URL prefix
  -> Int -- ^ iteration number
  -> T.Text  -- ^ starting commit
  -> (T.Text -> Bool)  -- ^ Stopping condition. May compare against last known commit (stopping commit)
  -> [GitHubRecords.GitHubCommit] -- ^ previously found commits
  -> IO (Either TL.Text ([GitHubRecords.GitHubCommit], Maybe GitHubRecords.GitHubCommit))
getCommitsRecurse
    ghsupport@(GitHubApiSupport mgr token)
    limit
    uri_prefix
    iteration_number
    target_sha1
    stopping_condition
    old_retrieved_items = do

  putStrLn $ "Querying URL for commits: " ++ uri_string

  runExceptT $ do

    unless (length old_retrieved_items < limit) $
      except $ Left "Too many commits to fetch!"

    uri <- except $ first (const $ "Bad URL: " <> TL.pack uri_string) either_uri

    newly_retrieved_items <- ExceptT $
      first displayOAuth2Error <$> OAuth2.authGetJSON mgr token uri

    let (novel_commits, known_commits) = break (stopping_condition . GitHubRecords.extractCommitSha) newly_retrieved_items
        is_merge_commit = (> 1) . length . GitHubRecords._parents
        merge_commits = filter is_merge_commit novel_commits
        combined_list = old_retrieved_items <> novel_commits

    unless (null merge_commits) $
      except $ Left "Commit ancestry is nonlinear!"

    let retval = (combined_list, Safe.headMay known_commits)

    Maybe.fromMaybe (return retval) $ do
      last_fetched_commit <- Safe.lastMay novel_commits

      -- Terminate recursion if we've encountered the root revision
      -- (which has no parents).
      parent_list <- GitHubRecords._parents last_fetched_commit
      parent_of_last_fetched <- Safe.headMay parent_list

      -- Terminate recursion if we've encountered a known commit.
      guard $ null known_commits

      -- start the next iteration at the parent of the last found commit
      let next_target_ref = GitHubRecords.extractParentSha parent_of_last_fetched

      return $ ExceptT $ getCommitsRecurse
        ghsupport
        limit
        uri_prefix
        (iteration_number + 1)
        next_target_ref
        stopping_condition
        combined_list

  where
    either_uri = parseURI strictURIParserOptions $ BSU.pack uri_string

    -- try a small number first, then immediately ramp up to the max
    fetch_quantity = if iteration_number == 0
      then expressPerPageCount
      else perPageCount

    uri_string = uri_prefix
      <> "?per_page=" <> show fetch_quantity
      <> "&sha=" <> T.unpack target_sha1


-- | This only works if the commit history is linear!
getCommits ::
     OAuth2.AccessToken -- ^ token
  -> DbHelpers.OwnerAndRepo
  -> T.Text  -- ^ starting commit
  -> T.Text  -- ^ last known commit (stopping commit)
  -> IO (Either TL.Text [GitHubRecords.GitHubCommit])
getCommits
    token
    owner_and_repo
    target_sha1
    stopping_sha1 = do

  mgr <- newManager tlsManagerSettings

  runExceptT $ do

    (combined_list, _first_known_commit) <- ExceptT $ getCommitsRecurse
      (GitHubApiSupport mgr token)
      maxGitHubCommitFetchCount
      (githubCommitsApiPrefix owner_and_repo)
      0
      target_sha1
      (== stopping_sha1)
      []

    return combined_list


-- | NOTE: This method assumes that the commit history is linear!
-- Returns the merge base commit and the distance between
-- the merge base and the starting commit.
--
-- WARNING: Accuracy of the "distance" result has not been verified.
findAncestor ::
     OAuth2.AccessToken -- ^ token
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit  -- ^ starting commit
  -> Set Builds.RawCommit  -- ^ known commits
  -> IO (Either TL.Text (Builds.RawCommit, Int))
findAncestor
    token
    owner_and_repo
    t@(Builds.RawCommit target_sha1)
    known_commit_set =

  if Set.member t known_commit_set
    then return $ Right (t, 0)
    else do
      mgr <- newManager tlsManagerSettings

      runExceptT $ do
        (combined_list, maybe_first_known_commit) <- ExceptT $ getCommitsRecurse
          (GitHubApiSupport mgr token)
          maxGitHubCommitFetchCount
          (githubCommitsApiPrefix owner_and_repo)
          0
          target_sha1
          ((`Set.member` known_commit_set) . Builds.RawCommit)
          []

        ancestor_commit_obj <- except $ maybeToEither "No merge base found" maybe_first_known_commit
        let ancestor_commit = Builds.RawCommit $ GitHubRecords.extractCommitSha ancestor_commit_obj
        return (ancestor_commit, length combined_list)


getBuildStatuses ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> IO (Either TL.Text [StatusEventQuery.GitHubStatusEventGetter])
getBuildStatuses
    token
    owner_and_repo
    (Builds.RawCommit target_sha1) = do

  mgr <- newManager tlsManagerSettings

  recursePaginated
    (GitHubApiSupport mgr token)
    uri_prefix
    "statuses"
    1
    []

  where
    uri_prefix = intercalate "/" [
        githubCommitsApiPrefix owner_and_repo
      , T.unpack target_sha1
      , "status"
      ]


fetchUser :: GitHubApiSupport -> IO (Either TL.Text LoginUser)
fetchUser (GitHubApiSupport mgr token) = do
  r2 <- do
    r <- OAuth2.authGetJSON mgr token Github.userInfoUri
    return $ second Github.toLoginUser r

  return $ first displayOAuth2Error r2


displayOAuth2Error :: OAuth2.OAuth2Error Errors -> TL.Text
displayOAuth2Error = TL.pack . show
