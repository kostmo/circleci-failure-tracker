{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module GithubApiFetch (
    getBuildStatuses
  , getCommits
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
import           Data.List                  (intercalate)
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Vector                as V
import           Network.HTTP.Conduit       hiding (Request)
import qualified Network.OAuth.OAuth2       as OAuth2
import           Prelude
import qualified Safe
import           URI.ByteString             (parseURI, strictURIParserOptions)

import qualified DbHelpers
import qualified Github
import qualified GitHubRecords
import           SillyMonoids               ()
import qualified StatusEventQuery
import           Types


maxGitHubCommitFetchCount :: Int
maxGitHubCommitFetchCount = 2000


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

    r <- ExceptT ((first displayOAuth2Error <$> OAuth2.authGetJSON mgr token uri) :: IO (Either TL.Text Value))

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


-- | Verifies that the commit chain is linear.
getCommitsRecurse ::
     GitHubApiSupport
  -> Int -- ^ commit fetch limit
  -> String -- ^ URL prefix
  -> T.Text  -- ^ starting commit
  -> T.Text  -- ^ last known commit (stopping commit)
  -> [GitHubRecords.GitHubCommit] -- ^ previously found commits
  -> IO (Either TL.Text [GitHubRecords.GitHubCommit])
getCommitsRecurse
    ghsupport@(GitHubApiSupport mgr token)
    limit
    uri_prefix
    target_sha1
    stopping_sha1
    old_retrieved_items = do

  putStrLn $ "Querying URL for commits: " ++ uri_string

  runExceptT $ do

    uri <- except $ first (const $ "Bad URL: " <> TL.pack uri_string) either_uri

    newly_retrieved_items <- ExceptT $ first displayOAuth2Error <$> OAuth2.authGetJSON mgr token uri

    let (novel_commits, known_commits) = break ((== stopping_sha1) . GitHubRecords._sha) newly_retrieved_items
        merge_commits = filter ((> 1) . length . GitHubRecords._parents) novel_commits
        combined_list = old_retrieved_items ++ novel_commits

    unless (null merge_commits) $
      except $ Left "Commit ancestry is nonlinear!"

    Maybe.fromMaybe (return combined_list) $ do
      last_fetched_commit <- Safe.lastMay novel_commits
      guard $ not $ null $ GitHubRecords._parents last_fetched_commit
      guard $ null known_commits && length combined_list < limit
      return $ ExceptT $ getCommitsRecurse
        ghsupport
        limit
        uri_prefix
        (GitHubRecords._sha last_fetched_commit <> "~")
        stopping_sha1
        combined_list

  where
    either_uri = parseURI strictURIParserOptions $ BSU.pack uri_string

    uri_string = uri_prefix
--      <> "?per_page=" <> show perPageCount
      <> "?per_page=" <> show 10
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

  getCommitsRecurse
    (GitHubApiSupport mgr token)
    maxGitHubCommitFetchCount
    (githubCommitsApiPrefix owner_and_repo)
    target_sha1
    stopping_sha1
    []


getBuildStatuses ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> T.Text
  -> IO (Either TL.Text [StatusEventQuery.GitHubStatusEventGetter])
getBuildStatuses
    token
    owner_and_repo
    target_sha1 = do

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
