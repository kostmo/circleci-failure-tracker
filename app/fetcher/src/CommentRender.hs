{-# LANGUAGE OverloadedStrings #-}

module CommentRender where

import           Data.List          (partition)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as LT
import qualified Data.Tree          as Tr

import qualified Builds
import qualified CircleCIParse
import qualified CommitBuilds
import qualified Constants
import qualified DbHelpers
import qualified GadgitFetch
import qualified Markdown           as M
import qualified MatchOccurrences
import qualified MyUtils
import qualified SqlRead
import qualified SqlUpdate
import qualified StatusUpdateTypes
import qualified WebApi


pullRequestCommentsLogContextLineCount :: Int
pullRequestCommentsLogContextLineCount = 10


viableBranchName = "viable/strict"


webserverBaseUrl :: LT.Text
webserverBaseUrl = "https://dr.pytorch.org"


viableCommitsHistoryUrl = webserverBaseUrl <> "/master-viable-commits.html"


circleCISmallAvatarUrl :: Text
circleCISmallAvatarUrl = "https://avatars0.githubusercontent.com/ml/7?s=12"


drCIApplicationTitle :: Text
drCIApplicationTitle = "Dr. CI"


drCIPullRequestCommentsReadmeUrl :: Text
drCIPullRequestCommentsReadmeUrl = "https://github.com/kostmo/circleci-failure-tracker/tree/master/docs/from-pull-request-comment"


circleCIBuildUrlPrefix :: Text
circleCIBuildUrlPrefix = "https://circleci.com/gh/pytorch/pytorch/"


genUnmatchedBuildsTable ::
     Set Text
  -> Builds.RawCommit
  -> [WebApi.UnmatchedBuild]
  -> NonEmpty Text
genUnmatchedBuildsTable pre_broken_set merge_base_commit unmatched_builds =
  M.table header_columns data_rows
  where
    header_columns = [
        "Job"
      , "Step"
      , "Status"
      ]

    data_rows = map gen_unmatched_build_row unmatched_builds

    gen_unmatched_build_row (WebApi.UnmatchedBuild _build step_name _ job_name _ _ _ _) = [
        T.unwords [
            M.image "CircleCI" circleCISmallAvatarUrl
          , M.sup job_name
          ]
      , M.sup step_name
      , upstream_brokenness_indicator
      ]
      where
        upstream_brokenness_text = T.unwords [
            "&#128721;"
          , M.link "Broken upstream" $ genGridViewSha1Link 1 merge_base_commit $ Just job_name
          ]

        upstream_brokenness_indicator = if Set.member job_name pre_broken_set
          then upstream_brokenness_text
          else "New in PR"


get_job_name_from_build_with_log_context (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild _ build_obj) _ _ _) _) = Builds.job_name build_obj

genBuildFailuresTable ::
     StatusUpdateTypes.CommitPageInfo
  -> StatusUpdateTypes.BuildSummaryStats
  -> [Text]
genBuildFailuresTable
    (StatusUpdateTypes.CommitPageInfo pattern_matched_builds unmatched_builds)
    (StatusUpdateTypes.NewBuildSummaryStats _ pre_broken_info _) =

  nonupstream_pattern_matched_section <> upstream_matched_section <> pattern_unmatched_section
  where

    pre_broken_set = SqlUpdate.inferred_upstream_caused_broken_jobs pre_broken_info
    merge_base_commit = SqlUpdate.merge_base pre_broken_info

    (upstream_breakages, non_upstream_breakages) = partition (\x -> Set.member (get_job_name_from_build_with_log_context x) pre_broken_set) pattern_matched_builds

    pattern_matched_header = M.heading 3 $ T.unwords [
        MyUtils.pluralize (length non_upstream_breakages) "new failure"
      , "recognized by patterns"
      ]

    matched_builds_details_block = concat $
      zipWith gen_matched_build_section [1..] non_upstream_breakages

    non_upstream_intro_text = M.colonize [
        "The following build failures don't appear to be due to upstream breakage"
      ]

    nonupstream_pattern_matched_section = if null non_upstream_breakages
      then mempty
      else pure pattern_matched_header
        <> pure non_upstream_intro_text
        <> matched_builds_details_block

    upstream_matched_header = M.heading 3 $ M.colonize [
        MyUtils.pluralize (length upstream_breakages) "upstream failure"
      , "recognized by patterns"
      ]


    render_upstream_matched_failure_item x@(CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild (DbHelpers.WithId ubuild_id _universal_build) _build_obj) _match_obj _ _) _) = pure $ pure $ M.link (get_job_name_from_build_with_log_context x) $ LT.toStrict webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)

    matched_upstream_builds_details_block = M.bulletTree $ map render_upstream_matched_failure_item upstream_breakages

    upstream_intro_text = M.colonize [
        "These builds matched patterns, but were probably caused by upstream breakages"
      ]

    upstream_matched_section = if null upstream_breakages
      then mempty
      else pure upstream_matched_header
        <> pure upstream_intro_text
        <> pure matched_upstream_builds_details_block

    pattern_unmatched_header = M.heading 3 $ M.colonize [
        MyUtils.pluralize (length unmatched_builds) "failure"
      , M.italic "not"
      , "recognized by patterns"
      ]

    pattern_unmatched_section = if null unmatched_builds
      then mempty
      else pure pattern_unmatched_header
        <> NE.toList (genUnmatchedBuildsTable pre_broken_set merge_base_commit unmatched_builds)

    gen_matched_build_section idx (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild (DbHelpers.WithId ubuild_id universal_build) build_obj) match_obj _ _) (CommitBuilds.LogContext _ log_lines)) = [
        M.heading 4 $ T.unwords [
            "<details>"
          , "<summary>"
          , circleci_image_link
          , Builds.job_name build_obj
          , M.parens $ T.pack $ MyUtils.renderFrac idx $ length non_upstream_breakages
          , "</summary>"
          ]
      , T.unwords summary_info_pieces
      ] <> code_block_lines <> ["</details>"]
      where
--        job_name = Builds.job_name build_obj

        summary_info_pieces = [
            M.bold "Step:"
          , M.quote $ MatchOccurrences._build_step match_obj
          , M.parens $ M.link "details" $ LT.toStrict webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)
          ]


        code_block_lines = NE.toList $ M.codeBlockFromList $
          -- NOTE: this commented-out code just renders the single matched line
--        pure $ MatchOccurrences._line_text match_obj
          map renderLogLineTuple log_lines


        (Builds.NewBuildNumber provider_build_number) = Builds.provider_buildnum universal_build
        circleci_icon = M.image "See CircleCI build" circleCISmallAvatarUrl
        circleci_image_link = M.link circleci_icon $
          circleCIBuildUrlPrefix <> T.pack (show provider_build_number)


--renderLogLineTuple tup = T.pack (show $ fst tup) <> ") " <> (sanitizeLongLine . LT.toStrict . snd) tup
renderLogLineTuple = sanitizeLongLine . LT.toStrict . snd


-- | Handles misbehaving carriage returns, as well as stripping some
-- ANSI control codes that weren't filtered on ingest
sanitizeLongLine :: Text -> Text
sanitizeLongLine line_text =
  T.drop final_character_count_to_drop recombined_chunks
  where
    absolute_character_count_to_preserve = 500
    final_character_count_to_drop = max 0 $ T.length recombined_chunks - absolute_character_count_to_preserve

    recombined_chunks = T.intercalate " " preserved_chunks
    preserved_chunks = drop chunk_count_to_drop carriage_return_chunks

    chunk_count_to_preserve = 5
    chunk_count_to_drop = max 0 $ length carriage_return_chunks - chunk_count_to_preserve
    carriage_return_chunks = T.split (== '\r') $ T.pack $ CircleCIParse.filterAnsiCursorMovement $ T.unpack line_text


generateCommentMarkdown ::
     Maybe SqlRead.PostedPRComment
  -> StatusUpdateTypes.BuildSummaryStats
  -> GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.CommitPageInfo
  -> Builds.RawCommit
  -> Text
generateCommentMarkdown
    maybe_previous_pr_comment
    build_summary_stats
    ancestry_result
    commit_page_info
    (Builds.RawCommit sha1_text) =
  M.paragraphs $ preliminary_lines_list ++ optional_suffix
  where
    build_failures_table_lines = genBuildFailuresTable commit_page_info build_summary_stats

    detailed_build_issues_section = if null build_failures_table_lines
      then []
      else [
          M.heading 2 "Detailed failure analysis"
        , M.sentence [
            "One may explore the probable reasons each build failed interactively"
          , M.link "on the Dr. CI website" dr_ci_commit_details_link
          ]
        , T.unlines build_failures_table_lines
        ]

    footer_section1 = T.unlines [
        "---"
      , M.sentence [
          "This comment was automatically generated by"
        , M.link drCIApplicationTitle drCIPullRequestCommentsReadmeUrl
        ]
      , M.sentence [
          "Follow"
        , M.link "this link to opt-out" opt_out_url
        , "of these comments for your Pull Requests"
        ]
      ]

    footer_section2 = M.sentence [
        "Please report bugs/suggestions on the"
      , M.link "GitHub issue tracker" "https://github.com/kostmo/circleci-failure-tracker/issues"
      ]

    preliminary_lines_list = [
        T.unlines [
          M.heading 2 "CircleCI build failures summary"
        , M.colonize [
            "As of commit"
          , T.take Constants.gitCommitPrefixLength sha1_text
          ]
        , M.bulletTree $ genMetricsTreeVerbose ancestry_result build_summary_stats
        ]
      ] ++ detailed_build_issues_section ++ [footer_section1, footer_section2]


    -- Note that using the current count of N comments as the revision count will be
    -- appropriate for the (N+1)th comment (the one that's about to be posted), because
    -- the first post doesn't count as a "revision".
    optional_suffix = case maybe_previous_pr_comment of
      Nothing -> []
      Just previous_pr_comment -> [
          M.italic $ M.sentence [
            "This comment has been revised"
          , MyUtils.pluralize (SqlRead._revision_count previous_pr_comment) "time"
          ]
        ]

    dr_ci_base_url = LT.toStrict webserverBaseUrl
    dr_ci_commit_details_link = dr_ci_base_url <> "/commit-details.html?sha1=" <> sha1_text
    opt_out_url = dr_ci_base_url <> "/admin/comments-opt-out.html"


genMetricsTreeVerbose ::
     GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> Tr.Forest (NonEmpty Text)
genMetricsTreeVerbose
    ancestry_response
    (StatusUpdateTypes.NewBuildSummaryStats flaky_count pre_broken_info all_failures) =
  optional_kb_metric <> failures_introduced_in_pull_request <> flaky_bullet_tree
  where

    (GadgitFetch.AncestryPropositionResponse (GadgitFetch.RefAncestryProposition _supposed_ancestor _supposed_descendant) ancestry_result) = ancestry_response

    merge_base_commit = SqlUpdate.merge_base pre_broken_info
    Builds.RawCommit merge_base_sha1_text = merge_base_commit

    definite_older_commit_advice = pure $ M.colonize [
        M.commaize [
          "Since your merge base"
--        , M.codeInline supposed_ancestor
        , "is older than"
        , M.codeInline viableBranchName
        ]
      , "run these commands"
      ]

    possible_older_commit_advice = pure $ M.colonize [
        "If your commit is older than"
      , M.codeInline viableBranchName
      ]

    older_commit_codeblock = M.codeBlock $
      ("git fetch " <> viableBranchName) :| ["git rebase " <> viableBranchName]

    newer_commit_advice = pure $ M.colonize [
        M.commaize [
            "If your commit is newer than"
          , M.codeInline viableBranchName
          ]
      , "you can try basing on an older, stable commit"
      ]

    newer_commit_codeblock = M.codeBlock $
      ("git fetch " <> viableBranchName) :| [
      T.unwords [
          "git rebase --onto"
        , viableBranchName
        , "$(git merge-base origin/master HEAD)"
        ]
     ]

    definite_older_rebase_advice_children = [
        definite_older_commit_advice <> older_commit_codeblock
      ]

    maybe_newer_rebase_advice_children = [
        newer_commit_advice <> newer_commit_codeblock
      , possible_older_commit_advice <> older_commit_codeblock
      ]

    rebase_advice_children = case ancestry_result of
      GadgitFetch.RefIsAncestor    -> definite_older_rebase_advice_children
      GadgitFetch.RefIsNotAncestor -> maybe_newer_rebase_advice_children

    rebase_advice_intro = pure $ M.colonize [
        "You may want to rebase on the"
      , M.codeInline viableBranchName
      , "branch"
      , M.parens $ T.unwords [
          "see its"
        , M.link "recency history" $ LT.toStrict viableCommitsHistoryUrl
        ]
      ]

    rebase_advice_section = Tr.Node rebase_advice_intro $ map pure rebase_advice_children

    upstream_breakage_bullet_children = [rebase_advice_section]

    pre_broken_set = SqlUpdate.inferred_upstream_caused_broken_jobs pre_broken_info
    upstream_broken_count = length pre_broken_set
    total_failcount = length all_failures
    broken_in_pr_count = total_failcount - upstream_broken_count

    bold_fraction a b = M.bold $ T.pack $ MyUtils.renderFrac a b

    grid_view_url = genGridViewSha1Link 1 merge_base_commit Nothing

    upstream_breakage_bullet_tree = Tr.Node (
       pure $ T.unwords [
           bold_fraction upstream_broken_count total_failcount
         , "broken upstream at merge base"
         , T.take Constants.gitCommitPrefixLength merge_base_sha1_text
         , M.parens $ T.unwords [
             "see"
           , M.link "grid view" grid_view_url
           ]
         ]
       ) upstream_breakage_bullet_children

    optional_kb_metric = if null pre_broken_set
      then []
      else [upstream_breakage_bullet_tree]


    failures_introduced_in_pull_request = [
        pure $ pure $ T.unwords [
            bold_fraction broken_in_pr_count total_failcount
          , "failures introduced in this PR"
          ]
      ]

    flaky_bullet_tree = if flaky_count > 0
      then [
        Tr.Node (pure $ T.unwords [
            bold_fraction flaky_count total_failcount
          , "recognized as flaky"
          ]) [pure $ pure "Re-run these jobs?"]
        ]
      else []


genGridViewSha1Link ::
     Int -- ^ row count
  -> Builds.RawCommit
  -> Maybe Text -- ^ job highlight
  -> Text
genGridViewSha1Link
    row_count
    (Builds.RawCommit merge_base_sha1_text)
    maybe_job_highlight =

  T.pack $ "https://dr.pytorch.org/master-timeline.html?" <> MyUtils.genUrlQueryString grid_view_query_parms
  where

    highlight_parm = case maybe_job_highlight of
      Nothing               -> []
      Just higlight_jobname -> [("highlight_job", T.unpack higlight_jobname)]

    grid_view_query_parms = [
        ("count", show row_count)
      , ("sha1", T.unpack merge_base_sha1_text)
      , ("should_suppress_scheduled_builds", "true")
      , ("should_suppress_fully_successful_columns", "true")
      , ("max_columns_suppress_successful", "35")
      ] <> highlight_parm
