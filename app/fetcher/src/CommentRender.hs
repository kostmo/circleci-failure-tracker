
{-# LANGUAGE OverloadedStrings #-}

module CommentRender where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (dropWhileEnd, intersperse)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Data.Time.Format    as TF
import qualified Data.Tree           as Tr

import qualified Builds
import qualified CircleCIParse
import qualified CommitBuilds
import qualified Constants
import qualified DbHelpers
import qualified GadgitFetch
import qualified Markdown            as M
import qualified MatchOccurrences
import qualified MyUtils
import qualified SqlRead
import qualified SqlUpdate
import qualified StatusUpdateTypes
import qualified WebApi


pullRequestCommentsLogContextLineCount :: Int
pullRequestCommentsLogContextLineCount = 10


viableBranchName :: Text
viableBranchName = "viable/strict"


webserverBaseUrl :: LT.Text
webserverBaseUrl = "https://dr.pytorch.org"


drCiIssueTrackerUrl :: Text
drCiIssueTrackerUrl = "https://github.com/kostmo/circleci-failure-tracker/issues"


viableCommitsHistoryUrl :: LT.Text
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
     HashMap Text SqlRead.UpstreamBrokenJob
  -> Builds.RawCommit
  -> [WebApi.UnmatchedBuild]
  -> NonEmpty Text
genUnmatchedBuildsTable pre_broken_jobs_map merge_base_commit unmatched_builds =
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

        upstream_brokenness_indicator = if HashMap.member job_name pre_broken_jobs_map
          then upstream_brokenness_text
          else "New in PR"


genBuildFailuresTable ::
     StatusUpdateTypes.CommitPageInfo
  -> StatusUpdateTypes.BuildSummaryStats
  -> [Text]
genBuildFailuresTable
    (StatusUpdateTypes.CommitPageInfo pattern_matched_builds unmatched_builds)
    (StatusUpdateTypes.NewBuildSummaryStats pre_broken_info _) =

     nonupstream_nonflaky_pattern_matched_section
  <> nonupstream_flaky_pattern_matched_section
  <> upstream_matched_section
  <> pattern_unmatched_section

  where
    pre_broken_jobs_map = SqlUpdate.inferred_upstream_breakages_by_job pre_broken_info
    merge_base_commit = SqlUpdate.merge_base pre_broken_info

    StatusUpdateTypes.UpstreamBuildPartition upstream_breakages non_upstream_breakages_raw = pattern_matched_builds

    StatusUpdateTypes.FlakyBuildPartition nonupstream_flaky_breakages nonupstream_nonflaky_breakages = non_upstream_breakages_raw

    nonupstream_nonflaky_pattern_matched_header = M.heading 3 $ T.unwords [
        ":detective:"
      , MyUtils.pluralize (length nonupstream_nonflaky_breakages) "new failure"
      , "recognized by patterns"
      ]

    nonflaky_matched_builds_details_block = concat $
      zipWith (genMatchedBuildSection $ length nonupstream_nonflaky_breakages) [1..] nonupstream_nonflaky_breakages

    flaky_matched_builds_details_block = concat $
      zipWith (genMatchedBuildSection $ length nonupstream_flaky_breakages) [1..] nonupstream_flaky_breakages

    non_upstream_nonflaky_intro_text = M.colonize [
        "The following build failures do not appear to be due to upstream breakage"
      ]

    non_upstream_flaky_intro_text = M.colonize [
        "The following build failures have been detected as flaky and"
      , M.bold "may not be your fault"
      ]

    nonupstream_nonflaky_pattern_matched_section = if null nonupstream_nonflaky_breakages
      then mempty
      else pure nonupstream_nonflaky_pattern_matched_header
        <> pure non_upstream_nonflaky_intro_text
        <> nonflaky_matched_builds_details_block


    nonupstream_flaky_pattern_matched_section = if null nonupstream_flaky_breakages
      then mempty
      else pure nonupstream_flaky_pattern_matched_header
        <> pure non_upstream_flaky_intro_text
        <> flaky_matched_builds_details_block


    nonupstream_flaky_pattern_matched_header = M.heading 3 $ T.unwords [
        ":snowflake:"
      , MyUtils.pluralize (length nonupstream_flaky_breakages) "failure"
      , "recognized as flaky"
      ]


    upstream_matched_header = M.heading 3 $ M.colonize [
        ":construction:"
      , MyUtils.pluralize (length upstream_breakages) "upstream failure"
      , "recognized by patterns"
      ]


    render_upstream_matched_failure_item (x@(CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild (DbHelpers.WithId ubuild_id _universal_build) _build_obj) _match_obj _ _) _), upstream_cause) =
      pure $ pure $ T.unwords [
          M.link link_label link_url
        , breakage_span_words
        ]

      where
        ft = T.pack . TF.formatTime TF.defaultTimeLocale "%b %d"
        breakage_span_words = T.unwords $ since_words ++ until_words
        since_words =  [
            "from"
          , ft $ SqlRead._breakage_start_time upstream_cause
          ]

        until_words = case SqlRead._breakage_end_time upstream_cause of
          Nothing -> []
          Just z -> [
              "until"
            , ft z
            ]

        link_label = StatusUpdateTypes.get_job_name_from_build_with_log_context x
        link_url = LT.toStrict webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)

    matched_upstream_builds_details_block = M.bulletTree $
      map render_upstream_matched_failure_item upstream_breakages

    upstream_intro_text = M.colonize [
        "These builds matched patterns, but were probably"
      , M.bold "caused by upstream breakages"
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
        <> NE.toList (genUnmatchedBuildsTable pre_broken_jobs_map merge_base_commit unmatched_builds)


genMatchedBuildSection total_count idx build_with_log_context = [
    M.heading 4 $ T.unwords [
        circleci_image_link
      , job_name
      , M.parens $ T.pack $ MyUtils.renderFrac idx total_count
      ]
  , T.unwords summary_info_pieces
  ] <> M.detailsExpanderForCode single_match_line code_block_lines

  where
    (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild storable_build match_obj _ failure_mode) (CommitBuilds.LogContext _ log_lines)) = build_with_log_context

    single_match_line = M.codeInlineHtml $ sanitizeLongLine $ MatchOccurrences._line_text match_obj
    Builds.StorableBuild (DbHelpers.WithId ubuild_id universal_build) build_obj = storable_build

    job_name = Builds.job_name build_obj

    optional_flakiness_indicator = if CommitBuilds._is_flaky failure_mode
      then [":snowflake:"]
      else []

    summary_info_pieces = [
        M.bold "Step:"
      , M.quote $ MatchOccurrences._build_step match_obj
      , M.parens $ T.intercalate " | " [
         M.link "full log" $ LT.toStrict webserverBaseUrl <> "/api/view-log-full?build_id=" <> T.pack (show ubuild_id)
        , M.link "pattern match details" $ LT.toStrict webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)
        ]
      ] ++ optional_flakiness_indicator

    code_block_lines = NE.toList $ M.codeBlockFromList $
      dropWhileEnd T.null $ map renderLogLineTuple log_lines

    (Builds.NewBuildNumber provider_build_number) = Builds.provider_buildnum universal_build
    circleci_icon = M.image "See CircleCI build" circleCISmallAvatarUrl
    circleci_image_link = M.link circleci_icon $
      circleCIBuildUrlPrefix <> T.pack (show provider_build_number)


renderLogLineTuple :: (a, LT.Text) -> Text
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


generateCommentFooter :: Maybe SqlRead.PostedPRComment -> [Text]
generateCommentFooter maybe_previous_pr_comment =
  ["---", footer_section]
  where
    footer_section = M.detailsExpander dr_ci_attribution_line $ T.unlines $
      intersperse "" footer_hidden_details

    footer_hidden_details = [
        M.sentence [
            "Follow"
          , M.htmlLink "this link to opt-out" opt_out_url
          , "of these comments for your Pull Requests"
          ]
      , M.sentence [
          "Please report bugs/suggestions on the"
        , M.htmlLink "GitHub issue tracker" drCiIssueTrackerUrl
        ]
      ] ++ optional_suffix

    dr_ci_attribution_line = M.sentence [
        "This comment was automatically generated by"
      , M.htmlLink drCIApplicationTitle drCIPullRequestCommentsReadmeUrl
      , M.parens "expand for details"
      ]

    -- Note that using the current count of N comments as the revision count will be
    -- appropriate for the (N+1)th comment (the one that's about to be posted), because
    -- the first post doesn't count as a "revision".
    optional_suffix = flip (maybe []) maybe_previous_pr_comment $ \previous_pr_comment -> [
        M.italic $ M.sentence [
          "This comment has been revised"
        , MyUtils.pluralize (SqlRead._revision_count previous_pr_comment) "time"
        ]
      ]

    dr_ci_base_url = LT.toStrict webserverBaseUrl
    opt_out_url = dr_ci_base_url <> "/admin/comments-opt-out.html"


generateCommentMarkdown ::
     Maybe SqlRead.PostedPRComment
  -> [Text]
  -> Builds.RawCommit
  -> Text
generateCommentMarkdown
    maybe_previous_pr_comment
    middle_sections
    (Builds.RawCommit sha1_text) =

  M.paragraphs $ [intro_section] ++ middle_sections ++ generateCommentFooter maybe_previous_pr_comment
  where
    intro_section = T.unlines [
        M.heading 2 ":pill: CircleCI build failures summary and remediations"
      , M.colonize [
          "As of commit"
        , T.take Constants.gitCommitPrefixLength sha1_text
        ]
      ]


generateMiddleSections ::
     GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> StatusUpdateTypes.CommitPageInfo
  -> Builds.RawCommit
  -> [Text]
generateMiddleSections
    ancestry_result
    build_summary_stats
    commit_page_info
    (Builds.RawCommit sha1_text) =

  summary_header ++ [summary_tree] ++ detailed_build_issues_section
  where

    (summary_header, summary_forrest) = genMetricsTree
      commit_page_info
      ancestry_result
      build_summary_stats

    summary_tree = M.bulletTree summary_forrest

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

    dr_ci_base_url = LT.toStrict webserverBaseUrl
    dr_ci_commit_details_link = dr_ci_base_url <> "/commit-details.html?sha1=" <> sha1_text


genMetricsTree ::
     StatusUpdateTypes.CommitPageInfo
  -> GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> ([Text], Tr.Forest (NonEmpty Text))
genMetricsTree
    commit_page_info
    ancestry_response
    (StatusUpdateTypes.NewBuildSummaryStats pre_broken_info all_failures) =

  (summary_header, forrest_parts)
  where
    forrest_parts = concat [
        optional_kb_metric
      , introduced_failures_section
      , flaky_bullet_tree
      ]

    summary_header = if broken_in_pr_count > 0
      then []
      else [M.sentence [M.bold "None of the build failures appear to be your fault"]]

    optional_kb_metric = [upstream_breakage_bullet_tree | not $ HashMap.null pre_broken_jobs_map]
    introduced_failures_section = [introduced_failures_section_inner | broken_in_pr_count > 0]
    flaky_bullet_tree = [flaky_bullet_tree_inner | flaky_count > 0]

    GadgitFetch.AncestryPropositionResponse _ ancestry_result = ancestry_response

    merge_base_commit = SqlUpdate.merge_base pre_broken_info
    Builds.RawCommit merge_base_sha1_text = merge_base_commit

    definite_older_commit_advice = pure $ M.colonize [
        M.commaize [
          "Since your merge base"
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
      ("git fetch origin " <> viableBranchName) :| ["git rebase " <> viableBranchName]

    newer_commit_advice = pure $ M.colonize [
        M.commaize [
            "If your commit is newer than"
          , M.codeInline viableBranchName
          ]
      , "you can try basing on an older, stable commit"
      ]

    newer_commit_codeblock = M.codeBlock $
      ("git fetch origin " <> viableBranchName) :| [
      T.unwords [
          "git rebase --onto"
        , viableBranchName
        , "$(git merge-base origin/master HEAD)"
        ]
     ]

    definite_older_rebase_advice_children =
        definite_older_commit_advice <> older_commit_codeblock

    maybe_newer_rebase_advice_children =
         newer_commit_advice
      <> newer_commit_codeblock
      <> possible_older_commit_advice
      <> older_commit_codeblock


    rebase_advice_children = case ancestry_result of
      GadgitFetch.RefIsAncestor    -> definite_older_rebase_advice_children
      GadgitFetch.RefIsNotAncestor -> maybe_newer_rebase_advice_children

    rebase_advice_footer = pure $ M.sentence [
        "Check out the"
      , M.link "recency history" $ LT.toStrict viableCommitsHistoryUrl
      , "of this"
      , M.quote "viable master"
      , "tracking branch"
      ]

    rebase_advice_intro = T.unwords [
        "You may want to rebase on the"
      , M.codeInlineHtml viableBranchName
      , "branch"
      , M.parens "expand for instructions"
      ]

    rebase_advice_section = M.detailsExpanderForCode rebase_advice_intro $ NE.toList $
      rebase_advice_children <> rebase_advice_footer


    pre_broken_jobs_map = SqlUpdate.inferred_upstream_breakages_by_job pre_broken_info

    upstream_broken_count = HashMap.size pre_broken_jobs_map
    total_failcount = length all_failures

    broken_in_pr_count = total_failcount - upstream_broken_count - flaky_count

    bold_fraction a b = M.bold $ T.pack $ MyUtils.renderFrac a b

    grid_view_url = genGridViewSha1Link 1 merge_base_commit Nothing

    latest_upstream_breakage = maximum $ map SqlRead._breakage_start_time $ HashMap.elems pre_broken_jobs_map
    latest_breakage_formatted_time = TF.formatTime TF.defaultTimeLocale "%b %d" latest_upstream_breakage

    upstream_brokenness_declaration = T.unwords [
        bold_fraction upstream_broken_count total_failcount
      , M.link "broken upstream" grid_view_url
      , "at merge base"
      , T.take Constants.gitCommitPrefixLength merge_base_sha1_text
      , "since"
      , T.pack latest_breakage_formatted_time
      ]

    upstream_breakage_bullet_tree = pure $
       upstream_brokenness_declaration :| rebase_advice_section

    introduced_failures_section_inner = pure $ pure $ T.unwords [
        bold_fraction broken_in_pr_count total_failcount
      , "failures introduced in this PR"
      ]

    flaky_count = length $ StatusUpdateTypes.flaky_builds $
      StatusUpdateTypes.nonupstream $ StatusUpdateTypes.pattern_matched_builds commit_page_info


    flaky_bullet_tree_inner_heading = pure $ T.unwords [
        bold_fraction flaky_count total_failcount
      , "recognized as flaky :snowflake:"
      ]

    flaky_bullet_tree_inner = Tr.Node
      flaky_bullet_tree_inner_heading
      [pure $ pure "Re-run these jobs?"]


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
