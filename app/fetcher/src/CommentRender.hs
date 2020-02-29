{-# LANGUAGE OverloadedStrings #-}

module CommentRender where

import qualified Data.HashMap.Strict as HashMap
import           Data.List           (dropWhileEnd, intercalate, intersperse)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Data.Time.Format    as TF
import qualified Data.Tree           as Tr
import           Debug.Trace         (trace)

import qualified Builds
import qualified CircleCIParse
import qualified CommentRebaseAdvice
import qualified CommentRenderCommon
import qualified CommitBuilds
import qualified Constants
import qualified DbHelpers
import qualified GadgitFetch
import qualified Markdown            as M
import qualified MatchOccurrences
import qualified MyUtils
import qualified Sql.Read            as SqlRead
import qualified Sql.Update          as SqlUpdate
import qualified StatusUpdateTypes
import qualified UnmatchedBuilds


pullRequestCommentsLogContextLineCount :: Int
pullRequestCommentsLogContextLineCount = 10


drCiGitHubRepoBase :: Text
drCiGitHubRepoBase = "https://github.com/kostmo/circleci-failure-tracker"


drCiIssueTrackerUrl :: Text
drCiIssueTrackerUrl = drCiGitHubRepoBase <> "/issues"

circleCISmallAvatarUrl :: Text
circleCISmallAvatarUrl = "https://avatars0.githubusercontent.com/ml/7?s=12"


drCIApplicationTitle :: Text
drCIApplicationTitle = "Dr. CI"


drCIPullRequestCommentsReadmeUrl :: Text
drCIPullRequestCommentsReadmeUrl = drCiGitHubRepoBase <> "/tree/master/docs/from-pull-request-comment"


circleCIBuildUrlPrefix :: Text
circleCIBuildUrlPrefix = "https://circleci.com/gh/pytorch/pytorch/"


genUnmatchedBuildsTable ::
     [UnmatchedBuilds.UnmatchedBuild]
  -> NonEmpty Text
genUnmatchedBuildsTable unmatched_nonupstream_builds =
  M.table header_columns data_rows
  where
    header_columns = [
        "Job"
      , "Step"
      , "Status"
      ]

    data_rows = map gen_unmatched_build_row unmatched_nonupstream_builds

    gen_unmatched_build_row (UnmatchedBuilds.UnmatchedBuild _build step_name _ job_name _ _ _ _) = [
        T.unwords [
          M.image "CircleCI" circleCISmallAvatarUrl
        , M.sup job_name
        ]
      , M.sup step_name
      ]


genBuildFailuresTable ::
     StatusUpdateTypes.CommitPageInfo
  -> [[Text]]
genBuildFailuresTable
    (StatusUpdateTypes.NewCommitPageInfo upstream_breakages nonupstream_builds) =
  major_sections
  where
    major_sections = pattern_matched_sections ++ [pattern_unmatched_section, upstream_matched_section]

    pattern_matched_sections = genPatternMatchedSections pattern_matched_builds

    StatusUpdateTypes.NewNonUpstreamBuildPartition pattern_matched_builds unmatched_nonupstream_builds = nonupstream_builds

    pattern_unmatched_header = M.heading 3 $ M.colonize [
        MyUtils.pluralize (length unmatched_nonupstream_builds) "failure"
      , M.italic "not"
      , "recognized by patterns"
      ]

    pattern_unmatched_section = if null unmatched_nonupstream_builds
      then mempty
      else pure pattern_unmatched_header
        <> NE.toList (genUnmatchedBuildsTable unmatched_nonupstream_builds)

    upstream_matched_section = genUpstreamFailuresSection upstream_breakages


genPatternMatchedSections pattern_matched_builds = [
    nonupstream_nonflaky_pattern_matched_section
  ] ++ tentative_and_confirmed_flaky_sections
  where
    StatusUpdateTypes.NewFlakyBuildPartition
      tentatively_flaky_builds
      classified_nonflaky_builds
      confirmed_flaky_builds
        = pattern_matched_builds


    StatusUpdateTypes.NewNonFlakyBuilds nonflaky_by_pattern nonflaky_by_empirical_confirmation = classified_nonflaky_builds
    all_nonflaky_builds = nonflaky_by_pattern ++ nonflaky_by_empirical_confirmation

    nonupstream_nonflaky_pattern_matched_section = if null all_nonflaky_builds
      then mempty
      else pure nonupstream_nonflaky_pattern_matched_header
        <> pure non_upstream_nonflaky_intro_text
        <> nonflaky_matched_builds_details_block

    nonupstream_nonflaky_pattern_matched_header = M.heading 3 $ T.unwords [
        ":detective:"
      , MyUtils.pluralize (length all_nonflaky_builds) "new failure"
      , "recognized by patterns"
      ]

    nonflaky_matched_builds_details_block = concat $
      zipWith (genMatchedBuildSection $ length all_nonflaky_builds)
        [1..] all_nonflaky_builds

    discounted_flakiness_blurb = if null nonflaky_by_empirical_confirmation
      then []
      else pure $ M.parens $ T.unwords [
          "reran"
        , MyUtils.pluralize (length nonflaky_by_empirical_confirmation) "job"
        , "to discount flakiness"
        ]

    nonflaky_intro_text_pieces =
        "The following build failures do not appear to be due to upstream breakages"
       : discounted_flakiness_blurb

    non_upstream_nonflaky_intro_text = M.colonize nonflaky_intro_text_pieces

    tentative_and_confirmed_flaky_sections = genFlakySections
      tentatively_flaky_builds
      confirmed_flaky_builds


genFlakySections ::
     StatusUpdateTypes.TentativeFlakyBuilds StatusUpdateTypes.CommitBuildWrapperTuple
  -> [StatusUpdateTypes.CommitBuildWrapperTuple]
  -> [[Text]]
genFlakySections
    tentative_flakies
    confirmed_flaky_builds =
  [
    confirmed_flaky_section
  , tentative_flaky_section
  ]

  where
    StatusUpdateTypes.NewTentativeFlakyBuilds tentative_flaky_triggered_reruns tentative_flaky_untriggered_reruns = tentative_flakies

    confirmed_flaky_section = if null confirmed_flaky_builds
      then mempty
      else pure $ T.unwords [
          MyUtils.pluralize (length confirmed_flaky_builds) "failure"
        , "confirmed as flaky and can be ignored."
        ]

    total_tentative_flaky_count = StatusUpdateTypes.count tentative_flakies


    tentative_flaky_section = if total_tentative_flaky_count > 0
      then pure tentative_flakies_header
        <> untriggered_subsection
        <> triggered_subsection
      else mempty

    tentative_flakies_header = M.heading 3 $ T.unwords [
        ":snowflake:"
      , MyUtils.pluralize total_tentative_flaky_count "tentatively flaky failure"
      ]

    untriggered_subsection = if null tentative_flaky_untriggered_reruns
      then mempty
      else pure untriggered_intro_text
        <> make_details_block tentative_flaky_untriggered_reruns

    triggered_subsection = if null tentative_flaky_triggered_reruns
      then mempty
      else pure triggered_intro_text
        <> make_details_block tentative_flaky_triggered_reruns

    untriggered_intro_text = M.colonize [
        MyUtils.pluralize (length tentative_flaky_untriggered_reruns) "failure"
      , M.bold "tentatively classified as flaky"
      , "but have not launched reruns to confirm"
      ]

    triggered_intro_text = M.colonize [
        MyUtils.pluralize (length tentative_flaky_triggered_reruns) "failure"
      , "tentatively classified as flaky and"
      , M.bold "rerunning now"
      , "to confirm"
      ]

    make_details_block x = concat $
      zipWith (genMatchedBuildSection $ length x)
        [1..] x


genUpstreamFailuresSection upstream_breakages =
  if null upstream_breakages
    then mempty
    else pure upstream_matched_header
      <> pure upstream_intro_text
      <> pure matched_upstream_builds_details_block
  where
    upstream_matched_header = M.heading 3 $ M.colonize [
        ":construction:"
      , MyUtils.pluralize (length upstream_breakages) "upstream failure"
      , "recognized by patterns"
      ]

    matched_upstream_builds_details_block = M.bulletTree $
      map render_upstream_matched_failure_item upstream_breakages

    upstream_intro_text = M.colonize [
        "These were probably"
      , M.bold "caused by upstream breakages"
      ]

    render_upstream_matched_failure_item (x, upstream_cause) =
      pure $ pure $ T.unwords [
          M.link link_label link_url
        , breakage_span_words
        ]

      where
        CommitBuilds.NewCommitBuild y _match_obj _ _ = CommitBuilds._commit_build x
        Builds.StorableBuild (DbHelpers.WithId ubuild_id _universal_build) build_obj = y

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

        link_label = Builds.job_name build_obj
        link_url = LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)


genMatchedBuildSection ::
     Int
  -> Int
  -> StatusUpdateTypes.CommitBuildWrapperTuple
  -> [Text]
genMatchedBuildSection total_count idx wrapped_build_with_log_context = [
    M.heading 4 $ T.unwords [
        circleci_image_link
      , M.link job_name circleci_build_url
      , M.parens $ T.pack $ MyUtils.renderFrac idx total_count
      ]
  , T.unwords summary_info_pieces
  ] <> M.detailsExpanderForCode single_match_line code_block_lines

  where
    (wrapped_commit_build, build_with_log_context) = wrapped_build_with_log_context

    (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild storable_build match_obj _ failure_mode) (CommitBuilds.LogContext _ log_lines)) = build_with_log_context

    single_match_line = M.codeInlineHtml $ sanitizeLongLine $ MatchOccurrences._line_text match_obj
    Builds.StorableBuild (DbHelpers.WithId ubuild_id universal_build) build_obj = storable_build

    job_name = Builds.job_name build_obj

    supplemental_commi_build_info = CommitBuilds._supplemental wrapped_commit_build
    is_confirmed_non_flaky = SqlRead.has_completed_rerun supplemental_commi_build_info
      && not (SqlRead.is_empirically_determined_flaky supplemental_commi_build_info)

    not_flaky_confirmation_text = M.htmlAngleBrackets $ T.unwords [
        "confirmed not flaky by"
      , M.bold $ MyUtils.pluralize (SqlRead.failure_count supplemental_commi_build_info) "failure"
      ]

    optional_flakiness_indicator
       | is_confirmed_non_flaky  = [not_flaky_confirmation_text]
       | CommitBuilds._is_flaky failure_mode = [":snowflake:"]
       | otherwise = []

    summary_info_pieces = [
        M.bold "Step:"
      , M.quote $ MatchOccurrences._build_step match_obj
      , M.parens $ T.intercalate " | " [
         M.link "full log" $ LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/api/view-log-full?build_id=" <> T.pack (show ubuild_id)
        , M.link "pattern match details" $ LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)
        ]
      ] ++ optional_flakiness_indicator

    code_block_lines = NE.toList $ M.codeBlockFromList $
      dropWhileEnd T.null $ map renderLogLineTuple log_lines

    circleci_build_url = circleCIBuildUrlPrefix <> T.pack (show provider_build_number)
    (Builds.NewBuildNumber provider_build_number) = Builds.provider_buildnum universal_build
    circleci_icon = M.image "See CircleCI build" circleCISmallAvatarUrl
    circleci_image_link = M.link circleci_icon circleci_build_url


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
  [footer_section]
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

    dr_ci_base_url = LT.toStrict CommentRenderCommon.webserverBaseUrl
    opt_out_url = dr_ci_base_url <> "/admin/comments-opt-out.html"


generateCommentMarkdown ::
     Maybe SqlRead.PostedPRComment
  -> CommentRenderCommon.PrCommentPayload
  -> Builds.RawCommit
  -> Text
generateCommentMarkdown
    maybe_previous_pr_comment
    (CommentRenderCommon.NewPrCommentPayload middle_sections _)
    (Builds.RawCommit sha1_text) =

  T.unlines $ intercalate ["", "---"] $ filter (not . null) $
    [[intro_section]] ++ middle_sections ++ [generateCommentFooter maybe_previous_pr_comment]

  where
    intro_section = T.unlines [
        M.heading 2 ":pill: CircleCI build failures summary and remediations"
      , M.colonize [
          "As of commit"
        , T.take Constants.gitCommitPrefixLength sha1_text
        , M.parens $ T.unwords [
            "more details"
          , M.link "on the Dr. CI page" dr_ci_commit_details_link
          ]
        ]
      ]

    dr_ci_commit_details_link = LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/commit-details.html?sha1=" <> sha1_text




generateMiddleSections ::
     GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> StatusUpdateTypes.CommitPageInfo
  -> Builds.RawCommit
  -> CommentRenderCommon.PrCommentPayload
generateMiddleSections
    ancestry_result
    build_summary_stats
    commit_page_info
    commit =

  CommentRenderCommon.NewPrCommentPayload sections is_all_no_fault_failures
  where
    sections = [summary_header] ++ [[summary_tree]] ++ build_failures_table_sections

    (summary_header, summary_forrest, is_all_no_fault_failures) = genMetricsTree
      commit_page_info
      ancestry_result
      build_summary_stats
      commit

    summary_tree = M.bulletTree summary_forrest

    build_failures_table_sections = genBuildFailuresTable commit_page_info


genMetricsTree ::
     StatusUpdateTypes.CommitPageInfo
  -> GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> Builds.RawCommit
  -> ([Text], Tr.Forest (NonEmpty Text), Bool)
genMetricsTree
    commit_page_info
    ancestry_response
    (StatusUpdateTypes.NewBuildSummaryStats pre_broken_info all_failures)
    (Builds.RawCommit commit_sha1_text) =

  (summary_header, forrest_parts, not has_user_caused_failures)
  where

    has_user_caused_failures = broken_in_pr_count > 0

    forrest_parts = concat [
        introduced_failures_section
      , flaky_bullet_tree
      , optional_kb_metric
      ]

    summary_header = if has_user_caused_failures
      then mempty
      else [

        T.unwords [
            ":white_check_mark:"
          , M.bold "None of the build failures appear to be your fault"
          , ":green_heart:"
          ]
        ]

    optional_kb_metric = [upstream_breakage_bullet_tree | not $ HashMap.null pre_broken_jobs_map]
    introduced_failures_section = [introduced_failures_section_inner | broken_in_pr_count > 0]
    flaky_bullet_tree = [flaky_bullet_tree_inner | tentatively_flaky_count > 0]


    GadgitFetch.AncestryPropositionResponse _ ancestry_result = ancestry_response

    merge_base_commit = SqlUpdate.merge_base pre_broken_info
    Builds.RawCommit merge_base_sha1_text = merge_base_commit


    pre_broken_jobs_map = SqlUpdate.inferred_upstream_breakages_by_job pre_broken_info

    upstream_broken_count = HashMap.size pre_broken_jobs_map
    total_failcount = length all_failures

    broken_in_pr_count = total_failcount - upstream_broken_count - tentatively_flaky_count

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

    rebase_advice_section = CommentRebaseAdvice.genRebaseAdviceSection ancestry_result

    upstream_breakage_bullet_tree = pure $
       upstream_brokenness_declaration :| rebase_advice_section

    introduced_failures_section_inner = pure $ pure $ T.unwords [
        bold_fraction broken_in_pr_count total_failcount
      , "failures introduced in this PR"
      ]


    tentatively_flaky_builds_partition = StatusUpdateTypes.tentatively_flaky_builds $
      StatusUpdateTypes.pattern_matched_builds $ StatusUpdateTypes.nonupstream_builds commit_page_info


    tentatively_flaky_count_foo = StatusUpdateTypes.count tentatively_flaky_builds_partition

    tentatively_flaky_count = trace (unwords ["tentative_flaky_triggered_reruns count:", show $ StatusUpdateTypes.count $ StatusUpdateTypes.tentative_flaky_triggered_reruns tentatively_flaky_builds_partition, "tentative_flaky_untriggered_reruns count:", show $ StatusUpdateTypes.count $ StatusUpdateTypes.tentative_flaky_untriggered_reruns tentatively_flaky_builds_partition]) tentatively_flaky_count_foo




    flaky_bullet_tree_inner_heading = pure $ T.unwords [
        bold_fraction tentatively_flaky_count total_failcount
      , "tentatively recognized as flaky :snowflake:"
      ]

    rebuild_request_query_parms = [
        ("sha1", T.unpack commit_sha1_text)
      ]

    rerun_trigger_url = LT.unpack CommentRenderCommon.webserverBaseUrl <> "/rebuild-from-pr-comment.html?"
      <> MyUtils.genUrlQueryString rebuild_request_query_parms

    flaky_bullet_tree_inner = Tr.Node
      flaky_bullet_tree_inner_heading
      [pure $ pure $ M.link "Click here to rerun these jobs" $ T.pack rerun_trigger_url]


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
