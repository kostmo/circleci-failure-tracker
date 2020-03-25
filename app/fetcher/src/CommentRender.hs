{-# LANGUAGE OverloadedStrings #-}

module CommentRender where

import qualified Data.HashMap.Strict as HashMap
import           Data.List           (dropWhileEnd, intercalate, intersperse)
import           Data.List.Extra     (maximumOn)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Data.Time           (UTCTime)
import qualified Data.Time.Format    as TF
import qualified Data.Tree           as Tr

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
import qualified Sql.ReadTypes       as SqlReadTypes
import qualified Sql.Update          as SqlUpdate
import qualified StatusUpdateTypes
import qualified UnmatchedBuilds


xlaContacts :: [Text]
xlaContacts = [
    "ailzhang"
  , "dlibenzi"
  , "jackCaoG"
  ]


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


genTimedOutSection ::
     [UnmatchedBuilds.UnmatchedBuild]
  -> [Text]
genTimedOutSection
  timed_out_builds =
  if null timed_out_builds
  then mempty
  else NE.toList $ M.colonize [
          MyUtils.pluralize (length timed_out_builds) "job"
        , M.bold "timed out"
        ] :| map ((\x -> "* " <> M.codeInline x) . UnmatchedBuilds._job_name) timed_out_builds


genSpecialCasedNonupstreamSection ::
     StatusUpdateTypes.SpecialCasedBuilds SqlReadTypes.StandardCommitBuildWrapper
  -> [Text]
genSpecialCasedNonupstreamSection
  (StatusUpdateTypes.NewSpecialCasedBuilds xla_build_failures) =
  if null xla_build_failures
  then mempty
  else pure $ T.unwords $ map M.sentence [
      [ "Job"
      , build_name_list
      , "is failing"
      ]
    , [ "Please create an issue with title prefixed by"
      , M.codeInline "[PT_BREAK]"
      , "in"
      , M.link
          (M.codeInline "pytorch/xla")
          "https://github.com/pytorch/xla/issues"
      , "and link to to this PR"
      ]
    , [ "If you have questions, please reach out to"
      , rendered_xla_contacts
      ]
    ]
  where
    build_name_list = T.intercalate ", " $
      map (M.bold . get_job_name) xla_build_failures

    get_job_name = Builds.job_name . Builds.build_record . CommitBuilds._build . CommitBuilds._commit_build

    rendered_xla_contacts = T.intercalate "/" $
      map (T.cons '@') xlaContacts


genBuildFailuresSections ::
     StatusUpdateTypes.CommitPageInfo
  -> [[Text]]
genBuildFailuresSections
    (StatusUpdateTypes.NewCommitPageInfo toplevel_partitioning) =
  pattern_matched_sections ++ [
      pattern_unmatched_section
    , timed_out_section
    , special_cased_nonupstream_section
    , upstream_matched_section
    ]
  where

    StatusUpdateTypes.NewUpstreamnessBuildsPartition upstream_breakages nonupstream_builds = toplevel_partitioning

    StatusUpdateTypes.NewNonUpstreamBuildPartition
      pattern_matched_builds
      unmatched_nonupstream_builds
      special_cased_builds
      timed_out_builds = nonupstream_builds

    special_cased_nonupstream_section = genSpecialCasedNonupstreamSection special_cased_builds

    timed_out_section = genTimedOutSection timed_out_builds

    pattern_matched_sections = genPatternMatchedSections pattern_matched_builds

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


    get_job_name = Builds.job_name . Builds.build_record . CommitBuilds._build . CommitBuilds._commit_build . fst

    confirmed_flaky_section = if null confirmed_flaky_builds
      then mempty
      else NE.toList $ M.colonize [
          MyUtils.pluralize (length confirmed_flaky_builds) "failure"
        , M.bold "confirmed as flaky"
        , "and can be ignored"
        ] :| map ((\x -> "* " <> M.codeInline x) . get_job_name) confirmed_flaky_builds

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
      , "but have not triggered reruns to confirm"
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


data BreakageSpanEndInfo = BreakageSpanEndInfo {
    end_time    :: UTCTime
  , span_length :: Int
  }

data BreakageTimeSpan = BreakageTimeSpan {
    start_time   :: UTCTime
  , maybe_end    :: Maybe BreakageSpanEndInfo
  , original_obj :: SqlReadTypes.UpstreamBrokenJob
  }


toBreakageTimeSpan upstream_cause = BreakageTimeSpan
  (SqlReadTypes._breakage_start_time upstream_cause)
  end_info
  upstream_cause
  where
    end_info = BreakageSpanEndInfo
      <$> SqlReadTypes._breakage_end_time upstream_cause
      <*> SqlReadTypes._span_length upstream_cause


formatBreakageTimeSpan :: BreakageTimeSpan -> Text
formatBreakageTimeSpan (BreakageTimeSpan breakage_start_time maybe_breakage_end original_obj) =
  T.unwords breakage_span_words_list
  where
    ft_date_only = T.pack . TF.formatTime TF.defaultTimeLocale "%b %d"
    ft_time_only = T.pack . TF.formatTime TF.defaultTimeLocale "%l:%M%P"
    start_time_date_only = ft_date_only breakage_start_time

    breakage_span_words_list = case maybe_breakage_end of
      Just (BreakageSpanEndInfo end_time span_length) ->
        if start_time_date_only == end_time_date_only
        then [
             "on"
           , ft_date_only breakage_start_time
           , "from"
           , ft_time_only breakage_start_time
           , "to"
           , ft_time_only end_time
           , commit_count_blurb
           ]
         else [
             "from"
           , start_time_date_only
           , "until"
           , end_time_date_only
           , commit_count_blurb
           ]
         where
           render_raw_commit (Builds.RawCommit x) = T.take Constants.gitCommitPrefixLength x

           end_time_date_only = ft_date_only end_time
           commit_count_blurb = M.parens $ T.unwords [
               MyUtils.pluralize span_length "commit" <> ";"
             , render_raw_commit $ SqlReadTypes._breakage_start_sha1 original_obj
             , "-"
               -- This Maybe should never be Nothing when the "end" timestamp is not Nothing
             , maybe "XXX" render_raw_commit $
                 SqlReadTypes._breakage_end_sha1 original_obj
             ]
      Nothing -> ["since", start_time_date_only]


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

        breakage_span_words = formatBreakageTimeSpan $ toBreakageTimeSpan upstream_cause

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
    is_confirmed_non_flaky = SqlReadTypes.has_completed_rerun supplemental_commi_build_info
      && not (SqlReadTypes.is_empirically_determined_flaky supplemental_commi_build_info)

    not_flaky_confirmation_text = M.htmlAngleBrackets $ T.unwords [
        "confirmed"
      , M.bold "not flaky"
      , "by"
      , M.bold $ MyUtils.pluralize (SqlReadTypes.failure_count supplemental_commi_build_info) "failure"
      ]

    optional_flakiness_indicator
       | is_confirmed_non_flaky  = [not_flaky_confirmation_text]
       | CommitBuilds._is_flaky failure_mode = [":snowflake:"]
       | otherwise = []

    console_log_link = M.link "full log" $ LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/api/view-log-full?build_id=" <> T.pack (show ubuild_id)

    pattern_match_details_link = M.link "pattern match details" $ LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)

    {-
    single_rebuild_request_query_parms = [
        ("build-id", show ubuild_id)
      ]

    single_build_rerun_trigger_url = T.pack $ LT.unpack CommentRenderCommon.webserverBaseUrl <> "/rebuild-from-pr-comment.html?"
      <> MyUtils.genUrlQueryString single_rebuild_request_query_parms
    rerun_link = M.link "rerun this build" single_build_rerun_trigger_url
    -}

    single_build_quick_links = [
        console_log_link
      , pattern_match_details_link
--      , rerun_link
      ]

    summary_info_pieces = [
        M.bold "Step:"
      , M.quote $ MatchOccurrences._build_step match_obj
      , M.parens $ T.intercalate " | " single_build_quick_links
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
    carriage_return_chunks = T.split (== '\r') $ T.pack $
      CircleCIParse.filterAnsiCursorMovement $ T.unpack line_text


generateCommentFooter ::
     Maybe SqlRead.PostedPRComment
  -> Builds.PullRequestNumber
  -> [Text]
generateCommentFooter
    maybe_previous_pr_comment
    (Builds.PullRequestNumber pr_number) =

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
      ] ++ [performance_link] ++ optional_suffix

    performance_link_url = dr_ci_base_url <> "/posted-comment-details.html?pr=" <> T.pack (show pr_number)

    performance_link = T.unwords [
        "See"
      , M.link "how this bot performed" performance_link_url
      ]

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
  -> Builds.PullRequestNumber
  -> Builds.RawCommit
  -> Text
generateCommentMarkdown
    maybe_previous_pr_comment
    (CommentRenderCommon.NewPrCommentPayload middle_sections _ _)
    pr_number
    (Builds.RawCommit sha1_text) =

  T.unlines $ intercalate ["", "---"] $ filter (not . null) $
    [[intro_section]] ++ middle_sections ++ [generateCommentFooter maybe_previous_pr_comment pr_number]

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

  CommentRenderCommon.NewPrCommentPayload sections is_all_no_fault_failures is_all_successful_circleci_builds
  where
    sections = [summary_header] ++ [summary_tree_lines] ++ build_failures_table_sections

    (summary_header, summary_forrest, is_all_no_fault_failures, is_all_successful_circleci_builds) = genMetricsTree
      commit_page_info
      ancestry_result
      build_summary_stats
      commit

    summary_tree_lines = if null summary_forrest
      then mempty
      else [M.bulletTree summary_forrest]

    build_failures_table_sections = genBuildFailuresSections commit_page_info


genMetricsTree ::
     StatusUpdateTypes.CommitPageInfo
  -> GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> Builds.RawCommit
  -> ([Text], Tr.Forest (NonEmpty Text), Bool, Bool)
genMetricsTree
    x_commit_page_info
    ancestry_response
    (StatusUpdateTypes.NewBuildSummaryStats pre_broken_info all_failures)
    (Builds.RawCommit commit_sha1_text) =

  (summary_header, forrest_parts, not has_user_caused_failures, is_all_successful_circleci_builds)
  where

    toplevel_builds = StatusUpdateTypes.toplevel_partitioning x_commit_page_info

    is_all_successful_circleci_builds = null all_failures

    has_user_caused_failures = broken_in_pr_count > 0

    forrest_parts = concat [
        introduced_failures_section
      , flaky_bullet_tree
      , optional_kb_metric
      ]

    summary_header
      | has_user_caused_failures = mempty
      | is_all_successful_circleci_builds = pure $ T.unwords [
          ":green_heart:"
        , ":green_heart:"
        , M.bold "Looks good so far! There are no CircleCI failures yet."
        , ":green_heart:"
        , ":green_heart:"
        ]
      | otherwise = pure $ T.unwords [
          ":white_check_mark:"
        , M.bold "None of the build failures appear to be your fault"
        , ":green_heart:"
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

    latest_upstream_breakage = maximumOn SqlReadTypes._breakage_start_time $ HashMap.elems pre_broken_jobs_map

    upstream_brokenness_declaration = T.unwords [
        bold_fraction upstream_broken_count total_failcount
      , M.link "broken upstream" grid_view_url
      , "at merge base"
      , T.take Constants.gitCommitPrefixLength merge_base_sha1_text
      , formatBreakageTimeSpan $ toBreakageTimeSpan latest_upstream_breakage
      ]

    rebase_advice_section = CommentRebaseAdvice.genRebaseAdviceSection ancestry_result

    upstream_breakage_bullet_tree = pure $
       upstream_brokenness_declaration :| rebase_advice_section

    introduced_failures_section_inner = pure $ pure $ T.unwords [
        bold_fraction broken_in_pr_count total_failcount
      , "failures introduced in this PR"
      ]


    tentatively_flaky_builds_partition = StatusUpdateTypes.tentatively_flaky_builds $
      StatusUpdateTypes.pattern_matched_builds $ StatusUpdateTypes.my_nonupstream_builds toplevel_builds

    tentatively_flaky_count = StatusUpdateTypes.count tentatively_flaky_builds_partition

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
