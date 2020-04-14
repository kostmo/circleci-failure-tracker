{-# LANGUAGE OverloadedStrings #-}

module CommentRender where

import qualified Data.HashMap.Strict   as HashMap
import           Data.List             (dropWhileEnd, intercalate, intersperse)
import           Data.List.Extra       (maximumOn)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NE
import qualified Data.Maybe            as Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as LT
import           Data.Time             (UTCTime)
import qualified Data.Time.Format      as TF
import           Data.Time.LocalTime   (utcToZonedTime)
import qualified Data.Tree             as Tr

import qualified Builds
import qualified CircleCIParse
import qualified CommentRebaseAdvice
import qualified CommentRenderCommon
import qualified CommitBuilds
import qualified Constants
import qualified DbHelpers
import qualified GadgitFetch
import qualified GithubChecksApiData
import qualified GithubChecksApiFetch
import qualified Markdown              as M
import qualified MatchOccurrences
import qualified MyUtils
import qualified Sql.Read.PullRequests as ReadPullRequests
import qualified Sql.Read.Types        as SqlReadTypes
import qualified Sql.Update            as SqlUpdate
import qualified StatusEventQuery
import qualified StatusUpdateTypes
import qualified UnmatchedBuilds


xlaContacts :: [Text]
xlaContacts = [
    "ailzhang"
  , "dlibenzi"
  , "jackCaoG"
  ]


drCiGitHubRepoBase :: Text
drCiGitHubRepoBase = "https://github.com/kostmo/circleci-failure-tracker"


drCiIssueTrackerUrl :: Text
drCiIssueTrackerUrl = drCiGitHubRepoBase <> "/issues"

circleCISmallAvatarUrl :: Text
circleCISmallAvatarUrl = "https://avatars0.githubusercontent.com/ml/7?s=12"


drCIApplicationTitle :: Text
drCIApplicationTitle = "Dr. CI"


drCIPullRequestCommentsReadmeUrl :: Text
drCIPullRequestCommentsReadmeUrl = drCiGitHubRepoBase <> "/tree/master/docs/from-pull-request-comment/README.md"


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
      , "Action"
      ]

    data_rows = map gen_unmatched_build_row unmatched_nonupstream_builds

    gen_unmatched_build_row (UnmatchedBuilds.UnmatchedBuild _build step_name _ job_name _ universal_build_number _ _) = [
        T.unwords [
          M.image "CircleCI" circleCISmallAvatarUrl
        , M.sup job_name
        ]
      , M.sup step_name
      , rerun_link
      ]
      where
        rerun_link = renderSingleJobRerunLink universal_build_number


genTimedOutSection ::
     [UnmatchedBuilds.UnmatchedBuild]
  -> Maybe (Tr.Tree NodeType)
genTimedOutSection
  timed_out_builds =
  if null timed_out_builds
  then Nothing
  else Just $ pure $ LeafNode $ NewFailureSection
    NonUpstream
    NonFlaky
    (UnmatchedBuildMembers timed_out_builds)
    jobs_header
    jobs_list
  where
    jobs_header = M.colonize [
        MyUtils.pluralize (length timed_out_builds) "job"
      , M.bold "timed out"
      ]

    make_list_item = ((\x -> "* " <> M.codeInline x) . UnmatchedBuilds._job_name)
    jobs_list = map make_list_item timed_out_builds


genSpecialCasedNonupstreamSection ::
     StatusUpdateTypes.SpecialCasedBuilds SqlReadTypes.StandardCommitBuildWrapper
  -> Maybe (Tr.Tree NodeType)
genSpecialCasedNonupstreamSection
  (StatusUpdateTypes.NewSpecialCasedBuilds xla_build_failures) =
  if null xla_build_failures
  then Nothing
  else Just $ pure $ LeafNode $ NewFailureSection
    NonUpstream
    NonFlaky
    (SpecialCasedMembers xla_build_failures)
    (T.unlines [M.heading 3 "XLA failure", explanation_paragraph])
    xla_match_details_block

  where
    -- TODO
    xla_match_details_block = []
{-
    xla_match_details_block = zipWith (genMatchedBuildSection $ length xla_build_failures)
        [1..] xla_build_failures
-}

    explanation_paragraph = T.unwords $
      map M.sentence explanation_sentences

    explanation_sentences = [
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

    build_name_list = T.intercalate ", " $
      map (M.bold . get_job_name) xla_build_failures

    get_job_name = Builds.job_name . Builds.build_record . CommitBuilds._build . CommitBuilds._commit_build

    rendered_xla_contacts = T.intercalate " / " $
      map (T.cons '@') xlaContacts


data Upstreamness = Upstream | NonUpstream


data Flakiness = Flaky | NonFlaky


data NodeType = InteriorNode Text | LeafNode FailureSection


data BuildMembers =
    UnmatchedBuildMembers [UnmatchedBuilds.UnmatchedBuild]
  | TupleBuildMembers [SqlReadTypes.CommitBuildWrapperTuple]
  | TentativeFlakyBuildMembers (StatusUpdateTypes.TentativeFlakyBuilds SqlReadTypes.CommitBuildWrapperTuple)
  | UpstreamBreakageMembers [(SqlReadTypes.StandardCommitBuildWrapper, SqlReadTypes.UpstreamBrokenJob)]
  | SpecialCasedMembers [SqlReadTypes.StandardCommitBuildWrapper]
  | RawGitHubEventMembers [StatusEventQuery.GitHubStatusEventGetter]
  | GitHubCheckRunMembers [GithubChecksApiFetch.GitHubCheckRunsEntry]


getFailureCount :: BuildMembers -> Int
getFailureCount (UnmatchedBuildMembers x)      = length x
getFailureCount (TupleBuildMembers x)          = length x
getFailureCount (TentativeFlakyBuildMembers x) = StatusUpdateTypes.count x
getFailureCount (UpstreamBreakageMembers x)    = length x
getFailureCount (SpecialCasedMembers x)        = length x
getFailureCount (RawGitHubEventMembers x)      = length x
getFailureCount (GitHubCheckRunMembers x)      = length x


countNonCircleCINonFacebookFailures :: BuildMembers -> Int

countNonCircleCINonFacebookFailures (RawGitHubEventMembers x) = length x
countNonCircleCINonFacebookFailures (GitHubCheckRunMembers x) = length x
countNonCircleCINonFacebookFailures _                         = 0



data FailureSection = NewFailureSection {
    upstreamness  :: Upstreamness
  , flakiness     :: Flakiness
  , build_members :: BuildMembers
  , intro_blurb   :: Text
  , details_lines :: [Text]
  }


genNewFailuresSections nonupstream_builds =
  Just $ Tr.Node (InteriorNode "New failures") $
    Maybe.catMaybes new_failure_internal_node_maybes
  where

    new_failure_internal_node_maybes = [
        maybe_determinisitic_failures_node
      , maybe_nondeterminisitic_failures_node
      ]

    maybe_determinisitic_failures_node = if null extant_deterministic_children
      then Nothing
      else Just $ Tr.Node (InteriorNode "Determinisitic failures") extant_deterministic_children

    maybe_nondeterminisitic_failures_node = if null extant_nondeterministic_children
      then Nothing
      else Just $ Tr.Node (InteriorNode "Flaky failures") extant_nondeterministic_children

    extant_deterministic_children = Maybe.catMaybes deterministic_failure_node_maybes

    extant_nondeterministic_children = Maybe.catMaybes nondeterministic_failure_node_maybes


    nondeterministic_failure_node_maybes = [
        timed_out_section
      , maybe_tentative_flaky_section
      , maybe_confirmed_flaky_section
      ]

    deterministic_failure_node_maybes = [
        nonupstream_nonflaky_pattern_matched_section
      , pattern_unmatched_section
      , special_cased_nonupstream_section
      ]


    StatusUpdateTypes.NewNonUpstreamBuildPartition
      pattern_matched_builds
      unmatched_nonupstream_builds
      special_cased_builds
      timed_out_builds = nonupstream_builds

    special_cased_nonupstream_section = genSpecialCasedNonupstreamSection special_cased_builds

    timed_out_section = genTimedOutSection timed_out_builds

    (nonupstream_nonflaky_pattern_matched_section, maybe_tentative_flaky_section, maybe_confirmed_flaky_section) = genPatternMatchedSections pattern_matched_builds

    pattern_unmatched_header = M.heading 3 $ M.colonize [
        MyUtils.pluralize (length unmatched_nonupstream_builds) "failure"
      , M.italic "not"
      , "recognized by patterns"
      ]

    pattern_unmatched_section = if null unmatched_nonupstream_builds
      then Nothing
      else Just $ pure $ LeafNode $ NewFailureSection
        NonUpstream
        NonFlaky
        (UnmatchedBuildMembers unmatched_nonupstream_builds)
        pattern_unmatched_header
        (NE.toList $ genUnmatchedBuildsTable unmatched_nonupstream_builds)


genCheckRunsSection ::
     [GithubChecksApiFetch.GitHubCheckRunsEntry]
  -> Maybe (Tr.Tree NodeType)
genCheckRunsSection failed_check_run_entries_excluding_facebook =
  if null failed_check_run_entries_excluding_facebook
  then Nothing
  else Just $ pure $ LeafNode $ NewFailureSection
    NonUpstream
    NonFlaky
    (GitHubCheckRunMembers failed_check_run_entries_excluding_facebook)
    (M.heading 3 "Extra GitHub checks")
    failed_run_bullets
  where
    failed_run_bullets = map gen_bullet failed_check_run_entries_excluding_facebook
    gen_bullet x = T.unwords [
        "*"
      , M.bold "Failed:"
      , M.link link_label $ GithubChecksApiFetch.html_url x
      ]
      where
        link_label = T.intercalate " - " [
            GithubChecksApiData.name $ GithubChecksApiFetch.app x
          , M.codeInline $ GithubChecksApiFetch.name x
          ]


genOtherProviderSection ::
     StatusUpdateTypes.NonCircleCIItems
  -> Maybe (Tr.Tree NodeType)
genOtherProviderSection non_circle_items =
  if null provider_output_nodes
  then Nothing
  else Just $ Tr.Node
    (InteriorNode "Non-CircleCI jobs")
    provider_output_nodes

  where
    StatusUpdateTypes.NewNonCircleCIItems non_circleci_provided_statuses check_runs = non_circle_items

    maybe_check_runs_node = genCheckRunsSection check_runs

    provider_output_nodes = Maybe.catMaybes $ maybe_check_runs_node : provider_maybes
    provider_maybes = map gen_provider_maybe non_circleci_provided_statuses

    -- TODO these lists are guaranteed nonempty!
    gen_provider_maybe (nonempty_failed_event_list, provider_info) =

      Just $ pure $ LeafNode $ NewFailureSection
          NonUpstream
          NonFlaky
          (RawGitHubEventMembers failed_event_list)
          (M.heading 3 $ T.pack $ get_provider_string provider_info)
          reportable_event_bullets
      where
        failed_event_list = NE.toList nonempty_failed_event_list
        get_provider_string (DbHelpers.WithId _ (SqlReadTypes.CiProviderHostname hostname_string)) = hostname_string

        mk_bullet x = T.unwords [
            "*"
          , M.bold "Failed:"
          , M.link (M.codeInline $ LT.toStrict $ StatusEventQuery._context x) $ LT.toStrict $ StatusEventQuery._target_url x
          ]

        reportable_event_bullets = map mk_bullet failed_event_list


genBuildFailuresSections ::
     StatusUpdateTypes.CommitPageInfo
  -> Tr.Tree NodeType
genBuildFailuresSections commit_page_info =

  Tr.Node (InteriorNode "All failures") $ Maybe.catMaybes maybe_nodes

  where
    StatusUpdateTypes.NewCommitPageInfo toplevel_partitioning raw_github_statuses = commit_page_info

    maybe_nodes = [
        genNewFailuresSections nonupstream_builds
      , genUpstreamFailuresSection upstream_breakages
      , genOtherProviderSection $ StatusUpdateTypes.non_circleci_items raw_github_statuses
      ]

    StatusUpdateTypes.NewUpstreamnessBuildsPartition upstream_breakages nonupstream_builds = toplevel_partitioning


genPatternMatchedSections pattern_matched_builds =
  (nonupstream_nonflaky_pattern_matched_section, maybe_tentative_flaky_section, maybe_confirmed_flaky_section)
  where
    StatusUpdateTypes.NewFlakyBuildPartition
      tentatively_flaky_builds
      classified_nonflaky_builds
      confirmed_flaky_builds
        = pattern_matched_builds

    StatusUpdateTypes.NewNonFlakyBuilds nonflaky_by_pattern nonflaky_by_empirical_confirmation = classified_nonflaky_builds

    all_nonflaky_builds = nonflaky_by_pattern ++ nonflaky_by_empirical_confirmation

    nonupstream_nonflaky_pattern_matched_section = if null all_nonflaky_builds
      then Nothing
      else Just $ pure $ LeafNode $ NewFailureSection
        NonUpstream
        NonFlaky
        (TupleBuildMembers all_nonflaky_builds)
        nonupstream_nonflaky_pattern_matched_header
        (pure non_upstream_nonflaky_intro_text
          <> nonflaky_matched_builds_details_block)

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

    (maybe_tentative_flaky_section, maybe_confirmed_flaky_section) =
      genFlakySections
        tentatively_flaky_builds
        confirmed_flaky_builds


genFlakySections ::
     StatusUpdateTypes.TentativeFlakyBuilds SqlReadTypes.CommitBuildWrapperTuple
  -> [SqlReadTypes.CommitBuildWrapperTuple]
  -> (Maybe (Tr.Tree NodeType), Maybe (Tr.Tree NodeType))
genFlakySections
    tentative_flakies
    confirmed_flaky_builds =

  (maybe_tentative_flaky_section, maybe_confirmed_flaky_section)

  where
    StatusUpdateTypes.NewTentativeFlakyBuilds tentative_flaky_triggered_reruns tentative_flaky_untriggered_reruns = tentative_flakies


    get_job_name = Builds.job_name . Builds.build_record . CommitBuilds._build . CommitBuilds._commit_build . fst

    maybe_confirmed_flaky_section = if null confirmed_flaky_builds
      then Nothing
      else Just $ pure $ LeafNode $ NewFailureSection
        NonUpstream
        Flaky
        (TupleBuildMembers confirmed_flaky_builds)
        (M.colonize [
            MyUtils.pluralize (length confirmed_flaky_builds) "failure"
          , M.bold "confirmed as flaky"
          , "and can be ignored"
          ])
        (map ((\x -> "* " <> M.codeInline x) . get_job_name) confirmed_flaky_builds)

    total_tentative_flaky_count = StatusUpdateTypes.count tentative_flakies


    maybe_tentative_flaky_section = if total_tentative_flaky_count > 0
      then Just $ pure $ LeafNode $ NewFailureSection
        NonUpstream
        Flaky
        (TentativeFlakyBuildMembers tentative_flakies)
        tentative_flakies_header
        (untriggered_subsection
            <> triggered_subsection)

      else Nothing

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
      , "but reruns have not yet been triggered to confirm"
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
formatBreakageTimeSpan
    (BreakageTimeSpan breakage_start_time maybe_breakage_end original_obj) =

  T.unwords breakage_span_words_list
  where
    inTimeZone = utcToZonedTime $ read "PDT"

    ft_date_only = T.pack . TF.formatTime TF.defaultTimeLocale "%b %d" . inTimeZone
    ft_time_only = T.pack . TF.formatTime TF.defaultTimeLocale "%l:%M%P" . inTimeZone

    ft_timezone = T.pack . TF.formatTime TF.defaultTimeLocale "%Z" . inTimeZone

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
           , ft_timezone end_time
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


genUpstreamFailuresSection ::
     [(SqlReadTypes.StandardCommitBuildWrapper, SqlReadTypes.UpstreamBrokenJob)]
  -> Maybe (Tr.Tree NodeType)
genUpstreamFailuresSection upstream_breakages =
  if null upstream_breakages
    then Nothing
    else Just $ Tr.Node (InteriorNode "Upstream failures") $
      pure $ pure $ LeafNode $ NewFailureSection
        Upstream
        NonFlaky
        (UpstreamBreakageMembers upstream_breakages)
        upstream_matched_header
          (pure upstream_intro_text
          <> pure matched_upstream_builds_details_block)
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
      Tr.Node node_text node_children

      where
        node_text = pure $ T.unwords [
            M.link link_label link_url
          , breakage_span_words
          ]

        node_children = pure $ pure $ pure $
          renderSingleJobRerunLink $ Builds.UniversalBuildId ubuild_id

        CommitBuilds.NewCommitBuild y _match_obj _ _ = CommitBuilds._commit_build x
        Builds.StorableBuild (DbHelpers.WithId ubuild_id _universal_build) build_obj = y

        breakage_span_words = formatBreakageTimeSpan $ toBreakageTimeSpan upstream_cause

        link_label = Builds.job_name build_obj
        link_url = LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)


renderSingleJobRerunLink :: Builds.UniversalBuildId -> Text
renderSingleJobRerunLink (Builds.UniversalBuildId ubuild_id) =
  M.link ":repeat: rerun" single_build_rerun_trigger_url
  where
    single_rebuild_request_query_parms = [
        ("build-id", show ubuild_id)
      ]

    single_build_rerun_trigger_url = T.pack $ LT.unpack CommentRenderCommon.webserverBaseUrl <> "/rebuild-from-pr-comment.html?"
      <> MyUtils.genUrlQueryString single_rebuild_request_query_parms


genMatchedBuildSection ::
     Int
  -> Int
  -> SqlReadTypes.CommitBuildWrapperTuple
  -> [Text]
genMatchedBuildSection
    total_count
    idx
    wrapped_build_with_log_context =

  [ M.heading 4 $ T.unwords [
        circleci_image_link
      , M.link job_name circleci_build_url
      , M.parens $ T.pack $ MyUtils.renderFrac idx total_count
      ]
  , T.unwords summary_info_pieces
  ] <> M.detailsExpanderForCode single_match_line code_block_lines

  where
    (wrapped_commit_build, build_with_log_context) = wrapped_build_with_log_context

    CommitBuilds.NewCommitBuild storable_build match_obj _ failure_mode = commit_build
    CommitBuilds.BuildWithLogContext commit_build (CommitBuilds.LogContext _ log_lines) = build_with_log_context

    single_match_line = M.codeInlineHtml $ sanitizeLongLine $ MatchOccurrences._line_text match_obj
    Builds.StorableBuild (DbHelpers.WithId ubuild_id universal_build) build_obj = storable_build

    job_name = Builds.job_name build_obj

    supplemental_commit_build_info = CommitBuilds._supplemental wrapped_commit_build
    is_confirmed_non_flaky = SqlReadTypes.has_completed_rerun supplemental_commit_build_info
      && not (SqlReadTypes.is_empirically_determined_flaky supplemental_commit_build_info)

    not_flaky_confirmation_text = M.htmlAngleBrackets $ T.unwords [
        "confirmed"
      , M.bold "not flaky"
      , "by"
      , M.bold $ MyUtils.pluralize (SqlReadTypes.failure_count supplemental_commit_build_info) "failure"
      ]

    optional_flakiness_indicator
       | is_confirmed_non_flaky  = [not_flaky_confirmation_text]
       | CommitBuilds._is_flaky failure_mode = [":snowflake:"]
       | otherwise = []

    console_log_link = M.link "full log" $
      LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/api/view-log-full?build_id=" <> T.pack (show ubuild_id)

    pattern_match_details_link = M.link "pattern match details" $
      LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/build-details.html?build_id=" <> T.pack (show ubuild_id)

    rerun_link = renderSingleJobRerunLink $ Builds.UniversalBuildId ubuild_id

    single_build_quick_links = [
        console_log_link
      , pattern_match_details_link
      , rerun_link
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
     Maybe ReadPullRequests.PostedPRComment
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

    performance_link = M.sentence [
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
        , MyUtils.pluralize (ReadPullRequests._revision_count previous_pr_comment) "time"
        ]
      ]

    dr_ci_base_url = LT.toStrict CommentRenderCommon.webserverBaseUrl
    opt_out_url = dr_ci_base_url <> "/admin/comments-opt-out.html"


generateCommentMarkdown ::
     Maybe ReadPullRequests.PostedPRComment
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
    dr_ci_commit_details_link = M.link
      (T.unwords ["on the", drCIApplicationTitle, "page"])
      dr_ci_commit_details_url

    intro_section = T.unlines [
        M.heading 2 ":pill: Build failures summary and remediations"
      , M.colonize [
          "As of commit"
        , T.take Constants.gitCommitPrefixLength sha1_text
        , M.parens $ T.unwords [
            "more details"
          , dr_ci_commit_details_link
          ]
        ]
      ]

    dr_ci_commit_details_url = LT.toStrict CommentRenderCommon.webserverBaseUrl <> "/commit-details.html?sha1=" <> sha1_text


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
    sections = [summary_header] ++ [summary_tree_lines] ++ build_failures_section_line_groups

    just_get_leaf y = case y of
      LeafNode x -> Just x
      _          -> Nothing

    get_section_lines x = intro_blurb x : details_lines x

    extant_failure_sections = Maybe.mapMaybe just_get_leaf $
      Tr.flatten build_failures_table_sections

    non_circleci_non_facebook_failure_count = sum $
      map (countNonCircleCINonFacebookFailures . build_members) extant_failure_sections


    build_failures_section_line_groups = map get_section_lines extant_failure_sections

    (summary_header, summary_forrest, is_all_no_fault_failures, is_all_successful_circleci_builds) = genMetricsTree
      commit_page_info
      ancestry_result
      build_summary_stats
      non_circleci_non_facebook_failure_count
      commit

    summary_tree_lines = if null summary_forrest
      then mempty
      else [M.bulletTree summary_forrest]

    build_failures_table_sections = genBuildFailuresSections commit_page_info


genMetricsTree ::
     StatusUpdateTypes.CommitPageInfo
  -> GadgitFetch.AncestryPropositionResponse
  -> StatusUpdateTypes.BuildSummaryStats
  -> Int -- ^ non-CircleCI failure count
  -> Builds.RawCommit
  -> ([Text], Tr.Forest (NonEmpty Text), Bool, Bool)
genMetricsTree
    x_commit_page_info
    ancestry_response
    (StatusUpdateTypes.NewBuildSummaryStats pre_broken_info all_circleci_failures)
    non_circleci_non_facebook_failure_count
    (Builds.RawCommit commit_sha1_text) =

  (summary_header, forrest_parts, not has_user_caused_failures, is_all_successful_builds)
  where

    toplevel_builds = StatusUpdateTypes.toplevel_partitioning x_commit_page_info

    is_all_successful_builds = null all_circleci_failures && non_circleci_non_facebook_failure_count == 0

    has_user_caused_failures = broken_in_pr_count > 0 || non_circleci_non_facebook_failure_count > 0

    forrest_parts = concat [
        introduced_failures_section
      , flaky_bullet_tree
      , optional_kb_metric
      ]


    optional_non_circlecli_non_facebook_section =
      [non_circlecli_non_facebook_bullet_tree | non_circleci_non_facebook_failure_count > 0]

    non_circlecli_non_facebook_bullet_tree = pure $ non_circlecli_non_facebook_summary_line :| []
    non_circlecli_non_facebook_summary_line = T.unwords [
        bold_fraction non_circleci_non_facebook_failure_count total_failcount
      , "non-CircleCI failure(s)"
      ]

    summary_header
      | has_user_caused_failures = mempty
      | is_all_successful_builds = pure $ T.unwords [
          ":green_heart:"
        , ":green_heart:"
        , M.bold "Looks good so far! There are no failures yet."
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
    total_failcount = length all_circleci_failures + non_circleci_non_facebook_failure_count

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


    optional_qualified_possibly_asterisk = "possibly" <> M.linkWithTooltip
      "\\*"
      (drCiGitHubRepoBase <> "/tree/master/docs/from-pull-request-comment/LIMITATIONS.md")
      "Currently, non-CircleCI failures are not distinguished as upstream failures"

    introduced_failures_header_words = [
        bold_fraction broken_in_pr_count total_failcount
      , "failures"
      ] ++ [optional_qualified_possibly_asterisk | non_circleci_non_facebook_failure_count > 0] ++ [
        "introduced in this PR"
      ]

    introduced_failures_header_text = pure $ T.unwords introduced_failures_header_words
    introduced_failures_section_inner = Tr.Node introduced_failures_header_text optional_non_circlecli_non_facebook_section


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
