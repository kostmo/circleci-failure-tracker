--
-- PostgreSQL database dump
--

-- Dumped from database version 10.6
-- Dumped by pg_dump version 11.4 (Ubuntu 11.4-1.pgdg18.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: loganci; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE loganci WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'en_US.UTF-8' LC_CTYPE = 'en_US.UTF-8';


ALTER DATABASE loganci OWNER TO postgres;

\connect loganci

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: work_queues; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA work_queues;


ALTER SCHEMA work_queues OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: build_steps; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.build_steps (
    id integer NOT NULL,
    name text,
    is_timeout boolean,
    universal_build integer NOT NULL
);


ALTER TABLE public.build_steps OWNER TO postgres;

--
-- Name: TABLE build_steps; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.build_steps IS 'There should be zero or one build steps associated with a build, depending on whether the CircleCI JSON build structure indicates that one of the steps failed.

This is known before the console logs are "scanned".';


--
-- Name: build_steps_deduped_mitigation; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.build_steps_deduped_mitigation WITH (security_barrier='false') AS
 SELECT DISTINCT ON (build_steps.universal_build) build_steps.id,
    build_steps.name,
    build_steps.is_timeout,
    build_steps.universal_build
   FROM public.build_steps
  ORDER BY build_steps.universal_build, build_steps.id DESC;


ALTER TABLE public.build_steps_deduped_mitigation OWNER TO postgres;

--
-- Name: VIEW build_steps_deduped_mitigation; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.build_steps_deduped_mitigation IS 'TODO: Add uniqueness constraint on "universal_build" column in "build_steps" table
Then remove this mitigation from "builds_join_steps" view';


--
-- Name: builds; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.builds (
    queued_at timestamp with time zone,
    job_name text NOT NULL,
    branch character varying,
    succeeded boolean,
    started_at timestamp with time zone,
    finished_at timestamp with time zone,
    global_build_num integer NOT NULL
);


ALTER TABLE public.builds OWNER TO postgres;

--
-- Name: TABLE builds; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.builds IS 'In contrast with the "universal_builds" table, this is information that may be fetched using a provider-specific API and may be populated asynchronously from the "universal_builds" info which is available from the GitHub status notification.';


--
-- Name: universal_builds; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.universal_builds (
    id integer NOT NULL,
    build_number integer,
    build_namespace text,
    provider integer,
    commit_sha1 character(40) NOT NULL,
    succeeded boolean NOT NULL
);


ALTER TABLE public.universal_builds OWNER TO postgres;

--
-- Name: TABLE universal_builds; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.universal_builds IS 'Currently, these records are stored even if the build succeeds.';


--
-- Name: global_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.global_builds WITH (security_barrier='false') AS
 SELECT builds.global_build_num,
    universal_builds.succeeded,
    universal_builds.commit_sha1 AS vcs_revision,
    builds.job_name,
    builds.branch,
    universal_builds.build_number,
    universal_builds.build_namespace,
    builds.queued_at,
    builds.started_at,
    builds.finished_at,
    universal_builds.provider
   FROM (public.universal_builds
     JOIN public.builds ON ((builds.global_build_num = universal_builds.id)));


ALTER TABLE public.global_builds OWNER TO postgres;

--
-- Name: builds_deduped; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.builds_deduped WITH (security_barrier='false') AS
 SELECT DISTINCT ON (global_builds.provider, global_builds.job_name, global_builds.vcs_revision) global_builds.build_number AS build_num,
    global_builds.vcs_revision,
    global_builds.queued_at,
    global_builds.job_name,
    global_builds.branch,
    global_builds.succeeded,
    global_builds.started_at,
    global_builds.finished_at,
    count(*) OVER (PARTITION BY global_builds.provider, global_builds.job_name, global_builds.vcs_revision) AS rebuild_count,
    global_builds.global_build_num AS global_build,
    global_builds.provider,
    global_builds.build_namespace
   FROM public.global_builds
  ORDER BY global_builds.provider, global_builds.job_name, global_builds.vcs_revision, global_builds.global_build_num DESC;


ALTER TABLE public.builds_deduped OWNER TO postgres;

--
-- Name: log_metadata; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.log_metadata (
    line_count integer NOT NULL,
    byte_count integer NOT NULL,
    step integer NOT NULL,
    content text,
    modified_by_ansi_stripping boolean
);


ALTER TABLE public.log_metadata OWNER TO postgres;

--
-- Name: matches; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.matches (
    id integer NOT NULL,
    build_step integer,
    pattern integer,
    line_number integer,
    line_text text,
    span_start integer,
    span_end integer,
    scan_id integer
);


ALTER TABLE public.matches OWNER TO postgres;

--
-- Name: matches_distinct; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.matches_distinct AS
 SELECT DISTINCT ON (matches.build_step, matches.pattern, matches.line_number) matches.id,
    matches.build_step,
    matches.pattern,
    matches.line_number,
    matches.line_text,
    matches.span_start,
    matches.span_end,
    matches.scan_id
   FROM public.matches
  ORDER BY matches.build_step, matches.pattern, matches.line_number, matches.scan_id DESC;


ALTER TABLE public.matches_distinct OWNER TO postgres;

--
-- Name: VIEW matches_distinct; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.matches_distinct IS 'Somehow, certain logs got scanned by the same pattern more than once (e.g. universal build 716793),
yielding multiple rows for the same pattern on the same line.

This intermediate view eliminates those duplicate matches.';


--
-- Name: match_positions; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.match_positions WITH (security_barrier='false') AS
 SELECT foo.pattern,
    universal_builds.build_number AS build,
    log_metadata.step AS step_id,
    build_steps_deduped_mitigation.name AS step_name,
    foo.first_line,
    foo.last_line,
    log_metadata.line_count,
    foo.matched_line_count,
    ((foo.first_line)::double precision / (log_metadata.line_count)::double precision) AS first_position_fraction,
    ((foo.last_line)::double precision / (log_metadata.line_count)::double precision) AS last_position_fraction,
    (log_metadata.line_count - (1 + foo.last_line)) AS lines_from_end,
    universal_builds.id AS universal_build
   FROM (((( SELECT matches_distinct.pattern,
            matches_distinct.build_step,
            min(matches_distinct.line_number) AS first_line,
            max(matches_distinct.line_number) AS last_line,
            count(matches_distinct.line_number) AS matched_line_count
           FROM public.matches_distinct
          GROUP BY matches_distinct.pattern, matches_distinct.build_step) foo
     JOIN public.log_metadata ON ((log_metadata.step = foo.build_step)))
     JOIN public.build_steps_deduped_mitigation ON ((build_steps_deduped_mitigation.id = log_metadata.step)))
     JOIN public.universal_builds ON ((universal_builds.id = build_steps_deduped_mitigation.universal_build)))
  ORDER BY foo.matched_line_count DESC, foo.pattern, universal_builds.id DESC;


ALTER TABLE public.match_positions OWNER TO postgres;

--
-- Name: VIEW match_positions; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.match_positions IS 'Each row of this table represents statistics across the (potential multiple) pattern matches for a given *pattern* on a given *build log*.  That is, there is no aggregation across builds or patterns.';


--
-- Name: match_last_position_frequencies; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.match_last_position_frequencies WITH (security_barrier='false') AS
 SELECT DISTINCT ON (foo.pattern) foo.pattern,
    foo.lines_from_end,
    foo.distance_from_end_frequency,
    bar.build_count,
    (NOT (foo.lines_from_end)::boolean) AS usually_last_line,
    ((foo.distance_from_end_frequency)::double precision / (bar.build_count)::double precision) AS position_likelihood
   FROM (( SELECT match_positions.pattern,
            match_positions.lines_from_end,
            count(match_positions.build) AS distance_from_end_frequency
           FROM public.match_positions
          GROUP BY match_positions.pattern, match_positions.lines_from_end
          ORDER BY match_positions.pattern, (count(match_positions.build)) DESC) foo
     JOIN ( SELECT match_positions.pattern,
            count(match_positions.build) AS build_count
           FROM public.match_positions
          GROUP BY match_positions.pattern) bar ON ((foo.pattern = bar.pattern)));


ALTER TABLE public.match_last_position_frequencies OWNER TO postgres;

--
-- Name: pattern_authorship; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.pattern_authorship (
    pattern integer NOT NULL,
    author text,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE public.pattern_authorship OWNER TO postgres;

--
-- Name: scanned_patterns; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.scanned_patterns (
    scan integer NOT NULL,
    newest_pattern integer NOT NULL,
    step_id integer NOT NULL
);


ALTER TABLE public.scanned_patterns OWNER TO postgres;

--
-- Name: pattern_scan_counts; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.pattern_scan_counts WITH (security_barrier='false') AS
 SELECT scanned_patterns.newest_pattern,
    count(scanned_patterns.step_id) AS count
   FROM public.scanned_patterns
  GROUP BY scanned_patterns.newest_pattern
  ORDER BY scanned_patterns.newest_pattern DESC;


ALTER TABLE public.pattern_scan_counts OWNER TO postgres;

--
-- Name: VIEW pattern_scan_counts; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.pattern_scan_counts IS 'FIXME: counting on the "step_id" column may not account for repetition across multiple scans';


--
-- Name: pattern_step_applicability; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.pattern_step_applicability (
    id integer NOT NULL,
    pattern integer,
    step_name text
);


ALTER TABLE public.pattern_step_applicability OWNER TO postgres;

--
-- Name: pattern_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.pattern_tags (
    pattern integer NOT NULL,
    tag character varying(20) NOT NULL
);


ALTER TABLE public.pattern_tags OWNER TO postgres;

--
-- Name: patterns; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.patterns (
    expression text,
    id integer NOT NULL,
    description text,
    regex boolean,
    has_nondeterministic_values boolean,
    is_retired boolean DEFAULT false NOT NULL,
    specificity integer DEFAULT 1 NOT NULL,
    lines_from_end integer
);


ALTER TABLE public.patterns OWNER TO postgres;

--
-- Name: patterns_augmented; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.patterns_augmented WITH (security_barrier='false') AS
 SELECT patterns.expression,
    patterns.id,
    patterns.description,
    patterns.regex,
    patterns.has_nondeterministic_values,
    patterns.is_retired,
    patterns.specificity,
    COALESCE(foo.tags, ''::text) AS tags,
    COALESCE(bar.steps, ''::text) AS steps,
    pattern_authorship.author,
    pattern_authorship.created,
    ( SELECT COALESCE(sum(pattern_scan_counts.count), (0)::numeric) AS "coalesce"
           FROM public.pattern_scan_counts
          WHERE (pattern_scan_counts.newest_pattern >= patterns.id)) AS scanned_count,
    ( SELECT sum(pattern_scan_counts.count) AS sum
           FROM public.pattern_scan_counts) AS total_scanned_builds,
    match_last_position_frequencies.usually_last_line,
    match_last_position_frequencies.position_likelihood,
    patterns.lines_from_end
   FROM ((((public.patterns
     LEFT JOIN ( SELECT pattern_tags.pattern,
            string_agg((pattern_tags.tag)::text, ';'::text) AS tags
           FROM public.pattern_tags
          GROUP BY pattern_tags.pattern) foo ON ((foo.pattern = patterns.id)))
     LEFT JOIN ( SELECT pattern_step_applicability.pattern,
            string_agg(pattern_step_applicability.step_name, ';'::text) AS steps
           FROM public.pattern_step_applicability
          GROUP BY pattern_step_applicability.pattern) bar ON ((bar.pattern = patterns.id)))
     LEFT JOIN public.pattern_authorship ON ((pattern_authorship.pattern = patterns.id)))
     LEFT JOIN public.match_last_position_frequencies ON ((patterns.id = match_last_position_frequencies.pattern)));


ALTER TABLE public.patterns_augmented OWNER TO postgres;

--
-- Name: flaky_patterns_augmented; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.flaky_patterns_augmented AS
 SELECT foo.pattern,
    patterns_augmented.expression,
    patterns_augmented.id,
    patterns_augmented.description,
    patterns_augmented.regex,
    patterns_augmented.has_nondeterministic_values,
    patterns_augmented.is_retired,
    patterns_augmented.specificity,
    patterns_augmented.tags,
    patterns_augmented.steps,
    patterns_augmented.author,
    patterns_augmented.created
   FROM (( SELECT pattern_tags.pattern
           FROM public.pattern_tags
          WHERE ((pattern_tags.tag)::text = 'flaky'::text)) foo
     JOIN public.patterns_augmented ON ((patterns_augmented.id = foo.pattern)));


ALTER TABLE public.flaky_patterns_augmented OWNER TO postgres;

--
-- Name: matches_for_build; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.matches_for_build WITH (security_barrier='false') AS
 SELECT matches_distinct.pattern AS pat,
    builds_deduped.build_num AS build,
    build_steps_deduped_mitigation.name AS step_name,
    builds_deduped.global_build AS universal_build,
    matches_distinct.id AS match_id,
    matches_distinct.build_step AS step_id
   FROM ((public.matches_distinct
     JOIN public.build_steps_deduped_mitigation ON ((matches_distinct.build_step = build_steps_deduped_mitigation.id)))
     JOIN public.builds_deduped ON ((builds_deduped.global_build = build_steps_deduped_mitigation.universal_build)));


ALTER TABLE public.matches_for_build OWNER TO postgres;

--
-- Name: best_pattern_match_for_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.best_pattern_match_for_builds WITH (security_barrier='false') AS
 SELECT DISTINCT ON (matches_for_build.universal_build) matches_for_build.build,
    matches_for_build.pat AS pattern_id,
    patterns.expression,
    patterns.regex,
    patterns.has_nondeterministic_values,
    patterns.is_retired,
    patterns.specificity,
    NULL::bigint AS distinct_matching_pattern_count,
    (count(matches_for_build.match_id) OVER (PARTITION BY matches_for_build.universal_build))::numeric AS total_pattern_matches,
    (EXISTS ( SELECT flaky_patterns_augmented.id
           FROM public.flaky_patterns_augmented
          WHERE (flaky_patterns_augmented.pattern = matches_for_build.pat))) AS is_flaky,
    matches_for_build.universal_build,
    matches_for_build.match_id,
    matches_for_build.step_id
   FROM (public.matches_for_build
     JOIN public.patterns ON ((matches_for_build.pat = patterns.id)))
  ORDER BY matches_for_build.universal_build, patterns.specificity DESC, patterns.is_retired, patterns.regex, patterns.id DESC, matches_for_build.match_id DESC;


ALTER TABLE public.best_pattern_match_for_builds OWNER TO postgres;

--
-- Name: VIEW best_pattern_match_for_builds; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.best_pattern_match_for_builds IS 'TODO: Fix computation of "distinct_matching_pattern_count" column';


--
-- Name: aggregated_build_matches; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.aggregated_build_matches WITH (security_barrier='false') AS
 SELECT best_pattern_match_for_builds.pattern_id AS pat,
    count(best_pattern_match_for_builds.universal_build) AS matching_build_count,
    max(global_builds.queued_at) AS most_recent,
    min(global_builds.queued_at) AS earliest
   FROM (public.best_pattern_match_for_builds
     JOIN public.global_builds ON ((global_builds.global_build_num = best_pattern_match_for_builds.universal_build)))
  GROUP BY best_pattern_match_for_builds.pattern_id;


ALTER TABLE public.aggregated_build_matches OWNER TO postgres;

--
-- Name: created_github_statuses; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.created_github_statuses (
    id bigint NOT NULL,
    url text,
    state character varying(7),
    description text,
    target_url text,
    context text,
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    sha1 character(40),
    project text,
    repo text
);


ALTER TABLE public.created_github_statuses OWNER TO postgres;

--
-- Name: aggregated_github_status_postings; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.aggregated_github_status_postings AS
 SELECT created_github_statuses.sha1,
    count(*) AS count,
    max(created_github_statuses.created_at) AS last_time,
    (max(created_github_statuses.created_at) - min(created_github_statuses.created_at)) AS time_interval
   FROM public.created_github_statuses
  GROUP BY created_github_statuses.sha1
  ORDER BY (max(created_github_statuses.created_at)) DESC;


ALTER TABLE public.aggregated_github_status_postings OWNER TO postgres;

--
-- Name: builds_join_steps; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.builds_join_steps WITH (security_barrier='false') AS
 SELECT build_steps_deduped_mitigation.id AS step_id,
    build_steps_deduped_mitigation.name AS step_name,
    builds_deduped.build_num,
    builds_deduped.vcs_revision,
    builds_deduped.queued_at,
    builds_deduped.job_name,
    builds_deduped.branch,
    build_steps_deduped_mitigation.is_timeout,
    build_steps_deduped_mitigation.universal_build,
    builds_deduped.provider,
    builds_deduped.succeeded,
    builds_deduped.build_namespace,
    builds_deduped.started_at,
    builds_deduped.finished_at
   FROM (public.build_steps_deduped_mitigation
     JOIN public.builds_deduped ON ((build_steps_deduped_mitigation.universal_build = builds_deduped.global_build)))
  ORDER BY builds_deduped.vcs_revision, builds_deduped.global_build DESC;


ALTER TABLE public.builds_join_steps OWNER TO postgres;

--
-- Name: matches_with_log_metadata; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.matches_with_log_metadata WITH (security_barrier='false') AS
 SELECT matches_distinct.id,
    matches_distinct.build_step,
    matches_distinct.pattern,
    matches_distinct.line_number,
    matches_distinct.line_text,
    matches_distinct.span_start,
    matches_distinct.span_end,
    matches_distinct.scan_id,
    log_metadata.line_count,
    log_metadata.byte_count,
    log_metadata.step,
    builds_join_steps.step_name,
    builds_join_steps.build_num,
    builds_join_steps.universal_build
   FROM ((public.matches_distinct
     JOIN public.log_metadata ON ((log_metadata.step = matches_distinct.build_step)))
     JOIN public.builds_join_steps ON ((builds_join_steps.step_id = log_metadata.step)));


ALTER TABLE public.matches_with_log_metadata OWNER TO postgres;

--
-- Name: best_pattern_match_augmented_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.best_pattern_match_augmented_builds WITH (security_barrier='false') AS
 SELECT DISTINCT ON (best_pattern_match_for_builds.universal_build) best_pattern_match_for_builds.build,
    matches_with_log_metadata.step_name,
    matches_with_log_metadata.line_number,
    matches_with_log_metadata.line_count,
    matches_with_log_metadata.line_text,
    matches_with_log_metadata.span_start,
    matches_with_log_metadata.span_end,
    builds_join_steps.vcs_revision,
    builds_join_steps.queued_at,
    builds_join_steps.job_name,
    builds_join_steps.branch,
    best_pattern_match_for_builds.pattern_id,
    best_pattern_match_for_builds.specificity,
    NULL::boolean AS is_broken,
    NULL::text AS reporter,
    NULL::timestamp with time zone AS report_timestamp,
    matches_with_log_metadata.id AS match_id,
    best_pattern_match_for_builds.is_flaky,
    builds_join_steps.universal_build,
    builds_join_steps.provider,
    builds_join_steps.succeeded,
    builds_join_steps.build_namespace,
    builds_join_steps.started_at,
    builds_join_steps.finished_at
   FROM ((public.best_pattern_match_for_builds
     JOIN public.matches_with_log_metadata ON ((matches_with_log_metadata.id = best_pattern_match_for_builds.match_id)))
     JOIN public.builds_join_steps ON ((builds_join_steps.step_id = matches_with_log_metadata.build_step)))
  ORDER BY best_pattern_match_for_builds.universal_build DESC, matches_with_log_metadata.line_number;


ALTER TABLE public.best_pattern_match_augmented_builds OWNER TO postgres;

--
-- Name: VIEW best_pattern_match_augmented_builds; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.best_pattern_match_augmented_builds IS 'TODO: Remove 3 fields:
is_broken, reporter, report_timestamp';


--
-- Name: broken_revisions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.broken_revisions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.broken_revisions_id_seq OWNER TO postgres;

--
-- Name: code_breakage_affected_jobs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.code_breakage_affected_jobs (
    job text NOT NULL,
    cause integer NOT NULL,
    reporter text,
    reported_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.code_breakage_affected_jobs OWNER TO postgres;

--
-- Name: code_breakage_cause; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.code_breakage_cause (
    id integer NOT NULL,
    sha1 character(40) NOT NULL,
    description text,
    reporter text,
    reported_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.code_breakage_cause OWNER TO postgres;

--
-- Name: code_breakage_resolution; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.code_breakage_resolution (
    id integer NOT NULL,
    cause integer NOT NULL,
    sha1 character(40) NOT NULL,
    reporter text,
    reported_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.code_breakage_resolution OWNER TO postgres;

--
-- Name: master_failure_mode_attributions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.master_failure_mode_attributions (
    cause_id integer NOT NULL,
    reporter text NOT NULL,
    mode_id integer NOT NULL,
    reported_at timestamp with time zone,
    id integer NOT NULL
);


ALTER TABLE public.master_failure_mode_attributions OWNER TO postgres;

--
-- Name: ordered_master_commits; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.ordered_master_commits (
    id integer NOT NULL,
    sha1 character(40) NOT NULL
);


ALTER TABLE public.ordered_master_commits OWNER TO postgres;

--
-- Name: TABLE ordered_master_commits; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.ordered_master_commits IS 'Warning: the "id" column may not be contiguous.';


--
-- Name: code_breakage_spans; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.code_breakage_spans WITH (security_barrier='false') AS
 SELECT DISTINCT ON (foo.cause_id) foo.cause_id,
    foo.commit_index AS cause_commit_index,
    foo.sha1 AS cause_sha1,
    foo.description,
    foo.reporter AS cause_reporter,
    foo.reported_at AS cause_reported_at,
    bar.resolution_id,
    bar.commit_index AS resolved_commit_index,
    bar.sha1 AS resolution_sha1,
    bar.reporter AS resolution_reporter,
    bar.reported_at AS resolution_reported_at,
    barx.mode_id AS failure_mode,
    barx.reporter AS failure_mode_reporter,
    barx.reported_at AS failure_mode_reported_at
   FROM ((( SELECT code_breakage_cause.reporter,
            code_breakage_cause.reported_at,
            code_breakage_cause.id AS cause_id,
            ordered_master_commits.id AS commit_index,
            code_breakage_cause.description,
            code_breakage_cause.sha1
           FROM (public.code_breakage_cause
             JOIN public.ordered_master_commits ON ((ordered_master_commits.sha1 = code_breakage_cause.sha1)))) foo
     LEFT JOIN ( SELECT code_breakage_resolution.reporter,
            code_breakage_resolution.reported_at,
            code_breakage_resolution.id AS resolution_id,
            code_breakage_resolution.cause AS cause_id,
            ordered_master_commits.id AS commit_index,
            code_breakage_resolution.sha1
           FROM (public.code_breakage_resolution
             JOIN public.ordered_master_commits ON ((ordered_master_commits.sha1 = code_breakage_resolution.sha1)))) bar ON ((foo.cause_id = bar.cause_id)))
     LEFT JOIN ( SELECT DISTINCT ON (master_failure_mode_attributions.cause_id) master_failure_mode_attributions.cause_id,
            master_failure_mode_attributions.reporter,
            master_failure_mode_attributions.reported_at,
            master_failure_mode_attributions.mode_id
           FROM public.master_failure_mode_attributions
          ORDER BY master_failure_mode_attributions.cause_id, master_failure_mode_attributions.id DESC) barx ON ((foo.cause_id = barx.cause_id)))
  ORDER BY foo.cause_id, bar.resolution_id DESC;


ALTER TABLE public.code_breakage_spans OWNER TO postgres;

--
-- Name: master_commit_known_breakage_causes; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_commit_known_breakage_causes AS
 SELECT ordered_master_commits.sha1,
    code_breakage_spans.cause_id
   FROM (public.ordered_master_commits
     JOIN public.code_breakage_spans ON (((code_breakage_spans.cause_commit_index <= ordered_master_commits.id) AND ((code_breakage_spans.resolved_commit_index IS NULL) OR (ordered_master_commits.id < code_breakage_spans.resolved_commit_index)))))
  ORDER BY ordered_master_commits.id DESC;


ALTER TABLE public.master_commit_known_breakage_causes OWNER TO postgres;

--
-- Name: VIEW master_commit_known_breakage_causes; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.master_commit_known_breakage_causes IS 'This is just an intermediate view for simplifying more complex view definitions.';


--
-- Name: known_broken_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.known_broken_builds WITH (security_barrier='false') AS
 SELECT global_builds_1.global_build_num AS universal_build,
    count(*) AS cause_count,
    string_agg((foo.cause_id)::text, ';'::text) AS causes
   FROM (public.global_builds global_builds_1
     JOIN ( SELECT master_commit_known_breakage_causes.sha1,
            code_breakage_affected_jobs.job,
            master_commit_known_breakage_causes.cause_id
           FROM (public.master_commit_known_breakage_causes
             JOIN public.code_breakage_affected_jobs ON ((code_breakage_affected_jobs.cause = master_commit_known_breakage_causes.cause_id)))) foo ON (((global_builds_1.vcs_revision = foo.sha1) AND (global_builds_1.job_name = foo.job))))
  WHERE (NOT COALESCE(global_builds_1.succeeded, false))
  GROUP BY global_builds_1.global_build_num;


ALTER TABLE public.known_broken_builds OWNER TO postgres;

--
-- Name: VIEW known_broken_builds; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.known_broken_builds IS 'These are *master* builds covered by the "code_breakage_spans" view.';


--
-- Name: build_failure_causes; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.build_failure_causes WITH (security_barrier='false') AS
 SELECT builds_deduped.build_num,
    builds_deduped.vcs_revision,
    builds_deduped.queued_at,
    builds_deduped.job_name,
    builds_deduped.branch,
    builds_deduped.succeeded,
    (build_steps_deduped_mitigation.universal_build IS NULL) AS is_idiopathic,
    build_steps_deduped_mitigation.id AS step_id,
    build_steps_deduped_mitigation.name AS step_name,
    COALESCE(build_steps_deduped_mitigation.is_timeout, false) AS is_timeout,
    (best_pattern_match_for_builds.pattern_id IS NULL) AS is_unmatched,
    best_pattern_match_for_builds.pattern_id,
    COALESCE(best_pattern_match_for_builds.is_flaky, false) AS is_flaky,
    (known_broken_builds.universal_build IS NOT NULL) AS is_known_broken,
    known_broken_builds.causes AS known_cause_ids,
    (best_pattern_match_for_builds.pattern_id IS NOT NULL) AS is_matched,
    builds_deduped.rebuild_count,
    builds_deduped.global_build,
    builds_deduped.provider,
    builds_deduped.build_namespace,
    builds_deduped.started_at,
    builds_deduped.finished_at
   FROM (((public.builds_deduped
     LEFT JOIN public.build_steps_deduped_mitigation ON ((build_steps_deduped_mitigation.universal_build = builds_deduped.global_build)))
     LEFT JOIN public.best_pattern_match_for_builds ON ((best_pattern_match_for_builds.universal_build = builds_deduped.global_build)))
     LEFT JOIN public.known_broken_builds ON ((known_broken_builds.universal_build = builds_deduped.global_build)));


ALTER TABLE public.build_failure_causes OWNER TO postgres;

--
-- Name: build_failure_causes_disjoint; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.build_failure_causes_disjoint WITH (security_barrier='false') AS
 SELECT build_failure_causes.build_num,
    build_failure_causes.succeeded,
    (build_failure_causes.is_idiopathic AND (NOT build_failure_causes.is_known_broken)) AS is_idiopathic,
    build_failure_causes.step_id,
    build_failure_causes.step_name,
    (build_failure_causes.is_timeout AND (NOT build_failure_causes.is_known_broken)) AS is_timeout,
    (build_failure_causes.is_unmatched AND (NOT build_failure_causes.is_known_broken) AND (NOT build_failure_causes.is_timeout) AND (NOT build_failure_causes.is_idiopathic)) AS is_unmatched,
    build_failure_causes.pattern_id,
    (build_failure_causes.is_flaky AND (NOT build_failure_causes.is_known_broken)) AS is_flaky,
    build_failure_causes.vcs_revision,
    build_failure_causes.queued_at,
    build_failure_causes.job_name,
    build_failure_causes.branch,
    build_failure_causes.is_known_broken,
    (build_failure_causes.is_matched AND (NOT build_failure_causes.is_known_broken) AND (NOT build_failure_causes.is_flaky)) AS is_matched,
    build_failure_causes.rebuild_count,
    build_failure_causes.global_build
   FROM public.build_failure_causes;


ALTER TABLE public.build_failure_causes_disjoint OWNER TO postgres;

--
-- Name: VIEW build_failure_causes_disjoint; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.build_failure_causes_disjoint IS 'The "known broken" cause shall dominate; it is made mutually-exclusive with all other causes for a given build failure because a known broken build is unreliable for other statistics collection.

Additionally, the "is_matched" field excludes "is_flaky", while the "is_unmatched" field excludes "is_timeout" and "is_idiopathic".  This means that the sum of all "is_flaky" + "is_matched" + "is_timeout" + "is_unmatched" + "is_known_broken" should be equal to the total failure count.';


--
-- Name: build_failure_disjoint_causes_by_commit; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.build_failure_disjoint_causes_by_commit AS
 SELECT build_failure_causes_disjoint.vcs_revision AS sha1,
    count(build_failure_causes_disjoint.vcs_revision) AS total,
    COALESCE(sum((build_failure_causes_disjoint.is_idiopathic)::integer), (0)::bigint) AS idiopathic,
    COALESCE(sum((build_failure_causes_disjoint.is_timeout)::integer), (0)::bigint) AS timeout,
    COALESCE(sum((build_failure_causes_disjoint.is_known_broken)::integer), (0)::bigint) AS known_broken,
    COALESCE(sum((build_failure_causes_disjoint.is_matched)::integer), (0)::bigint) AS pattern_matched,
    COALESCE(sum((build_failure_causes_disjoint.is_flaky)::integer), (0)::bigint) AS flaky,
    COALESCE(sum((build_failure_causes_disjoint.is_unmatched)::integer), (0)::bigint) AS pattern_unmatched,
    COALESCE(sum((build_failure_causes_disjoint.succeeded)::integer), (0)::bigint) AS succeeded
   FROM public.build_failure_causes_disjoint
  GROUP BY build_failure_causes_disjoint.vcs_revision;


ALTER TABLE public.build_failure_disjoint_causes_by_commit OWNER TO postgres;

--
-- Name: build_steps_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.build_steps_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.build_steps_id_seq OWNER TO postgres;

--
-- Name: build_steps_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.build_steps_id_seq OWNED BY public.build_steps.id;


--
-- Name: cached_master_merge_base; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cached_master_merge_base (
    branch_commit character(40) NOT NULL,
    master_commit character(40) NOT NULL,
    computation_timestamp timestamp without time zone DEFAULT now() NOT NULL,
    distance integer NOT NULL
);


ALTER TABLE public.cached_master_merge_base OWNER TO postgres;

--
-- Name: TABLE cached_master_merge_base; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.cached_master_merge_base IS 'Caches the computed "merge base" of a PR commit with the master branch.

In general, it would be possible for the merge base to change over time if the master branch is advanced to a more recent ancestor (up to and including the HEAD) of the PR branch.  In practice, however, this will not happen in the Facebook mirrored repo configuration, as a novel commit is produced for every change to the master branch.

Therefore, this cache of merge bases never needs to be invalidated.';


--
-- Name: ci_providers; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.ci_providers (
    id integer NOT NULL,
    hostname text NOT NULL,
    label text,
    icon_url text
);


ALTER TABLE public.ci_providers OWNER TO postgres;

--
-- Name: TABLE ci_providers; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.ci_providers IS 'Append "?s=32" to the icon URL to specify the size of the image';


--
-- Name: ci_providers_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.ci_providers_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ci_providers_id_seq OWNER TO postgres;

--
-- Name: ci_providers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.ci_providers_id_seq OWNED BY public.ci_providers.id;


--
-- Name: code_breakage_cause_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.code_breakage_cause_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.code_breakage_cause_id_seq OWNER TO postgres;

--
-- Name: code_breakage_cause_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.code_breakage_cause_id_seq OWNED BY public.code_breakage_cause.id;


--
-- Name: code_breakage_resolution_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.code_breakage_resolution_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.code_breakage_resolution_id_seq OWNER TO postgres;

--
-- Name: code_breakage_resolution_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.code_breakage_resolution_id_seq OWNED BY public.code_breakage_resolution.id;


--
-- Name: commit_authorship_supplemental; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.commit_authorship_supplemental (
    sha1 character(40) NOT NULL,
    pulled_by text,
    reviewed_by text,
    differential_revision text,
    fbshipit_source_id text,
    ghstack_source_id text
);


ALTER TABLE public.commit_authorship_supplemental OWNER TO postgres;

--
-- Name: commit_metadata; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.commit_metadata (
    sha1 character(40) NOT NULL,
    message text,
    tree_sha1 character(40),
    author_name text,
    author_email text,
    author_date timestamp with time zone,
    committer_name text,
    committer_email text,
    committer_date timestamp with time zone
);


ALTER TABLE public.commit_metadata OWNER TO postgres;

--
-- Name: idiopathic_build_failures; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.idiopathic_build_failures WITH (security_barrier='false') AS
 SELECT global_builds.branch,
    global_builds.global_build_num
   FROM (public.build_steps_deduped_mitigation
     JOIN public.global_builds ON ((build_steps_deduped_mitigation.universal_build = global_builds.global_build_num)))
  WHERE (build_steps_deduped_mitigation.name IS NULL);


ALTER TABLE public.idiopathic_build_failures OWNER TO postgres;

--
-- Name: job_failure_frequencies; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.job_failure_frequencies WITH (security_barrier='false') AS
 SELECT global_builds.job_name,
    count(*) AS freq,
    max(global_builds.queued_at) AS last
   FROM public.global_builds
  GROUP BY global_builds.job_name
  ORDER BY (count(*)) DESC, global_builds.job_name;


ALTER TABLE public.job_failure_frequencies OWNER TO postgres;

--
-- Name: known_breakage_summaries; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.known_breakage_summaries WITH (security_barrier='false') AS
 SELECT code_breakage_spans.cause_id,
    code_breakage_spans.cause_commit_index,
    code_breakage_spans.cause_sha1,
    code_breakage_spans.description,
    code_breakage_spans.cause_reporter,
    code_breakage_spans.cause_reported_at,
    COALESCE(foo.jobs, ''::text) AS cause_jobs,
    code_breakage_spans.resolution_id,
    code_breakage_spans.resolved_commit_index,
    code_breakage_spans.resolution_sha1,
    code_breakage_spans.resolution_reporter,
    code_breakage_spans.resolution_reported_at,
    COALESCE(meta1.author_name, ''::text) AS breakage_commit_author,
    COALESCE(meta1.message, ''::text) AS breakage_commit_message,
    COALESCE(meta2.author_name, ''::text) AS resolution_commit_author,
    COALESCE(meta2.message, ''::text) AS resolution_commit_message,
    COALESCE(meta1.committer_date, now()) AS breakage_commit_date,
    COALESCE(meta2.committer_date, now()) AS resolution_commit_date,
    COALESCE(code_breakage_spans.failure_mode, 1) AS failure_mode_id,
    COALESCE(code_breakage_spans.failure_mode_reporter, ''::text) AS failure_mode_reporter,
    COALESCE(code_breakage_spans.failure_mode_reported_at, now()) AS failure_mode_reported_at
   FROM (((public.code_breakage_spans
     LEFT JOIN ( SELECT code_breakage_affected_jobs.cause,
            string_agg(code_breakage_affected_jobs.job, ';'::text) AS jobs
           FROM public.code_breakage_affected_jobs
          GROUP BY code_breakage_affected_jobs.cause) foo ON ((foo.cause = code_breakage_spans.cause_id)))
     LEFT JOIN public.commit_metadata meta1 ON ((meta1.sha1 = code_breakage_spans.cause_sha1)))
     LEFT JOIN public.commit_metadata meta2 ON ((meta2.sha1 = code_breakage_spans.resolution_sha1)));


ALTER TABLE public.known_breakage_summaries OWNER TO postgres;

--
-- Name: master_contiguous_failures; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_contiguous_failures WITH (security_barrier='false') AS
 SELECT count(*) OVER (PARTITION BY quux.group_index, quux.job_name) AS run_length,
    quux.group_index,
    quux.global_build,
    quux.job_name,
    quux.sha1,
    quux.id,
    first_value(quux.id) OVER (PARTITION BY quux.group_index, quux.job_name ORDER BY quux.commit_number DESC) AS start_commit_index,
    last_value(quux.id) OVER (PARTITION BY quux.group_index, quux.job_name ORDER BY quux.commit_number DESC) AS end_commit_index,
    quux.global_build AS universal_build,
    row_number() OVER (PARTITION BY quux.group_index, quux.job_name ORDER BY quux.commit_number DESC) AS group_position
   FROM ( SELECT ((bar.delta_next = 1) OR (bar.delta_prev = 1)) AS contiguous_failure,
            COALESCE(sum((((bar.delta_next = 1) AND (bar.delta_prev <> 1)))::integer) OVER (PARTITION BY bar.job_name ORDER BY bar.commit_number DESC, bar.job_name), (0)::bigint) AS group_index,
            bar.delta_next,
            bar.delta_prev,
            bar.prev_commit_number,
            bar.next_commit_number,
            bar.commit_number,
            bar.global_build,
            bar.job_name,
            bar.sha1,
            bar.id
           FROM ( SELECT (foo.commit_number - foo.next_commit_number) AS delta_next,
                    (foo.prev_commit_number - foo.commit_number) AS delta_prev,
                    foo.prev_commit_number,
                    foo.next_commit_number,
                    foo.commit_number,
                    foo.global_build,
                    foo.job_name,
                    foo.sha1,
                    foo.id
                   FROM ( SELECT lag(blah.commit_number) OVER (PARTITION BY build_failure_causes_disjoint.job_name ORDER BY blah.commit_number DESC, build_failure_causes_disjoint.job_name) AS prev_commit_number,
                            lead(blah.commit_number) OVER (PARTITION BY build_failure_causes_disjoint.job_name ORDER BY blah.commit_number DESC, build_failure_causes_disjoint.job_name) AS next_commit_number,
                            blah.commit_number,
                            build_failure_causes_disjoint.global_build,
                            build_failure_causes_disjoint.job_name,
                            blah.sha1,
                            blah.id
                           FROM (( SELECT ordered_master_commits.id,
                                    ordered_master_commits.sha1,
                                    row_number() OVER (ORDER BY ordered_master_commits.id DESC) AS commit_number
                                   FROM public.ordered_master_commits) blah
                             JOIN public.build_failure_causes_disjoint ON ((build_failure_causes_disjoint.vcs_revision = blah.sha1)))) foo) bar) quux
  WHERE quux.contiguous_failure
  ORDER BY quux.job_name, quux.commit_number;


ALTER TABLE public.master_contiguous_failures OWNER TO postgres;

--
-- Name: master_contiguous_failure_job_groups; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_contiguous_failure_job_groups AS
 SELECT min(master_contiguous_failures.id) AS first_commit_id,
    max(master_contiguous_failures.end_commit_index) AS last_commit_id,
    max(master_contiguous_failures.run_length) AS run_length,
    master_contiguous_failures.job_name,
    master_contiguous_failures.group_index
   FROM public.master_contiguous_failures
  GROUP BY master_contiguous_failures.job_name, master_contiguous_failures.group_index
  ORDER BY (min(master_contiguous_failures.id)) DESC;


ALTER TABLE public.master_contiguous_failure_job_groups OWNER TO postgres;

--
-- Name: master_contiguous_failure_blocks; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_contiguous_failure_blocks AS
 SELECT master_contiguous_failure_job_groups.first_commit_id,
    string_agg(master_contiguous_failure_job_groups.job_name, ';'::text) AS jobs,
    count(*) AS job_count,
    min(master_contiguous_failure_job_groups.run_length) AS min_run_length,
    max(master_contiguous_failure_job_groups.run_length) AS max_run_length,
    min(master_contiguous_failure_job_groups.last_commit_id) AS min_last_commit_id,
    max(master_contiguous_failure_job_groups.last_commit_id) AS max_last_commit_id,
    mode() WITHIN GROUP (ORDER BY master_contiguous_failure_job_groups.run_length DESC) AS modal_run_length,
    mode() WITHIN GROUP (ORDER BY master_contiguous_failure_job_groups.last_commit_id DESC) AS modal_last_commit_id
   FROM public.master_contiguous_failure_job_groups
  GROUP BY master_contiguous_failure_job_groups.first_commit_id
  ORDER BY master_contiguous_failure_job_groups.first_commit_id DESC;


ALTER TABLE public.master_contiguous_failure_blocks OWNER TO postgres;

--
-- Name: master_contiguous_failure_blocks_with_commits; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_contiguous_failure_blocks_with_commits AS
 SELECT master_contiguous_failure_blocks.first_commit_id,
    master_contiguous_failure_blocks.jobs,
    master_contiguous_failure_blocks.job_count,
    master_contiguous_failure_blocks.min_run_length,
    master_contiguous_failure_blocks.max_run_length,
    master_contiguous_failure_blocks.min_last_commit_id,
    master_contiguous_failure_blocks.max_last_commit_id,
    omc1.sha1 AS first_commit,
    omc2.sha1 AS min_last_commit,
    omc3.sha1 AS max_last_commit,
    omc4.sha1 AS modal_last_commit,
    master_contiguous_failure_blocks.modal_run_length,
    master_contiguous_failure_blocks.modal_last_commit_id
   FROM ((((public.master_contiguous_failure_blocks
     JOIN public.ordered_master_commits omc1 ON ((omc1.id = master_contiguous_failure_blocks.first_commit_id)))
     JOIN public.ordered_master_commits omc2 ON ((omc2.id = master_contiguous_failure_blocks.min_last_commit_id)))
     JOIN public.ordered_master_commits omc3 ON ((omc3.id = master_contiguous_failure_blocks.max_last_commit_id)))
     JOIN public.ordered_master_commits omc4 ON ((omc4.id = master_contiguous_failure_blocks.modal_last_commit_id)));


ALTER TABLE public.master_contiguous_failure_blocks_with_commits OWNER TO postgres;

--
-- Name: master_intra_commit_failure_groups; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_intra_commit_failure_groups WITH (security_barrier='false') AS
 SELECT foo.universal_build,
    foo.commit_index,
    foo.vcs_revision,
    foo.cluster_member_count,
    foo.cluster_id,
    foo.step_name,
    foo.pattern_id
   FROM ( SELECT best_pattern_match_for_builds.universal_build,
            ordered_master_commits.id AS commit_index,
            builds_deduped.vcs_revision,
            count(*) OVER (PARTITION BY ordered_master_commits.id, build_steps_deduped_mitigation.name, best_pattern_match_for_builds.pattern_id) AS cluster_member_count,
            dense_rank() OVER (ORDER BY ordered_master_commits.id DESC, build_steps_deduped_mitigation.name, best_pattern_match_for_builds.pattern_id) AS cluster_id,
            build_steps_deduped_mitigation.name AS step_name,
            best_pattern_match_for_builds.pattern_id
           FROM (((public.builds_deduped
             JOIN public.ordered_master_commits ON ((ordered_master_commits.sha1 = builds_deduped.vcs_revision)))
             JOIN public.build_steps_deduped_mitigation ON ((build_steps_deduped_mitigation.universal_build = builds_deduped.global_build)))
             JOIN public.best_pattern_match_for_builds ON ((best_pattern_match_for_builds.universal_build = builds_deduped.global_build)))
          WHERE (NOT builds_deduped.succeeded)) foo
  WHERE (foo.cluster_member_count > 1)
  ORDER BY foo.cluster_id;


ALTER TABLE public.master_intra_commit_failure_groups OWNER TO postgres;

--
-- Name: master_detected_adjacent_breakages; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_detected_adjacent_breakages WITH (security_barrier='false') AS
 SELECT COALESCE(master_contiguous_failures.universal_build, master_intra_commit_failure_groups.universal_build) AS universal_build,
    master_contiguous_failures.run_length AS contiguous_length,
    master_contiguous_failures.group_position AS contiguous_group_position,
    master_contiguous_failures.group_index AS contiguous_group_index,
    master_contiguous_failures.start_commit_index AS contiguous_start_commit_index,
    master_contiguous_failures.end_commit_index AS contiguous_end_commit_index,
    master_intra_commit_failure_groups.cluster_id,
    master_intra_commit_failure_groups.cluster_member_count,
    (master_contiguous_failures.universal_build IS NOT NULL) AS is_longitudinal_breakage,
    (master_intra_commit_failure_groups.universal_build IS NOT NULL) AS is_lateral_breakage
   FROM (public.master_contiguous_failures
     FULL JOIN public.master_intra_commit_failure_groups ON ((master_intra_commit_failure_groups.universal_build = master_contiguous_failures.universal_build)));


ALTER TABLE public.master_detected_adjacent_breakages OWNER TO postgres;

--
-- Name: VIEW master_detected_adjacent_breakages; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.master_detected_adjacent_breakages IS 'Detects longitudinal (contiguous in commit sequence) and lateral (jobs within the same commit) groupings of failures.';


--
-- Name: master_detected_breakages_without_annotations; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_detected_breakages_without_annotations WITH (security_barrier='false') AS
 SELECT master_detected_adjacent_breakages.universal_build,
    master_detected_adjacent_breakages.contiguous_group_position,
    master_detected_adjacent_breakages.contiguous_group_index,
    master_detected_adjacent_breakages.contiguous_start_commit_index,
    master_detected_adjacent_breakages.contiguous_end_commit_index,
    master_detected_adjacent_breakages.cluster_id,
    master_detected_adjacent_breakages.cluster_member_count,
    master_detected_adjacent_breakages.contiguous_length,
    master_detected_adjacent_breakages.is_longitudinal_breakage,
    master_detected_adjacent_breakages.is_lateral_breakage
   FROM (public.master_detected_adjacent_breakages
     LEFT JOIN public.known_broken_builds ON ((master_detected_adjacent_breakages.universal_build = known_broken_builds.universal_build)))
  WHERE (known_broken_builds.universal_build IS NULL);


ALTER TABLE public.master_detected_breakages_without_annotations OWNER TO postgres;

--
-- Name: master_failure_mode_attributions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.master_failure_mode_attributions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.master_failure_mode_attributions_id_seq OWNER TO postgres;

--
-- Name: master_failure_mode_attributions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.master_failure_mode_attributions_id_seq OWNED BY public.master_failure_mode_attributions.id;


--
-- Name: master_failure_modes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.master_failure_modes (
    id integer NOT NULL,
    label text NOT NULL,
    revertible boolean DEFAULT false NOT NULL
);


ALTER TABLE public.master_failure_modes OWNER TO postgres;

--
-- Name: master_failure_modes_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.master_failure_modes_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.master_failure_modes_id_seq OWNER TO postgres;

--
-- Name: master_failure_modes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.master_failure_modes_id_seq OWNED BY public.master_failure_modes.id;


--
-- Name: master_failures_by_commit; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_failures_by_commit WITH (security_barrier='false') AS
 SELECT ordered_master_commits.sha1,
    COALESCE(build_failure_disjoint_causes_by_commit.total, (0)::bigint) AS total,
    COALESCE(build_failure_disjoint_causes_by_commit.idiopathic, (0)::bigint) AS idiopathic,
    COALESCE(build_failure_disjoint_causes_by_commit.timeout, (0)::bigint) AS timeout,
    COALESCE(build_failure_disjoint_causes_by_commit.known_broken, (0)::bigint) AS known_broken,
    COALESCE(build_failure_disjoint_causes_by_commit.pattern_matched, (0)::bigint) AS pattern_matched,
    COALESCE(build_failure_disjoint_causes_by_commit.flaky, (0)::bigint) AS flaky,
    ordered_master_commits.id AS commit_index,
    COALESCE(build_failure_disjoint_causes_by_commit.pattern_unmatched, (0)::bigint) AS pattern_unmatched,
    COALESCE(build_failure_disjoint_causes_by_commit.succeeded, (0)::bigint) AS succeeded
   FROM (public.ordered_master_commits
     LEFT JOIN public.build_failure_disjoint_causes_by_commit ON ((build_failure_disjoint_causes_by_commit.sha1 = ordered_master_commits.sha1)))
  ORDER BY ordered_master_commits.id DESC;


ALTER TABLE public.master_failures_by_commit OWNER TO postgres;

--
-- Name: master_failures_raw_causes; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_failures_raw_causes WITH (security_barrier='false') AS
 SELECT ordered_master_commits.sha1,
    build_failure_causes.succeeded,
    build_failure_causes.is_idiopathic,
    build_failure_causes.is_flaky,
    build_failure_causes.is_timeout,
    build_failure_causes.is_matched,
    build_failure_causes.is_known_broken,
    build_failure_causes.build_num,
    build_failure_causes.queued_at,
    build_failure_causes.job_name,
    build_failure_causes.branch,
    COALESCE(build_failure_causes.step_name, ''::text) AS step_name,
    COALESCE(build_failure_causes.pattern_id, '-1'::integer) AS pattern_id,
    COALESCE(best_pattern_match_augmented_builds.match_id, '-1'::integer) AS match_id,
    COALESCE(best_pattern_match_augmented_builds.line_number, '-1'::integer) AS line_number,
    COALESCE(best_pattern_match_augmented_builds.line_count, '-1'::integer) AS line_count,
    COALESCE(best_pattern_match_augmented_builds.line_text, ''::text) AS line_text,
    COALESCE(best_pattern_match_augmented_builds.span_start, '-1'::integer) AS span_start,
    COALESCE(best_pattern_match_augmented_builds.span_end, '-1'::integer) AS span_end,
    COALESCE(best_pattern_match_augmented_builds.specificity, '-1'::integer) AS specificity,
    (master_detected_adjacent_breakages.universal_build IS NULL) AS is_serially_isolated,
    master_detected_adjacent_breakages.contiguous_length AS contiguous_run_count,
    master_detected_adjacent_breakages.contiguous_group_index,
    master_detected_adjacent_breakages.contiguous_start_commit_index,
    master_detected_adjacent_breakages.contiguous_end_commit_index,
    ordered_master_commits.id AS commit_index,
    master_detected_adjacent_breakages.contiguous_length,
    build_failure_causes.global_build,
    build_failure_causes.provider,
    build_failure_causes.build_namespace,
    master_detected_adjacent_breakages.cluster_id,
    master_detected_adjacent_breakages.cluster_member_count,
    build_failure_causes.started_at,
    build_failure_causes.finished_at
   FROM (((public.ordered_master_commits
     JOIN public.build_failure_causes ON ((build_failure_causes.vcs_revision = ordered_master_commits.sha1)))
     LEFT JOIN public.best_pattern_match_augmented_builds ON ((build_failure_causes.global_build = best_pattern_match_augmented_builds.universal_build)))
     LEFT JOIN public.master_detected_adjacent_breakages ON ((build_failure_causes.global_build = master_detected_adjacent_breakages.universal_build)));


ALTER TABLE public.master_failures_raw_causes OWNER TO postgres;

--
-- Name: VIEW master_failures_raw_causes; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.master_failures_raw_causes IS 'This uses the raw "build_failure_causes" table rather than the mutually-exclusive causes table.';


--
-- Name: master_failures_weekly_aggregation; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_failures_weekly_aggregation WITH (security_barrier='false') AS
 SELECT sum(foo.has_failure) AS had_failure,
    sum(foo.has_idiopathic) AS had_idiopathic,
    sum(foo.has_timeout) AS had_timeout,
    sum(foo.has_known_broken) AS had_known_broken,
    sum(foo.has_pattern_matched) AS had_pattern_matched,
    sum(foo.has_flaky) AS had_flaky,
    date_trunc('week'::text, foo.committer_date) AS week,
    count(*) AS commit_count,
    sum(foo.failure_count) AS failure_count,
    sum(foo.idiopathic_count) AS idiopathic_count,
    sum(foo.timeout_count) AS timeout_count,
    sum(foo.known_broken_count) AS known_broken_count,
    sum(foo.pattern_matched_count) AS pattern_matched_count,
    sum(foo.flaky_count) AS flaky_count,
    min(foo.commit_index) AS earliest_commit_index,
    max(foo.commit_index) AS latest_commit_index,
    sum(foo.pattern_unmatched_count) AS pattern_unmatched_count,
    sum(foo.succeeded_count) AS succeeded_count
   FROM ( SELECT ((master_failures_by_commit.total > 0))::integer AS has_failure,
            ((master_failures_by_commit.idiopathic > 0))::integer AS has_idiopathic,
            ((master_failures_by_commit.timeout > 0))::integer AS has_timeout,
            ((master_failures_by_commit.known_broken > 0))::integer AS has_known_broken,
            ((master_failures_by_commit.pattern_matched > 0))::integer AS has_pattern_matched,
            ((master_failures_by_commit.flaky > 0))::integer AS has_flaky,
            master_failures_by_commit.total AS failure_count,
            master_failures_by_commit.succeeded AS succeeded_count,
            master_failures_by_commit.idiopathic AS idiopathic_count,
            master_failures_by_commit.timeout AS timeout_count,
            master_failures_by_commit.known_broken AS known_broken_count,
            master_failures_by_commit.pattern_matched AS pattern_matched_count,
            master_failures_by_commit.pattern_unmatched AS pattern_unmatched_count,
            master_failures_by_commit.flaky AS flaky_count,
            commit_metadata.committer_date,
            master_failures_by_commit.commit_index
           FROM (public.master_failures_by_commit
             JOIN public.commit_metadata ON ((master_failures_by_commit.sha1 = commit_metadata.sha1)))
          ORDER BY master_failures_by_commit.commit_index DESC) foo
  GROUP BY (date_trunc('week'::text, foo.committer_date))
  ORDER BY (date_trunc('week'::text, foo.committer_date)) DESC;


ALTER TABLE public.master_failures_weekly_aggregation OWNER TO postgres;

--
-- Name: VIEW master_failures_weekly_aggregation; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.master_failures_weekly_aggregation IS 'NOTE: "pattern_matched_count" excludes "flaky" builds

FIXME: "failure_count" is a misnomer; it is actually the total that includes successes.
"has_failure" is also a misnomer.';


--
-- Name: master_ordered_commits_with_metadata; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_ordered_commits_with_metadata AS
 SELECT ordered_master_commits.id,
    ordered_master_commits.sha1,
    commit_metadata.message,
    commit_metadata.tree_sha1,
    commit_metadata.author_name,
    commit_metadata.author_email,
    commit_metadata.author_date,
    commit_metadata.committer_name,
    commit_metadata.committer_email,
    commit_metadata.committer_date
   FROM (public.ordered_master_commits
     JOIN public.commit_metadata ON ((commit_metadata.sha1 = ordered_master_commits.sha1)));


ALTER TABLE public.master_ordered_commits_with_metadata OWNER TO postgres;

--
-- Name: master_unmarked_breakage_regions_by_commit; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.master_unmarked_breakage_regions_by_commit AS
 SELECT bar.vcs_revision,
    bar.only_longitudinal_breakages,
    bar.only_lateral_breakages,
    bar.both_breakages,
    master_ordered_commits_with_metadata.id,
    master_ordered_commits_with_metadata.sha1,
    master_ordered_commits_with_metadata.message,
    master_ordered_commits_with_metadata.tree_sha1,
    master_ordered_commits_with_metadata.author_name,
    master_ordered_commits_with_metadata.author_email,
    master_ordered_commits_with_metadata.author_date,
    master_ordered_commits_with_metadata.committer_name,
    master_ordered_commits_with_metadata.committer_email,
    master_ordered_commits_with_metadata.committer_date
   FROM (( SELECT foo.vcs_revision,
            sum(((foo.is_longitudinal_breakage AND (NOT foo.is_lateral_breakage)))::integer) AS only_longitudinal_breakages,
            sum(((foo.is_lateral_breakage AND (NOT foo.is_longitudinal_breakage)))::integer) AS only_lateral_breakages,
            sum(((foo.is_longitudinal_breakage AND foo.is_lateral_breakage))::integer) AS both_breakages
           FROM ( SELECT master_detected_breakages_without_annotations.universal_build,
                    master_detected_breakages_without_annotations.contiguous_group_position,
                    master_detected_breakages_without_annotations.contiguous_group_index,
                    master_detected_breakages_without_annotations.contiguous_start_commit_index,
                    master_detected_breakages_without_annotations.contiguous_end_commit_index,
                    master_detected_breakages_without_annotations.cluster_id,
                    master_detected_breakages_without_annotations.cluster_member_count,
                    master_detected_breakages_without_annotations.contiguous_length,
                    master_detected_breakages_without_annotations.is_longitudinal_breakage,
                    master_detected_breakages_without_annotations.is_lateral_breakage,
                    builds_join_steps.step_id,
                    builds_join_steps.step_name,
                    builds_join_steps.build_num,
                    builds_join_steps.vcs_revision,
                    builds_join_steps.queued_at,
                    builds_join_steps.job_name,
                    builds_join_steps.branch,
                    builds_join_steps.is_timeout,
                    builds_join_steps.universal_build,
                    builds_join_steps.provider,
                    builds_join_steps.succeeded,
                    builds_join_steps.build_namespace,
                    builds_join_steps.started_at,
                    builds_join_steps.finished_at
                   FROM (public.master_detected_breakages_without_annotations
                     JOIN public.builds_join_steps ON ((builds_join_steps.universal_build = master_detected_breakages_without_annotations.universal_build)))) foo(universal_build, contiguous_group_position, contiguous_group_index, contiguous_start_commit_index, contiguous_end_commit_index, cluster_id, cluster_member_count, contiguous_length, is_longitudinal_breakage, is_lateral_breakage, step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, is_timeout, universal_build_1, provider, succeeded, build_namespace, started_at, finished_at)
          GROUP BY foo.vcs_revision) bar
     JOIN public.master_ordered_commits_with_metadata ON ((bar.vcs_revision = master_ordered_commits_with_metadata.sha1)))
  ORDER BY master_ordered_commits_with_metadata.id DESC;


ALTER TABLE public.master_unmarked_breakage_regions_by_commit OWNER TO postgres;

--
-- Name: match_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.match_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.match_id_seq OWNER TO postgres;

--
-- Name: match_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.match_id_seq OWNED BY public.matches.id;


--
-- Name: match_position_stats; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.match_position_stats AS
 SELECT match_positions.pattern,
    count(match_positions.build) AS build_count,
    round(avg(match_positions.matched_line_count)) AS average_match_count,
    round(avg(match_positions.line_count)) AS average_line_count,
    avg(match_positions.first_position_fraction) AS average_first_position_fraction,
    avg(match_positions.last_position_fraction) AS average_last_position_fraction,
    min(match_positions.first_line) AS earliest_position,
    max(match_positions.last_line) AS latest_position
   FROM public.match_positions
  GROUP BY match_positions.pattern;


ALTER TABLE public.match_position_stats OWNER TO postgres;

--
-- Name: ordered_master_commits_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.ordered_master_commits_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ordered_master_commits_id_seq OWNER TO postgres;

--
-- Name: ordered_master_commits_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.ordered_master_commits_id_seq OWNED BY public.ordered_master_commits.id;


--
-- Name: pattern_build_job_occurrences; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.pattern_build_job_occurrences WITH (security_barrier='false') AS
 SELECT count(*) AS occurrence_count,
    builds_join_steps.job_name,
    matches_distinct.pattern
   FROM (public.matches_distinct
     JOIN public.builds_join_steps ON ((builds_join_steps.step_id = matches_distinct.build_step)))
  GROUP BY builds_join_steps.job_name, matches_distinct.pattern
  ORDER BY (count(*)) DESC, matches_distinct.pattern DESC;


ALTER TABLE public.pattern_build_job_occurrences OWNER TO postgres;

--
-- Name: pattern_build_step_occurrences; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.pattern_build_step_occurrences WITH (security_barrier='false') AS
 SELECT count(*) AS occurrence_count,
    build_steps_deduped_mitigation.name,
    matches_distinct.pattern
   FROM (public.matches_distinct
     JOIN public.build_steps_deduped_mitigation ON ((build_steps_deduped_mitigation.id = matches_distinct.build_step)))
  GROUP BY build_steps_deduped_mitigation.name, matches_distinct.pattern
  ORDER BY (count(*)) DESC, matches_distinct.pattern DESC;


ALTER TABLE public.pattern_build_step_occurrences OWNER TO postgres;

--
-- Name: pattern_frequency_summary; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.pattern_frequency_summary WITH (security_barrier='false') AS
 SELECT patterns_augmented.expression,
    patterns_augmented.description,
    COALESCE(aggregated_build_matches.matching_build_count, (0)::bigint) AS matching_build_count,
    aggregated_build_matches.most_recent,
    aggregated_build_matches.earliest,
    patterns_augmented.id,
    patterns_augmented.regex,
    patterns_augmented.specificity,
    patterns_augmented.tags,
    patterns_augmented.steps,
    patterns_augmented.scanned_count,
    patterns_augmented.total_scanned_builds,
    patterns_augmented.usually_last_line,
    patterns_augmented.position_likelihood,
    patterns_augmented.has_nondeterministic_values
   FROM (public.patterns_augmented
     LEFT JOIN public.aggregated_build_matches ON ((patterns_augmented.id = aggregated_build_matches.pat)));


ALTER TABLE public.pattern_frequency_summary OWNER TO postgres;

--
-- Name: pattern_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.pattern_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pattern_id_seq OWNER TO postgres;

--
-- Name: pattern_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.pattern_id_seq OWNED BY public.patterns.id;


--
-- Name: pattern_step_applicability_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.pattern_step_applicability_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pattern_step_applicability_id_seq OWNER TO postgres;

--
-- Name: pattern_step_applicability_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.pattern_step_applicability_id_seq OWNED BY public.pattern_step_applicability.id;


--
-- Name: presumed_stable_branches; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.presumed_stable_branches (
    branch text NOT NULL
);


ALTER TABLE public.presumed_stable_branches OWNER TO postgres;

--
-- Name: TABLE presumed_stable_branches; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.presumed_stable_branches IS 'A (small) list of branches that are presumed to be stable.  That is, the branch is "policed" for human-caused breakages.  Such breakages are annotated and reverted ASAP.';


--
-- Name: scans; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.scans (
    id integer NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now(),
    latest_pattern_id integer NOT NULL,
    initiator text
);


ALTER TABLE public.scans OWNER TO postgres;

--
-- Name: scans_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.scans_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.scans_id_seq OWNER TO postgres;

--
-- Name: scans_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.scans_id_seq OWNED BY public.scans.id;


--
-- Name: unattributed_failed_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.unattributed_failed_builds WITH (security_barrier='false') AS
 SELECT global_builds.branch,
    global_builds.global_build_num AS global_build
   FROM (( SELECT build_steps_deduped_mitigation.universal_build
           FROM (public.build_steps_deduped_mitigation
             LEFT JOIN public.matches_distinct ON ((matches_distinct.build_step = build_steps_deduped_mitigation.id)))
          WHERE ((matches_distinct.pattern IS NULL) AND (build_steps_deduped_mitigation.name IS NOT NULL) AND (NOT build_steps_deduped_mitigation.is_timeout))) foo
     JOIN public.global_builds ON ((foo.universal_build = global_builds.global_build_num)));


ALTER TABLE public.unattributed_failed_builds OWNER TO postgres;

--
-- Name: universal_builds_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.universal_builds_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.universal_builds_id_seq OWNER TO postgres;

--
-- Name: universal_builds_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.universal_builds_id_seq OWNED BY public.universal_builds.id;


--
-- Name: unscanned_patterns; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.unscanned_patterns WITH (security_barrier='false') AS
 SELECT builds_join_steps.step_id,
    builds_join_steps.step_name,
    builds_join_steps.universal_build,
    builds_join_steps.vcs_revision,
    builds_join_steps.job_name,
    bar.unscanned_patterns_delimited,
    bar.unscanned_pattern_count,
    builds_join_steps.build_num,
    builds_join_steps.provider,
    COALESCE(builds_join_steps.succeeded, false) AS succeeded,
    builds_join_steps.build_namespace
   FROM (public.builds_join_steps
     JOIN ( SELECT string_agg((patterns.id)::text, ';'::text) AS unscanned_patterns_delimited,
            count(patterns.id) AS unscanned_pattern_count,
            foo.step_id
           FROM (( SELECT build_steps_deduped_mitigation.id AS step_id,
                    COALESCE(scanned_patterns.newest_pattern, '-1'::integer) AS latest_pattern
                   FROM (public.build_steps_deduped_mitigation
                     LEFT JOIN public.scanned_patterns ON ((scanned_patterns.step_id = build_steps_deduped_mitigation.id)))
                  WHERE ((build_steps_deduped_mitigation.name IS NOT NULL) AND (NOT build_steps_deduped_mitigation.is_timeout))) foo
             JOIN public.patterns ON ((patterns.id > foo.latest_pattern)))
          GROUP BY foo.step_id) bar ON ((builds_join_steps.step_id = bar.step_id)));


ALTER TABLE public.unscanned_patterns OWNER TO postgres;

--
-- Name: unvisited_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.unvisited_builds WITH (security_barrier='false') AS
 SELECT universal_builds.build_number AS build_num,
    universal_builds.id AS universal_build_id,
    universal_builds.build_namespace,
    universal_builds.provider,
    universal_builds.commit_sha1,
    universal_builds.succeeded
   FROM (public.universal_builds
     LEFT JOIN public.build_steps_deduped_mitigation ON ((universal_builds.id = build_steps_deduped_mitigation.universal_build)))
  WHERE ((build_steps_deduped_mitigation.universal_build IS NULL) AND (NOT COALESCE(universal_builds.succeeded, true)));


ALTER TABLE public.unvisited_builds OWNER TO postgres;

--
-- Name: build_steps id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps ALTER COLUMN id SET DEFAULT nextval('public.build_steps_id_seq'::regclass);


--
-- Name: ci_providers id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ci_providers ALTER COLUMN id SET DEFAULT nextval('public.ci_providers_id_seq'::regclass);


--
-- Name: code_breakage_cause id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_cause ALTER COLUMN id SET DEFAULT nextval('public.code_breakage_cause_id_seq'::regclass);


--
-- Name: code_breakage_resolution id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_resolution ALTER COLUMN id SET DEFAULT nextval('public.code_breakage_resolution_id_seq'::regclass);


--
-- Name: master_failure_mode_attributions id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.master_failure_mode_attributions ALTER COLUMN id SET DEFAULT nextval('public.master_failure_mode_attributions_id_seq'::regclass);


--
-- Name: master_failure_modes id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.master_failure_modes ALTER COLUMN id SET DEFAULT nextval('public.master_failure_modes_id_seq'::regclass);


--
-- Name: matches id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches ALTER COLUMN id SET DEFAULT nextval('public.match_id_seq'::regclass);


--
-- Name: ordered_master_commits id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ordered_master_commits ALTER COLUMN id SET DEFAULT nextval('public.ordered_master_commits_id_seq'::regclass);


--
-- Name: pattern_step_applicability id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_step_applicability ALTER COLUMN id SET DEFAULT nextval('public.pattern_step_applicability_id_seq'::regclass);


--
-- Name: patterns id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.patterns ALTER COLUMN id SET DEFAULT nextval('public.pattern_id_seq'::regclass);


--
-- Name: scans id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scans ALTER COLUMN id SET DEFAULT nextval('public.scans_id_seq'::regclass);


--
-- Name: universal_builds id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.universal_builds ALTER COLUMN id SET DEFAULT nextval('public.universal_builds_id_seq'::regclass);


--
-- Name: build_steps build_steps_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps
    ADD CONSTRAINT build_steps_pkey PRIMARY KEY (id);


--
-- Name: builds builds_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.builds
    ADD CONSTRAINT builds_pkey PRIMARY KEY (global_build_num);


--
-- Name: cached_master_merge_base cached_master_merge_base_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cached_master_merge_base
    ADD CONSTRAINT cached_master_merge_base_pkey PRIMARY KEY (branch_commit);


--
-- Name: ci_providers ci_providers_hostname_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ci_providers
    ADD CONSTRAINT ci_providers_hostname_key UNIQUE (hostname);


--
-- Name: ci_providers ci_providers_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ci_providers
    ADD CONSTRAINT ci_providers_pkey PRIMARY KEY (id);


--
-- Name: code_breakage_affected_jobs code_breakage_affected_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_affected_jobs
    ADD CONSTRAINT code_breakage_affected_jobs_pkey PRIMARY KEY (job, cause);


--
-- Name: code_breakage_cause code_breakage_cause_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_cause
    ADD CONSTRAINT code_breakage_cause_pkey PRIMARY KEY (id);


--
-- Name: code_breakage_resolution code_breakage_resolution_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_resolution
    ADD CONSTRAINT code_breakage_resolution_pkey PRIMARY KEY (id);


--
-- Name: commit_authorship_supplemental commit_authorship_supplemental_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.commit_authorship_supplemental
    ADD CONSTRAINT commit_authorship_supplemental_pkey PRIMARY KEY (sha1);


--
-- Name: commit_metadata commit_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.commit_metadata
    ADD CONSTRAINT commit_metadata_pkey PRIMARY KEY (sha1);


--
-- Name: created_github_statuses created_github_statuses_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.created_github_statuses
    ADD CONSTRAINT created_github_statuses_pkey PRIMARY KEY (id);


--
-- Name: log_metadata log_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.log_metadata
    ADD CONSTRAINT log_metadata_pkey PRIMARY KEY (step);


--
-- Name: master_failure_mode_attributions master_failure_mode_attributions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.master_failure_mode_attributions
    ADD CONSTRAINT master_failure_mode_attributions_pkey PRIMARY KEY (id);


--
-- Name: master_failure_modes master_failure_modes_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.master_failure_modes
    ADD CONSTRAINT master_failure_modes_pkey PRIMARY KEY (id);


--
-- Name: matches match_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT match_pkey PRIMARY KEY (id);


--
-- Name: ordered_master_commits ordered_master_commits_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ordered_master_commits
    ADD CONSTRAINT ordered_master_commits_pkey PRIMARY KEY (id);


--
-- Name: ordered_master_commits ordered_master_commits_sha1_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.ordered_master_commits
    ADD CONSTRAINT ordered_master_commits_sha1_key UNIQUE (sha1);


--
-- Name: pattern_authorship pattern_authorship_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_authorship
    ADD CONSTRAINT pattern_authorship_pkey PRIMARY KEY (pattern);


--
-- Name: patterns pattern_expression_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.patterns
    ADD CONSTRAINT pattern_expression_key UNIQUE (expression);


--
-- Name: patterns pattern_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.patterns
    ADD CONSTRAINT pattern_pkey PRIMARY KEY (id);


--
-- Name: pattern_step_applicability pattern_step_applicability_pattern_step_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_step_applicability
    ADD CONSTRAINT pattern_step_applicability_pattern_step_name_key UNIQUE (pattern, step_name);


--
-- Name: pattern_step_applicability pattern_step_applicability_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_step_applicability
    ADD CONSTRAINT pattern_step_applicability_pkey PRIMARY KEY (id);


--
-- Name: pattern_tags pattern_tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_tags
    ADD CONSTRAINT pattern_tags_pkey PRIMARY KEY (pattern, tag);


--
-- Name: presumed_stable_branches presumed_stable_branches_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.presumed_stable_branches
    ADD CONSTRAINT presumed_stable_branches_pkey PRIMARY KEY (branch);


--
-- Name: scanned_patterns scanned_patterns_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT scanned_patterns_pkey PRIMARY KEY (scan, newest_pattern, step_id);


--
-- Name: scans scans_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scans
    ADD CONSTRAINT scans_pkey PRIMARY KEY (id);


--
-- Name: universal_builds universal_builds_build_number_build_namespace_provider_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.universal_builds
    ADD CONSTRAINT universal_builds_build_number_build_namespace_provider_key UNIQUE (build_number, build_namespace, provider);


--
-- Name: universal_builds universal_builds_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.universal_builds
    ADD CONSTRAINT universal_builds_pkey PRIMARY KEY (id);


--
-- Name: blah; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX blah ON public.code_breakage_affected_jobs USING btree (cause);


--
-- Name: breakage_sha1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX breakage_sha1 ON public.code_breakage_cause USING btree (sha1);


--
-- Name: builds_job_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX builds_job_name_idx ON public.builds USING btree (job_name);


--
-- Name: cause_fk; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX cause_fk ON public.code_breakage_resolution USING btree (cause);


--
-- Name: fk_ci_provider; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_ci_provider ON public.universal_builds USING btree (provider);


--
-- Name: fk_master_commit; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_master_commit ON public.cached_master_merge_base USING btree (master_commit);


--
-- Name: fk_pattern_step; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_pattern_step ON public.pattern_step_applicability USING btree (pattern);


--
-- Name: fk_patternid; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_patternid ON public.scans USING btree (latest_pattern_id);


--
-- Name: fk_sha1_thing; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_sha1_thing ON public.code_breakage_resolution USING btree (sha1);


--
-- Name: fk_tag_pattern; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_tag_pattern ON public.pattern_tags USING btree (pattern);


--
-- Name: fki_bre; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_bre ON public.master_failure_mode_attributions USING btree (cause_id);


--
-- Name: fki_fk_global_buildnum; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_global_buildnum ON public.builds USING btree (global_build_num);


--
-- Name: fki_fk_mode_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_mode_id ON public.master_failure_mode_attributions USING btree (mode_id);


--
-- Name: fki_fk_pattern; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_pattern ON public.scanned_patterns USING btree (newest_pattern);


--
-- Name: fki_fk_scan; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_scan ON public.scanned_patterns USING btree (scan);


--
-- Name: fki_fk_step_id_scanned_pattern; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_step_id_scanned_pattern ON public.scanned_patterns USING btree (step_id);


--
-- Name: fki_fk_ubuild; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_ubuild ON public.build_steps USING btree (universal_build);


--
-- Name: idx_github_status_post_description; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_github_status_post_description ON public.created_github_statuses USING btree (description);


--
-- Name: idx_is_timeout; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_is_timeout ON public.build_steps USING btree (is_timeout);


--
-- Name: idx_line_number; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_line_number ON public.matches USING btree (line_number);


--
-- Name: idx_specificity; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_specificity ON public.patterns USING btree (specificity);


--
-- Name: idx_step_name; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_step_name ON public.build_steps USING btree (name);


--
-- Name: idx_universal_builds_sha1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_universal_builds_sha1 ON public.universal_builds USING btree (commit_sha1);


--
-- Name: idx_universal_builds_succeeded; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idx_universal_builds_succeeded ON public.universal_builds USING btree (succeeded);


--
-- Name: cached_master_merge_base cached_master_merge_base_master_commit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cached_master_merge_base
    ADD CONSTRAINT cached_master_merge_base_master_commit_fkey FOREIGN KEY (master_commit) REFERENCES public.ordered_master_commits(sha1);


--
-- Name: code_breakage_affected_jobs code_breakage_affected_jobs_cause_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_affected_jobs
    ADD CONSTRAINT code_breakage_affected_jobs_cause_fkey FOREIGN KEY (cause) REFERENCES public.code_breakage_cause(id) ON DELETE CASCADE;


--
-- Name: code_breakage_cause code_breakage_cause_sha1_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_cause
    ADD CONSTRAINT code_breakage_cause_sha1_fkey FOREIGN KEY (sha1) REFERENCES public.ordered_master_commits(sha1);


--
-- Name: code_breakage_resolution code_breakage_resolution_cause_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_resolution
    ADD CONSTRAINT code_breakage_resolution_cause_fkey FOREIGN KEY (cause) REFERENCES public.code_breakage_cause(id) ON DELETE CASCADE;


--
-- Name: code_breakage_resolution code_breakage_resolution_sha1_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.code_breakage_resolution
    ADD CONSTRAINT code_breakage_resolution_sha1_fkey FOREIGN KEY (sha1) REFERENCES public.ordered_master_commits(sha1);


--
-- Name: builds fk_global_buildnum; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.builds
    ADD CONSTRAINT fk_global_buildnum FOREIGN KEY (global_build_num) REFERENCES public.universal_builds(id);


--
-- Name: master_failure_mode_attributions fk_mode_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.master_failure_mode_attributions
    ADD CONSTRAINT fk_mode_id FOREIGN KEY (mode_id) REFERENCES public.master_failure_modes(id) ON DELETE CASCADE;


--
-- Name: scanned_patterns fk_pattern; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT fk_pattern FOREIGN KEY (newest_pattern) REFERENCES public.patterns(id);


--
-- Name: scanned_patterns fk_scan; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT fk_scan FOREIGN KEY (scan) REFERENCES public.scans(id);


--
-- Name: scanned_patterns fk_step_id_scanned_pattern; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT fk_step_id_scanned_pattern FOREIGN KEY (step_id) REFERENCES public.build_steps(id) ON DELETE CASCADE;


--
-- Name: build_steps fk_ubuild; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps
    ADD CONSTRAINT fk_ubuild FOREIGN KEY (universal_build) REFERENCES public.universal_builds(id);


--
-- Name: log_metadata log_metadata_step_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.log_metadata
    ADD CONSTRAINT log_metadata_step_fkey FOREIGN KEY (step) REFERENCES public.build_steps(id) ON DELETE CASCADE;


--
-- Name: matches match_pattern_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT match_pattern_fkey FOREIGN KEY (pattern) REFERENCES public.patterns(id);


--
-- Name: matches matches_build_step_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT matches_build_step_fkey FOREIGN KEY (build_step) REFERENCES public.build_steps(id) ON DELETE CASCADE;


--
-- Name: matches matches_scan_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT matches_scan_id_fkey FOREIGN KEY (scan_id) REFERENCES public.scans(id);


--
-- Name: master_failure_mode_attributions mode_cause_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.master_failure_mode_attributions
    ADD CONSTRAINT mode_cause_id_fk FOREIGN KEY (cause_id) REFERENCES public.code_breakage_cause(id) ON DELETE CASCADE;


--
-- Name: pattern_authorship pattern_authorship_pattern_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_authorship
    ADD CONSTRAINT pattern_authorship_pattern_fkey FOREIGN KEY (pattern) REFERENCES public.patterns(id);


--
-- Name: pattern_step_applicability pattern_step_applicability_pattern_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_step_applicability
    ADD CONSTRAINT pattern_step_applicability_pattern_fkey FOREIGN KEY (pattern) REFERENCES public.patterns(id);


--
-- Name: pattern_tags pattern_tags_pattern_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.pattern_tags
    ADD CONSTRAINT pattern_tags_pattern_fkey FOREIGN KEY (pattern) REFERENCES public.patterns(id);


--
-- Name: scans scans_latest_pattern_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scans
    ADD CONSTRAINT scans_latest_pattern_id_fkey FOREIGN KEY (latest_pattern_id) REFERENCES public.patterns(id);


--
-- Name: universal_builds universal_builds_provider_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.universal_builds
    ADD CONSTRAINT universal_builds_provider_fkey FOREIGN KEY (provider) REFERENCES public.ci_providers(id) ON DELETE CASCADE;


--
-- Name: SCHEMA work_queues; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA work_queues TO logan;


--
-- Name: TABLE build_steps; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_steps TO logan;


--
-- Name: TABLE build_steps_deduped_mitigation; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_steps_deduped_mitigation TO logan;


--
-- Name: TABLE builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.builds TO logan;


--
-- Name: TABLE universal_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.universal_builds TO logan;


--
-- Name: TABLE global_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.global_builds TO logan;


--
-- Name: TABLE builds_deduped; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.builds_deduped TO logan;


--
-- Name: TABLE log_metadata; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.log_metadata TO logan;


--
-- Name: TABLE matches; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.matches TO logan;


--
-- Name: TABLE matches_distinct; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.matches_distinct TO logan;


--
-- Name: TABLE match_positions; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.match_positions TO logan;


--
-- Name: TABLE match_last_position_frequencies; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.match_last_position_frequencies TO logan;


--
-- Name: TABLE pattern_authorship; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_authorship TO logan;


--
-- Name: TABLE scanned_patterns; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.scanned_patterns TO logan;


--
-- Name: TABLE pattern_scan_counts; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_scan_counts TO logan;


--
-- Name: TABLE pattern_step_applicability; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_step_applicability TO logan;


--
-- Name: TABLE pattern_tags; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_tags TO logan;


--
-- Name: TABLE patterns; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.patterns TO logan;


--
-- Name: TABLE patterns_augmented; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.patterns_augmented TO logan;


--
-- Name: TABLE flaky_patterns_augmented; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.flaky_patterns_augmented TO logan;


--
-- Name: TABLE matches_for_build; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.matches_for_build TO logan;


--
-- Name: TABLE best_pattern_match_for_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.best_pattern_match_for_builds TO logan;


--
-- Name: TABLE aggregated_build_matches; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.aggregated_build_matches TO logan;


--
-- Name: TABLE created_github_statuses; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.created_github_statuses TO logan;


--
-- Name: TABLE aggregated_github_status_postings; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.aggregated_github_status_postings TO logan;


--
-- Name: TABLE builds_join_steps; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.builds_join_steps TO logan;


--
-- Name: TABLE matches_with_log_metadata; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.matches_with_log_metadata TO logan;


--
-- Name: TABLE best_pattern_match_augmented_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.best_pattern_match_augmented_builds TO logan;


--
-- Name: SEQUENCE broken_revisions_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.broken_revisions_id_seq TO logan;


--
-- Name: TABLE code_breakage_affected_jobs; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.code_breakage_affected_jobs TO logan;


--
-- Name: TABLE code_breakage_cause; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.code_breakage_cause TO logan;


--
-- Name: TABLE code_breakage_resolution; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.code_breakage_resolution TO logan;


--
-- Name: TABLE master_failure_mode_attributions; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_failure_mode_attributions TO logan;


--
-- Name: TABLE ordered_master_commits; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.ordered_master_commits TO logan;


--
-- Name: TABLE code_breakage_spans; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.code_breakage_spans TO logan;


--
-- Name: TABLE master_commit_known_breakage_causes; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_commit_known_breakage_causes TO logan;


--
-- Name: TABLE known_broken_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.known_broken_builds TO logan;


--
-- Name: TABLE build_failure_causes; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_failure_causes TO logan;


--
-- Name: TABLE build_failure_causes_disjoint; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_failure_causes_disjoint TO logan;


--
-- Name: TABLE build_failure_disjoint_causes_by_commit; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_failure_disjoint_causes_by_commit TO logan;


--
-- Name: SEQUENCE build_steps_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.build_steps_id_seq TO logan;


--
-- Name: TABLE cached_master_merge_base; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.cached_master_merge_base TO logan;


--
-- Name: TABLE ci_providers; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.ci_providers TO logan;


--
-- Name: SEQUENCE ci_providers_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.ci_providers_id_seq TO logan;


--
-- Name: SEQUENCE code_breakage_cause_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.code_breakage_cause_id_seq TO logan;


--
-- Name: SEQUENCE code_breakage_resolution_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.code_breakage_resolution_id_seq TO logan;


--
-- Name: TABLE commit_authorship_supplemental; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.commit_authorship_supplemental TO logan;


--
-- Name: TABLE commit_metadata; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.commit_metadata TO logan;


--
-- Name: TABLE idiopathic_build_failures; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.idiopathic_build_failures TO logan;


--
-- Name: TABLE job_failure_frequencies; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.job_failure_frequencies TO logan;


--
-- Name: TABLE known_breakage_summaries; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.known_breakage_summaries TO logan;


--
-- Name: TABLE master_contiguous_failures; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_contiguous_failures TO logan;


--
-- Name: TABLE master_contiguous_failure_job_groups; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_contiguous_failure_job_groups TO logan;


--
-- Name: TABLE master_contiguous_failure_blocks; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_contiguous_failure_blocks TO logan;


--
-- Name: TABLE master_contiguous_failure_blocks_with_commits; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_contiguous_failure_blocks_with_commits TO logan;


--
-- Name: TABLE master_intra_commit_failure_groups; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_intra_commit_failure_groups TO logan;


--
-- Name: TABLE master_detected_adjacent_breakages; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_detected_adjacent_breakages TO logan;


--
-- Name: TABLE master_detected_breakages_without_annotations; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_detected_breakages_without_annotations TO logan;


--
-- Name: SEQUENCE master_failure_mode_attributions_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.master_failure_mode_attributions_id_seq TO logan;


--
-- Name: TABLE master_failure_modes; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_failure_modes TO logan;


--
-- Name: SEQUENCE master_failure_modes_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.master_failure_modes_id_seq TO logan;


--
-- Name: TABLE master_failures_by_commit; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_failures_by_commit TO logan;


--
-- Name: TABLE master_failures_raw_causes; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_failures_raw_causes TO logan;


--
-- Name: TABLE master_failures_weekly_aggregation; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_failures_weekly_aggregation TO logan;


--
-- Name: TABLE master_ordered_commits_with_metadata; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_ordered_commits_with_metadata TO logan;


--
-- Name: TABLE master_unmarked_breakage_regions_by_commit; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.master_unmarked_breakage_regions_by_commit TO logan;


--
-- Name: SEQUENCE match_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.match_id_seq TO logan;


--
-- Name: TABLE match_position_stats; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.match_position_stats TO logan;


--
-- Name: SEQUENCE ordered_master_commits_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.ordered_master_commits_id_seq TO logan;


--
-- Name: TABLE pattern_build_job_occurrences; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_build_job_occurrences TO logan;


--
-- Name: TABLE pattern_build_step_occurrences; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_build_step_occurrences TO logan;


--
-- Name: TABLE pattern_frequency_summary; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_frequency_summary TO logan;


--
-- Name: SEQUENCE pattern_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.pattern_id_seq TO logan;


--
-- Name: SEQUENCE pattern_step_applicability_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.pattern_step_applicability_id_seq TO logan;


--
-- Name: TABLE presumed_stable_branches; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.presumed_stable_branches TO logan;


--
-- Name: TABLE scans; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.scans TO logan;


--
-- Name: SEQUENCE scans_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.scans_id_seq TO logan;


--
-- Name: TABLE unattributed_failed_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.unattributed_failed_builds TO logan;


--
-- Name: SEQUENCE universal_builds_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.universal_builds_id_seq TO logan;


--
-- Name: TABLE unscanned_patterns; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.unscanned_patterns TO logan;


--
-- Name: TABLE unvisited_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.unvisited_builds TO logan;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON TABLES  TO logan;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: work_queues; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA work_queues REVOKE ALL ON TABLES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA work_queues GRANT ALL ON TABLES  TO logan;


--
-- PostgreSQL database dump complete
--

