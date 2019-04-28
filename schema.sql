--
-- PostgreSQL database dump
--

-- Dumped from database version 10.7 (Ubuntu 10.7-1.pgdg18.04+1)
-- Dumped by pg_dump version 10.7 (Ubuntu 10.7-1.pgdg18.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
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
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: build_steps; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.build_steps (
    id integer NOT NULL,
    build integer,
    name text,
    is_timeout boolean
);


ALTER TABLE public.build_steps OWNER TO postgres;

--
-- Name: TABLE build_steps; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.build_steps IS 'There should be zero or one build steps associated with a build, depending on whether the CircleCI JSON build structure indicates that one of the steps failed.

This is known before the console logs are "scanned".';


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
-- Name: matches_for_build; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.matches_for_build AS
 SELECT matches.pattern AS pat,
    build_steps.build
   FROM (public.matches
     JOIN public.build_steps ON ((matches.build_step = build_steps.id)));


ALTER TABLE public.matches_for_build OWNER TO postgres;

--
-- Name: build_match_repetitions; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.build_match_repetitions WITH (security_barrier='false') AS
 SELECT matches_for_build.build,
    matches_for_build.pat,
    count(*) AS repetitions
   FROM public.matches_for_build
  GROUP BY matches_for_build.pat, matches_for_build.build;


ALTER TABLE public.build_match_repetitions OWNER TO postgres;

--
-- Name: builds; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.builds (
    build_num integer NOT NULL,
    vcs_revision character(40),
    queued_at timestamp with time zone,
    job_name text
);


ALTER TABLE public.builds OWNER TO postgres;

--
-- Name: aggregated_build_matches; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.aggregated_build_matches WITH (security_barrier='false') AS
 SELECT build_match_repetitions.pat,
    count(build_match_repetitions.build) AS matching_build_count,
    max(builds.queued_at) AS most_recent,
    sum(build_match_repetitions.repetitions) AS repetitions_across_builds,
    min(builds.queued_at) AS earliest
   FROM (public.build_match_repetitions
     JOIN public.builds ON ((builds.build_num = build_match_repetitions.build)))
  GROUP BY build_match_repetitions.pat;


ALTER TABLE public.aggregated_build_matches OWNER TO postgres;

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
-- Name: patterns; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.patterns (
    expression text,
    id integer NOT NULL,
    description text,
    is_infra boolean,
    regex boolean,
    has_nondeterministic_values boolean,
    is_retired boolean DEFAULT false NOT NULL,
    specificity integer DEFAULT 1 NOT NULL
);


ALTER TABLE public.patterns OWNER TO postgres;

--
-- Name: global_match_frequency; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.global_match_frequency WITH (security_barrier='false') AS
 SELECT patterns.expression,
    patterns.description,
    COALESCE(aggregated_build_matches.matching_build_count, (0)::bigint) AS matching_build_count,
    aggregated_build_matches.most_recent,
    aggregated_build_matches.earliest,
    patterns.id,
    patterns.regex
   FROM (public.aggregated_build_matches
     RIGHT JOIN public.patterns ON ((patterns.id = aggregated_build_matches.pat)));


ALTER TABLE public.global_match_frequency OWNER TO postgres;

--
-- Name: scanned_patterns; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.scanned_patterns (
    scan integer NOT NULL,
    pattern integer NOT NULL,
    build integer NOT NULL
);


ALTER TABLE public.scanned_patterns OWNER TO postgres;

--
-- Name: scanned_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.scanned_builds WITH (security_barrier='false') AS
 SELECT builds.build_num,
    count(*) AS scanned_pattern_count
   FROM (public.scanned_patterns
     LEFT JOIN public.builds ON ((builds.build_num = scanned_patterns.build)))
  GROUP BY builds.build_num
  ORDER BY (count(*)) DESC, builds.build_num DESC;


ALTER TABLE public.scanned_builds OWNER TO postgres;

--
-- Name: idiopathic_build_failures; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.idiopathic_build_failures WITH (security_barrier='false') AS
 SELECT scanned_builds.build_num,
    build_steps.is_timeout
   FROM (public.scanned_builds
     LEFT JOIN public.build_steps ON ((scanned_builds.build_num = build_steps.build)))
  WHERE (build_steps.id IS NULL);


ALTER TABLE public.idiopathic_build_failures OWNER TO postgres;

--
-- Name: VIEW idiopathic_build_failures; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON VIEW public.idiopathic_build_failures IS 'Failed builds that have been scanned, but no specific step had failed';


--
-- Name: job_failure_frequencies; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.job_failure_frequencies AS
 SELECT builds.job_name,
    count(*) AS freq,
    max(builds.queued_at) AS last
   FROM public.builds
  GROUP BY builds.job_name
  ORDER BY (count(*)) DESC, builds.job_name;


ALTER TABLE public.job_failure_frequencies OWNER TO postgres;

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
-- Name: pattern_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.pattern_tags (
    pattern integer NOT NULL,
    tag character varying(20) NOT NULL
);


ALTER TABLE public.pattern_tags OWNER TO postgres;

--
-- Name: pattern_frequency_summary; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.pattern_frequency_summary AS
 SELECT global_match_frequency.id,
    global_match_frequency.regex,
    global_match_frequency.expression,
    global_match_frequency.description,
    global_match_frequency.matching_build_count,
    global_match_frequency.most_recent,
    global_match_frequency.earliest,
    COALESCE(foo.tags, ''::text) AS tags
   FROM (public.global_match_frequency
     LEFT JOIN ( SELECT pattern_tags.pattern,
            string_agg((pattern_tags.tag)::text, ','::text) AS tags
           FROM public.pattern_tags
          GROUP BY pattern_tags.pattern) foo ON ((foo.pattern = global_match_frequency.id)))
  ORDER BY global_match_frequency.matching_build_count DESC;


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
-- Name: pattern_step_applicability; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.pattern_step_applicability (
    id integer NOT NULL,
    pattern integer,
    step_name text
);


ALTER TABLE public.pattern_step_applicability OWNER TO postgres;

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
-- Name: scannable_build_steps; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.scannable_build_steps AS
 SELECT builds.build_num,
    build_steps.id AS step_id,
    build_steps.name AS step_name
   FROM (public.builds
     LEFT JOIN public.build_steps ON ((builds.build_num = build_steps.build)))
  WHERE ((build_steps.name IS NOT NULL) AND (NOT build_steps.is_timeout));


ALTER TABLE public.scannable_build_steps OWNER TO postgres;

--
-- Name: scans; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.scans (
    id integer NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now(),
    latest_pattern_id integer NOT NULL
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

CREATE VIEW public.unattributed_failed_builds AS
 SELECT foo.build
   FROM (( SELECT DISTINCT scanned_patterns.build
           FROM public.scanned_patterns) foo
     LEFT JOIN public.matches_for_build ON ((matches_for_build.build = foo.build)))
  WHERE (matches_for_build.pat IS NULL);


ALTER TABLE public.unattributed_failed_builds OWNER TO postgres;

--
-- Name: unscanned_patterns; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.unscanned_patterns WITH (security_barrier='false') AS
 SELECT foo.build_num,
    count(foo.patt) AS patt_count,
    string_agg((foo.patt)::text, ','::text ORDER BY foo.patt) AS unscanned_patts
   FROM (( SELECT patterns.id AS patt,
            scannable_build_steps.build_num
           FROM public.patterns,
            public.scannable_build_steps) foo
     LEFT JOIN public.scanned_patterns ON (((foo.patt = scanned_patterns.pattern) AND (foo.build_num = scanned_patterns.build))))
  WHERE (scanned_patterns.scan IS NULL)
  GROUP BY foo.build_num;


ALTER TABLE public.unscanned_patterns OWNER TO postgres;

--
-- Name: unvisited_builds; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.unvisited_builds AS
 SELECT builds.build_num
   FROM (public.builds
     LEFT JOIN public.build_steps ON ((builds.build_num = build_steps.build)))
  WHERE (build_steps.id IS NULL);


ALTER TABLE public.unvisited_builds OWNER TO postgres;

--
-- Name: build_steps id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps ALTER COLUMN id SET DEFAULT nextval('public.build_steps_id_seq'::regclass);


--
-- Name: matches id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches ALTER COLUMN id SET DEFAULT nextval('public.match_id_seq'::regclass);


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
-- Name: builds build_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.builds
    ADD CONSTRAINT build_pkey PRIMARY KEY (build_num);


--
-- Name: build_steps build_steps_build_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps
    ADD CONSTRAINT build_steps_build_name_key UNIQUE (build, name);


--
-- Name: build_steps build_steps_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps
    ADD CONSTRAINT build_steps_pkey PRIMARY KEY (id);


--
-- Name: matches match_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT match_pkey PRIMARY KEY (id);


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
-- Name: scanned_patterns scanned_patterns_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT scanned_patterns_pkey PRIMARY KEY (scan, pattern, build);


--
-- Name: scans scans_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scans
    ADD CONSTRAINT scans_pkey PRIMARY KEY (id);


--
-- Name: fk_build_step_build; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_build_step_build ON public.build_steps USING btree (build);


--
-- Name: fk_build_step_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_build_step_id ON public.matches USING btree (build_step);


--
-- Name: fk_pattern_step; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_pattern_step ON public.pattern_step_applicability USING btree (pattern);


--
-- Name: fk_patternid; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_patternid ON public.scans USING btree (latest_pattern_id);


--
-- Name: fk_scan_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_scan_id ON public.matches USING btree (scan_id);


--
-- Name: fk_tag_pattern; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fk_tag_pattern ON public.pattern_tags USING btree (pattern);


--
-- Name: fki_fk_build; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_build ON public.scanned_patterns USING btree (build);


--
-- Name: fki_fk_pattern; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_pattern ON public.scanned_patterns USING btree (pattern);


--
-- Name: fki_fk_scan; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX fki_fk_scan ON public.scanned_patterns USING btree (scan);


--
-- Name: build_steps build_steps_build_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.build_steps
    ADD CONSTRAINT build_steps_build_fkey FOREIGN KEY (build) REFERENCES public.builds(build_num);


--
-- Name: scanned_patterns fk_pattern; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT fk_pattern FOREIGN KEY (pattern) REFERENCES public.patterns(id);


--
-- Name: scanned_patterns fk_scan; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT fk_scan FOREIGN KEY (scan) REFERENCES public.scans(id);


--
-- Name: matches match_pattern_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT match_pattern_fkey FOREIGN KEY (pattern) REFERENCES public.patterns(id);


--
-- Name: matches matches_build_step_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT matches_build_step_fkey FOREIGN KEY (build_step) REFERENCES public.build_steps(id);


--
-- Name: matches matches_scan_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.matches
    ADD CONSTRAINT matches_scan_id_fkey FOREIGN KEY (scan_id) REFERENCES public.scans(id);


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
-- Name: scanned_patterns scanned_patterns_build_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scanned_patterns
    ADD CONSTRAINT scanned_patterns_build_fkey FOREIGN KEY (build) REFERENCES public.builds(build_num);


--
-- Name: scans scans_latest_pattern_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scans
    ADD CONSTRAINT scans_latest_pattern_id_fkey FOREIGN KEY (latest_pattern_id) REFERENCES public.patterns(id);


--
-- Name: TABLE build_steps; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_steps TO logan;


--
-- Name: TABLE matches; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.matches TO logan;


--
-- Name: TABLE matches_for_build; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.matches_for_build TO logan;


--
-- Name: TABLE build_match_repetitions; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.build_match_repetitions TO logan;


--
-- Name: TABLE builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.builds TO logan;


--
-- Name: TABLE aggregated_build_matches; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.aggregated_build_matches TO logan;


--
-- Name: SEQUENCE build_steps_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.build_steps_id_seq TO logan;


--
-- Name: TABLE patterns; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.patterns TO logan;


--
-- Name: TABLE global_match_frequency; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.global_match_frequency TO logan;


--
-- Name: TABLE scanned_patterns; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.scanned_patterns TO logan;


--
-- Name: TABLE scanned_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.scanned_builds TO logan;


--
-- Name: TABLE idiopathic_build_failures; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.idiopathic_build_failures TO logan;


--
-- Name: TABLE job_failure_frequencies; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.job_failure_frequencies TO logan;


--
-- Name: SEQUENCE match_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.match_id_seq TO logan;


--
-- Name: TABLE pattern_tags; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_tags TO logan;


--
-- Name: TABLE pattern_frequency_summary; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_frequency_summary TO logan;


--
-- Name: SEQUENCE pattern_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.pattern_id_seq TO logan;


--
-- Name: TABLE pattern_step_applicability; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.pattern_step_applicability TO logan;


--
-- Name: SEQUENCE pattern_step_applicability_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.pattern_step_applicability_id_seq TO logan;


--
-- Name: TABLE scannable_build_steps; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.scannable_build_steps TO logan;


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
-- Name: TABLE unscanned_patterns; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.unscanned_patterns TO logan;


--
-- Name: TABLE unvisited_builds; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE public.unvisited_builds TO logan;


--
-- PostgreSQL database dump complete
--

