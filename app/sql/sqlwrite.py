from sql.demo_patterns import SAMPLE_PATTERNS


SQL_BUILD_INSERTION = """
INSERT INTO builds(build_num, vcs_revision, queued_at, job_name)
VALUES(%s, %s, %s, %s)
ON CONFLICT (build_num) DO NOTHING;
"""


# XXX Keep this list up to date so that "scrubbing" the database
# catches everything.
TABLE_NAMES = [
    "scanned_patterns",
    "scans",
    "matches",
    "build_steps",
    "builds",
    "pattern_tags",
    "patterns",
]


def insert_builds(conn, values_to_insert):

    with conn.cursor() as cur:
        for vals in values_to_insert:
            cur.execute(SQL_BUILD_INSERTION, vals)

        conn.commit()


match_insertion_sql = """
INSERT INTO matches(build_step, pattern, line_number, line_text, span_start, span_end)
VALUES(%s, %s, %s, %s, %s, %s);
"""


build_scan_record_insertion_sql = """
INSERT INTO scanned_patterns(build, scan, pattern)
VALUES(%s, %s, %s);
"""


def insert_matches(engine, results):

    results_list = list(results)
    with engine.conn.cursor() as cur:

        # There can be en entry for a build_num even if it had no matches.
        for i, (fetch_success, build_num, searched_pattern_ids, scan_id, build_step_data_tuples) in enumerate(results_list):

            engine.logger.log("On %d/%d results..." % (i, len(results_list)))

            if not fetch_success:
                engine.logger.log("WARNING: Skipping unsuccessful log request for build %d..." % build_num)
                continue

            # NOTE: There will be at most one iteration of this loop, because it is always a singleton list upon success
            for j, (step_name, is_timeout, (line_count, match_list)) in enumerate(build_step_data_tuples):

                engine.logger.log("\tOn %d/%d build_step_data_tuples..." % (j, len(build_step_data_tuples)))

                step_id = insert_build_step(engine.conn, build_num, step_name, is_timeout)

                for k, (pattern_id, line_number, line_text, match_span) in enumerate(match_list):

                    engine.logger.log("\t\tOn %d/%d match_list..." % (k, len(match_list)))

                    match_start, match_end = match_span
                    vals = (step_id, pattern_id, line_number, line_text, match_start, match_end)
                    cur.execute(match_insertion_sql, vals)


            # FIXME represent this differently
            # for j, patt_id in enumerate(searched_pattern_ids):
            #
            #     engine.logger.log("\tXXXX On %d/%d searched_pattern_ids..." % (j, len(searched_pattern_ids)))
            #
            #     build_scan_vals = (build_num, scan_id, patt_id)
            #     cur.execute(build_scan_record_insertion_sql, build_scan_vals)

        engine.conn.commit()


def scrub_tables(conn):
    with conn.cursor() as cur:
        for table in TABLE_NAMES:
            cur.execute("TRUNCATE %s CASCADE;" % table)


def insert_for_id(conn, sql, args):
    with conn.cursor() as cur:
        cur.execute(sql, args)
        conn.commit()
        return cur.fetchone()[0]


def insert_scan_row(conn):
    insertion_sql = """
    INSERT INTO scans(timestamp)
    VALUES(now()) RETURNING id;
    """
    return insert_for_id(conn, insertion_sql, ())


def insert_build_step(conn, build_id, name, is_timeout):
    insertion_sql = """
    INSERT INTO build_steps(build, name, is_timeout)
    VALUES(%s, %s, %s) RETURNING id;
    """
    return insert_for_id(conn, insertion_sql, (build_id, name, is_timeout))


def populate_patterns(conn):

    pattern_insertion_sql = """
    INSERT INTO patterns(regex, expression, description, is_infra) VALUES(%s, %s, %s, %s) RETURNING id;
    """

    tag_insertion_sql = """
    INSERT INTO pattern_tags(tag, pattern) VALUES(%s, %s);
    """

    applicable_step_insertion_sql = """
    INSERT INTO pattern_step_applicability(step_name, pattern) VALUES(%s, %s);
    """

    with conn.cursor() as cur:

        for is_regex, sample_pattern, description, is_infra, tags, applicable_steps in SAMPLE_PATTERNS:
            cur.execute(pattern_insertion_sql, (is_regex, sample_pattern, description, is_infra))
            pattern_id = cur.fetchone()[0]

            for tag in tags:
                cur.execute(tag_insertion_sql, (tag, pattern_id))

            for applicable_step in applicable_steps:
                cur.execute(applicable_step_insertion_sql, (applicable_step, pattern_id))

        conn.commit()
