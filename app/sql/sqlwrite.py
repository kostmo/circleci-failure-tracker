
SQL_BUILD_INSERTION = """
INSERT INTO builds(build_num, vcs_revision, queued_at, job_name)
VALUES(%s, %s, %s, %s)
ON CONFLICT (build_num) DO NOTHING;
"""

SAMPLE_PATTERNS = [
    (False, "FAILED: ", "Ninja build failed", False, []),
    (True, r"([^\s]+):(\d+):(\d+): error:", "Compilation error", False, ["compile"]),



    (False, "[  FAILED  ]", "Failed test", False, ["runtime"]),
    (False, "Error: ", "Generic code error", False, []),
    (False, "ERROR: ", "Another Generic code error", False, []),
    (False, "Segmentation fault", "Segfault", False, ["runtime"]),
    (True,  "find: (.+): No such file or directory", "find error", False, []),
    (False, "unzip:  cannot find zipfile directory", "Unzip failed", False, []),
    (False, "RuntimeError: ", "Python runtime error", False, []),
    (False, "Build left local git repository checkout dirty", "Build dirtied the source tree", False, []),
    (False, "E: Failed to fetch", "apt error", True, ["apt"]),
    (False, "E: Could not get lock /var/lib/apt/lists/lock", "CircleCI apt lock failure", True, ["apt"]),
    (False, "E: Unable to acquire the dpkg frontend lock", "apt failure", True, ["apt"]),
    (False, "Waiting for a VM assignment", "CircleCI outage", True, ["circleci"]),
    (False, "Probably the package for the version we want does not exist", "Conda error", False, []),
    (False, "error: failed to push some refs to", "Git push failed", True, ["git"]),

    (True, "Failed to recurse into submodule path '(.+)'", "Git submodules failure", True, ["git"]),
    (True, "::(.+) FAILED", "Unit test failure", True, ["runtine"]),


    (True, r"fatal: unable to access '(.+)': gnutls_handshake\(\) failed: Error in the pull function", "Git fetch failed", True, ["git"]),

    (False, "E: Unable to correct problems, you have held broken packages", "apt package incompatibility", True, ["apt"]),
]


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


def insert_matches(conn, results):

    with conn.cursor() as cur:

        # There can be en entry for a build_num even if it had no matches.
        for (build_num, searched_pattern_ids, scan_id, build_step_data_tuples) in results:

            for step_name, is_timeout, (line_count, match_list) in build_step_data_tuples:

                step_id = insert_build_step(conn, build_num, step_name, is_timeout)

                for (pattern_id, line_number, line_text, match_span) in match_list:
                    match_start, match_end = match_span
                    vals = (step_id, pattern_id, line_number, line_text, match_start, match_end)
                    cur.execute(match_insertion_sql, vals)

            for patt_id in searched_pattern_ids:

                build_scan_vals = (build_num, scan_id, patt_id)
                cur.execute(build_scan_record_insertion_sql, build_scan_vals)

        conn.commit()


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

    with conn.cursor() as cur:

        for is_regex, sample_pattern, description, is_infra, tags in SAMPLE_PATTERNS:
            cur.execute(pattern_insertion_sql, (is_regex, sample_pattern, description, is_infra))
            pattern_id = cur.fetchone()[0]

            for tag in tags:
                cur.execute(tag_insertion_sql, (tag, pattern_id))

        conn.commit()
