
def get_rows(conn, sql):
    with conn.cursor() as cur:
        cur.execute(sql)
        return cur.fetchall()


def get_single_value(conn, sql):
    with conn.cursor() as cur:
        cur.execute(sql)
        return cur.fetchone()[0]


def get_failed_commits_by_day(conn):
    # Note that Highcharts expects the dates to be in ascending order

    sql = """
    SELECT queued_at::date AS date, COUNT(*)
    FROM (SELECT vcs_revision, MAX(queued_at) queued_at FROM builds GROUP BY vcs_revision) foo
    GROUP BY date ORDER BY date ASC
    """
    return get_rows(conn, sql)


def get_builds(conn):
    sql = "SELECT build_num, vcs_revision, queued_at, job_name FROM builds ORDER BY build_num"
    return get_rows(conn, sql)


def get_patterns(conn):
    sql = "SELECT id, regex, expression, description FROM patterns ORDER BY description"
    rows = get_rows(conn, sql)
    return {row[0]: tuple(row[1:]) for row in rows}


def get_unscanned_build_patterns(conn):
    sql = """
    SELECT build_num, unscanned_patts
    FROM unscanned_patterns
    ORDER BY patt_count
    """

    unscanned_patterns_by_build = {}
    for row in get_rows(conn, sql):
        unscanned_patterns_by_build[row[0]] = set(map(int, row[1].split(",")))

    return unscanned_patterns_by_build


def get_pattern_scan_histogram(conn):
    sql = """
    SELECT scanned_pattern_count, COUNT(build_num) AS build_count
    FROM scanned_builds GROUP BY scanned_pattern_count
    """
    return get_rows(conn, sql)


def get_pattern_occurrence_rows(conn, pattern_id):
    sql = """
    SELECT build_steps.build AS build_num, build_steps.name, line_number, line_text, span_start, span_end
    FROM (SELECT * FROM matches WHERE pattern = %s) foo
    JOIN build_steps ON build_steps.id = foo.build_step
    ORDER BY build_num DESC
    """
    with conn.cursor() as cur:
        cur.execute(sql, (pattern_id,))
        return cur.fetchall()


def get_match_frequencies(conn):
    sql = """
    SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, COALESCE(foo.tags, '')
    FROM global_match_frequency
    LEFT JOIN (SELECT pattern, string_agg(pattern_tags.tag, ',') AS tags FROM pattern_tags GROUP BY pattern) foo
    ON foo.pattern = global_match_frequency.id
    ORDER BY matching_build_count DESC
    """
    return get_rows(conn, sql)



def get_build_step_failure_frequencies(conn):
    sql = """
    SELECT name, COUNT(*) AS freq FROM build_steps GROUP BY name ORDER BY freq DESC
    """
    return get_rows(conn, sql)


def count_total_matched_builds(conn):
    sql = """
    SELECT SUM(matching_build_count) FROM global_match_frequency
    """
    return get_single_value(conn, sql)


def get_job_failure_frequencies(conn):
    sql = """
    SELECT job_name, freq, last FROM job_failure_frequencies
    """
    return get_rows(conn, sql)


def sum_job_failure_frequencies(conn):
    sql = """
    SELECT SUM(freq) FROM job_failure_frequencies
    """
    return get_single_value(conn, sql)


def get_unattributed_failures(conn, limit=None):
    sql = """
    SELECT build_num, vcs_revision, queued_at, job_name FROM unattributed_failed_builds
    JOIN builds on builds.build_num = unattributed_failed_builds.build
    """
    if limit is not None:
        sql += " LIMIT %d" % limit

    return get_rows(conn, sql)


def count_unattributed_failures(conn):
    sql = """
    SELECT COUNT(*) FROM unattributed_failed_builds
    """
    return get_single_value(conn, sql)


def get_idiopathic_failures(conn, limit=None):
    sql = """
    SELECT builds.build_num, vcs_revision, queued_at, job_name FROM idiopathic_build_failures
    JOIN builds on builds.build_num = idiopathic_build_failures.build_num WHERE idiopathic_build_failures.is_timeout = FALSE
    """
    if limit is not None:
        sql += " LIMIT %d" % limit

    return get_rows(conn, sql)


def count_idiopathic_failures(conn):
    sql = """
    SELECT COUNT(*) FROM idiopathic_build_failures WHERE is_timeout = FALSE
    """
    return get_single_value(conn, sql)


def get_timeout_failures(conn, limit=None):
    sql = """
    SELECT builds.build_num, vcs_revision, queued_at, job_name FROM idiopathic_build_failures
    JOIN builds on builds.build_num = idiopathic_build_failures.build_num WHERE idiopathic_build_failures.is_timeout = TRUE
    """
    if limit is not None:
        sql += " LIMIT %d" % limit

    return get_rows(conn, sql)


def count_timeout_failures(conn):
    sql = """
    SELECT COUNT(*) FROM idiopathic_build_failures WHERE is_timeout = TRUE
    """
    return get_single_value(conn, sql)
