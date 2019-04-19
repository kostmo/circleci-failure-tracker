import sql.sqlread as sqlread
import sql.sqlbase as sqlbase

import server.htmlgen as htmlgen

from server.htmlgen import tag


SCRIPT_URLS = [
    "https://code.highcharts.com/highcharts.js",
    "https://code.highcharts.com/modules/exporting.js",
    "https://code.highcharts.com/modules/export-data.js",
    "https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js",
    "/static/script/mychart.js",
]


HEAD_CONTENT = "\n".join([
    tag("link", "", {"rel": "stylesheet", "type": "text/css", "href": "/static/style.css"})
] + [tag("script", "", {"src": url}) for url in SCRIPT_URLS])


PAGE_HEADER = tag("head", tag("title", "Build Failure Analysis") + HEAD_CONTENT)


def gen_pattern_page(db_hostname, pattern_id):
    conn = sqlbase.get_conn(db_hostname)

    rows = sqlread.get_pattern_occurrence_rows(conn, pattern_id)

    def format_row(row):
        build_num = row[0]

        line = row[3]
        start = row[4]
        end = row[5]

        return [
            htmlgen.make_link(htmlgen.gen_circleci_build_url(build_num), str(build_num)),
            row[1],
            str(row[2]),
            tag("span", line[0:start])
                + tag("span", line[start:end], {"class": "highlight"})
                + tag("span", line[end:])
        ]

    headings = [
        "Build number",
        "Build step",
        "Line number",
        "Line text",
    ]

    body = "\n".join([
        tag("h2", "Occurrences of a pattern"),
        tag("p", "pattern id: " + str(pattern_id)),
        htmlgen.make_table(headings, list(map(format_row, rows)), truncate=100),
    ])

    return tag("html", PAGE_HEADER + tag("body", body))


def get_match_rows(conn):

    match_rows = sqlread.get_match_frequencies(conn)

    formatted_rows = []
    for row in match_rows:

        pattern_id = row[0]
        is_regex = row[1]
        pattern_text = row[2]
        pattern_description = row[3]
        frequency = row[4]
        tags = map(lambda x: tag("span", x, {"class": "tag"}), sorted(row[7].split(",")))

        pattern_url = "/".join(["dynamic", "pattern", str(pattern_id)])
        pattern_link = htmlgen.make_link(pattern_url, str(frequency) + " builds")

        pattern_text_wrapped = tag("span", tag("span", pattern_text, {"class": "is_regex"} if is_regex else {}), {"style": "font-family: sans-serif"})

        formatted_row = [
            ", ".join(tags),
            pattern_text_wrapped,
            pattern_description,
            pattern_link,
            htmlgen.format_date(row[5]),
            htmlgen.format_date(row[6]),
        ]

        formatted_rows.append(formatted_row)

    return formatted_rows


def gen_patterns_section(conn):

    headings = [
        "Tags",
        "Pattern " + htmlgen.parens(tag("span", "regex", {"class": "is_regex"})),
        "Description",
        "Frequency",
        "Last occurrence",
        "First occurrence",
    ]

    formatted_rows = get_match_rows(conn)

    matched_build_count = sqlread.count_total_matched_builds(conn)

    explanation_string = """
    This is the sum of the frequency of pattern matches across all builds.
    It might be more than the total number of builds if more than one pattern matches
    a given build.
    """

    deflist_content = "\n".join([
        tag("dt", "Total: " + tag("b", str(matched_build_count) + " matched builds")),
        tag("dd", explanation_string),
    ])

    lines = [
        tag("h2", "Pattern occurrences"),
        htmlgen.make_table(headings, formatted_rows),
        tag("dl", deflist_content)

    ]
    return "\n".join(lines)


def gen_circleci_failures_section(conn):

    idiopathic_failure_rows = list(map(htmlgen.prep_build_row, sqlread.get_idiopathic_failures(conn, 10)))
    idiopathic_failure_count = sqlread.count_idiopathic_failures(conn)

    timeout_failure_rows = list(map(htmlgen.prep_build_row, sqlread.get_timeout_failures(conn, 10)))
    timeout_failure_count = sqlread.count_timeout_failures(conn)

    headings = [
        "Build number",
        "Git SHA1",
        "Timestamp",
        "Job name",
    ]

    lines = [
        tag("h2", "CircleCI failures"),

        tag("h3", "Unexplained"),

        tag("p", "Failed builds that have been scanned, but no specific step had failed"),
        htmlgen.make_table(headings, idiopathic_failure_rows),
        tag("span", "Showing %d of %d" % (len(idiopathic_failure_rows), idiopathic_failure_count)),

        tag("h3", "Timeouts"),

        htmlgen.make_table(headings, timeout_failure_rows),
        tag("span", "Showing %d of %d" % (len(timeout_failure_rows), timeout_failure_count)),
    ]
    return "\n".join(lines)


def gen_job_failure_frequencies_section(conn):

    job_failure_histogram_rows = sqlread.get_job_failure_frequencies(conn)

    job_failure_frequencies_sum = sqlread.sum_job_failure_frequencies(conn)

    lines = [
        tag("h2", "Job failure frequencies"),
        htmlgen.make_table(["Job name", "Frequency", "Most recent"], job_failure_histogram_rows, truncate=10),
        tag("p", "Total: " + str(job_failure_frequencies_sum)),
    ]

    return "\n".join(lines)


def gen_unattributed_failures_section(conn):

    unattributed_failure_rows = list(map(htmlgen.prep_build_row, sqlread.get_unattributed_failures(conn, 10)))
    unattributed_failure_count = sqlread.count_unattributed_failures(conn)

    lines = [
        tag("h2", "Unattributed failures"),
        htmlgen.make_table(["Build number", "Git SHA1", "Timestamp", "Job name"], unattributed_failure_rows),
        tag("span", "Showing %d of %d" % (len(unattributed_failure_rows), unattributed_failure_count)),
    ]

    return "\n".join(lines)


def gen_toplevel_page(db_hostname):
    conn = sqlbase.get_conn(db_hostname)

    pattern_scan_histogram_rows = sqlread.get_pattern_scan_histogram(conn)

    body = "\n".join([

        tag("div", "", {"id": "container-step-failures",
                        "style": "min-width: 310px; height: 400px; max-width: 600px; margin: 0 auto"}),

        tag("div", "", {"id": "container-job-failures",
                        "style": "min-width: 310px; height: 400px; max-width: 600px; margin: 0 auto"}),

        tag("div", "", {"id": "container-failed-commits-by-day",
                        "style": "min-width: 310px; height: 400px; max-width: 600px; margin: 0 auto"}),

        gen_patterns_section(conn),
        gen_unattributed_failures_section(conn),
        gen_job_failure_frequencies_section(conn),
        gen_circleci_failures_section(conn),

        tag("h2", "Builds scanned"),
        htmlgen.make_table(["Pattern Count", "# of builds with this many scanned patterns"], pattern_scan_histogram_rows),
    ])

    return tag("html", PAGE_HEADER + tag("body", body, {"onload": "main();"}))
