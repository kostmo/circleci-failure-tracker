#!/usr/bin/env python3

import re
import sys
import os
import threading
from concurrent.futures import ThreadPoolExecutor
import urllib.parse
import json
import argparse
import requests

import myutils
import sql.sqlbase as sqlbase
import sql.sqlread as sqlread
import sql.sqlwrite as sqlwrite
import circlefetch
import requests_cache


CACHE_LOGS_TO_DISK = True


BRANCH_NAME = "master"
# BRANCH_NAME = "merge-libtorch-libcaffe2-dev"


MAX_NETWORK_THREADS = 8


class CounterWrapper:
    def __init__(self):
        self.val = 0
        self.counter_lock = threading.Lock()

    def atomic_increment(self, count=1):
        with self.counter_lock:
            self.val += count


def populate_builds(conn, api_token, branch_name, build_count):
    earliest_date_limit = 0
    earliest_date_found = float("inf")

    counter_wrapper = CounterWrapper()

    myutils.printflush("Populating builds list...")

    # FIXME
    # while counter_wrapper.val < 300 or earliest_date_found > earliest_date_limit:
    while counter_wrapper.val < build_count:

        def callback(r_json):

            values_to_insert = []
            for build in r_json:
                vals = (
                    build["build_num"],
                    build["vcs_revision"],
                    build["queued_at"],
                    build["workflows"]["job_name"],
                )

                values_to_insert.append(vals)

            sqlwrite.insert_builds(conn, values_to_insert)
            counter_wrapper.atomic_increment(len(values_to_insert))

        r = requests.get("/".join([
                circlefetch.CIRCLECI_API_BASE,
                "tree",
                urllib.parse.quote(branch_name),
            ]), params=circlefetch.get_parms(api_token, offset=counter_wrapper.val))

        myutils.printflush("Fetch builds starting at offset %d..." % counter_wrapper.val)

        circlefetch.get_json_or_fail(r, callback, "Build list fetch failed for branch: " + branch_name)

        # TODO
        earliest_date_found = earliest_date_limit


def get_matches(regular_expressions, output_url, cache_key):

    def callback(r_json):

        message_raw = r_json[0]["message"]

        split_message_lines = message_raw.splitlines()
        line_count = len(split_message_lines)

        matches = []
        for i, line in enumerate(split_message_lines):

            for pattern_id, is_regex, regex_or_literal, description in regular_expressions:

                if is_regex:
                    match_object = regex_or_literal.search(line)

                    if match_object:
                        match_tuple = (pattern_id, i, line, match_object.span())
                        matches.append(match_tuple)

                else:
                    found_index = line.find(regex_or_literal)

                    if found_index >= 0:
                        match_tuple = (pattern_id, i, line, (found_index, found_index + len(regex_or_literal)))
                        matches.append(match_tuple)

        return line_count, matches

    return from_cache_or_download(output_url, cache_key, callback)


def from_cache_or_download(url, cache_key, callback):
    """
    NOTE: We cannot cache the AWS URL, because it changes with the signature.
    Instead, we cache based on the parameters that were used to request the AWS URL.
    """

    url_cache_basedir = os.path.join(os.path.dirname(__file__), "download-cache")

    import hashlib
    m = hashlib.md5()
    m.update(cache_key.encode('utf-8'))

    filepath = os.path.join(url_cache_basedir, m.hexdigest())
    if CACHE_LOGS_TO_DISK and os.path.isfile(filepath):
        with open(filepath) as fh:
            return callback(json.load(fh))
    else:
        print("Downloading from:", url)

        s = requests.Session()
        r = s.get(url)

        result = circlefetch.get_json_or_fail(r, callback, "Console output fetch failed for URL: " + url)

        if CACHE_LOGS_TO_DISK:
            os.makedirs(url_cache_basedir, exist_ok=True)
            with open(filepath, "w") as fh:
                fh.write(r.text)

        return result


# TODO This doesn't need to return a list;
# Perhaps the database schema could also be updated to combine the "build steps" table
# and the "builds" table?
def get_failed_build_step(regular_expressions, r_url, r_json):
    for step in r_json["steps"]:
        build_step_name = step["name"]
        for action in step["actions"]:

            if action.get("failed"):

                output_url = action.get("output_url")

                if output_url:
                    line_count, matches = get_matches(regular_expressions, output_url, r_url + build_step_name)
                    return [(build_step_name, False, (line_count, matches))]

                else:
                    sys.stderr.write(
                        'WARNING: No output URL for build step "%s", from JSON at URL: %s\n' % (build_step_name, r_url))

            elif action.get("timedout"):
                return [(build_step_name, True, (0, []))]

    return []


def search_log(api_token, patterns_by_id, unscanned_pattern_ids, build_number):

    regular_expressions = []
    for pattern_id in unscanned_pattern_ids:
        is_regex, pattern, description = patterns_by_id[pattern_id]

        compiled_pattern = re.compile(pattern) if is_regex else pattern
        regular_expressions.append((pattern_id, is_regex, compiled_pattern, description))

    r_url = "/".join([circlefetch.CIRCLECI_API_BASE, str(build_number)])

    s = requests_cache.core.CachedSession() if CACHE_LOGS_TO_DISK else requests.Session()
    r = s.get(r_url, params={"circle-token": api_token})

    def callback(r_json):

        build_step_failure_tuples = get_failed_build_step(regular_expressions, r_url, r_json)
        if not build_step_failure_tuples:
            sys.stderr.write(
                'WARNING: No specific step failed for build "%d"\n' % build_number)

        return build_step_failure_tuples

    return circlefetch.get_json_or_fail(r, callback, "Build details fetch failed for build number: " + str(build_number))


def find_matches(conn, api_token):

    unscanned_patterns_by_build = sorted(sqlread.get_unscanned_build_patterns(conn).items(), reverse=True)
    patterns_by_id = sqlread.get_patterns(conn)
    scan_id = sqlwrite.insert_scan_row(conn)

    counter_wrapper = CounterWrapper()

    def search_log_partial(build_pattern_tuple):

        build_num, unscanned_pattern_ids = build_pattern_tuple

        build_step_failure_tuples = search_log(api_token, patterns_by_id, unscanned_pattern_ids, build_num)

        line_count_info_string_parts = []
        for build_step_name, is_timeout, (line_count, matches) in build_step_failure_tuples:
            line_count_info_string_parts.append('"%s": %d' % (build_step_name, line_count))

        counter_wrapper.atomic_increment()

        line_counts_string = ";".join(line_count_info_string_parts)
        substitutions = (counter_wrapper.val, len(unscanned_patterns_by_build), build_num, line_counts_string)

        myutils.printflush("Processed %d/%d logs (build id: %d; linecounts: %s)..." % substitutions)

        return build_num, unscanned_pattern_ids, scan_id, build_step_failure_tuples

    executor = ThreadPoolExecutor(max_workers=MAX_NETWORK_THREADS)
    results = executor.map(search_log_partial, unscanned_patterns_by_build)

    sqlwrite.insert_matches(conn, results)


def run(options):
    conn = sqlbase.get_conn()

    sqlwrite.scrub_tables(conn)

    sqlwrite.populate_patterns(conn)

    populate_builds(conn, options.token, BRANCH_NAME, options.count)
    find_matches(conn, options.token)


def parse_args():
    parser = argparse.ArgumentParser(description='Fetch CircleCI build logs')
    parser.add_argument('--token', dest='token', help='CircleCI API token (optional)')
    parser.add_argument('--count', dest='count', type=int, default=100, help='How many builds to fetch')

    return parser.parse_args()


if __name__ == "__main__":

    parsed_args = parse_args()
    run(parsed_args)
