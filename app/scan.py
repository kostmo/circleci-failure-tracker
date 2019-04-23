#!/usr/bin/env python3

import argparse

import scanner.scanlib as scanlib
import sql.sqlbase as sqlbase


def parse_args():
    parser = argparse.ArgumentParser(description='Fetch CircleCI build logs')
    parser.add_argument('--token', dest='token', help='CircleCI API token (optional)')
    parser.add_argument('--branch', dest='branch', default=scanlib.DEFAULT_BRANCH_NAME, help='Target branch')
    parser.add_argument('--reuse-builds-list', dest='reuse_builds_list', default=False, action='store_true', help="Don't re-fetch the list of builds")
    parser.add_argument('--count', dest='count', type=int, default=100, help='How many builds to fetch')
    parser.add_argument('--hostname', dest='hostname', default="localhost", help='Database hostname (default: localhost)')

    return parser.parse_args()


if __name__ == "__main__":

    options = parse_args()

    conn = sqlbase.get_conn(options.hostname)
    engine = scanlib.Engine(conn)

    scanlib.run(engine, options)
