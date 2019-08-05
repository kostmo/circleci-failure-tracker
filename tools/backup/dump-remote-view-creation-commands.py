#!/usr/bin/env python3

import os
import re
import sys
import json

CURRENT_DIR = os.path.dirname(__file__)
REPO_ROOT_DIR = os.path.abspath(os.path.join(CURRENT_DIR, "../.."))


RUNNING_BACKUP = False


def get_view_names():

    view_names = []

    schema_filepath = os.path.join(REPO_ROOT_DIR, "configuration/schema.sql")
    for line in open(schema_filepath):
        matches = re.search("CREATE VIEW public\.([^\s+]+) ", line)
        if matches:
            view_names.append(matches.group(1))


    return view_names


def get_db_hostname():

    with open(os.path.join(REPO_ROOT_DIR, "../circleci-failure-tracker-credentials/database-credentials-remote.json")) as fh:
        data = json.load(fh)
        return data["db-hostname"]


SCRIPT_PATH = "view-creation.sql"

def dump_view_creation_script():


    view_names = get_view_names()
    print("There are", len(view_names), "views.")

    db_hostname = get_db_hostname()

    cli_args = [
        "pg_dump",
        "-h",
        db_hostname,
#        "--create",
        "-s",
        "-U",
        "postgres",
        "-d",
        "loganci",
    ]

    for v in view_names:
        cli_args.extend(["-t", v])

    cli_args.extend([">", SCRIPT_PATH])

    cli_string = " ".join(cli_args)

    print("CLI string:", cli_string)
    os.system(cli_string)



def run_view_creation_script():

    # psql --no-password -U postgres -h $DB_HOSTNAME < ../configuration/schema.sql

    db_hostname = get_db_hostname()

    cli_args = [
        "psql",
        "--no-password",
        "-U",
        "postgres",
        "-h",
        db_hostname,
        "-d",
        "loganci",
        "<",
        SCRIPT_PATH,
    ]

    cli_string = " ".join(cli_args)

    print("CLI string:", cli_string)
    os.system(cli_string)



if __name__ == "__main__":

    if RUNNING_BACKUP:
        dump_view_creation_script()
    else:
        run_view_creation_script()

