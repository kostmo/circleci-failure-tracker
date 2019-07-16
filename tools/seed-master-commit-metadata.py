#!/usr/bin/env python3

import os
import sys
import subprocess
import argparse
import requests
import json

PARENT_DIRECTORY = os.path.abspath(os.path.dirname(__file__))


def get_first_merge_commit(repo_path):
    """
    Returns the most recent sequence of commits
    that have a linear ancestry, ordered from oldest to newest
    """
    command_args = [
        "git",
        "rev-list",
        "--parents",
        "origin/master",
    ]

    command_string = " ".join(command_args)
    # print("Command:", command_string)
    output = subprocess.check_output(command_args, cwd=repo_path)

    for line in output.decode('utf-8').splitlines():
        stripped = line.strip()
        splitted = stripped.split()
        if len(splitted) > 2:
            return splitted[0]


def get_log_json_list(repo_path, merge_commit):
    """
    Returns the most recent sequence of commits
    that have a linear ancestry, ordered from oldest to newest
    """
    command_args = [
        os.path.join(PARENT_DIRECTORY, "git-log2json.sh"),
        merge_commit + ".." + "origin/master",
    ]

    # print("command: " + " ".join(command_args))

    output = subprocess.check_output(command_args, cwd=repo_path)
    return json.loads(output)


def upload_commits(hostname, auth_token, commits):
    url = hostname + '/api/populate-master-commit-metadata'

    headers_dict = {
        'content-type': 'application/json',
        'token': auth_token,
    }

    r = requests.post(url, verify=False, json=commits, headers=headers_dict)
    print(r.json())
    print(r.status_code)


def get_last_excluded_commit(options):

    if options.from_scratch:
        return get_first_merge_commit(options.repo_path)
    else:
        print("Determining latest commit that has metadata...")
        url = options.hostname + '/api/latest-master-commit-with-metadata'
        r = requests.get(url, verify=False)
        parsed_json = r.json()
        print(parsed_json)
        return parsed_json["payload"]


def parse_args():
    parser = argparse.ArgumentParser(description='Fetch master commits')
    parser.add_argument('--repo-path', dest='repo_path', required=True, help='PyTorch repo path')
    parser.add_argument('--token', dest='token', required=True, help='GitHub auth token')
    parser.add_argument('--hostname', dest='hostname', required=True, help='Server hostname')
    parser.add_argument('--from-scratch', dest='from_scratch', action="store_true", help='Populate the database from scratch')

    return parser.parse_args()


if __name__ == "__main__":

    options = parse_args()
    merge_commit = get_last_excluded_commit(options)
    print("Starting (excluded) commit:", merge_commit)
    commit_list_json = get_log_json_list(options.repo_path, merge_commit)
    print("Populating metadata for", len(commit_list_json), "commits...")

    upload_commits(options.hostname, options.token, commit_list_json)
