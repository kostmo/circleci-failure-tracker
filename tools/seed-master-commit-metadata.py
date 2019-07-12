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
    url = hostname + '/api/populate-master-commits'

    headers_dict = {
        'content-type': 'application/json',
        'token': auth_token,
    }

    r = requests.post(url, verify=False, json=commits, headers=headers_dict)
    print(r.json())
    print(r.status_code)


def parse_args():
    parser = argparse.ArgumentParser(description='Fetch master commits')
    parser.add_argument('--repo-path', dest='repo_path', required=True, help='PyTorch repo path')
    # parser.add_argument('--token', dest='token', required=True, help='GitHub auth token')
    # parser.add_argument('--hostname', dest='hostname', required=True, help='Server hostname')

    return parser.parse_args()


if __name__ == "__main__":

    options = parse_args()
    merge_commit = get_first_merge_commit(options.repo_path)
    commit_list_json = get_log_json_list(options.repo_path, merge_commit)
    print("Commit count:", len(commit_list_json))

#    upload_commits(options.hostname, options.token, linear_commits)
