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


def get_log_json_list(repo_path, merge_commit, maybe_single_commit):
    """
    Returns the most recent sequence of commits
    that have a linear ancestry, ordered from oldest to newest
    """
    command_args = [
        os.path.join(PARENT_DIRECTORY, "git-log2json.sh"),
    ]

    if maybe_single_commit:
        command_args.extend(["-n1", maybe_single_commit])
    else:
        command_args.append(merge_commit + ".." + "origin/master")


    # print("command: " + " ".join(command_args))

    output = subprocess.check_output(command_args, cwd=repo_path)

    old_json = json.loads(output)

    # Get sanitized commit messages
    new_json = []
    for i, item in enumerate(old_json):

        print("progress: %d/%d" % (i + 1, len(old_json)))

        commit_sha1 = item["sha1"];

        my_command = "git log --format=%B -n 1 " + commit_sha1
        commit_message = subprocess.check_output(my_command, cwd=repo_path, shell=True)
        item["message"] = commit_message.strip()
        new_json.append(item)

    return new_json


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
        if parsed_json["success"]:
            return parsed_json["payload"]
        else:
            return get_first_merge_commit(options.repo_path)


def parse_args():
    parser = argparse.ArgumentParser(description='Fetch master commits')
    parser.add_argument('--repo-path', dest='repo_path', required=True, help='PyTorch repo path')
    parser.add_argument('--token', dest='token', required=True, help='GitHub auth token')
    parser.add_argument('--hostname', dest='hostname', required=True, help='Server hostname')
    parser.add_argument('--from-scratch', dest='from_scratch', action="store_true", help='Populate the database from scratch')
    parser.add_argument('--single-commit', dest='single_commit', help='Single commit to retrieve')

    return parser.parse_args()


if __name__ == "__main__":

    options = parse_args()
    merge_commit = get_last_excluded_commit(options)
    print("Starting (excluded) commit:", merge_commit)
    commit_list_json = get_log_json_list(options.repo_path, merge_commit, options.single_commit)
    print("Populating metadata for", len(commit_list_json), "commits...")

    upload_commits(options.hostname, options.token, commit_list_json)
