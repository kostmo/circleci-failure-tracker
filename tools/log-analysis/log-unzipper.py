#!/usr/bin/env python3

import os, sys
import subprocess

import zipfile


LOG_DIR_PREFIX = "var/log/eb-docker/containers/eb-current-app"


def process_zip_file(full_path):
            relevant_lines_for_log = []
            with zipfile.ZipFile(full_path, "r") as zip_ref:

                log_files = filter(lambda info: info.filename.startswith(LOG_DIR_PREFIX), zip_ref.infolist())
                for info in sorted(log_files, key=lambda x: x.date_time, reverse=True):
                    with zip_ref.open(info) as log_fh:
                        log_lines = log_fh.readlines()

                        for line in log_lines:
                            line_string = line.decode('UTF-8')
                            if line_string.startswith("Posted to: /worker/scan-sha1"):

                                relevant_lines_for_log.append(line_string)

                    break  # Only examine one log per zip file



def unzip_all(base_path):

    lines_by_log = {}

    for filename in os.listdir(base_path):
        stem, extension = os.path.splitext(filename)
        if extension == ".zip":
            full_path = os.path.join(base_path, filename)

            relevant_lines_for_log = process_zip_file(full_path)
            lines_by_log[filename] = relevant_lines_for_log


    for k, v in lines_by_log.items():
        print(k)
        for line in v:
            sys.stdout.write("\t" + line)


if __name__ == "__main__":
    unzip_all(sys.argv[1])
