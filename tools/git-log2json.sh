#!/usr/bin/env bash

# Adapted from: https://gist.github.com/textarcana/1306223


# Use this one-liner to produce a JSON literal from the Git log:

git log \
    --pretty=format:'{%n  "sha1": "%H",%n  "message": "%f",%n  "tree_sha1": "%T",%n  "author_name": "%an",%n  "author_email": "%aE",%n  "author_date": "%ai",%n  "committer_name": "%cN",%n  "committer_email": "%cE",%n  "committer_date": "%ci"%n},' \
    $@ | \
    perl -pe 'BEGIN{print "["}; END{print "]\n"}' | \
    perl -pe 's/},]/}]/' | \
    sed 's/"author_name": ".*".*",/"author_name": "<invalid>",/' | \
    sed 's/"committer_name": ".*".*",/"committer_name": "<invalid>",/'

# This sed hack at the end is a bit ridiculous; some author names can have double quotes, which break JSON formatting.
