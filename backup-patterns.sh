#!/bin/bash -xe

LOCAL_PATTERNS_FILEPATH=data/patterns-dump.json

curl -s $SERVER_BASE_URL/api/patterns-dump | jq . --sort-keys > $LOCAL_PATTERNS_FILEPATH

curl -s $SERVER_BASE_URL/api/presumed-stable-branches-dump | jq . --sort-keys > data/presumed-stable-branches.json

curl -s $SERVER_BASE_URL/api/breakages-dump | jq . --sort-keys > data/breakage-reports.json

