#!/bin/bash -xe

SERVER_BASE_URL=http://localhost:3001
LOCAL_FILEPATH=data/patterns-dump.json

curl -s $SERVER_BASE_URL/api/patterns-dump | jq . --sort-keys > $LOCAL_FILEPATH
