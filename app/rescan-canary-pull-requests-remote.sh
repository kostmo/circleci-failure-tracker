#!/bin/bash -xe

REPO_ROOT_DIR=..
DB_PASSWORD=$(jq -r '.["db-password"]' $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/database-credentials-remote.json)
DB_HOSTNAME=$(jq -r '.["db-hostname"]' $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/database-credentials-remote.json)

stack build --fast
stack exec run-scanner -- --count 6000 --branch master --db-password $DB_PASSWORD --db-hostname $DB_HOSTNAME 2>&1 | tee mylog.txt
