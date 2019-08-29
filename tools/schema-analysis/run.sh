#!/bin/bash -xe



REPO_ROOT_DIR=../..
DB_CREDENTIALS_JSON_PATH=$REPO_ROOT_DIR/../circleci-failure-tracker-credentials/database-credentials-remote.json

DB_USERNAME=$(jq -r '.["db-user"]' $DB_CREDENTIALS_JSON_PATH)
DB_PASSWORD=$(jq -r '.["db-password"]' $DB_CREDENTIALS_JSON_PATH)
DB_HOSTNAME=$(jq -r '.["db-hostname"]' $DB_CREDENTIALS_JSON_PATH)


./postgres-deps.py --dbname loganci --dbhost $DB_HOSTNAME --dbuser $DB_USERNAME --dbpass $DB_PASSWORD
