#!/bin/bash -xe

REPO_ROOT_DIR=..

DB_PASSWORD=$(jq -r '.["db-password"]' $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/database-credentials-remote.json)
DB_HOSTNAME=$(jq -r '.["db-hostname"]' $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/database-credentials-remote.json)
CIRCLECI_API_TOKEN=$(cat $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/circleci-api-token.txt)

#GITHUB_APP_RSA_KEY=$(cat $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/circleci-failure-attribution.private-key.pem)
GITHUB_APP_RSA_KEY=$(cat $REPO_ROOT_DIR/../circleci-failure-tracker-credentials/circleci-failure-attribution.private-key.pem.base64)


stack build --fast
stack exec scan-oneoff -- --db-password "$DB_PASSWORD" --db-hostname "$DB_HOSTNAME" --circleci-api-token "$CIRCLECI_API_TOKEN" --github-app-rsa-pem "$GITHUB_APP_RSA_KEY" --repo-git-dir "$HOME/github/pytorch-repos/pytorch/.git" 2>&1 | tee mylog.txt
