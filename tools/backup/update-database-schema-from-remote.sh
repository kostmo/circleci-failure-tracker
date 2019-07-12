#!/bin/bash -xe

DB_HOSTNAME=$(jq -r '.["db-hostname"]' ../circleci-failure-tracker-credentials/database-credentials-remote.json)

pg_dump -h $DB_HOSTNAME --create -s -U postgres -d loganci > configuration/schema.sql
