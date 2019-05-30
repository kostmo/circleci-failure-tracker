#!/bin/bash -xe

# This requires a ~/.pgpass file to be set up!

DB_HOSTNAME=circleci-failure-tracker.cmivczl9ccia.us-east-2.rds.amazonaws.com

dropdb --no-password -U postgres -h $DB_HOSTNAME loganci
psql --no-password -U postgres -h $DB_HOSTNAME < ../schema.sql
