#!/bin/bash

DB_HOSTNAME=circleci-failure-tracker.cmivczl9ccia.us-east-2.rds.amazonaws.com

dropdb -U postgres -h $DB_HOSTNAME loganci
psql -U postgres -h $DB_HOSTNAME < ../schema.sql
