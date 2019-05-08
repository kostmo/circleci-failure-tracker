#!/bin/bash

DB_HOSTNAME=circleci-failure-tracker.cmivczl9ccia.us-east-2.rds.amazonaws.com
DB_PASSWORD=$1

time stack run run-scanner -- --wipe --count 1000 --branch pull/18339 --branch pull/18340 --branch pull/18341 --branch pull/18342 --branch pull/18343 --branch pull/18907 --db-password $DB_PASSWORD --db-hostname $DB_HOSTNAME 2>&1 | tee mylog.txt
