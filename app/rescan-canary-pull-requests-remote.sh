#!/bin/bash -xe

DB_PASSWORD=$1
DB_HOSTNAME=$2

time stack run run-scanner -- --count 2000 --branch pull/18339 --branch pull/18340 --branch pull/18341 --branch pull/18342 --branch pull/18343 --branch pull/18907 --db-password $DB_PASSWORD --db-hostname $DB_HOSTNAME 2>&1 | tee mylog.txt
