#!/bin/bash -xe

DB_HOSTNAME=$1

pg_dump --data-only --exclude-table=log_metadata -h $DB_HOSTNAME -U postgres -d loganci > remote-data-dump.sql
