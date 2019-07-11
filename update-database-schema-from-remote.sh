#!/bin/bash -xe

DB_HOSTNAME=$1

pg_dump -h $DB_HOSTNAME --create -s -U postgres -d loganci > configuration/schema.sql
