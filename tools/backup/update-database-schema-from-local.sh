#!/bin/bash -xe

# Allows this script to be invoked from any directory:
cd $(dirname "$0")


sudo -u postgres pg_dump --create -s -U postgres -d loganci > schema-local.sql
