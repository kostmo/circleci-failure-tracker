#!/bin/bash -xe

sudo -u postgres pg_dump --create -s -U postgres -d loganci > schema.sql
