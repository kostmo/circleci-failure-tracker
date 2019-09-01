#!/bin/bash -xe

sudo service postgresql restart
sudo -u postgres dropdb loganci

sudo -u postgres psql < ../configuration/schema.sql
