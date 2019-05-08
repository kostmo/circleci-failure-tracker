#!/bin/bash

sudo service postgresql restart
sudo -u postgres dropdb loganci

sudo -u postgres psql < ../schema.sql
