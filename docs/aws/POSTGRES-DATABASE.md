# Postgres Database

The database named `circleci-failure-tracker` on Amazon RDS.


## Maintenance
Daily backups are taken automatically.


## Schema modifications

The DB schema is [here](../../configuration/schema.sql).

[pgAdmin4](https://www.pgadmin.org/) is recommended to inspect the database and make schema changes.

After schema changes, the schema should be backed up by running the following:

    cd tools/backup
    ./update-database-schema-from-remote.sh

## Stats

The database consists of:

* 3 schemas
* ~50 tables
* ~135 views
* ~15 materialized views
* 1 function
* 1 trigger function
* 2 main database users (besides "postgres", the admin)

## Data ingestion

Data is ingested via two different routes:

* Elastic Beanstalk application
* AWS Lambda functions

## Main Entities

The primary entities in the database schema include:

* "Universal build": represents a conclusive build outcome.  Abstracts over various CI providers
* Provider-specific build
- Console logs
* Patterns
* Scans
* Pattern matches
* commits
* pull requests
* PR comments

