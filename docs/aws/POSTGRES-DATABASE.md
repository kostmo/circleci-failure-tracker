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
