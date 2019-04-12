# A log analyzer for CircleCI

One would like to determine what the most common causes of intermittent build
failures/flaky tests are in the a repository so that effort can be prioritized
to fix them.

This tool obtains a list of CircleCI builds run against a GitHub repository for
a given branch, downloads their logs from AWS, and scans the logs for a
predefined list of labeled patterns (regular expressions).

The frequency of occurrence of each pattern are tracked and presented in a web
UI.

The database tracks which builds have been already scanned for a given pattern,
so that scanning may be performed incrementally, or resumed after abort.

## Usage

### Prerequisites

On Ubuntu 18.04:

    # installs version 10
    sudo apt install postgresql

### Running

1. Prepare the database

        # create a database user
        sudo -u postgres psql -c "CREATE USER logan WITH PASSWORD 'logan01';"

        # create the database
        sudo -u postgres psql < schema.sql

2. Run the scanning tool:

        ./scan.py

3. Start the webservice:

        ./frontend.py

#### Troubleshooting

If you need to start over, you can drop the database with:

    sudo -u postgres dropdb loganci


## Development

### TODO

* Why are "Unexplained" and "Timeouts" empty?

### Capturing the database schema

Ran this command:

    ./update-database-schema.sh
