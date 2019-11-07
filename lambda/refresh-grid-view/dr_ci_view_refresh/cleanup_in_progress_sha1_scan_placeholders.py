#!/usr/bin/env python3

import psycopg2
import json
from timeit import default_timer as timer
import logging

import logan_db_config


# Set up logging
logging.basicConfig(level=logging.DEBUG,
                    format='%(levelname)s: %(asctime)s: %(message)s')


def do_cleanup_lambda_handler(event, context):

    my_payload = run()

    return {
        "statusCode": 200,
        "body": json.dumps({
            "message": "hello world",
            "payload": my_payload,
        }),
    }


def run():

    print("Now connecting to database...")

    conn = psycopg2.connect(
        host=logan_db_config.db_hostname,
        database=logan_db_config.db_name,
        user=logan_db_config.db_username,
        password=logan_db_config.db_password)

    with conn.cursor() as cur:

        cur.execute('SET SESSION lock_timeout = 3000;')
        cur.execute('SET SESSION statement_timeout = %d;' % (1000*60))  # 1 minute

        print("Work begins now...")

        start = timer()

        cur.execute("WITH deleted AS (DELETE FROM work_queues.queued_sha1_scans WHERE inserted_at < now() - interval '3 hours' RETURNING *) SELECT count(*) FROM deleted;")
        deletion_count = cur.fetchone()[0]
        conn.commit()

        end = timer()

        execution_seconds = end - start
        print("Completed in", execution_seconds, "seconds")

        return {
            "elapsed_time_seconds": execution_seconds,
            "deletion_count": deletion_count,
        }


if __name__ == "__main__":

    payload = run()
    print(payload)

