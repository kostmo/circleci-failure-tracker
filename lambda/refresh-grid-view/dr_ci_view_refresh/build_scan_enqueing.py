#!/usr/bin/env python3

import psycopg2
import json
from timeit import default_timer as timer
import logging
import boto3
from botocore.exceptions import ClientError

import logan_db_config


# Assign this value before running the program
SQS_QUEUE_URL = 'https://sqs.us-east-2.amazonaws.com/308535385114/awseb-e-ev8fq2dhbv-stack-AWSEBWorkerQueue-ABA62VCOU74N'


# Set up logging
logging.basicConfig(level=logging.DEBUG,
                    format='%(levelname)s: %(asctime)s: %(message)s')


def send_sqs_message(sqs_queue_url, msg_body):
    """
    :param sqs_queue_url: String URL of existing SQS queue
    :param msg_body: String message body
    :return: Dictionary containing information about the sent message. If
        error, returns None.
    """

    # Send the SQS message
    sqs_client = boto3.client('sqs', region_name='us-east-2')
    try:
        msg = sqs_client.send_message(QueueUrl=sqs_queue_url,
                                      MessageBody=msg_body)
    except ClientError as e:
        logging.error(e)
        return None
    return msg


def enqueue_sha1_scans_lambda_handler(event, context):

    my_payload = run(15)

    return {
        "statusCode": 200,
        "body": json.dumps({
            "message": "hello world",
            "payload": my_payload,
        }),
    }


def run(sha1_limit):

    print("Now connecting to database...")

    conn = psycopg2.connect(
        host=logan_db_config.db_hostname,
        database=logan_db_config.db_name,
        user=logan_db_config.db_username,
        password=logan_db_config.db_password)

    with conn.cursor() as cur:

        cur.execute('SET SESSION lock_timeout = 3000;')
        cur.execute('SET SESSION statement_timeout = %d;' % (1000*60*30))  # 30 minutes

        print("Work begins now...")

        start = timer()

        cur.execute("SELECT sha1, is_master, last_event_time FROM work_queues.unqueued_sha1_scan_backlog LIMIT %s;", (sha1_limit,))
        rows = cur.fetchall()

        print("Fetched %d rows from database..." % len(list(rows)))

        for i, (sha1_to_enqueue, is_master, last_event_time) in enumerate(rows):

            # Send some SQS messages
            msg_body_string = json.dumps({
                "sha1": sha1_to_enqueue,
                "msg": f'SQS message #{i}',
            })

            # TODO Consider using "send_message_batch"
            msg = send_sqs_message(SQS_QUEUE_URL, msg_body_string)
            if msg is not None:
                logging.info(f'Sent SQS message ID: {msg["MessageId"]}')

                cur.execute('INSERT INTO work_queues.queued_sha1_scans (sha1) VALUES (%s);', (sha1_to_enqueue,))
                conn.commit()

        end = timer()

        execution_seconds = end - start
        print("Completed in", execution_seconds, "seconds")

        return {
            "elapsed_time_seconds": execution_seconds,
        }


if __name__ == "__main__":

    payload = run(3)
    print(payload)

