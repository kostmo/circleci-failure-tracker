#!/usr/bin/env python3

import psycopg2
import json
from timeit import default_timer as timer
import logging
import boto3
from botocore.exceptions import ClientError

import logan_db_config
import time
import urllib.request
import zipfile
import io


# Set up logging
#logging.basicConfig(level=logging.DEBUG,
#                    format='%(levelname)s: %(asctime)s: %(message)s')


MAX_LOG_URLS_RETRIEVAL_RETRIES = 5


LOG_DIR_PREFIX = "var/log/eb-docker/containers/eb-current-app"


def process_zip_file(file_obj):

    relevant_lines_for_log = []
    with zipfile.ZipFile(file_obj) as zip_ref:

        log_files = filter(lambda info: info.filename.startswith(LOG_DIR_PREFIX), zip_ref.infolist())
        for info in sorted(log_files, key=lambda x: x.date_time, reverse=True):
            with zip_ref.open(info) as log_fh:
                log_lines = log_fh.readlines()

                for line in log_lines:
                    line_string = line.decode('UTF-8').strip()
                    if line_string.startswith("Posted to: /worker/scan-sha1"):

                        relevant_lines_for_log.append(line_string)

            # Only examine one log per zip file
            break

    return relevant_lines_for_log


def get_eb_worker_logs(eb_environment_id):

    eb_client = boto3.client('elasticbeanstalk', region_name='us-east-2')

    try:
        msg = eb_client.request_environment_info(
                EnvironmentId=eb_environment_id,
                InfoType='bundle',
        )

        print("First message:", msg)

        for i in range(MAX_LOG_URLS_RETRIEVAL_RETRIES):
            msg2 = eb_client.retrieve_environment_info(
                    EnvironmentId=eb_environment_id,
                    InfoType='bundle',
            )

    #        print("BLARG:", msg2)

            environment_info_list = msg2.get("EnvironmentInfo", [])

            if environment_info_list:
                log_timestamp_url_tuples_by_instance_id = {}
                for log_item in environment_info_list:
                    s3_url = log_item['Message']

                    log_timestamp = log_item['SampleTimestamp']
                    ec2_instance_id = log_item['Ec2InstanceId']

                    log_timestamp_url_tuples_by_instance_id.setdefault(ec2_instance_id, []).append((log_timestamp, s3_url, ec2_instance_id))

                log_timestamp_url_tuples = list(map(lambda x: x[0], sorted(log_timestamp_url_tuples_by_instance_id.values(), key=lambda x: x[0], reverse=True)))
                print("Log URL count:", len(log_timestamp_url_tuples))
                # request_id = msg2.get("ResponseMetadata", {}).get('RequestId')
                # print("second request_id:", request_id)

                return log_timestamp_url_tuples

            else:
                print("Environment info was empty. Sleeping...")
                time.sleep(5)

    except ClientError as e:
        logging.error(e)
        return None


def get_queue_depth(sqs_queue_url):

    sqs_client = boto3.client('sqs', region_name='us-east-2')
    try:
        msg = sqs_client.get_queue_attributes(
            QueueUrl=sqs_queue_url,
            AttributeNames=['ApproximateNumberOfMessages'],
        )
    except ClientError as e:
        logging.error(e)
        return None
    return msg['Attributes']['ApproximateNumberOfMessages']


def run():

    print("Now connecting to database...")

    conn = psycopg2.connect(
        host=logan_db_config.db_hostname,
        database=logan_db_config.db_name,
        user=logan_db_config.db_username,
        password=logan_db_config.db_password)

    with conn.cursor() as cur:

        cur.execute('SET SESSION lock_timeout = 3000;')
        cur.execute('SET SESSION statement_timeout = %d;' % (1000*60*3))  # 3 minutes

        print("Work begins now...")

        start = timer()

#        cur.execute("SELECT sha1, is_master, last_event_time FROM work_queues.unqueued_sha1_scan_backlog LIMIT %s;", (sha1_limit,))
#        rows = cur.fetchall()

#        print("Fetched %d rows from database..." % len(list(rows)))


        msg = get_queue_depth(logan_db_config.sqs_queue_url)


        msg2 = get_eb_worker_logs('e-ev8fq2dhbv')
        for timestamp, url, instance_id in msg2[:2]:
            print("timestamp:", timestamp)
            print("url:", url)

            with urllib.request.urlopen(url) as download_file_obj:
                relevant_lines = process_zip_file(io.BytesIO(download_file_obj.read()))
                for i, line in enumerate(relevant_lines):
                    print("\t", i, ":", line)


        end = timer()

        execution_seconds = end - start
        print("Completed in", execution_seconds, "seconds")

        return msg


if __name__ == "__main__":

    payload = run()
    print(payload)

