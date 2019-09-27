#!/usr/bin/env python3

import psycopg2
import psycopg2.extras
import yaml
import json
from timeit import default_timer as timer
from multiprocessing.pool import ThreadPool
#import urllib.request
import requests

import db_config
import logan_db_config


def view_refresh_lambda_handler(event, context):
    """Sample pure Lambda function

    Parameters
    ----------
    event: dict, required
        API Gateway Lambda Proxy Input Format

        Event doc: https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-input-format

    context: object, required
        Lambda Context runtime methods and attributes

        Context doc: https://docs.aws.amazon.com/lambda/latest/dg/python-context-object.html

    Returns
    ------
    API Gateway Lambda Proxy Output Format: dict

        Return doc: https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html
    """


    view_names = event["view-names"]
    payload = update_multiple_views(view_names, "lambda")

    return {
        "statusCode": 200,
        "body": json.dumps({
            "message": "hello world",
            "payload": payload,
        }),
    }


def populate_config_info(cur, commit_sha1, build_number):

    url = "https://circleci.com/api/v1.1/project/github/pytorch/pytorch/%d" % build_number

    r = requests.get(url) 
    api_json_obj = r.json()

    yaml_text = api_json_obj.get("circle_yml").get("string")
    yaml_obj = yaml.safe_load(yaml_text)

    workflows_dict = yaml_obj.get("workflows")
    for workflow_name, workflow_obj in filter(lambda x: x[0] != "version", workflows_dict.items()):

        if type(workflow_obj) is dict:
            cur.execute('INSERT INTO circleci_workflows_by_commit (sha1, name) VALUES (%s, %s) RETURNING id;', (commit_sha1, workflow_name))
            workflow_id = cur.fetchone()[0]

            for trigger in workflow_obj.get("triggers", []):
                schedule_obj = trigger.get("schedule", {})

                for k, v in schedule_obj.items():
                    if k == "cron":
                        cur.execute('INSERT INTO circleci_workflow_schedules (workflow, cron_schedule) VALUES (%s, %s);', (workflow_id, v))

            jobs_insertion_values = []

            branch_filters_by_job = {}
            for job_obj in workflow_obj.get("jobs", []):
                if type(job_obj) is dict:
                    job_name = list(job_obj.keys())[0]

                    for key_job_name, job_value_obj in job_obj.items():
                        branch_filter_only_obj = job_value_obj.get("filters", {}).get("branches", {}).get("only")
                        if type(branch_filter_only_obj) is list:
                            branch_filters_by_job.setdefault(job_name, []).extend(branch_filter_only_obj)
                        elif type(branch_filter_only_obj) is str:
                            branch_filters_by_job.setdefault(job_name, []).append(branch_filter_only_obj)

                else:
                    job_name = job_obj

                jobs_insertion_values.append((workflow_id, job_name))

            insert_query = 'INSERT INTO circleci_workflow_jobs (workflow, job_name) VALUES %s'
            psycopg2.extras.execute_values (
                cur, insert_query, jobs_insertion_values, template=None, page_size=100
            )

            for job_name, filters_list in branch_filters_by_job.items():

                filter_insertion_values = [(workflow_id, job_name, branch, True) for branch in filters_list]

                insert_query2 = 'INSERT INTO circleci_job_branch_filters (workflow, job_name, branch, filter_include) VALUES %s'
                psycopg2.extras.execute_values (
                    cur, insert_query2, filter_insertion_values, template=None, page_size=100
                )


def run():

    conn = psycopg2.connect(host=logan_db_config.db_hostname, database=logan_db_config.db_name, user=logan_db_config.db_username, password=logan_db_config.db_password)

    with conn.cursor() as cur1:

        cur1.execute("SELECT sha1, build_num FROM master_commits_unpopulated_circleci_configs LIMIT %s;", (10,))
        rows = cur1.fetchall()

        enumerated_rows = list(enumerate(rows))
        def single_commit_populator(args_tuple):
            (i, (commit_sha1, build_number)) = args_tuple
            print("%d/%d: Populating CircleCI config for commit %s..." % (i + 1, len(enumerated_rows), commit_sha1))

            with conn.cursor() as cur2:
                populate_config_info(cur2, commit_sha1, build_number)

        p = ThreadPool(2)
        p.map(single_commit_populator, enumerated_rows)


    conn.commit()

    return {
        "foo": "bar",
    }


if __name__ == "__main__":

    payload = run()

    print(payload)

