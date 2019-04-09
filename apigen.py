import json

import sqlread
import sqlbase


def gen_step_json():

    conn = sqlbase.get_conn()

    match_rows = sqlread.get_build_step_failure_frequencies(conn)

    rows = []
    for row in match_rows:
        rows.append({"name": row[0], "y": row[1]})

    data_dict = {
        "rows": rows,
    }

    return json.dumps(data_dict)


def gen_job_json():

    conn = sqlbase.get_conn()

    match_rows = sqlread.get_job_failure_frequencies(conn)

    rows = []
    for row in match_rows:
        rows.append({"name": row[0], "data": [row[1]]})

    data_dict = {
        "rows": rows,
    }

    return json.dumps(data_dict)
