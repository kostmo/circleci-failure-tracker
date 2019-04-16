from datetime import date, datetime
import json

import sql.sqlread as sqlread
import sql.sqlbase as sqlbase


# From here: https://stackoverflow.com/a/27058505/105137
class DateTimeEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, date) or isinstance(o, datetime):
            return o.isoformat()

        return json.JSONEncoder.default(self, o)


def gen_step_json(db_hostname):

    conn = sqlbase.get_conn(db_hostname)

    match_rows = sqlread.get_build_step_failure_frequencies(conn)

    rows = []
    for row in match_rows:
        rows.append({"name": row[0], "y": row[1]})

    data_dict = {
        "rows": rows,
    }

    return json.dumps(data_dict)


def gen_job_json(db_hostname):

    conn = sqlbase.get_conn(db_hostname)

    match_rows = sqlread.get_job_failure_frequencies(conn)

    rows = []
    for row in match_rows:
        rows.append({"name": row[0], "data": [row[1]]})

    data_dict = {
        "rows": rows,
    }

    return json.dumps(data_dict)


def gen_failed_commits_by_day_json(db_hostname):

    conn = sqlbase.get_conn(db_hostname)

    rows = sqlread.get_failed_commits_by_day(conn)
    data_dict = {
        "rows": rows,
    }

    return json.dumps(data_dict, cls=DateTimeEncoder)
