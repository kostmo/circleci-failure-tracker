PROJECT_NAME = "pytorch"
REPO_NAME = "pytorch"

CIRCLECI_API_BASE = "/".join(["https://circleci.com/api/v1.1/project/github", PROJECT_NAME, REPO_NAME])


def get_parms(api_token, offset=0):
    parms = {
        "shallow": True,
        "limit": 100,
        "offset": offset,
        "filter": "failed",
        "circle-token": api_token,
    }

    return parms


class FetchException(Exception):
    pass


def get_json_or_fail(r, callback, failure_message):
    if r.status_code == 200:
        return callback(r.json())
    else:
        raise FetchException(failure_message + "\nHTTP status code was: %d" % r.status_code)
