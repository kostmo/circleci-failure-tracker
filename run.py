#!/usr/bin/env python3

"""
Obtains credentials and passes them as CLI args to stack invocation
"""

import os
import argparse
import json
import subprocess

import tools.deployment.args_assembly as args_assembly

THIS_DIRECTORY = os.path.abspath(os.path.dirname(__file__))

DEFAULT_CREDENTIALS_DIRECTORY = os.path.join(THIS_DIRECTORY, "../circleci-failure-tracker-credentials")


def parse_args():
    parser = argparse.ArgumentParser(description='Run webapp locally')
    parser.add_argument('--personal-access-token-file', dest='personal_token_file',
                        default=os.path.join(DEFAULT_CREDENTIALS_DIRECTORY, "github-personal-access-token.txt"),
                        help='File containing GitHub personal access token')

    # Note: the "local" credentials use "github-client-id" and "github-client-secret" for
    # the GitHub app named "circleci-failure-attribution-dev", while
    # the "remote" credentials use a client id and secret for the GitHub app named "circleci-failure-attribution".
    # The local credentials should be used along with ngrok
    # (or something similar, like localtunnel: https://localtunnel.github.io/www/) for exposing the app
    # on a local port.
    parser.add_argument('--prod-app', dest='prod_app', action="store_true", help='For production deployment (default is local).  Implies --remote-db')

    parser.add_argument('--prod-db', dest='prod_db', action="store_true", help='Use production (remote) database (default is local)')

    parser.add_argument('--credentials-json-basedir', dest='credentials_json_basedir',
                        default=DEFAULT_CREDENTIALS_DIRECTORY,
                        help='Path to JSON file containing various webapp credentials')

    parser.add_argument('--dockerrun-json-output-path', dest='dockerrun_json',
                        default="Dockerrun.aws.json",
                        help='Path to write Dockerrun.aws.json file')

    parser.add_argument('--no-force-ssl', dest='no_force_ssl', action="store_true", help='Do not force SSL redirection in args placed into Dockerrun.aws.json')

    parser.add_argument('--entrypoint', dest='entrypoint_override', help='Entrypoint binary name (excluding leading path) for Dockerrun.aws.json')


    parser.add_argument('--notification-ingester', dest='notification_ingester', action="store_true", help='Build for the notification ingester application')


    return parser.parse_args()


def gen_credentials_filename(is_db, is_remote, suffix=None):

    credential_type = "database" if is_db else "app"
    locality_suffix = "remote" if is_remote else "local"

    arglist = [credential_type, "credentials", locality_suffix]

    if suffix:
        arglist.append(suffix)

    return "-".join(arglist) + ".json"


if __name__ == "__main__":

    options = parse_args()

    using_prod_db = options.prod_app or options.prod_db

    app_credentials_json_path = os.path.join(options.credentials_json_basedir, gen_credentials_filename(False, options.prod_app))
    db_credentials_json_path = os.path.join(options.credentials_json_basedir, gen_credentials_filename(True, using_prod_db))
    db_mview_credentials_json_path = os.path.join(options.credentials_json_basedir, gen_credentials_filename(True, using_prod_db, "mview-refresher"))



    with open(app_credentials_json_path) as fh_app, open(db_credentials_json_path) as fh_db, open(db_mview_credentials_json_path) as fh_mview_db:

        personal_access_token = open(options.personal_token_file).read().strip()

        nondefault_cli_arglist = args_assembly.generate_app_nondefault_cli_arglist(
            json.load(fh_app),
            json.load(fh_db),
            json.load(fh_mview_db),
            personal_access_token,
            options.notification_ingester,
            options.no_force_ssl)

        if options.prod_app:
            args_assembly.generate_dockerrun_aws_json(options.dockerrun_json, nondefault_cli_arglist, options.entrypoint_override)

        else:
            os.system('find -name "*.tix" -delete')

            cli_args = [
                   "stack",
                   "run",
                   args_assembly.WEBAPP_BINARY_NAME,
                   "--",
                   "--local",
                   "--data-path",
                   "static",
               ] + nondefault_cli_arglist

            command_string = " ".join(cli_args)
            print("Executing command:", command_string)
            subprocess.check_call(command_string, shell=True, cwd="app")


