#!/usr/bin/env python3

from http.server import BaseHTTPRequestHandler, HTTPServer
import os
import argparse

import server.apigen as apigen
import server.pagegen as pagegen
import scanner.scanlib as scanlib
import sql.sqlbase as sqlbase



CONTENT_TYPE_BY_EXTENSION = {
    ".css": "text/css",
    ".js": "text/javascript",
    ".html": "text/html",
}


class LoganRequestHandler(BaseHTTPRequestHandler):

    def __init__(self, cli_options, *args):
        self.cli_options = cli_options
        self.active_scan_engine = None

        BaseHTTPRequestHandler.__init__(self, *args)

    def do_GET(self):

        status_code = 200
        content_type = 'text/html'
        message = ""

        lstripped_path = self.path.lstrip("/")
        path_parts = lstripped_path.split("/")

        if self.path == "/":

            message = pagegen.gen_toplevel_page(self.cli_options.hostname)

        elif path_parts[0] == "static":

            _stem, file_ext = os.path.splitext(lstripped_path)
            content_type = CONTENT_TYPE_BY_EXTENSION.get(file_ext, "text/plain")

            static_base = os.path.join(os.path.dirname(__file__), "web")

            filepath = os.path.join(static_base, lstripped_path)

            print("Reading file from disk:", filepath)

            if os.path.exists(filepath):
                with open(filepath) as fh:
                    message = fh.read()

            else:
                status_code = 400
                message = "File not found: " + filepath

        elif path_parts[0] == "dynamic":

            if path_parts[1] == "pattern":
                pattern_id = int(path_parts[2])
                message = pagegen.gen_pattern_page(self.cli_options.hostname, pattern_id)

        elif path_parts[0] == "api":

            content_type = 'application/json'

            if path_parts[1] == "job":
                message = apigen.gen_job_json(self.cli_options.hostname)

            elif path_parts[1] == "step":
                message = apigen.gen_step_json(self.cli_options.hostname)

            elif path_parts[1] == "failed-commits-by-day":
                message = apigen.gen_failed_commits_by_day_json(self.cli_options.hostname)

            elif path_parts[1] == "start-scan":

                # FIXME
                hostname = "localhost"

                conn = sqlbase.get_conn(hostname)

                self.active_scan_engine = scanlib.Engine(conn)

                options = scanlib.ScanOptions(hostname)

                # TODO start a thread
                scanlib.run(self.active_scan_engine, options)

            elif path_parts[1] == "get-scan-progress":
                message = apigen.scan_progress(self)

        else:
            message = "Unrecognized path: " + self.path

        self.send_response(status_code)

        self.send_header('Content-type', content_type)
        self.end_headers()

        self.wfile.write(bytes(message, "utf8"))


def parse_args():
    parser = argparse.ArgumentParser(description='Fetch CircleCI build logs')
    parser.add_argument('--hostname', dest='hostname', default="localhost", help='Database hostname (default: localhost)')

    return parser.parse_args()


def run():
    print('starting server...')

    parsed_args = parse_args()

    def handler(*args):
        LoganRequestHandler(parsed_args, *args)

    server_address = ('127.0.0.1', 8081)
    httpd = HTTPServer(server_address, handler)
    print('running server...')
    httpd.serve_forever()


if __name__ == "__main__":
    run()
