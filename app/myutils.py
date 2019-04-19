import sys


def printflush(content):
    sys.stdout.write(content + "\n")
    sys.stdout.flush()


class Logger:
    def __init__(self):
        self.buffer = []
        self.warnings = []
        self.should_tee = True

    def log(self, line):
        self.buffer.append(line)
        if self.should_tee:
            printflush(line)

    def warn(self, line):
        self.warnings.append(line)

        sys.stderr.write(line + "\n")
        sys.stderr.flush()
