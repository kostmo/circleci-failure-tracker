
SAMPLE_PATTERNS = [
    (False, "FAILED: ", "Ninja build failed", False, [], []),
    (True, r"([^\s]+):(\d+):(\d+): error:", "Compilation error", False, ["compile"], []),

    (False, "[  FAILED  ]", "Failed test", False, ["runtime"], ["Test"]),
    (False, "TypeError: ", "Python error", False, ["runtime", "python"], ["Doc Build and Push"]),
    (False, "AssertionError: ", "Test assertion failure", False, ["runtime", "python"], ["Test"]),

    (False, "CalledProcessError: Command '\['ninja', '-v'\]' returned non-zero exit status", "Ninja build failure", False, ["build"], ["Test"]),

    (False, "ERROR: You need Python (\d+)\.(\d+) or later to use mypy", "Python version error for mypy", False, ["python"], []),
    (False, "ERROR: ", "A generic code error", False, [], []),



    (False, "Segmentation fault", "Segfault", False, ["runtime"], []),
    (True,  "find: (.+): No such file or directory", "find error", False, [], ["Build"]),
    (False, "unzip:  cannot find zipfile directory", "Unzip failed", False, [], []),
    (True, "RuntimeError: test_(.+) failed!", "Python runtime error on test", False, ["runtime", "python"], ["Test"]),
    (True, "RuntimeError: Error building extension '([^']+)'", "Python build error", False, ["build", "python"], ["Test"]),
    (True, r"RuntimeError: \[([^:]+):(\d+)\] Read error \[127.0.0.1\]:(\d+): Connection reset by peer", "Python network error", False, ["python"], ["Test"]),
    (False, "Build left local git repository checkout dirty", "Build dirtied the source tree", False, [], ["Test"]),
    (False, "E: Failed to fetch", "apt error", True, ["apt"], ["Set Up CI Environment After Checkout"]),
    (False, "E: Could not get lock /var/lib/apt/lists/lock", "CircleCI apt lock failure", True, ["apt"], []),
    (False, "E: Unable to acquire the dpkg frontend lock", "apt failure", True, ["apt"], []),
    (False, "Waiting for a VM assignment", "CircleCI outage", True, ["circleci"], []),
    (False, "Probably the package for the version we want does not exist", "Conda error", False, [], []),
    (False, "error: failed to push some refs to", "Git push failed", True, ["git"], ["Doc Build and Push"]),

    (True, "Failed to recurse into submodule path '(.+)'", "Git submodules failure", True, ["git"], ["Run in docker", "Build"]),
    (True, "::(.+) FAILED", "Unit test failure", True, ["runtine"], []),

    (True, r"fatal: unable to access '(.+)': gnutls_handshake\(\) failed: Error in the pull function", "Git fetch failed", True, ["git"], []),

    (False, "E: Unable to correct problems, you have held broken packages", "apt package incompatibility", True, ["apt"], []),
]
