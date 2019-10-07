#!/bin/bash -xe

# First, run "" in a separate window.

curl -d '{"sha1":"1250acef90a3387febb2d70ec643038270c6ef84", "msg":"value2"}' -H "Content-Type: application/json" -X POST http://localhost:3001/worker/scan-sha1
