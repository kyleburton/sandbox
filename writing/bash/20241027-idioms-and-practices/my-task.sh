#!/bin/bash
timestamp () {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

set -eEuo pipefail
>&2 echo "[$(timestamp)] stderr: a message"
echo "[$(timestamp)] stdout: a message"
