#!/bin/bash

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_DIR="$(realpath "${SCRIPT_DIR}/..")"

CONTAINER=$1

docker cp "${SOURCE_DIR}/." "${CONTAINER}:/home/ubuntu/PyGeopack"
docker exec -u root "${CONTAINER}" chown -R ubuntu:ubuntu /home/ubuntu/PyGeopack

docker cp "${SCRIPT_DIR}/create-env.sh" "${CONTAINER}:/home/ubuntu/create-env.sh"


docker exec -u ubuntu "${CONTAINER}" bash /home/ubuntu/create-env.sh

docker exec -u ubuntu "${CONTAINER}" bash -c "cd /home/ubuntu/PyGeopack && source /home/ubuntu/env/bin/activate && python3.14 setup.py sdist"

docker exec -u ubuntu "${CONTAINER}" bash -c "cd /home/ubuntu/PyGeopack && source /home/ubuntu/env/bin/activate && pip install dist/*.tar.gz"

docker exec -u ubuntu "${CONTAINER}" bash -c "rm -rf /home/ubuntu/PyGeopack"

docker cp "${SCRIPT_DIR}/test_run.py" "${CONTAINER}:/home/ubuntu/test_run.py"

docker exec -u ubuntu "${CONTAINER}" bash -c "source /home/ubuntu/env/bin/activate && python3.14 /home/ubuntu/test_run.py"
