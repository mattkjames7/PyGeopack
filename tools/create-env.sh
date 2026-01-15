#!/bin/bash

set -euo pipefail

python3 -m venv env
source env/bin/activate
pip install --upgrade pip setuptools wheel

