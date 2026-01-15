#!/bin/bash

set -euo pipefail

python3.14 -m venv env
source env/bin/activate
pip install --upgrade pip setuptools wheel

