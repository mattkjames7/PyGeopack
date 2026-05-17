from __future__ import annotations

import os
from pathlib import Path

import pytest

from cases import GOLDEN_FILENAME
from golden_utils import (
    assert_not_repo_import,
    data_path,
    read_golden_json,
    remove_repo_root_from_syspath,
)


@pytest.fixture(scope="session")
def gp():
    os.environ.setdefault("GEOPACK_NOWARN", "1")
    os.environ.setdefault("MPLCONFIGDIR", "/tmp/pygeopack-matplotlib")
    remove_repo_root_from_syspath()
    import PyGeopack as pygeopack

    assert_not_repo_import(pygeopack)
    return pygeopack


@pytest.fixture(scope="session")
def golden_data():
    path = data_path(GOLDEN_FILENAME)
    if not path.exists():
        pytest.fail(
            f"Missing golden fixture {Path('data') / GOLDEN_FILENAME}. "
            "Run `cd test && python savetestdata/save_golden_data.py` with PyGeopack==1.2.7 installed."
        )
    return read_golden_json(path)
