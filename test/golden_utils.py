"""Helpers shared by PyGeopack golden-data generation and tests."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path
from typing import Any

import numpy as np

NONFINITE_STRINGS = {
    "NaN": math.nan,
    "Infinity": math.inf,
    "-Infinity": -math.inf,
}


def data_path(filename: str) -> Path:
    return Path(__file__).resolve().parent / "data" / filename


def encode_json_value(value: Any) -> Any:
    if isinstance(value, np.ndarray):
        return encode_json_value(value.tolist())
    if isinstance(value, np.generic):
        return encode_json_value(value.item())
    if isinstance(value, float):
        if math.isnan(value):
            return "NaN"
        if math.isinf(value):
            return "Infinity" if value > 0 else "-Infinity"
        return value
    if isinstance(value, (list, tuple)):
        return [encode_json_value(item) for item in value]
    if isinstance(value, dict):
        return {str(key): encode_json_value(item) for key, item in value.items()}
    return value


def decode_json_value(value: Any) -> Any:
    if isinstance(value, str) and value in NONFINITE_STRINGS:
        return NONFINITE_STRINGS[value]
    if isinstance(value, list):
        return [decode_json_value(item) for item in value]
    if isinstance(value, dict):
        return {key: decode_json_value(item) for key, item in value.items()}
    return value


def write_golden_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    encoded = encode_json_value(payload)
    path.write_text(json.dumps(encoded, indent=2, sort_keys=True) + "\n")


def read_golden_json(path: Path) -> dict[str, Any]:
    return decode_json_value(json.loads(path.read_text()))


def array(value: Any) -> np.ndarray:
    return np.asarray(value, dtype="float64")


def sample_indices(nstep: int) -> list[int]:
    if nstep <= 0:
        return []
    indices = [0, nstep // 2, nstep - 1]
    return sorted(set(indices))


def assert_close(actual: Any, expected: Any, *, rtol: float, atol: float) -> None:
    actual_arr = array(actual)
    expected_arr = array(expected)
    np.testing.assert_allclose(actual_arr, expected_arr, rtol=rtol, atol=atol, equal_nan=True)


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def remove_repo_root_from_syspath() -> None:
    root = repo_root().resolve()
    filtered = []
    for entry in sys.path:
        entry_path = Path(entry or ".").resolve()
        if entry_path != root:
            filtered.append(entry)
    sys.path[:] = filtered


def assert_not_repo_import(pygeopack_module: Any) -> None:
    module_file = Path(pygeopack_module.__file__).resolve()
    source_dir = repo_root() / "PyGeopack"
    try:
        module_file.relative_to(source_dir)
    except ValueError:
        return
    raise AssertionError(
        "Imported PyGeopack from the repository source tree. "
        "Run these tests from inside test/ against an installed package."
    )


def ensure_test_package_on_path() -> None:
    test_dir = Path(__file__).resolve().parent
    if str(test_dir) not in sys.path:
        sys.path.insert(0, str(test_dir))
