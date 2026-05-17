from __future__ import annotations

from cases import COORD_CASES, GOLDEN_VERSION, LAT_LON_CASES, MLT_CASES, MODEL_FIELD_CASES, TRACE_CASES
from golden_utils import assert_not_repo_import


def test_imported_package_is_not_repo_source(gp):
    assert_not_repo_import(gp)


def test_golden_fixture_metadata(golden_data):
    assert golden_data["metadata"]["source"] == "PyGeopack"
    assert golden_data["metadata"]["version"] == GOLDEN_VERSION


def test_golden_fixture_has_all_declared_cases(golden_data):
    assert set(golden_data["model_fields"]) == {case["name"] for case in MODEL_FIELD_CASES}
    assert set(golden_data["traces"]) == {case["name"] for case in TRACE_CASES}
    assert set(golden_data["coords"]) == {case["name"] for case in COORD_CASES}
    assert set(golden_data["lat_lon"]) == {case["name"] for case in LAT_LON_CASES}
    assert set(golden_data["mlt"]) == {case["name"] for case in MLT_CASES}
