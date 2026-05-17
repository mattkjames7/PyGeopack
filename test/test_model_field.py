from __future__ import annotations

import pytest

from cases import GOLDEN_VERSION, MODEL_FIELD_CASES
from golden_utils import assert_close


@pytest.mark.parametrize("case", MODEL_FIELD_CASES, ids=[case["name"] for case in MODEL_FIELD_CASES])
def test_model_field_matches_v1_2_7(gp, golden_data, case):
    expected = golden_data["model_fields"][case["name"]]

    bx, by, bz, params = gp.ModelField(ReturnParams=True, **case["args"])

    assert golden_data["metadata"]["version"] == GOLDEN_VERSION
    assert_close(bx, expected["Bx"], rtol=1e-7, atol=1e-8)
    assert_close(by, expected["By"], rtol=1e-7, atol=1e-8)
    assert_close(bz, expected["Bz"], rtol=1e-7, atol=1e-8)
    assert_close(params["iopt"], expected["params"]["iopt"], rtol=0.0, atol=0.0)
    assert_close(params["parmod"], expected["params"]["parmod"], rtol=1e-7, atol=1e-8)
    assert_close(params["Vx"], expected["params"]["Vx"], rtol=1e-7, atol=1e-8)
    assert_close(params["Vy"], expected["params"]["Vy"], rtol=1e-7, atol=1e-8)
    assert_close(params["Vz"], expected["params"]["Vz"], rtol=1e-7, atol=1e-8)
