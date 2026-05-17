from __future__ import annotations

import numpy as np
import pytest

from cases import COMMON_PARAMS, GOLDEN_VERSION, MODEL_FIELD_CASES
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


def test_model_field_within_mp_only_filters_far_dayside_point(gp):
    kwargs = {
        "Date": 20200101,
        "ut": 12.0,
        "Model": "T96",
        "CoordIn": "GSM",
        "CoordOut": "GSM",
        **COMMON_PARAMS,
    }

    filtered = gp.ModelField([50.0], [0.0], [0.0], WithinMPOnly=True, **kwargs)
    unfiltered = gp.ModelField([50.0], [0.0], [0.0], WithinMPOnly=False, **kwargs)

    assert all(np.isnan(component[0]) for component in filtered)
    assert all(np.isfinite(component[0]) for component in unfiltered)
