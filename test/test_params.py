from __future__ import annotations

import numpy as np

from golden_utils import assert_close


def test_get_model_params_applies_t96_scalar_and_vector_overrides(gp):
    params = gp.Params.GetModelParams(
        [20200101, 20200102],
        [12.0, 13.0],
        "T96",
        Pdyn=[1.5, 2.5],
        SymH=-10.0,
        By=3.0,
        Bz=-2.0,
        Vx=-400.0,
        Vy=1.0,
        Vz=2.0,
    )

    expected_parmod = np.array(
        [
            [1.5, -10.0, 3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            [2.5, -10.0, 3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        ]
    )
    assert_close(params["Date"], [20200101, 20200102], rtol=0.0, atol=0.0)
    assert_close(params["ut"], [12.0, 13.0], rtol=1e-7, atol=1e-7)
    assert_close(params["parmod"], expected_parmod, rtol=0.0, atol=0.0)
    assert_close(params["Vx"], [-400.0, -400.0], rtol=0.0, atol=0.0)
    assert_close(params["Vy"], [1.0, 1.0], rtol=0.0, atol=0.0)
    assert_close(params["Vz"], [2.0, 2.0], rtol=0.0, atol=0.0)
    assert_close(params["iopt"], [0, 0], rtol=0.0, atol=0.0)


def test_get_model_params_rejects_unknown_model(gp):
    assert gp.Params.GetModelParams(20200101, 12.0, "NOTAMODEL") is None
