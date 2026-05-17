from __future__ import annotations

import numpy as np

from cases import COMMON_PARAMS
from golden_utils import assert_close


def test_model_field_scalar_list_and_numpy_inputs_match(gp):
    kwargs = {
        "Date": 20200101,
        "ut": 12.0,
        "Model": "T96",
        "CoordIn": "GSM",
        "CoordOut": "GSM",
        **COMMON_PARAMS,
    }

    scalar = gp.ModelField(5.0, 0.0, 0.0, **kwargs)
    as_list = gp.ModelField([5.0], [0.0], [0.0], **kwargs)
    as_array = gp.ModelField(
        np.array([5.0], dtype="float64"),
        np.array([0.0], dtype="float64"),
        np.array([0.0], dtype="float64"),
        **kwargs,
    )

    assert_close(as_list, scalar, rtol=1e-7, atol=1e-8)
    assert_close(as_array, scalar, rtol=1e-7, atol=1e-8)


def test_conv_coords_scalar_list_and_numpy_inputs_match(gp):
    args = (20200101, 12.0, "GSM", "SM")
    kwargs = {"V": [-400.0, 0.0, 0.0]}

    scalar = gp.Coords.ConvCoords(1.0, 0.0, 0.25, *args, **kwargs)
    as_list = gp.Coords.ConvCoords([1.0], [0.0], [0.25], *args, **kwargs)
    as_array = gp.Coords.ConvCoords(
        np.array([1.0], dtype="float64"),
        np.array([0.0], dtype="float64"),
        np.array([0.25], dtype="float64"),
        *args,
        **kwargs,
    )

    assert_close(as_list, scalar, rtol=1e-7, atol=1e-8)
    assert_close(as_array, scalar, rtol=1e-7, atol=1e-8)


def test_trace_field_scalar_and_list_inputs_match_metadata(gp):
    kwargs = {
        "Model": "T96",
        "CoordIn": "GSM",
        "MaxLen": 500,
        "DSMax": 0.5,
        "alpha": [0.0, 90.0],
        **COMMON_PARAMS,
    }

    scalar = gp.TraceField(5.0, 0.0, 0.0, 20200101, 12.0, **kwargs)
    as_list = gp.TraceField([5.0], [0.0], [0.0], 20200101, 12.0, **kwargs)

    assert_close(as_list.nstep, scalar.nstep, rtol=0.0, atol=0.0)
    assert_close(as_list.alpha, scalar.alpha, rtol=0.0, atol=0.0)
    assert_close(as_list.Lshell, scalar.Lshell, rtol=1e-6, atol=1e-6)
    assert_close(as_list.MltE, scalar.MltE, rtol=1e-6, atol=1e-6)
    assert_close(as_list.FlLen, scalar.FlLen, rtol=1e-6, atol=1e-6)
