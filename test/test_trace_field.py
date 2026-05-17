from __future__ import annotations

import numpy as np
import pytest

from cases import TRACE_CASES
from golden_utils import assert_close

FOOTPRINT_NAMES = [
    "GlatN",
    "GlatS",
    "MlatN",
    "MlatS",
    "GlonN",
    "GlonS",
    "MlonN",
    "MlonS",
    "GltN",
    "GltS",
    "MltN",
    "MltS",
    "Lshell",
    "MltE",
    "FlLen",
]

TRACE_VALUE_NAMES = ["x", "y", "z", "Bx", "By", "Bz", "R", "Rnorm", "s", "halpha"]


def _run_trace(gp, case):
    args = case["args"]
    return gp.TraceField(
        args["Xin"],
        args["Yin"],
        args["Zin"],
        args["Date"],
        args["ut"],
        **case["kwargs"],
    )


@pytest.mark.parametrize("case", TRACE_CASES, ids=[case["name"] for case in TRACE_CASES])
def test_trace_metadata_and_footprints_match_v1_2_7(gp, golden_data, case):
    expected = golden_data["traces"][case["name"]]
    trace = _run_trace(gp, case)

    assert int(trace.n) == expected["n"]
    assert int(trace.nalpha) == expected["nalpha"]
    assert_close(trace.alpha, expected["alpha"], rtol=0.0, atol=0.0)
    assert_close(trace.nstep, expected["nstep"], rtol=0.0, atol=0.0)
    for name in FOOTPRINT_NAMES:
        assert_close(getattr(trace, name), expected["footprints"][name], rtol=1e-6, atol=1e-6)


@pytest.mark.parametrize("case", TRACE_CASES, ids=[case["name"] for case in TRACE_CASES])
def test_trace_samples_match_v1_2_7(gp, golden_data, case):
    expected = golden_data["traces"][case["name"]]
    trace = _run_trace(gp, case)

    for coord in case["coords"]:
        for trace_index, expected_trace in enumerate(expected["coords"][coord]):
            indices = np.asarray(expected_trace["indices"], dtype="int64")
            actual_values = trace.GetTrace(trace_index, Coord=coord)
            for name, actual in zip(TRACE_VALUE_NAMES, actual_values):
                actual_arr = np.asarray(actual)
                if name == "halpha":
                    actual_sample = actual_arr[:, indices] if indices.size else actual_arr[:, :0]
                else:
                    actual_sample = actual_arr[indices] if indices.size else actual_arr[:0]
                assert_close(
                    actual_sample,
                    expected_trace["values"][name],
                    rtol=1e-6,
                    atol=1e-6,
                )
