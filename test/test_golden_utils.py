from __future__ import annotations

import math

import numpy as np

from golden_utils import decode_json_value, encode_json_value, sample_indices


def test_json_encoding_round_trips_numpy_and_nonfinite_values():
    payload = {
        "array": np.array([1.0, np.nan, np.inf, -np.inf]),
        "scalar": np.float64(2.5),
    }

    encoded = encode_json_value(payload)
    decoded = decode_json_value(encoded)

    assert encoded == {
        "array": [1.0, "NaN", "Infinity", "-Infinity"],
        "scalar": 2.5,
    }
    assert decoded["array"][0] == 1.0
    assert math.isnan(decoded["array"][1])
    assert decoded["array"][2] == math.inf
    assert decoded["array"][3] == -math.inf
    assert decoded["scalar"] == 2.5


def test_sample_indices_are_unique_and_ordered():
    assert sample_indices(0) == []
    assert sample_indices(1) == [0]
    assert sample_indices(2) == [0, 1]
    assert sample_indices(5) == [0, 2, 4]
