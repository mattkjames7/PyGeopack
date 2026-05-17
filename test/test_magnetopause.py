from __future__ import annotations

import numpy as np

from golden_utils import assert_close


def _expected_shue_mp(theta, pdyn=2.0, bz=0.0):
    r0 = (10.22 + 1.29 * np.tanh(0.184 * (bz + 8.14))) * pdyn ** (-0.15151515)
    alpha = (0.58 - 0.007 * bz) * (1.0 + 0.024 * np.log(pdyn))
    return r0 * (2 / (1 + np.cos(theta))) ** alpha


def test_shue_mp_matches_formula(gp):
    theta = np.array([0.0, np.pi / 4.0, np.pi / 2.0], dtype="float64")
    actual = gp.ShueMP(theta, Pdyn=2.0, Bz=0.0)

    assert_close(actual, _expected_shue_mp(theta), rtol=1e-12, atol=1e-12)


def test_within_mp_classifies_inside_and_outside_points(gp):
    x = np.array([0.0, 20.0, 0.0], dtype="float64")
    y = np.array([0.0, 0.0, 0.0], dtype="float64")
    z = np.array([0.0, 0.0, 20.0], dtype="float64")

    actual = gp.WithinMP(x, y, z, Pdyn=2.0, Bz=0.0)

    np.testing.assert_array_equal(actual, np.array([True, False, False]))
