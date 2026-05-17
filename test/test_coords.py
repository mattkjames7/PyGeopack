from __future__ import annotations

import numpy as np
import pytest

from cases import COORD_CASES, LAT_LON_CASES, MLT_CASES
from golden_utils import assert_close


def _angular_delta_degrees(actual, expected):
    return (np.asarray(actual) - np.asarray(expected) + 180.0) % 360.0 - 180.0


@pytest.mark.parametrize("case", COORD_CASES, ids=[case["name"] for case in COORD_CASES])
def test_conv_coords_matches_v1_2_7(gp, golden_data, case):
    expected = golden_data["coords"][case["name"]]

    actual = gp.Coords.ConvCoords(
        case["Xin"],
        case["Yin"],
        case["Zin"],
        case["Date"],
        case["ut"],
        case["CoordIn"],
        case["CoordOut"],
        V=case["V"],
    )

    assert_close(actual, expected, rtol=1e-7, atol=1e-8)


@pytest.mark.parametrize("case", LAT_LON_CASES, ids=[case["name"] for case in LAT_LON_CASES])
def test_geo_mag_lat_lon_matches_v1_2_7(gp, golden_data, case):
    expected = golden_data["lat_lon"][case["name"]]

    mlon, mlat = gp.Coords.GEOtoMAGLL(
        case["lon"], case["lat"], case["Date"], case["ut"], V=case["V"]
    )
    lon, lat = gp.Coords.MAGtoGEOLL(mlon, mlat, case["Date"], case["ut"], V=case["V"])

    assert_close(mlon, expected["geo_to_mag"]["MLon"], rtol=1e-7, atol=1e-8)
    assert_close(mlat, expected["geo_to_mag"]["MLat"], rtol=1e-7, atol=1e-8)
    assert_close(lon, expected["mag_to_geo_round_trip"]["Lon"], rtol=1e-7, atol=1e-8)
    assert_close(lat, expected["mag_to_geo_round_trip"]["Lat"], rtol=1e-7, atol=1e-8)


@pytest.mark.parametrize("case", MLT_CASES, ids=[case["name"] for case in MLT_CASES])
def test_mlt_conversions_match_v1_2_7(gp, golden_data, case):
    expected = golden_data["mlt"][case["name"]]

    mlt = gp.Coords.MLONtoMLT(case["mlon"], case["Date"], case["ut"], V=case["V"])
    mlon = gp.Coords.MLTtoMLON(case["mlt"], case["Date"], case["ut"], V=case["V"])

    assert_close(mlt, expected["mlon_to_mlt"], rtol=1e-7, atol=1e-8)
    assert_close(mlon, expected["mlt_to_mlon"], rtol=1e-7, atol=1e-8)


@pytest.mark.parametrize("case", MLT_CASES, ids=[case["name"] for case in MLT_CASES])
def test_mlon_mlt_round_trip(gp, case):
    mlt = gp.Coords.MLONtoMLT(case["mlon"], case["Date"], case["ut"], V=case["V"])
    mlon = gp.Coords.MLTtoMLON(mlt, case["Date"], case["ut"], V=case["V"])

    assert_close(_angular_delta_degrees(mlon, case["mlon"]), 0.0, rtol=1e-7, atol=1e-5)


@pytest.mark.parametrize("case", LAT_LON_CASES, ids=[case["name"] for case in LAT_LON_CASES])
def test_geo_mag_lat_lon_round_trip(gp, case):
    mlon, mlat = gp.Coords.GEOtoMAGLL(
        case["lon"], case["lat"], case["Date"], case["ut"], V=case["V"]
    )
    lon, lat = gp.Coords.MAGtoGEOLL(mlon, mlat, case["Date"], case["ut"], V=case["V"])

    assert_close(_angular_delta_degrees(lon, case["lon"]), 0.0, rtol=1e-7, atol=1e-5)
    assert_close(lat, case["lat"], rtol=1e-7, atol=1e-6)


@pytest.mark.parametrize(
    "coord_in,coord_out",
    [
        ("GSM", "SM"),
        ("GSE", "GSM"),
        ("GEO", "GEI"),
        ("MAG", "GSE"),
    ],
)
def test_conv_coords_round_trip(gp, coord_in, coord_out):
    x = np.array([1.0, 3.0, -2.0], dtype="float64")
    y = np.array([0.0, 1.5, -0.5], dtype="float64")
    z = np.array([0.25, -0.75, 2.0], dtype="float64")
    date = 20200101
    ut = 12.0
    velocity = [-400.0, 0.0, 0.0]

    converted = gp.Coords.ConvCoords(x, y, z, date, ut, coord_in, coord_out, V=velocity)
    round_trip = gp.Coords.ConvCoords(*converted, date, ut, coord_out, coord_in, V=velocity)

    assert_close(round_trip, (x, y, z), rtol=1e-7, atol=1e-8)


def test_conv_coords_rejects_invalid_input_coordinate(gp):
    with pytest.raises(ValueError):
        gp.Coords.ConvCoords(1.0, 0.0, 0.0, 20200101, 12.0, "BAD", "GSM")


def test_conv_coords_rejects_invalid_output_coordinate(gp):
    with pytest.raises(ValueError):
        gp.Coords.ConvCoords(1.0, 0.0, 0.0, 20200101, 12.0, "GSM", "BAD")
