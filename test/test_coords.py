from __future__ import annotations

import pytest

from cases import COORD_CASES, LAT_LON_CASES, MLT_CASES
from golden_utils import assert_close


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
