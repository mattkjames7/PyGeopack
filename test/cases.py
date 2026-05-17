"""Shared regression cases for PyGeopack golden-data tests."""

GOLDEN_VERSION = "1.2.7"
GOLDEN_FILENAME = "pygeopack_v1_2_7_golden.json"

COMMON_PARAMS = {
    "Pdyn": 2.0,
    "SymH": 0.0,
    "By": 0.0,
    "Bz": 0.0,
    "Vx": -400.0,
    "Vy": 0.0,
    "Vz": 0.0,
}

MODEL_FIELD_CASES = [
    {
        "name": "t96_scalar_gsm",
        "args": {
            "Xin": 5.0,
            "Yin": 0.0,
            "Zin": 0.0,
            "Date": 20200101,
            "ut": 12.0,
            "Model": "T96",
            "CoordIn": "GSM",
            "CoordOut": "GSM",
            **COMMON_PARAMS,
        },
    },
    {
        "name": "t96_vector_gse_to_sm",
        "args": {
            "Xin": [3.5, 5.0, 7.5],
            "Yin": [0.5, -1.0, 2.0],
            "Zin": [1.0, 0.25, -0.5],
            "Date": 20200320,
            "ut": 6.5,
            "Model": "T96",
            "CoordIn": "GSE",
            "CoordOut": "SM",
            **COMMON_PARAMS,
        },
    },
    {
        "name": "t89_vector_gsm",
        "args": {
            "Xin": [2.0, 4.0, 6.0],
            "Yin": [0.0, 1.0, -1.5],
            "Zin": [0.0, 0.75, 1.25],
            "Date": 20200601,
            "ut": 18.25,
            "Model": "T89",
            "CoordIn": "GSM",
            "CoordOut": "GSM",
            "Kp": 2,
            "Pdyn": 2.0,
            "Bz": 0.0,
            "Vx": -400.0,
            "Vy": 0.0,
            "Vz": 0.0,
        },
    },
    {
        "name": "t01_scalar_gsm_to_gse",
        "args": {
            "Xin": 4.0,
            "Yin": 1.0,
            "Zin": -0.5,
            "Date": 20200922,
            "ut": 9.75,
            "Model": "T01",
            "CoordIn": "GSM",
            "CoordOut": "GSE",
            **COMMON_PARAMS,
            "G1": 0.0,
            "G2": 0.0,
        },
    },
    {
        "name": "ts05_scalar_sm",
        "args": {
            "Xin": 5.0,
            "Yin": 0.5,
            "Zin": 0.25,
            "Date": 20201215,
            "ut": 3.0,
            "Model": "TS05",
            "CoordIn": "SM",
            "CoordOut": "SM",
            **COMMON_PARAMS,
            "W1": 0.0,
            "W2": 0.0,
            "W3": 0.0,
            "W4": 0.0,
            "W5": 0.0,
            "W6": 0.0,
        },
    },
]

TRACE_CASES = [
    {
        "name": "single_t96_gsm",
        "args": {
            "Xin": 5.0,
            "Yin": 0.0,
            "Zin": 0.0,
            "Date": 20200101,
            "ut": 12.0,
        },
        "kwargs": {
            "Model": "T96",
            "CoordIn": "GSM",
            "MaxLen": 500,
            "DSMax": 0.5,
            "alpha": [0.0, 45.0, 90.0],
            **COMMON_PARAMS,
        },
        "coords": ["GSM", "GSE", "SM"],
    },
    {
        "name": "multi_t96_sm",
        "args": {
            "Xin": [3.0, 5.0],
            "Yin": [0.0, 0.75],
            "Zin": [1.0, -0.5],
            "Date": 20200320,
            "ut": 6.0,
        },
        "kwargs": {
            "Model": "T96",
            "CoordIn": "SM",
            "MaxLen": 500,
            "DSMax": 0.5,
            "alpha": [0.0, 90.0],
            **COMMON_PARAMS,
        },
        "coords": ["GSM", "SM"],
    },
]

COORD_CASES = [
    {
        "name": "gsm_to_sm_vector",
        "Xin": [1.0, 3.0, -2.0],
        "Yin": [0.0, 1.5, -0.5],
        "Zin": [0.25, -0.75, 2.0],
        "Date": 20200101,
        "ut": 12.0,
        "CoordIn": "GSM",
        "CoordOut": "SM",
        "V": [-400.0, 0.0, 0.0],
    },
    {
        "name": "gse_to_gsm_vector",
        "Xin": [2.0, -1.0, 4.5],
        "Yin": [0.5, 2.0, -1.0],
        "Zin": [-0.25, 1.0, 0.75],
        "Date": 20200601,
        "ut": 18.25,
        "CoordIn": "GSE",
        "CoordOut": "GSM",
        "V": [-410.0, 12.0, -8.0],
    },
    {
        "name": "geo_to_gei_vector",
        "Xin": [1.0, 0.0, 0.0],
        "Yin": [0.0, 1.0, 0.0],
        "Zin": [0.0, 0.0, 1.0],
        "Date": 20200922,
        "ut": 9.75,
        "CoordIn": "GEO",
        "CoordOut": "GEI",
        "V": None,
    },
    {
        "name": "mag_to_gse_vector",
        "Xin": [1.0, 2.0, 3.0],
        "Yin": [-0.25, 0.5, 1.0],
        "Zin": [0.75, -1.0, 1.5],
        "Date": 20201215,
        "ut": 3.0,
        "CoordIn": "MAG",
        "CoordOut": "GSE",
        "V": [-400.0, 0.0, 0.0],
    },
]

LAT_LON_CASES = [
    {
        "name": "geo_mag_round_trip",
        "lon": [0.0, 90.0, 180.0, 270.0],
        "lat": [-45.0, 0.0, 35.0, 70.0],
        "Date": 20200320,
        "ut": 6.0,
        "V": [-400.0, 0.0, 0.0],
    },
]

MLT_CASES = [
    {
        "name": "mlon_mlt_round_trip",
        "mlon": [0.0, 45.0, 180.0, 315.0],
        "mlt": [0.0, 6.0, 12.0, 18.0],
        "Date": 20200601,
        "ut": 18.25,
        "V": [-400.0, 0.0, 0.0],
    },
]
