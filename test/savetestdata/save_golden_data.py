"""Generate compact golden fixtures from an installed PyGeopack release.

Run from the test directory after installing PyGeopack==1.2.7:

    cd test
    python savetestdata/save_golden_data.py
"""

from __future__ import annotations

import platform
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

import numpy as np

TEST_DIR = Path(__file__).resolve().parents[1]
if str(TEST_DIR) not in sys.path:
    sys.path.insert(0, str(TEST_DIR))

os.environ.setdefault("GEOPACK_NOWARN", "1")
os.environ.setdefault("MPLCONFIGDIR", "/tmp/pygeopack-matplotlib")

from cases import (  # noqa: E402
    COORD_CASES,
    GOLDEN_FILENAME,
    GOLDEN_VERSION,
    LAT_LON_CASES,
    MLT_CASES,
    MODEL_FIELD_CASES,
    TRACE_CASES,
)
from golden_utils import (  # noqa: E402
    assert_not_repo_import,
    data_path,
    remove_repo_root_from_syspath,
    sample_indices,
    write_golden_json,
)


def _import_pygeopack():
    remove_repo_root_from_syspath()
    import PyGeopack as gp

    assert_not_repo_import(gp)
    if gp.__version__ != GOLDEN_VERSION:
        raise RuntimeError(
            f"Expected PyGeopack=={GOLDEN_VERSION}, imported {gp.__version__} from {gp.__file__}"
        )
    return gp


def _collect_model_fields(gp):
    out = {}
    for case in MODEL_FIELD_CASES:
        result = gp.ModelField(ReturnParams=True, **case["args"])
        bx, by, bz, params = result
        out[case["name"]] = {
            "Bx": bx,
            "By": by,
            "Bz": bz,
            "params": {
                "iopt": params["iopt"],
                "parmod": params["parmod"],
                "Vx": params["Vx"],
                "Vy": params["Vy"],
                "Vz": params["Vz"],
            },
        }
    return out


def _trace_coord_samples(trace, trace_index, coord):
    values = trace.GetTrace(trace_index, Coord=coord)
    names = ["x", "y", "z", "Bx", "By", "Bz", "R", "Rnorm", "s", "halpha"]
    nstep = int(trace.nstep[trace_index])
    indices = sample_indices(nstep)
    samples = {}
    for name, value in zip(names, values):
        arr = np.asarray(value)
        if name == "halpha":
            samples[name] = arr[:, indices] if indices else arr[:, :0]
        else:
            samples[name] = arr[indices] if indices else arr[:0]
    return {"indices": indices, "values": samples}


def _collect_traces(gp):
    footprint_names = [
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
    out = {}
    for case in TRACE_CASES:
        args = case["args"]
        trace = gp.TraceField(
            args["Xin"],
            args["Yin"],
            args["Zin"],
            args["Date"],
            args["ut"],
            **case["kwargs"],
        )
        trace_out = {
            "n": int(trace.n),
            "nalpha": int(trace.nalpha),
            "alpha": trace.alpha,
            "nstep": trace.nstep,
            "footprints": {name: getattr(trace, name) for name in footprint_names},
            "coords": {},
        }
        for coord in case["coords"]:
            trace_out["coords"][coord] = [
                _trace_coord_samples(trace, i, coord) for i in range(int(trace.n))
            ]
        out[case["name"]] = trace_out
    return out


def _collect_coords(gp):
    out = {}
    for case in COORD_CASES:
        out[case["name"]] = gp.Coords.ConvCoords(
            case["Xin"],
            case["Yin"],
            case["Zin"],
            case["Date"],
            case["ut"],
            case["CoordIn"],
            case["CoordOut"],
            V=case["V"],
        )
    return out


def _collect_lat_lon(gp):
    out = {}
    for case in LAT_LON_CASES:
        mlon, mlat = gp.Coords.GEOtoMAGLL(
            case["lon"], case["lat"], case["Date"], case["ut"], V=case["V"]
        )
        lon, lat = gp.Coords.MAGtoGEOLL(mlon, mlat, case["Date"], case["ut"], V=case["V"])
        out[case["name"]] = {
            "geo_to_mag": {"MLon": mlon, "MLat": mlat},
            "mag_to_geo_round_trip": {"Lon": lon, "Lat": lat},
        }
    return out


def _collect_mlt(gp):
    out = {}
    for case in MLT_CASES:
        mlt = gp.Coords.MLONtoMLT(case["mlon"], case["Date"], case["ut"], V=case["V"])
        mlon = gp.Coords.MLTtoMLON(case["mlt"], case["Date"], case["ut"], V=case["V"])
        out[case["name"]] = {
            "mlon_to_mlt": mlt,
            "mlt_to_mlon": mlon,
        }
    return out


def main() -> None:
    gp = _import_pygeopack()
    payload = {
        "metadata": {
            "source": "PyGeopack",
            "version": gp.__version__,
            "package_file": gp.__file__,
            "generated_at": datetime.now(timezone.utc).isoformat(),
            "python": sys.version,
            "platform": platform.platform(),
            "numpy": np.__version__,
        },
        "model_fields": _collect_model_fields(gp),
        "traces": _collect_traces(gp),
        "coords": _collect_coords(gp),
        "lat_lon": _collect_lat_lon(gp),
        "mlt": _collect_mlt(gp),
    }
    output = data_path(GOLDEN_FILENAME)
    write_golden_json(output, payload)
    print(f"Wrote {output}")


if __name__ == "__main__":
    main()
