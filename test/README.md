# PyGeopack regression tests

These tests compare the installed package against compact golden data generated
from `PyGeopack==1.2.7`.

Create the golden fixture in an environment where the PyPI release is installed:

```bash
python -m venv /tmp/pygeopack-1.2.7
source /tmp/pygeopack-1.2.7/bin/activate
python -m pip install "PyGeopack==1.2.7" pytest
cd /data/github/PyGeopack/test
python savetestdata/save_golden_data.py
```

Run the tests from inside this directory after installing the package under test:

```bash
cd /data/github/PyGeopack/test
pytest
```

Running from `test/` is intentional: it prevents Python from importing the
unbuilt `PyGeopack/` source directory from the repository checkout.
