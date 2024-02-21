# PyGeopack

A Python wrapper for Geopack-2008. This includes the T89, T96, T01 and TS05 magnetic field models for Earth's magnetosphere. See https://ccmc.gsfc.nasa.gov/modelweb/magnetos/tsygan.html and http://geo.phys.spbu.ru/~tsyganenko/modeling.html for more information.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7688061.svg)](https://zenodo.org/badge/DOI/10.5281/zenodo.7688061.svg)


## 1. Requirements

The following Python packages will be installed automatically:

* numpy
* PyFileIO
* RecarrayTools
* DateTimeTools

The following packages will also be required for running the  ```PyGeopack.UpdateParameters()``` routine:

* kpindex
* pyomnidata

This package is primarily a Python 3 wrapper for a mixture of C/C++/FORTRAN code which is compiled during installation. For building to work, the following are needed:

- g++

- binutils

- make

- gfortran

## 2. Installation

### 2.1 Linux

Firstly a few environment variables need setting up: `$KPDATA_PATH`, `$OMNIDATA_PATH` and `$GEOPACK_PATH`, which will point to the Kp index data, the omni data and the Geopack data, respectively. This can be done by including the following in your `~/.bashrc` file, or by running it in the terminal before starting Python:

```
export KPDATA_PATH=/path/to/kp
export OMNIDATA_PATH=/path/to/omni
export GEOPACK_PATH=/path/to/geopack/data
```

where both of those directories must be writable by the current user, unless the data already exist in them.

Then simply install using pip3:

```
pip3 install PyGeopack --user
```

or by downloading the latest release on GitHub and running:

```
pip3 install PyGeopack-1.2.2.tar.gz --user
```

NOTE: You should uninstall any previous versions before installing this. If you had a version installed before 0.0.12 - you will need to remove the old shared object files - they are likely to be contained somewhere like (depending on the Python version used):

```
~/.local/lib/python3.10/site-packages/PyGeopack/
```

It's best just to remove everything within that folder!

### 2.2 Windows

Install TDM-GCC (make sure to select `g++` and `gfortran`), install anaconda and then install the package in an anaconda power shell session using `pip3` as in the Linux instructions. The same environment variables will need to be set as those in the Linux instructions. This package has only briefly been tested in Windows 10 x64, mileage may vary with other versions of Windows. This code has also been successfully built using the Strawberry Perl compilers.

It appears that `ctypes` doesn't necessarily search the directories listed within the `$PATH` environment variable for the C++ dependencies on some versions of Python. To hack myself around this, I have added a function which with scan all of the directories within `$PATH` for `libstdc++*.dll`; if found then the paths which it is found in will be added using `os.add_dll_directory()`. If the directory of your compiler's libraries is not listed within `$PATH`, this may be the cause of any failures at build time or importing the library. If you have multiple compilers installed, then I am unsure what will happen! Feel free to post an issue if you encounter any errors, or if you have any suggestions for fixes.

### 2.3 Mac

This module should work on MacOS as it does on Linux, with the same requirements. I have tested it using MacOS Big Sur.

## 3 Post-install

After installation, the PyGeopack module will attempt to locate the OMNI data required for the models. If these data exist already in `$GEOPACK_PATH` then it will load into memory. If they don't exist, then the user will be shown a warning - the next section explains how to fix this.

## 4. Usage

There are three main uses for this Python package:

1. Calculating the model magnetic field at any given point in space.
2. Tracing along the magnetic field.
3. Coordinate conversions (usually for the purposes of 1 and 2).

Before doing any of the above, it's recommended that you grab the up to date omni parameters - the `UpdateParameters` routine will download and update the Kp index and OMNI parameters, then calculate the G and W parameters required for the models:

```python
import PyGeopack as gp
gp.Params.UpdateParameters(SkipWParameters=True)
```

The `SkipWParameters` keyword (set to `True` by default) can be used to skip the lengthy process of calculating the six W parameters for the TS05 magnetic field model - if `True` then these will be filled with zeros. Apparently they aren't all that important anyway. The code included in this module can calculate them and is Tsyganenko's own code, but it produces different numbers to those given in the files on Tsyganenko's website! No idea why, so use them with caution!

If the above parameters are not loaded, a warning will appear when the module is imported. To suppress warnings, set:
```bash
export GEOPACK_NOWARN=1
```


### 4.1 Calculating the model field.

To calculate the model field, use the `ModelField` function:

```python
Bx,By,Bz = gp.ModelField(x,y,z,Date,ut,Model='T96',CoordIn='GSM',CoordOut='GSM',**kwargs)
```

where the output field components `Bx`, `By` and `Bz` are in units of nT.

The `ModelField` function accepts the following arguments and keywords:

| Name           | Keyword/Argument | Description                                                                                                           |
|----------------|------------------|-----------------------------------------------------------------------------------------------------------------------|
| `x`            | argument         | Scalar or array of x-coordinates                                                                                      |
| `y`            | argument         | Scalar or array of y-coordinates                                                                                      |
| `z`            | argument         | Scalar or array of z-coordinates                                                                                      |
| `Date`         | argument         | Date(s) in the format yyyymmdd - must be an integer and have either one element or the same number of elements as `x` |
| `ut`           | argument         | Time in hours since the start of the day.                                                                             |
| `Model`        | keyword          | String denoting the model to use `'T89'\|'T96'\|'T01'\|'TS05'`                                                        |
| `CoordIn`      | keyword          | Input coordinate system string: `'GSE'\|'GSM'\|'SM'`                                                                  |
| `CoordOut`     | keyword          | Output coordinate system string: `'GSE'\|'GSM'\|'SM'`                                                                 |
| `WithinMPOnly  | keyword          | If `True` then return NaN's outside the magnetopause.                                                                 |
| `ReturnParams` | keyword          | If `True` then a dictionary containing the model parameters will be returned as a fourth output parameter.            |
| `**kwargs`     | keywords         | Keyword arguments can be used to define some or all of the model parameters used. See model parameters section.       |

### 4.2 Tracing the magnetic field

The `TraceField` object will accept either one argument or 5 arguements, alongside a range of keyword arguments:

```python
TraceField(*args,**kwargs)
```

Trace along the magnetic field from 1 or more starting positions in the magnetosphere `x`, `y` and `z` using the `TraceField` object, e.g.:

```python
T = gp.TraceField(x,y,z,Date,ut)
```

where `x`, `y`, `z`, `Date` and `ut` are defined in the previous section.

Or loading from file:

```python
T = gp.TraceField(filename)
```

where `filename` is the full path and file name of a file where a previous `TraceField` object was saved using `TraceField.Save`, e.g.:

```python
T.Save(filename)
```

The keyword arguments accepted by the `TraceField` object include one discussed in the model parameters section and ones specific to this object:

| Name                  | Description                                                                                                                                                                        |
| --------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Model`               | Model string: 'T89'\|'T96'\|'T01'\|'TS05'`                                                                                                                                         |
| `CoordIn`             | Input coordinate system string: `'GSE'\|'GSM'\|'SM'`                                                                                                                               |
| `alt`                 | Altitude (in km) to stop trace at.                                                                                                                                                 |
| `MaxLen`              | Maximum trace steps                                                                                                                                                                |
| `DSMax`               | Maximum step size (R<sub>E</sub>)                                                                                                                                                  |
| `FlattenSingleTraces` | Flattens all of the arrays if only a single field line is traced                                                                                                                   |
| `Verbose`             | Output trace progress                                                                                                                                                              |
| `TraceDir`            | Direction to trace in - normally `'both'` is used to trace in both directions, `1` traces along the field (to the northern hemisphere)  and `-1` traces in the opposite direction. |
| `alpha`               | This can be an array or a scalar defining a polarization angle in degrees. This is typically used for ULF waves.                                                                   |

The `TraceField` object, `T` in the above code snippet, contains the following arrays:

|                     |                                                                                                                                                                                                                                             |
|:------------------- |:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `xsm,ysm,zsm`       | x, y and z coordinates along the field trace(s) in the SM coordinate system                                                                                                                                                                 |
| `xgsm,ygsm,zgsm`    | x, y and z coordinates along the field trace(s) in the GSM coordinate system                                                                                                                                                                |
| `xgse,ygse,zgse`    | x, y and z coordinates along the field trace(s) in the GSE coordinate system                                                                                                                                                                |
| `Bxsm,Bysm,Bzsm`    | x, y and z components of the magnetic field along the trace(s) in the SM coordinate system                                                                                                                                                  |
| `Bxgsm,Bygsm,Bzgsm` | x, y and z components of the magnetic field along the trace(s) in the GSM coordinate system                                                                                                                                                 |
| `Bxgse,Bygse,Bzgse` | x, y and z components of the magnetic field along the trace(s) in the GSE coordinate system                                                                                                                                                 |
| `nstep`             | number of steps along the trace(s)                                                                                                                                                                                                          |
| `GlatN`             | Geographic latitude of the northern footprint(s)                                                                                                                                                                                            |
| `GlatS`             | Geographic latitude of the southern footprint(s)                                                                                                                                                                                            |
| `MlatN`             | Magnetic latitude of the northern footprint(s)                                                                                                                                                                                              |
| `MlatS`             | Magnetic latitude of the southern footprint(s)                                                                                                                                                                                              |
| `GlonN`             | Geographic longitude of the northern footprint(s)                                                                                                                                                                                           |
| `GlonS`             | Geographic longitude of the southern footprint(s)                                                                                                                                                                                           |
| `MlonN`             | Magnetic longitude of the northern footprint(s)                                                                                                                                                                                             |
| `MlonS`             | Magnetic longitude of the southern footprint(s)                                                                                                                                                                                             |
| `GltN`              | Geographic local time of the northern footprint(s)                                                                                                                                                                                          |
| `GltS`              | Geographic local time of the southern footprint(s)                                                                                                                                                                                          |
| `MltN`              | Magnetic local time of the northern footprint(s)                                                                                                                                                                                            |
| `MltS`              | Magnetic local time of the southern footprint(s)                                                                                                                                                                                            |
| `Lshell`            | L-shell of the field line(s) at the equator                                                                                                                                                                                                 |
| `MltE`              | Magnetic local time of the equatorial footprint(s)                                                                                                                                                                                          |
| `FlLen`             | Field line length in planetary radii                                                                                                                                                                                                        |
| `R`                 | `R = sqrt(x**2 + y**2 + z**2)`                                                                                                                                                                                                              |
| `s`                 | Distance along field line (in R<sub>E</sub>)                                                                                                                                                                                                |
| `halpha`            | *h<sub>&alpha;</sub>* (see Singer et al., 1981) - this is an array with the shape `(n,nalpha,MaxLen)`, where `n` is the number of traces, `nalpha` is the number of alpha values (polarizations) and  `MaxLen` is the maximum trace length. |

### 4.3 Coordinate conversion

Coordinate conversion code is now within the `PyGeopack.Coords` submodule. A simple function exists to convert between the a bunch of Cartesian coordinate systems:

```python
x1,y1,z1 = gp.Coords.ConvCoords(x0,y0,z0,Date,ut,CoordIn,CoordOut,V=V)
```

where `x0`, `y0` and `z0` are either scalars or arrays of positions to be transformed. `Date`is an integer in the format yyyymmdd, `ut` is the time in hours (i.e. `ut = hours + minutes/60.0`) and `V` can be used to set a custom solar wind velocity. `CoordIn` and `CoordOut` are strings defining the input and output systems, which can be any of the following: `'GEI'|'GEO'|'GSE'|'GSM'|'MAG'|'SM'`.  `x1`, `y1` and `z1` are the transformed coordinates.

There are other Cartesian coordinate conversion routines:

```python
x1,y1,z1 = gp.Coords.GEItoGEO(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEItoGSE(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEItoGSM(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEItoMAG(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEItoSM(x0,y0,z0,Date,ut)

x1,y1,z1 = gp.Coords.GEOtoGEI(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEOtoGEO(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEOtoGSE(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEOtoGSM(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GEOtoMAG(x0,y0,z0,Date,ut)

x1,y1,z1 = gp.Coords.GSEtoGEI(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSEtoGEO(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSEtoGSE(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSEtoGSM(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSEtoMAG(x0,y0,z0,Date,ut)

x1,y1,z1 = gp.Coords.GSMtoGEI(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSMtoGEO(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSMtoGSE(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSMtoGSM(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.GSMtoMAG(x0,y0,z0,Date,ut)

x1,y1,z1 = gp.Coords.MAGtoGEI(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.MAGtoGEO(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.MAGtoGSE(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.MAGtoGSM(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.MAGtoMAG(x0,y0,z0,Date,ut)

x1,y1,z1 = gp.Coords.SMtoGEI(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.SMtoGEO(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.SMtoGSE(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.SMtoGSM(x0,y0,z0,Date,ut)
x1,y1,z1 = gp.Coords.SMtoMAG(x0,y0,z0,Date,ut)
```

Also included are the following routines:

```python
MLon,MLat = gp.Coords.GEOtoMAG(Lat,Lon,Date,ut)
Lon,Lat = gp.Coords.MAGtoGEO(MLat,MLon,Date,ut)
```

which convert between geographic (`Lat` and `Lon`) and magnetic (`MLat` and `MLon`) latitude and longitudes.

And for converting between magnetic longitude (`MLon`) and magnetic local time (`MLT`):

```python
MLT = gp.Coords.MLONtoMLT(MLon,Date,ut)
MLon = gp.Coords.MLTtoMLON(MLT,Date,ut)
```

Descriptions of the coordinate systems:

| Name                                  | Description | x                                                              | y                                                                                                                           | z                                                  |
|:------------------------------------- |:-----------:|:-------------------------------------------------------------- |:--------------------------------------------------------------------------------------------------------------------------- |:-------------------------------------------------- |
| GSE - Geocentric Solar Ecliptic       | fixed       | Towards the Sun                                                | Opposite to Earth's orbit                                                                                                   | Perpendicular to the ecliptic plane                |
| GSM - Geocentric Solar Magnetospheric | fixed       | Towards the Sun                                                |                                                                                                                             | Projection of the dipole axis in the Y-Z GSE plane |
| SM - Solar Magnetic                   | fixed       | In the plane containing the Earth-Sum line and the dipole axis |                                                                                                                             | Along the dipole axis                              |
| MAG - Geomagnetic                     | rotating    |                                                                | Through intersection of magnetic equator and geographic meridian 90 degrees east of the meridian containing the dipole axis | Along the dipole axis                              |
| GEO - Geographic                      | rotating    | Through intersection of equator and Greenwich meridian         |                                                                                                                             | Along Earth's rotation axis                        |
| GEI - Geocentric Equatorial Inertial  | fixed       | Towards the first point of Ares                                |                                                                                                                             | Along Earth's rotation axisbetical list            |

NOTE: By "fixed", I mean that they do not rotate with the Earth's spin, they are not really fixed.

### 4.4 Model Parameters

In this section, the parameters and relevant `**kwargs` are discussed for each model. If `**kwargs` aren't used, then the relevant parameters are found autmoatically for the date and time provided when using the models. Individual parameters amy be altered without affecting the others - if only a single parameter is changed, then the others are still automatically calculated. The `**kwargs` accepted by `ModelField` and `TraceField` are:

| Keyword  | Data Type              | Description                                                                                        |
|:-------- |:---------------------- |:-------------------------------------------------------------------------------------------------- |
| `iopt`   | scalar integer         | iopt=Kp+1 (iopt=7 for Kp>=6)                                                                       |
| `parmod` | 10-element float array | Elements 0 - 3 are Pdyn, SymH, IMF By and IMF Bz, respectively. Elements 4 - 9 depend on the model |
| `tilt`   | scalar float           | The dipole tilt angle in radians                                                                   |
| `Vx`     | scalar float           | x component of solar wind velocity                                                                 |
| `Vy`     | scalar float           | y component of solar wind velocity                                                                 |
| `Vz`     | scalar float           | z component of solar wind velocity                                                                 |
| `Kp`     | scalar integer         | Kp index                                                                                           |
| `Pdyn`   | scalar float           | Dynamic pressure in nPa                                                                            |
| `SymH`   | scalar float           | SymH in nT                                                                                         |
| `By`     | scalar float           | IMF y component in nT                                                                              |
| `Bz`     | scalar float           | IMF z component in nT                                                                              |

A fucntion exists which will return a `dict` object containing the parameters for a given model, date and time:

```python
params = gp.Params.GetModelParams(Date,ut,Model)
```

We can also return the dipole tilt:

```python
tilt = gp.Params.GetDipoleDilt(Date,ut)
```

All models can be affected by the `Vx`, `Vy`, and `Vz` parameters as these are used to aberrate the coordinates into the GSW frame, where GSW is equivalent to GSM in the situation where `Vy=0` and `Vz=0`. `tilt` is calculated automatically for all models based on the date, time and the IGRF magnetic field model.

#### T89

The only parameter used here is `iopt` which can be controlled with either setting `iopt` or `Kp` keywords to an integer. Valid values for `iopt` are integers in the range 1-7, if `Kp` is set, then `iopt` is set automatically equal to `Kp+1`. For `Kp`>=6 `iopt=7`.

#### T96

The first four elements of the `parmod` array are used for this model where `parmod[0]` is the dynamic pressure, `parmod[1]` is the SymH, `parmod[2]` is the y component of the interplanetary magnetic field (IMF) and `parmod[3]` is the z component of the IMF. All other elements of this array are ignored. The entire `parmod` array can be set using the `parmod` keyword, otherwise individual elements can be edited using the `Pdyn`, `SymH`, `By` and `Bz` keywords, where other unchanged parameters will be calculated automatically.

#### T01

This model uses the first six elements of the `parmod` array, where the first four are set in exactly the same way as in the T96 model. `parmod[4]` and `parmod[5]` correspond to the G1 and G2 parameters calculated in Tsyganenko, 2002b. These can, I believe, be set to 0.

#### TS05

This model uses all of the `parmod` array, where the first four are as in the T96 model. The last 6 elements are the W1-W6 parameters described in Tsyganenko and Sitnov, 2005.

## References

1. N.A. Tsyganenko, A Magnetospheric Magnetic Field Model with a Warped Tail Current Sheet, Planet. Space Sci. 37, 5-20, 1989.
2. N.A. Tsyganenko, Modeling the Earth's Magnetospheric Magnetic Field Confined Within a Realistic Magnetopause, J.Geophys.Res., 100, 5599-5612, 1995.
3. N.A. Tsyganenko and D.P. Stern, Modeling the Global Magnetic Field of the Large-Scale Birkeland Current Systems, J. Geophys.Res., 101, 27187-27198, 1996.
4. N.A. Tsyganenko, A model of the near magnetosphere with a dawn-dusk asymmetry - 1. Mathematical Structure, J. Geophys.Res., 107, A8, 10.1029/2001JA000219, 2002.
5. N.A. Tsyganenko, A model of the near magnetosphere with a dawn-dusk asymmetry - 2. Parameterization and fitting to observations, J. Geophys.Res., 107, A7, 10.1029/2001JA000220, 2002.
6. N.A. Tsyganenko and M. I. Sitnov, Modeling the dynamics of the inner magnetosphere during strong geomagnetic storms, J. Geophys.Res., 110, A3, 10.1029/2004JA010798, 2005. 
7. Singer, H. & Southwood, D. & Walker, Raymond & Kivelson, M.. (1981). Alfven wave resonances in a realistic magnetospheric magnetic field geometry. Journal of Geophysical Research. 86. 4589-4596. 10.1029/JA086iA06p04589. 
