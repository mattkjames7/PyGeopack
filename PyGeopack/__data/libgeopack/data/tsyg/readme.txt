Yearly files YYYY_OMNI_5m_with_TS05_variables.dat (1995<=YYYY<=2017) in this folder contain
complete sets of input parameters for the TS05 magnetospheric magnetic field model (Tsyganenko
and Sitnov, JGRA v.110(A3), 2005) including Pdyn, SymH, ByIMF, BzIMF, dipole tilt angle, and
six solar wind driving variables W1, W2, ..., W6, quantifying the magnitudes of principal
magnetospheric current systems. Each file is about 14MB; for that reason they are packed into
zip-shells having the same names.  Data format is explained in a file of the same name.

The folder also contains a fortran source code Prepare_input_4.for, used for generating the
above yearly files. It can be used as a template for various modifications, aimed at experimenting
with the impact of different hypothetical solar wind and IMF conditions on the dynamics of the
storm-time magnetosphere.

Order of operations to create new files:

1. Create (by checking ALL boxes in the OMNI webpage) and download a yearly OMNI file.
2. Run Fill_IMF_gaps.for
3. Run Fill_SW_gaps.for
4. Run Prepare_intervals_1.for
5. Run Prepare_input_4.for

