@echo off

echo Compiling FORTRAN code

gfortran -w -c -fPIC -fno-automatic -o T89c.o T89c.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -fallow-argument-mismatch -o T96.o  T96.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o T01_01.o  T01_01.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o TS04c.o  TS04c.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o wparams.o  wparams.f95
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -ffree-line-length-none -o Geopack-2008_mkj_dp.o Geopack-2008_mkj_dp.f
if %ERRORLEVEL% neq 0 (goto CompileError)

exit /b 0

:CompileError
echo Compilation error
exit /b 8
