@echo off

where /q gcc
if %ERRORLEVEL% neq 0 (
	echo GCC wasn't found
	exit /b 6
)

WHERE /Q gfortran 
if %ERRORLEVEL% neq 0 (
	echo GFortran wasn't found 
	exit /b 7
)

echo Compiling libgeopack...

gcc -c -fPIC libgeopack.c -o libgeopack.o
if %ERRORLEVEL% neq 0 (goto CompileError)
gcc -c -fPIC TraceRoutines.c -o TraceRoutines.o
if %ERRORLEVEL% neq 0 (goto CompileError)
gcc -c -fPIC DateTimeTools.c -o DateTimeTools.o
if %ERRORLEVEL% neq 0 (goto CompileError)
gcc -c -fPIC ModelField.c -o ModelField.o
if %ERRORLEVEL% neq 0 (goto CompileError)
gcc -c -fPIC ConvCoords.c -o ConvCoords.o
if %ERRORLEVEL% neq 0 (goto CompileError)
gcc -c -fPIC Tom.c -o Tom.o
if %ERRORLEVEL% neq 0 (goto CompileError)
	
gcc -c -fPIC SandhuCoords.c -o SandhuCoords.o
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o T89c.o T89c.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o T96.o  T96.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o T01_01.o  T01_01.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o TS04c.o  TS04c.f
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o wparams.o  wparams.f95
if %ERRORLEVEL% neq 0 (goto CompileError)
gfortran -w -c -fPIC -fno-automatic -o Tom96.o  Tom96.f95
if %ERRORLEVEL% neq 0 (goto CompileError)

g++ -c -fPIC spline.cc -o spline.o
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -c -fPIC libspline.cc -o libspline.o
if %ERRORLEVEL% neq 0 (goto CompileError)

gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopackdp.dll Geopack-2008_mkj_dp.f wparams.o T89c.o T96.o T01_01.o TS04c.o TraceRoutines.o DateTimeTools.o ModelField.o ConvCoords.o Tom.o Tom96.o SandhuCoords.o libgeopack.o -lm
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -fPIC -shared -o libspline.dll libspline.o spline.o
if %ERRORLEVEL% neq 0 (goto CompileError)

@REM gfortran -c test.f95 -o test.o
@REM if %ERRORLEVEL% neq 0 (goto CompileError)
@REM gfortran -o test test.o libgeopackdp.dll
@REM if %ERRORLEVEL% neq 0 (goto CompileError)
@REM gfortran -c testsandhucoords.f95 -o testsandhucoords.o
@REM if %ERRORLEVEL% neq 0 (goto CompileError)
@REM gfortran -o testsandhucoords testsandhucoords.o libgeopackdp.dll
@REM if %ERRORLEVEL% neq 0 (goto CompileError)

echo Done

del *.o
exit /b 0

:CompileError
echo Compilation error
exit /b 8