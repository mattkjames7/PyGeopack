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

cd fortran
call compile.bat
cd ..

cd libdatetime
call compile.bat
cd ..

cd matrix
call compile.bat
cd ..

cd modelparams
call compile.bat
cd ..

cd spline
call compile.bat
cd ..

cd tools
call compile.bat
cd ..

cd tracing
call compile.bat
cd ..


g++ -fPIC -c -lm -fopenmp modelfield.cc -o modelfield.o
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -fPIC -c -lm -fopenmp withinmp.cc -o withinmp.o
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -fPIC -c -lm -fopenmp dummyfunc.cc -o dummyfunc.o
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -fPIC -c -lm -fopenmp ConvCoords.cc -o ConvCoords.o
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -fPIC -c -lm -fopenmp getdipoletilt.cc -o getdipoletilt.o
if %ERRORLEVEL% neq 0 (goto CompileError)
g++ -fPIC -c -lm -fopenmp recalc.cc -o recalc.o
	

gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack.dll -lm fortran/*.o spline/*.o libdatetime/*.o modelparams/*.o tools/*.o tracing/*.o matrix/*.o *.o -lstdc++ -lgomp
if %ERRORLEVEL% neq 0 (goto CompileError)


echo Done

del *.o
del fortran\*.o
del libdatetime\*.o
del matrix\*.o
del modelparams\*.o
del spline\*.o
del tools\*.o
del tracing\*.o
exit /b 0

:CompileError
echo Compilation error
exit /b 8
