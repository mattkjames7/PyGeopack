@echo off

echo Compiling spline

g++ -fPIC -c -lm -std=c++17 spline.cc -o spline.o
g++ -fPIC -c -lm -std=c++17 libspline.cc -o libspline.o

exit /b 0

:CompileError
echo Compilation error
exit /b 8
