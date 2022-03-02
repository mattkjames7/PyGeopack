@echo off

echo Compiling matrix

g++ -fPIC -c -lm -fopenmp matrix.cc -o matrix.o
g++ -fPIC -c -lm -fopenmp matrixarray.cc -o matrixarray.o
g++ -fPIC -c -lm -fopenmp matrixmath.cc -o matrixmath.o
g++ -fPIC -c -lm -fopenmp identity.cc -o identity.o
g++ -fPIC -c -lm -fopenmp rotmatrix.cc -o rotmatrix.o
g++ -fPIC -c -lm -fopenmp libmatrix.cc -o libmatrix.o

exit /b 0

:CompileError
echo Compilation error
exit /b 8
