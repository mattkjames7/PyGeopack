@echo off

echo Compiling modelparams


g++ -fPIC -c -lm calculatew.cc -o calcualtew.o
g++ -fPIC -c -lm calculateg.cc -o calculateg.o
g++ -fPIC -c -lm tsygdata.cc -o tsygdata.o
g++ -fPIC -c -lm modelparams.cc -o modelparams.o
g++ -fPIC -c -lm checkv.cc -o checkv.o
g++ -fPIC -c -lm fillinkp.cc -o fillinkp.o

exit /b 0

:CompileError
echo Compilation error
exit /b 8
