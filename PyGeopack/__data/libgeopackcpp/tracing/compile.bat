
@echo off

echo Compiling tracing

g++ -fPIC -c -lm -Wall converttracecoords.cc -o converttracecoords.o
g++ -fPIC -c -lm -Wall fieldlinedist.cc -o fieldlinedist.o
g++ -fPIC -c -lm -Wall fieldlinemidpoint.cc -o fieldlinemidpoint.o
g++ -fPIC -c -lm -Wall fieldliner.cc -o fieldliner.o
g++ -fPIC -c -lm -Wall fieldlinernorm.cc -o fieldlinernorm.o
g++ -fPIC -c -lm -Wall getmagequatorfp.cc -o getmagequatorfp.o
g++ -fPIC -c -lm -Wall latlonlt.cc -o latlonlt.o
g++ -fPIC -c -lm -Wall tracefield.cc -o tracefield.o
g++ -fPIC -c -lm -Wall tracefieldline.cc -o tracefieldline.o
g++ -fPIC -c -lm -Wall tracefootprints.cc -o tracefootprints.o
g++ -fPIC -c -lm -Wall calculatehalpha.cc -o calculatehalpha.o
g++ -fPIC -c -lm -Wall trace.cc -o trace.o
g++ -fPIC -c -lm -Wall interptraceclosestpos.cc -o interptraceclosestpos.o
g++ -fPIC -c -lm -Wall tracerotationmatrices.cc -o tracerotationmatrices.o

exit /b 0

:CompileError
echo Compilation error
exit /b 8
