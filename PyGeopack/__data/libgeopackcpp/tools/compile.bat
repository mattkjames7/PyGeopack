@echo off

echo Compiling tools

g++ -fPIC -c -lm argmax.cc -o argmax.o
g++ -fPIC -c -lm carttospherical.cc -o carttospherical.o
g++ -fPIC -c -lm linterp.cc -o linterp.o
g++ -fPIC -c -lm reverseelements.cc -o reverseelements.o
g++ -fPIC -c -lm BubbleArgSort.cc -o BubbleArgSort.o

exit /b 0

:CompileError
echo Compilation error
exit /b 8
