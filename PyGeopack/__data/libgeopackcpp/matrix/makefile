# CFLAGS for CC
CFLAGS=-lm -fopenmp

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ $(CFLAGS)

all: obj lib

obj:
	$(CCo) matrix.cc -o matrix.o
	$(CCo) matrixarray.cc -o matrixarray.o
	$(CCo) matrixmath.cc -o matrixmath.o
	$(CCo) identity.cc -o identity.o
	$(CCo) rotmatrix.cc -o rotmatrix.o
	$(CCo) libmatrix.cc -o libmatrix.o


lib: 
	$(CC) -shared -o libmatrix.so *.o

clean:
	rm -v *.o
