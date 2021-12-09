# CFLAGS for CC
CFLAGS=-lm -std=c++17

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)

all: obj lib

obj:
	$(CCo) spline.cc -o spline.o
	$(CCo) libspline.cc -o libspline.o
	
lib: 
	$(CC) -shared -o libspline.so libspline.o spline.o
	
clean:
	rm -v *.o *.so
