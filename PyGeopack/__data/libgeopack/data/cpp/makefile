all: cfiles test

cfiles:
	g++ -c -fPIC test.cc
	g++ -c -fPIC IGRFParams.cc


test:
	g++ IGRFParams.o test.o -o test
	rm -v *.o
	
clean:
	rm -v  test
