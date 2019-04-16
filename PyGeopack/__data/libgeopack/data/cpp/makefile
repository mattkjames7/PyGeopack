all: obj lib

CC=g++
CFLAGS=-c -fPIC

obj:
	g++ -c -fPIC ConvCoords.cc
	g++ -c -fPIC DateTimeTools.cc
	g++ -c -fPIC IGRF.cc
	g++ -c -fPIC IGRFParams.cc
	g++ -c -fPIC libgeopack.cc
	g++ -c -fPIC Magnetopause.cc
	g++ -c -fPIC ModelField.cc
	g++ -c -fPIC Recalc.cc
	g++ -c -fPIC RHand.cc
	g++ -c -fPIC Step.cc
	g++ -c -fPIC Sun.cc
	g++ -c -fPIC T96.cc
	g++ -c -fPIC Trace.cc

lib:
	g++ -shared -lm -o libgeopack.so ConvCoords.o DateTimeTools.o IGRF.o IGRFParams.o libgeopack.o Magnetopause.o ModelField.o Recalc.o RHand.o Step.o Sun.o T96.o Trace.o
	rm -v *.o
	
clean:
	rm -v  *.o *.so
