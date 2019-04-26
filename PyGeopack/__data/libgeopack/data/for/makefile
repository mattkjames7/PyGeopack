all: obj lib

obj:
	gcc -c -fPIC TraceRoutines.c -o TraceRoutines.o
	gcc -c -fPIC DateTimeTools.c -o DateTimeTools.o
	gcc -c -fPIC ModelField.c -o ModelField.o
	gcc -c -fPIC ConvCoords.c -o ConvCoords.o
	gcc -c -fPIC libgeopack.c -o libgeopack.o
	gfortran -w -c -fPIC -fno-automatic -o T89c.o T89c.f
	gfortran -w -c -fPIC -fno-automatic -o T96.o  T96.f
	gfortran -w -c -fPIC -fno-automatic -o T01_01.o  T01_01.f
	gfortran -w -c -fPIC -fno-automatic -o TS04c.o  TS04c.f

lib: 
	gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack.so geopack2008_rcf.f T89c.o T96.o T01_01.o TS04c.o TraceRoutines.o DateTimeTools.o ModelField.o ConvCoords.o libgeopack.o -lm
	rm -v *.o

clean:
	rm -v  *.so
