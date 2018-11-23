all: obj lib

obj:
	gcc -c -fPIC TraceRoutines.c
	gcc -c -fPIC DateTimeTools.c
	gcc -c -fPIC ModelField.c
	gcc -c -fPIC ConvCoords.c
	gcc -c -fPIC libgeopack.c
	gfortran -w -c -fPIC -fno-automatic -o T89c.o  T89c.f
	gfortran -w -c -fPIC -fno-automatic -o T96.o  T96.f
	gfortran -w -c -fPIC -fno-automatic -o T01_01.o  T01_01.f
	gfortran -w -c -fPIC -fno-automatic -o TS04c.o  TS04c.f

lib: 
	gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack.so geopack2008_rcf.f T89c.o T96.o T01_01.o TS04c.o TraceRoutines.o DateTimeTools.o ModelField.o ConvCoords.o libgeopack.o -lm
	rm -v *.o
	
clean:
	rm -v  *.so
