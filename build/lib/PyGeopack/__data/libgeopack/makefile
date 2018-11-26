all: obj64 obj32 lib64 lib32

obj64:
	gcc -c -fPIC TraceRoutines.c -o TraceRoutines.o64
	gcc -c -fPIC DateTimeTools.c -o DateTimeTools.o64
	gcc -c -fPIC ModelField.c -o ModelField.o64
	gcc -c -fPIC ConvCoords.c -o ConvCoords.o64
	gcc -c -fPIC libgeopack.c -o libgeopack.o64
	gfortran -w -c -fPIC -fno-automatic -o T89c.o64  T89c.f
	gfortran -w -c -fPIC -fno-automatic -o T96.o64  T96.f
	gfortran -w -c -fPIC -fno-automatic -o T01_01.o64  T01_01.f
	gfortran -w -c -fPIC -fno-automatic -o TS04c.o64  TS04c.f

obj32:
	gcc -m32 -c -fPIC TraceRoutines.c -o TraceRoutines.o32
	gcc -m32 -c -fPIC DateTimeTools.c -o DateTimeTools.o32
	gcc -m32 -c -fPIC ModelField.c -o ModelField.o32
	gcc -m32 -c -fPIC ConvCoords.c -o ConvCoords.o32
	gcc -m32 -c -fPIC libgeopack.c -o libgeopack.o32
	gfortran -m32 -w -c -fPIC -fno-automatic -o T89c.o32  T89c.f
	gfortran -m32 -w -c -fPIC -fno-automatic -o T96.o32  T96.f
	gfortran -m32 -w -c -fPIC -fno-automatic -o T01_01.o32  T01_01.f
	gfortran -m32 -w -c -fPIC -fno-automatic -o TS04c.o32  TS04c.f

lib64: 
	gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack64.so geopack2008_rcf.f T89c.o64 T96.o64 T01_01.o64 TS04c.o64 TraceRoutines.o64 DateTimeTools.o64 ModelField.o64 ConvCoords.o64 libgeopack.o64 -lm
	rm -v *.o64

lib32: 
	gfortran -m32 -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack32.so geopack2008_rcf.f T89c.o32 T96.o32 T01_01.o32 TS04c.o32 TraceRoutines.o32 DateTimeTools.o32 ModelField.o32 ConvCoords.o32 libgeopack.o32 -lm
	rm -v *.o32
	
clean:
	rm -v  *.so
