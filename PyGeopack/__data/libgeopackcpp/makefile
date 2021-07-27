# CFLAGS for CC
CFLAGS=-lm 

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)


all: obj lib

obj:
	cd fortran;	make all
	cd spline; make all
	cd libdatetime; make all
	cd modelparams; make all
	cd tracing; make all
	cd tools; make all
	
	$(CCo) modelfield.cc -o modelfield.o
	$(CCo) withinmp.cc -o withinmp.o
	$(CCo) dummyfunc.cc -o dummyfunc.o
	$(CCo) ConvCoords.cc -o ConvCoords.o
	
	
lib:
	gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack.so -lm fortran/*.o spline/*.o libdatetime/*.o modelparams/*.o *.o

	

clean:
	cd fortran;	make clean
	cd spline; make clean
	cd libdatetime; make clean
	cd modelparams; make clean
	cd tracing; make clean
	cd tools; make clean
	rm -v  *.so
