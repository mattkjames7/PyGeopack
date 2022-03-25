# CFLAGS for CC
CFLAGS=-lm -g

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)


all: obj lib tests

obj:
	cd fortran;	make all
	cd spline; make all
	cd libdatetime; make all
	cd modelparams; make all
	cd tracing; make all
	cd tools; make all
	#cd matrix; make all
	
	$(CCo) modelfield.cc -o modelfield.o
	$(CCo) withinmp.cc -o withinmp.o
	$(CCo) dummyfunc.cc -o dummyfunc.o
	$(CCo) ConvCoords.cc -o ConvCoords.o
	$(CCo) getdipoletilt.cc -o getdipoletilt.o
	$(CCo) recalc.cc -o recalc.o
	
lib:
	gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libgeopack.so -lm \
		fortran/*.o \
		spline/*.o \
		libdatetime/*.o \
		modelparams/*.o \
		tools/*.o \
		tracing/*.o \
		*.o -lstdc++ -lgomp
		#matrix/*.o \
		

tests:
	cd test; make all
	

clean:
	cd fortran;	make clean
	cd spline; make clean
	cd libdatetime; make clean
	cd modelparams; make clean
	cd tracing; make clean
	cd tools; make clean
	cd matrix; make clean
	cd test; make clean
	rm *.o
	rm -v  *.so
