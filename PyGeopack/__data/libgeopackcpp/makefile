all: obj

obj:
	cd fortran;	make all
	cd spline; make all
	cd libdatetime; make all

clean:
	cd fortran;	make clean
	cd spline; make clean
	cd libdatetime; make clean
	rm -v  *.so
