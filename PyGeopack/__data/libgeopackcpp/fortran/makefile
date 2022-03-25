# CFLAGS for CC
FCFLAGS=-w -c -fPIC -fno-automatic -std=legacy

# Compiler
FC=gfortran $(FCFLAGS)

all: obj

obj:
	$(FC) -o T89c.o T89c.f
	$(FC) -o T96.o  T96.f
	$(FC) -o T01_01.o  T01_01.f
	$(FC) -o TS04c.o  TS04c.f
	$(FC) -o wparams.o  wparams.f95
	$(FC) -ffree-line-length-none -o Geopack-2008_mkj_dp.o Geopack-2008_mkj_dp.f
	
clean:
	rm -v *.o
