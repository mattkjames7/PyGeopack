# CFLAGS for CC
CFLAGS=-lm

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)

all: obj

obj:
	gfortran -w -c -fPIC -fno-automatic -o T89c.o T89c.f
	gfortran -w -c -fPIC -fno-automatic -o T96.o  T96.f
	gfortran -w -c -fPIC -fno-automatic -o T01_01.o  T01_01.f
	gfortran -w -c -fPIC -fno-automatic -o TS04c.o  TS04c.f
	gfortran -w -c -fPIC -fno-automatic -o wparams.o  wparams.f95
	gfortran -w -c -fPIC -fno-automatic -ffree-line-length-none -o Geopack-2008_mkj_dp.o Geopack-2008_mkj_dp.f
	
clean:
	rm -v *.o
