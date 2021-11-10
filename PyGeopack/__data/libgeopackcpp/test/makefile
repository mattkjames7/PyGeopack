# CFLAGS for CC
CFLAGS=-lm -fopenmp -Wall -lgfortran

# Compiler
CC=g++  -fPIC $(CFLAGS) 


all:
	$(CC) test.cc ../*.o ../*/*.o -o test
	$(CC) testrotate.cc ../*.o ../*/*.o -o testrotate


clean:
	rm -v test
