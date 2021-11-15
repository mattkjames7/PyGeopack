# CFLAGS for CC
CFLAGS=-lm -fopenmp -Wall -g

# Compiler
CC=g++  -fPIC $(CFLAGS) 


all:
	$(CC) test.cc ../*.o ../*/*.o -o test  -lgfortran
	$(CC) testrotate.cc ../*.o ../*/*.o -o testrotate  -lgfortran


clean:
	rm -v test
