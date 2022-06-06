# CFLAGS for CC
CFLAGS=-lm -Wall -g

# Compiler
CC=g++  -fPIC $(CFLAGS) 


all:
	$(CC) test.cc ../*.o ../*/*.o -o test  -lgfortran
	#$(CC) testrotate.cc ../*.o ../*/*.o -o testrotate  -lgfortran
	$(CC) testswitchmodel.cc ../*.o ../*/*.o -o testswitchmodel -lgfortran


clean:
	rm -v test
