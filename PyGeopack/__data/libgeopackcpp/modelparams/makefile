# CFLAGS for CC
CFLAGS=-lm 

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)

all: obj

obj:
	$(CCo) calculatew.cc -o calcualtew.o
	$(CCo) calculateg.cc -o calculateg.o
	$(CCo) tsygdata.cc -o tsygdata.o
	$(CCo) modelparams.cc -o modelparams.o
	$(CCo) checkv.cc -o checkv.o
	$(CCo) fillinkp.cc -o fillinkp.o
clean:
	rm -v *.o
