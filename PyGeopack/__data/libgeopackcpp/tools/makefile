# CFLAGS for CC
CFLAGS=-lm 

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)

all: obj

obj:
	$(CCo) argmax.cc -o argmax.o
	$(CCo) carttospherical.cc -o carttospherical.o
	$(CCo) linterp.cc -o linterp.o
	$(CCo) reverseelements.cc -o reverseelements.o
	$(CCo) BubbleArgSort.cc -o BubbleArgSort.o
		
clean:
	rm -v *.o
