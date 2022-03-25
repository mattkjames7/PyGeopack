# CFLAGS for CC
CFLAGS=-lm -Wall -g

# Compiler
CCo=g++ -fPIC -c $(CFLAGS)
CC=g++ -fPIC $(CFLAGS)

all: obj

obj:
	$(CCo) converttracecoords.cc -o converttracecoords.o
	$(CCo) fieldlinedist.cc -o fieldlinedist.o
	$(CCo) fieldlinemidpoint.cc -o fieldlinemidpoint.o
	$(CCo) fieldliner.cc -o fieldliner.o
	$(CCo) fieldlinernorm.cc -o fieldlinernorm.o
	$(CCo) getmagequatorfp.cc -o getmagequatorfp.o
	$(CCo) latlonlt.cc -o latlonlt.o
	$(CCo) tracefield.cc -o tracefield.o
	$(CCo) tracefieldline.cc -o tracefieldline.o
	$(CCo) tracefootprints.cc -o tracefootprints.o
	$(CCo) calculatehalpha.cc -o calculatehalpha.o
	$(CCo) trace.cc -o trace.o
	$(CCo) interptraceclosestpos.cc -o interptraceclosestpos.o
	#$(CCo) tracerotationmatrices.cc -o tracerotationmatrices.o
		
clean:
	rm -v *.o
