all:
	gfortran -w -c -fPIC -fno-automatic -o T89c.o T89c.f
	gcc -c -fPIC wrap89.c -o wrap89.o

	gfortran -fPIC -fno-automatic -ffree-line-length-none -shared -o libt89.so wrap89.o T89c.o -lm
	rm -v *.o
	
clean:
	rm -v libt89.so
