#include <stdio.h>
#include <stdlib.h>

extern void t89c_(int *iopt, float *parmod, float *ps, float *x, float *y, float *z, float *bx, float *by, float *bz);

void wrap89(int *iopt, float *parmod, float *ps, float *x, float *y, float *z, float *bx, float *by, float *bz) {
	
	t89c_(iopt,parmod,ps,x,y,z,bx,by,bz);
	
}
