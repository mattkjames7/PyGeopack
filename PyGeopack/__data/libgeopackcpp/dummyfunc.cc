#include "dummyfunc.h"

void DummyFunc(	int *iopt, double *parmod, double *ps, 
				double *x, double *y, double *z, 
				double *bx, double *by, double *bz) {
	bx[0] = 0.0;
	by[0] = 0.0;
	bz[0] = 0.0;
	return;
}
