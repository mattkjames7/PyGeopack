#include "argmax.h"

void argmax(double *x, int n, double *xmx, int *Imx) {
	/*finds the index of the maximum within an array*/
	int i;
	xmx[0] = 0.0;
	Imx[0] = 0;
	for (i=0;i<n;i++) {
		if (x[i] > xmx[0]) {
			xmx[0] = x[i];
			Imx[0] = i;
		}
	}
}
