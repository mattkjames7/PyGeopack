#include "reverseelements.h"


void ReverseElements(double *x, int n) {
	double tmp[n];
	int i;
	for (i=0;i<n;i++) {
		tmp[i] = x[i];
	}
	for (i=0;i<n;i++) {
		x[i] = tmp[n-i-1];
	}
	return;
}
