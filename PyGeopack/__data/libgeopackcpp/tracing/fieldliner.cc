#include "fieldliner.h"

void FieldLineR(int n, double *x, double *y, double *z, double *R) {
	/*******************************************************************
	 * Calculate R along the field line.
	 * 
	 * ****************************************************************/
	int i;
	
	/*calculate R*/
	for (i=0;i<n;i++) {
		R[i] = sqrt(x[i]*x[i] + y[i]*y[i] + z[i]*z[i]);
	}

}
