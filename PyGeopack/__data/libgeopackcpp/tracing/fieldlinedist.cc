#include "fieldlinedist.h"


void FieldLineDist(int n, double *x, double *y, double *z, double *s) {
	/*******************************************************************
	 * Calculate the distance along the field line.
	 * 
	 * ****************************************************************/
	int i;
	double ds, dx, dy, dz;
	s[0] = 0.0;
	for (i=1;i<n;i++) {
		dx = x[i] - x[i-1];
		dy = y[i] - y[i-1];
		dz = z[i] - z[i-1];
		ds = sqrt(dx*dx + dy*dy + dz*dz);
		s[i] = s[i-1] + ds;
	}
}
