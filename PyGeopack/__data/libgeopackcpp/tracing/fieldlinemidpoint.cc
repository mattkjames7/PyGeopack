#include "fieldlinemidpoint.h"

void FieldLineMidPoint(double *x, double *y, double *z, double *s, int n, 
	double *xm, double *ym, double *zm) {
	/*******************************************************************
	 * Calculate the coordinates of the mid point along a field line.
	 * 
	 * ****************************************************************/
	int i, i0, i1;
	double sm, ds, m;
	sm = s[n-1]/2.0;
	/* find the midpoint indices*/
	for (i=0;i<n-1;i++) {
		if ((s[i] <= sm) && (s[i+1] > sm)) {
			i0 = i;
			i1 = i + 1;
			break;
		}
	}
	
	/* now interpolate x*/
	xm[0] = linterp(s[i0],s[i1],x[i0],x[i1],sm);
	ym[0] = linterp(s[i0],s[i1],y[i0],y[i1],sm);
	zm[0] = linterp(s[i0],s[i1],z[i0],z[i1],sm);
	

} 
