#include "libspline.h"

void spline(int n0, double *x0, double *y0, 
			int n1, double *x1, double *y1) {
	
	/* create the spline object */
	Spline spl(n0,x0,y0);
	
	/* interpolate the new x1 positions */
	spl.Interpolate(n1,x1,y1);
	

}
