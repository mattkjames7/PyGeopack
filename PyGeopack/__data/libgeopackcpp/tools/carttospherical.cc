#include "carttospherical.h"

/*convert cartesian to spherical polar coordinates*/	
void CartToSpherical(	double x, double y, double z, 
						double *r, double *theta, double *phi) {
	
	double sq = pow(x,2) + pow(y,2);
	*r = sqrt(sq + pow(z,2));
	if (sq > 0.0) {
		sq = sqrt(sq);
		*phi = atan2(y,x);
		*theta = atan2(sq,z);
	} else {
		*phi = 0.0;
		if (z < 0.0) {
			*theta = M_PI;
		} else {
			*theta = 0.0;
		}
	}		
	return;
}
