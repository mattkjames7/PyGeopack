#include "withinmp.h"

bool WithinMP(double x, double y, double z, double Bz, double Pdyn) {
	/*******************************************************************
	 * Check whether a point is inside the magnetosphere
	 * 
	 * ****************************************************************/
	 int i;
	 double r0, alpha, r, rm;
	 
	 r0 = (10.22 + 1.29*tanh(0.184*(Bz + 8.14)))*pow(Pdyn,-0.15151515);
	 alpha = (0.58-0.007*Bz)*(1.0 + 0.024*log(Pdyn));
	 r = sqrt(x*x + y*y + z*z);
	 rm = r0*pow(2.0/(1.0 + x/r),alpha);
	 return (r < rm);
}
