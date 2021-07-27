#include "getmagequatorfp.h"

void GetMagEquatorFP(	double *x, double *y, double *z, 
						double *s, double *R, int n, 
						double *Lshell, double *MltE) {
	/*******************************************************************
	 * This function will replace the old "NorthSouthFLs" and find the 
	 * point farthest away from the planet, unless that point is a 
	 * significant distance away from the SM x-y plane in the dayside.
	 * 
	 * ****************************************************************/
	int i, Imx, dirn=-1;
	double Rmx, rho, xt, yt, zt, a;
	



	/*find the maximum R*/
	argmax(R,n,&Rmx,&Imx);

	/*Convert to SM */
	smgsw_08_(&xt,&yt,&zt,&x[Imx],&y[Imx],&z[Imx],&dirn);

	if (x[Imx] < 0.0) {
		/* if we are on the night side, just use the furthest point */
		Lshell[0] = R[Imx];
		MltE[0] = fmod(atan2(-yt,-xt)*12.0/M_PI + 24.0,24.0);
	} else {
		/*convert to SM and check that it is within 10 degrees of SM x-y plane*/
		
		rho = sqrt(xt*xt + yt*yt);
		a = acos(rho/Rmx)*180.0/M_PI;
		if (a > 10.0) {
			/* at this point use the midpoint along the field line */
			FieldLineMidPoint(x,y,z,s,n,&xt,&yt,&zt);
			Lshell[0] = sqrt(xt*xt + yt*yt + zt*zt);
			MltE[0] = fmod(atan2(-yt,-xt)*12.0/M_PI + 24.0,24.0);
			
		} else { 
			/*just use the largest R*/
			Lshell[0] = R[Imx];
			MltE[0] = fmod(atan2(-yt,-xt)*12.0/M_PI + 24.0,24.0);	
		}
	}
}
