#include "getmagequatorfp.h"

void GetMagEquatorFP(	double *x, double *y, double *z, 
						double *s, double *R, int n,
						double *xfe, double *yfe, double *zfe, 
						double *Lshell, double *MltE) {
	/*******************************************************************
	 * This function will replace the old "NorthSouthFLs" and find the 
	 * point farthest away from the planet, unless that point is a 
	 * significant distance away from the SM x-y plane in the dayside.
	 * 
	 * ****************************************************************/
	int i, Imx, dirn=-1;
	double Rmx, rho, a;
	double xt, yt, zt;



	/*find the maximum R*/
	argmax(R,n,&Rmx,&Imx);

	/*Convert to SM */
	smgsw_08_(xfe,yfe,zfe,&x[Imx],&y[Imx],&z[Imx],&dirn);

	if (x[Imx] < 0.0) {
		/* if we are on the night side, just use the furthest point */
		Lshell[0] = R[Imx];
		MltE[0] = fmod(atan2(-yfe[0],-xfe[0])*12.0/M_PI + 24.0,24.0);
	} else {
		/*convert to SM and check that it is within 10 degrees of SM x-y plane*/
		
		rho = sqrt(xfe[0]*xfe[0] + yfe[0]*yfe[0]);
		a = acos(rho/Rmx)*180.0/M_PI;
		if (a > 10.0) {
			/* at this point use the midpoint along the field line */
			FieldLineMidPoint(x,y,z,s,n,&xt,&yt,&zt);
			smgsw_08_(xfe,yfe,zfe,&xt,&yt,&zt,&dirn);
			Lshell[0] = sqrt(xfe[0]*xfe[0] + yfe[0]*yfe[0] + zfe[0]*zfe[0]);
			MltE[0] = fmod(atan2(-yfe[0],-xfe[0])*12.0/M_PI + 24.0,24.0);
			
		} else { 
			/*just use the largest R*/
			Lshell[0] = R[Imx];
			MltE[0] = fmod(atan2(-yfe[0],-xfe[0])*12.0/M_PI + 24.0,24.0);	
		}
	}
}

void GetMagEquatorFPSM(	double *xsm, double *ysm, double *zsm, 
						double *s, double *R, int n,
						double *xfe, double *yfe, double *zfe, 
						double *Lshell, double *MltE) {
	/*******************************************************************
	 * This function will replace the old "NorthSouthFLs" and find the 
	 * point farthest away from the planet, unless that point is a 
	 * significant distance away from the SM x-y plane in the dayside.
	 * 
	 * ****************************************************************/
	int i, Imx, dirn=-1;
	double Rmx, rho, a;
	double xt, yt, zt;



	/*find the maximum R*/
	argmax(R,n,&Rmx,&Imx);

	/*Convert to SM */
	//smgsw_08_(xfe,yfe,zfe,&x[Imx],&y[Imx],&z[Imx],&dirn);

	xfe[0] = xsm[Imx];
	yfe[0] = ysm[Imx];
	zfe[0] = zsm[Imx];

	if (xsm[Imx] < 0.0) {
		/* if we are on the night side, just use the furthest point */
		Lshell[0] = R[Imx];
		MltE[0] = fmod(atan2(-yfe[0],-xfe[0])*12.0/M_PI + 24.0,24.0);
	} else {
		/*convert to SM and check that it is within 10 degrees of SM x-y plane*/
		rho = sqrt(xfe[0]*xfe[0] + yfe[0]*yfe[0]);
		a = acos(rho/Rmx)*180.0/M_PI;

		if (a > 10.0) {
			/* at this point use the midpoint along the field line */
			FieldLineMidPoint(xsm,ysm,zsm,s,n,xfe,yfe,zfe);

			//smgsw_08_(xfe,yfe,zfe,&xt,&yt,&zt,&dirn);
			Lshell[0] = sqrt(xfe[0]*xfe[0] + yfe[0]*yfe[0] + zfe[0]*zfe[0]);
			MltE[0] = fmod(atan2(-yfe[0],-xfe[0])*12.0/M_PI + 24.0,24.0);
		} else { 
			/*just use the largest R*/
			Lshell[0] = R[Imx];
			MltE[0] = fmod(atan2(-yfe[0],-xfe[0])*12.0/M_PI + 24.0,24.0);	
		}
	}
}
