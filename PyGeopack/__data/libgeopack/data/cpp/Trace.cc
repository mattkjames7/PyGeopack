#include "Trace.h"

void Trace(double XI, double YI, double ZI, double Dir, double DsMax, double Err, double Rlim, double R0, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName, double *XF, double *YF, double *ZF, double *XX, double *YY, double *ZZ, int *L, int LMax) {
	/*******************************************************************
	* Traces along the magnetic field to some boundary (either some
	* altitude above the surface, or to some maximum distance).
	* This function is based on the Geopack subroutine Trace_08.
	* Make sure that Recalc is called prior to using this function.
	*
	*******************************************************************/

	/* Initialize the tracing parameters */
	int NRev = 0;
	double Ds, X, Y, Z, R1, R2, R3, AD, RR, RYZ, XR, YR, ZR, AL, FC, DRP, R, DR;
	*L = 0;
	GP1.DS3 = Dir;
	Ds = 0.5*GP1.DS3;
	X = XI;
	Y = YI;
	Z = ZI;

	/* An initial call to RHand determines the direction of the trace */
	RHand(X,Y,Z,&R1,&R2,&R3,Iopt,ParMod,ExName,InName);
	AD = 0.01;
	if ((X*R1 + Y*R2 + Z*R3) < 0.0) {
		AD = -0.01;
	}
	
	/* Radial distance at previous step */
	RR = sqrt(X*X + Y*Y + Z*Z) + AD;
	
	/* Start tracing */
	while (1) {
		L[0] = L[0] + 1;
		if (L[0] > LMax) {
			L[0] = LMax;
			break;
		}
		
		/* Add the current position to the ouput array */
		XX[L[0]-1] = X;
		YY[L[0]-1] = Y;
		ZZ[L[0]-1] = Z;
		RYZ = Y*Y + Z*Z;
		R2 = X*X + RYZ;
		R = sqrt(R2);
		
		/* Check that we are still within the allowed tracing boundaries,
		 * RYZ < 40 Re, R < Rlim, X < 20*/ 
		if ((R > Rlim) || (RYZ > 1600.0) || (X > 20.0)) {
			break;
		}
		
		/* Check if we have crossed the altitude boundary R0 (technically
		 * this is Re + Alt in Re units) */
		if ((R < R0) && (RR > R)) {
			R1 = (R0 - R)/(RR - R);
			X = X - (X - XR)*R1;
			Y = Y - (Y - YR)*R1;
			Z = Z - (Z - ZR)*R1;			
			break;
		}
		
		/* Force smaller steps to be taken if we are tracing close to the
		 * Earth */
		if ((R < RR) && (R < 3.0)) {
			FC = 0.2;
			if (R-R0 < 0.05) {
				FC = 0.05;
			}
			AL = FC*(R - R0 + 0.2);
			Ds = Dir*AL;
		}
		
		/* Previous position */
		XR = X;
		YR = Y;
		ZR = Z;	
		
		DRP = R - RR;
		RR = R;
		
		/* Perform a step */
		Step(&X,&Y,&Z,&Ds, DsMax, Err, Iopt, ParMod, ExName,InName);
		
		/* Check that the radial direction of the trace hasn't changed
		 * too many times - i.e. tracing in loops */
		R = sqrt(X*X + Y*Y + Z*Z);
		DR = R - RR;
		if (DRP*DR < 0.0) {
			NRev++;
		}
		if (NRev > 2) {
			break;
		}
	}
	
	*XF = X;
	*YF = Y;
	*ZF = Z;
	
	XX[L[0]-1] = *XF;
	YY[L[0]-1] = *YF;
	ZZ[L[0]-1] = *ZF;
	return;
}
