#include "Trace.h"

void Trace(double XI, double YI, double ZI, double Dir, double DsMax, double Err, double Rlim, double R0, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName, double *XF, double *YF, double *ZF, double *XX, double *YY, double *ZZ, int *L, int LMax) {
	int NRev = 0;
	double Ds, X, Y, Z, R1, R2, R3, AD, RR, RYZ, XR, YR, ZR, AL, FC, DRP, R, DR;
	*L = 0;
	GP1.DS3 = Dir;
	Ds = 0.5*GP1.DS3;
	X = XI;
	Y = YI;
	Z = ZI;

	RHand(X,Y,Z,&R1,&R2,&R3,Iopt,ParMod,ExName,InName);
	AD = 0.01;
	if ((X*R1 + Y*R2 + Z*R3) < 0.0) {
		AD = -0.01;
	}
	
	RR = sqrt(X*X + Y*Y + Z*Z) + AD;
	while (1) {
		L[0] = L[0] + 1;
		if (L[0] > LMax) {
			L[0] = LMax;
			break;
		}
		XX[L[0]-1] = X;
		YY[L[0]-1] = Y;
		ZZ[L[0]-1] = Z;
		RYZ = Y*Y + Z*Z;
		R2 = X*X + RYZ;
		R = sqrt(R2);
		
		if ((R > Rlim) || (RYZ > 1600.0) || (X > 20.0)) {
			break;
		}
		
		if ((R < R0) && (RR > R)) {
			R1 = (R0 - R)/(RR - R);
			X = X - (X - XR)*R1;
			Y = Y - (Y - YR)*R1;
			Z = Z - (Z - ZR)*R1;			
			break;
		}
		
		if ((R < RR) && (R < 3.0)) {
			FC = 0.2;
			if (R-R0 < 0.05) {
				FC = 0.05;
			}
			AL = FC*(R - R0 + 0.2);
			Ds = Dir*AL;
		}
		XR = X;
		YR = Y;
		ZR = Z;	
		
		DRP = R - RR;
		RR = R;
		
		Step(&X,&Y,&Z,&Ds, DsMax, Err, Iopt, ParMod, ExName,InName);
		
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
