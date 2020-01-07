#include "Step.h"

void Step(double *X, double *Y, double *Z, double *Ds, double DsMax, double ErrIn, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName){
	/* Makes a step in the direction of the magnetic field*/
	
	double R11, R12, R13, R21, R22, R23, R31, R32, R33, R41, R42, R43, R51, R52, R53, ErrCur;

	

	while (1) {
		GP1.DS3 = -Ds[0]/3.0;
		RHand(X[0],Y[0],Z[0],&R11,&R12,&R13,Iopt,ParMod,ExName,InName);
		RHand(X[0]+R11,Y[0]+R12,Z[0]+R13,&R21,&R22,&R23,Iopt,ParMod,ExName,InName);
		RHand(X[0]+0.5*(R11+R21),Y[0]+0.5*(R12+R22),Z[0]+0.5*(R13+R23),&R31,&R32,&R33,Iopt,ParMod,ExName,InName);
		RHand(X[0]+0.375*(R11+3.0*R31),Y[0]+0.375*(R12+3.0*R32),Z[0]+0.375*(R13+3.0*R33),&R41,&R42,&R43,Iopt,ParMod,ExName,InName);
		RHand(X[0]+1.5*(R11-3.0*R31+4.0*R41),Y[0]+1.5*(R12-3.0*R32+4.0*R42),Z[0]+1.5*(R13-3.0*R33+4.0*R43),&R51,&R52,&R53,Iopt,ParMod,ExName,InName);
		ErrCur = fabs(R11-4.5*R31+4.0*R41-0.5*R51)+fabs(R12-4.5*R32+4.0*R42-0.5*R52)+fabs(R13-4.5*R33+4.0*R43-0.5*R53);
		
		if (ErrCur > ErrIn) {
			Ds[0] = Ds[0]*0.5;
		} else {
			if (fabs(Ds[0]) > DsMax) {
				Ds[0] = (Ds[0]/fabs(Ds[0]))*DsMax;
			} else {
				break;
			}
		}
	}
	
	X[0] = X[0] + 0.5*(R11 + 4.0*R41 + R51);
	Y[0] = Y[0] + 0.5*(R12 + 4.0*R42 + R52);
	Z[0] = Z[0] + 0.5*(R13 + 4.0*R43 + R53);
	
	if ((ErrCur < ErrIn*0.04) && (*Ds < DsMax/1.5)) {
		Ds[0] = Ds[0]*1.5;
	}
}
