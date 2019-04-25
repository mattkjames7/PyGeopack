#include "RHand.h"

void RHand(double X, double Y, double Z, double *R1, double *R2, double *R3, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName) {
	/* Calculates the right hand side vector of the magnetic field*/
	double BxGSW, ByGSW, BzGSW, HxGSW, HyGSW, HzGSW, Bx, By, Bz, B;
	ExName(Iopt, ParMod, GP1.PSI, X, Y, Z, &BxGSW, &ByGSW, &BzGSW);
	InName(X, Y, Z, &HxGSW, &HyGSW, &HzGSW);
	
	Bx = BxGSW + HxGSW;
	By = ByGSW + HyGSW;
	Bz = BzGSW + HzGSW;
	
	B = GP1.DS3/sqrt(Bx*Bx + By*By + Bz*Bz);

	*R1 = Bx*B;
	*R2 = By*B;
	*R3 = Bz*B;
}
