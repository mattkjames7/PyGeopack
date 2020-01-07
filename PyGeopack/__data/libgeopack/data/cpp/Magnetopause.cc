#include "Magnetopause.h"

void ShueEtAlMP(double XN_PD, double Vel, double BzIMF, double XGSW, double YGSW, double ZGSW, double *XMGNP, double *YMGNP, double *ZMGNP, double *Dist, int *Id) {
	/*******************************************************************
	 * This routine is based on the SHUETAL_MGNP_08 subroutine from
	 * Geopack.
	 * 
	 * This will determine whether a point in space is within the Shue et al 
	 * magnetopause, and also determine the nearest point on the magnetopause.
	 * 
	 * 
	 * ****************************************************************/
	
	
	double P, R0, Alpha, Rm, XMT96, YMT96, ZMT96, Rho2, R, ST, CT, Phi, T, F, GradF_T, GradF_R, GradF, DR, DT, DS, Rho;
	int Id96, NIT;
	
	/* Check if the ram pressure is provided in XN_PD or if it needs to
	 * be calculated (based on the sign of Vel */
	if (Vel < 0.0) {
		P = XN_PD;
	} else {
		P = 1.94e-6 * XN_PD * Vel*Vel;
	}
	
	/* Work out Phi - the angle between the y and z componenets of the 
	 * input vector */
	if (YGSW != 0.0 || ZGSW != 0.0) {
		Phi = atan2(YGSW,ZGSW);
	} else { 
		Phi = 0.0;
	}
	
	Id[0] = -1;
	/* These two quantities are calculated using equations 10 and 11 of
	 * Shue et al 1998 */ 
	R0 = (10.22 + 1.29*tanh(0.184*(BzIMF + 8.14)))*pow(P,(-0.15151515));
	Alpha = (0.58 - 0.007*BzIMF)*(1.0 + 0.024*log(P));
	
	/* Compare the current radial position to that of the MP */
	R = sqrt(XGSW*XGSW + YGSW*YGSW + ZGSW*ZGSW);
	Rm = R0*pow((2.0/(1.0 + XGSW/R)),Alpha);
	
	if (R <= Rm) {
		/* Within the MP */
		Id[0] = 1;
	}
	
	/* Search for the T96 MP position, as a starting point for finding the
	 * nearest point on the Shue et al one */
	T96MP(P,-1.0,XGSW,YGSW,ZGSW,&XMT96,&YMT96,&ZMT96,Dist,&Id96);
	
	Rho2 = YMT96*YMT96 + ZMT96*ZMT96;
	R = sqrt(Rho2 + XMT96*XMT96);
	ST = sqrt(Rho2)/R;
	CT = XMT96/R;
	
	/* Apparently this is Newton's iterative method which will find the
	 * nearest point on the MP */
	NIT = 0;
	while (1) {
		T = atan2(ST,CT);
		Rm = R0*pow((2.0/(1.0+CT)),Alpha);
		F = R - Rm;
		GradF_R = 1.0;
		GradF_T = -Alpha/R*Rm*ST/(1.0 + CT);
		GradF = sqrt(pow(GradF_T,2.0) + pow(GradF_R,2.0));
		DR = -F/pow(GradF,2.0);
		DT = DR/R*GradF_T;
		
		R = R + DR;
		T = T + DT;
		
		ST = sin(T);
		CT = cos(T);
		
		DS = sqrt(pow(DR,2.0) + pow(R*DT,2.0));
		
		NIT++;
		
		if (DS <= 1.0e-4) {
			break;
		}
	}
	
	*XMGNP = R*cos(T);
	Rho = R*sin(T);
	*YMGNP = Rho * sin(Phi);
	*ZMGNP = Rho * cos(Phi);
	
	*Dist = sqrt(pow(XGSW-*XMGNP,2.0) + pow(YGSW-*YMGNP,2.0) + pow(ZGSW-*ZMGNP,2.0));
	
}

void T96MP(double XN_PD, double Vel, double XGSW, double YGSW, double ZGSW, double *XMGNP, double *YMGNP, double *ZMGNP, double *Dist, int *ID) {
	/*******************************************************************
	 * This function is based on the T96_MGNP_08 routine of Geopack.
	 * It works out a point on the T96 magnetopause with the same Tau 
	 * parameter (see Tsyganenko 1995) as the current position - this is 
	 * used as a starting point for finding the actual nearest position 
	 * on the Shue et al 1998 MP.
	 * ****************************************************************/
	double Pd, Rat, Rat16, A0, S00, X00, A, S0, Z0, Xm, Phi, Rho, RhoMGNP, XKSI, XDZT, SQ1, SQ2, Sigma, Tau, Arg, X0;
	
	if (Vel < 0.0) {
		Pd = XN_PD;
	} else { 
		Pd = 1.94e-6*XN_PD*pow(Vel,2.0);
	}
	
	Rat = Pd/2.0;
	Rat16 = pow(Rat,0.14);
	
	A0 = 70.0;
	S00 = 1.08;
	X00 = 5.48;
	A = A0/Rat16;
	S0 = S00;
	X0 = X00/Rat16;
	Xm = X0-A;
	
	if (YGSW != 0.0 || ZGSW != 0.0) {
		Phi = atan2(YGSW,ZGSW);
	} else {
		Phi = 0.0;
	}
	
	Rho = sqrt(pow(YGSW,2.0) + pow(ZGSW,2.0));
	
	if (XGSW < Xm) {
		*XMGNP = XGSW;
		RhoMGNP = A*sqrt(S0*S0-1);
		*YMGNP = RhoMGNP*sin(Phi);
		*ZMGNP = RhoMGNP*cos(Phi);
		*Dist =  sqrt(pow(XGSW - *XMGNP,2.0) + pow(YGSW - *YMGNP,2.0) +pow(ZGSW - *ZMGNP,2.0));
		if (RhoMGNP > Rho) {
			ID[0] = 1;
		} else {
			ID[0] = -1;
		}
		return;
	}
	
	XKSI = (XGSW - X0)/A + 1.0;
	XDZT = Rho/A;
	SQ1 = sqrt(pow(1.0+XKSI,2.0) + pow(XDZT,2.0));
	SQ2 = sqrt(pow(1.0-XKSI,2.0) + pow(XDZT,2.0));
	Sigma = 0.5*(SQ1 + SQ2);
	Tau = 0.5*(SQ1 - SQ2);
	
	*XMGNP = X0 - A*(1.0 - S0*Tau);
	Arg = (S0*S0 - 1.0)*(1.0 - Tau*Tau);
	if (Arg < 0) {
		Arg = 0.0;
	}
	RhoMGNP = A*sqrt(Arg);
	*YMGNP = RhoMGNP*sin(Phi);
	*ZMGNP = RhoMGNP*cos(Phi);
	
	*Dist =  sqrt(pow(XGSW - *XMGNP,2.0) + pow(YGSW - *YMGNP,2.0) + pow(ZGSW - *ZMGNP,2.0));
	if (Sigma > S0) {
		ID[0] = -1;
	} else { 
		ID[0] = 1;
	} 
	return;
}
