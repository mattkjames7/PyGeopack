#include "IGRF.h"


void IGRF_GSW(double XGSW, double YGSW, double ZGSW, double *HxGSW, double *HyGSW, double *HzGSW) {
	/* make sure you run recalc before this! 
	 * Calculates the IGRF magnetic field in GSW coordinates */
	
	double A[14], B[14];
	double XGEO, YGEO, ZGEO, Rho2, R, C, Rho, S, CF, SF, PP, P, IRP3, D;
	double BI, P2, D2, X, Y, Z, W, BBR, BBT, BBF, Q, AN, E, HH, QQ, XK;
	double DP, PM, Br, Bt, Bf, He, HxGEO, HyGEO, HzGEO;
	int k , n, nm, m, mm, mn;
	
	GSWtoGEO(XGSW,YGSW,ZGSW,&XGEO,&YGEO,&ZGEO);

	Rho2 = XGEO*XGEO + YGEO*YGEO;
	R = sqrt(Rho2 + ZGEO*ZGEO);
	C = ZGEO/R;
	Rho = sqrt(Rho2);
	S = Rho/R;
	if (S < 1.0e-5) {
		CF = 1.0;
		SF = 0.0;
	} else {
		CF = XGEO/Rho;
		SF = YGEO/Rho;
	}

	PP = 1.0/R;
	P = PP;

	IRP3 = R + 2;
	nm = 3 + 30/((int) IRP3);
	if (nm > 13) {
		nm = 13;
	}
	
	
	k = nm + 1;
	
	for (n=1;n<=k;n++) {
		P = P*PP;
		A[n-1] = P;
		B[n-1] = P*n; 
	}
	
	P = 1.0;
	D = 0.0;
	BBR = 0.0;
	BBT = 0.0;
	BBF = 0.0;

	for (m=1;m<=k;m++) {
		
		if (m == 1) {
			X = 0.0;
			Y = 1.0;
		} else { 
			mm = m - 1;
			W = X;
			X = W*CF + Y*SF;
			Y = Y*CF - W*SF;
		}
		
		Q = P;
		Z = D;
		BI = 0.0;
		P2 = 0.0;
		D2 = 0.0;
		
		for (n=m;n<=k;n++) {
			AN = A[n-1];
			mn = n*(n-1)/2 + m;
			E = IGRFCurr.g[mn-1];
			HH = IGRFCurr.h[mn-1]; 
			W = E*Y + HH*X;
			BBR += B[n-1]*W*Q;
			BBT -= AN*W*Z;
			if (m != 1) {
				QQ = Q;
				if (S < 1.0e-5) {
					QQ = Z;
				}
				BI += AN*(E*X - HH*Y)*QQ;
			}
			XK = IGRFCurr.rec[mn-1];
			DP = C*Z - S*Q - XK*D2;
			PM = C*Q - XK*P2;
			D2 = Z;
			P2 = Q;
			Z = DP;
			Q = PM;
		}
		D = S*D + C*P;
		P = S*P;
		if (m != 1) {
			BI = BI*mm;
			BBF = BBF + BI;
		}
	}
	
	Br = BBR;
	Bt = BBT;
	if (S >= 1.0e-5) {
		Bf = BBF/S;
	} else {
		if (C < 0) {
			BBF = -BBF;
		}
		Bf = BBF;
	}

	He = Br*S + Bt*C;
	HxGEO = He*CF - Bf*SF;
	HyGEO = He*SF + Bf*CF;
	HzGEO = Br*C - Bt*S;
	
	GEOtoGSW(HxGEO,HyGEO,HzGEO,HxGSW,HyGSW,HzGSW);

	return;
}

void IGRF_GEO(double R, double Theta, double Phi, double *Br, double *Btheta, double *Bphi) {
	/* make sure you run recalc before this! 
	 * Calculates the IGRF magnetic field in spherical coordinates */
	
	double A[14], B[14];
	double C, S, CF, SF, PP, P, IRP3, D;
	double BI, P2, D2, X, Y, Z, W, BBR, BBT, BBF, Q, AN, E, HH, QQ, XK;
	double DP, PM, He;
	int k , n, nm, m, mm, mn;
	
	C = cos(Theta);
	S = sin(Theta);
	CF = cos(Phi);
	SF = sin(Phi);
	
	PP = 1.0/R;
	P = PP;
	
	IRP3 = R + 2;
	nm = 3 + 30/((int) IRP3);
	if (nm > 13) {
		nm = 13;
	}
	
	k = nm + 1;
	
	for (n=1;n<=k;n++) {
		P = P*PP;
		A[n-1] = P;
		B[n-1] = P*n; 
	}
	
	P = 1.0;
	D = 0.0;
	BBR = 0.0;
	BBT = 0.0;
	BBF = 0.0;
	
	for (m=1;m<=k;m++) {
		if (m == 1) {
			X = 0.0;
			Y = 1.0;
		} else { 
			mm = m-1;
			W = X;
			X = W*CF + Y*SF;
			Y = Y*CF - W*SF;
		}
		Q = P;
		Z = D;
		BI = 0.0;
		P2 = 0.0;
		D2 = 0.0;
		for (n=m;n<=k;n++) {
			AN = A[n-1];
			mn = n*(n-1)/2 + m;
			E = IGRFCurr.g[mn-1];
			HH = IGRFCurr.h[mn-1]; 
			W = E*Y + HH*X;
			BBR += B[n-1]*W*Q;
			BBT -= AN*W*Z;
			if (m != 1) {
				QQ = Q;
				if (S < 1.0e-5) {
					QQ = Z;
				}
				BI += AN*(E*X - HH*Y)*QQ;
			}
			XK = IGRFCurr.rec[mn-1];
			DP = C*Z - S*Q - XK*D2;
			PM = C*Q - XK*P2;
			D2 = Z;
			P2 = Q;
			Z = DP;
			Q = PM;
		}
		D = S*D + C*P;
		P = S*P;
		if (m != 1) {
			BI = BI*mm;
			BBF = BBF + BI;
		}
	}
	
	*Br = BBR;
	*Btheta = BBT;
	if (S >= 1.0e-5) {
		*Bphi = BBF/S;
	} else {
		if (C < 0) {
			BBF = -BBF;
		}
		*Bphi = BBF;
	}

	return;
}

void Dipole(double XGSW, double YGSW, double ZGSW, double *BxGSW, double *ByGSW, double *BzGSW) {
	/* Calculates the magnetic field due to a basic magnetic dipole*/
	
	double DipMom, P, U, V, T, Q;
	
	DipMom = sqrt(pow(IGRFCurr.g[1],2.0) + pow(IGRFCurr.g[2],2.0) + pow(IGRFCurr.h[2],2.0));
	
	P = XGSW*XGSW;
	U = ZGSW*ZGSW;
	V = 3.0 * ZGSW*XGSW;
	T = YGSW*YGSW;
	Q = DipMom/pow(sqrt(P + T + U),5.0);
	*BxGSW = Q*((T + U - 2.0*P)*GP1.SPS - V*GP1.CPS);
	*ByGSW = -3.0*YGSW*Q*(XGSW*GP1.SPS + ZGSW*GP1.CPS);
	*BzGSW = Q*((P + T - 2.0*U)*GP1.CPS - V*GP1.SPS);
	return;
}
