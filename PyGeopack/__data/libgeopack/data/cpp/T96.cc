#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "T96.h"

RhDr rhdr = {9.0,4.0};
Dx1 dx1 = {-0.16,0.08,0.4};
LoopDip1 loopdip1 = {1.00891,2.28397,-5.60831,1.86106,7.83281,1.12541,0.945719};
Coord11 coord11 = {{-11.0,-7.0,-7.0,-3.0,-3.0,1.0,1.0,1.0,5.0,5.0,9.0,9.0},{2.0,0.0,4.0,2.0,6.0,0.0,4.0,8.0,2.0,6.0,0.0,4.0}};
Coord21 coord21 = {{-10.0,-7.0,-4.0,-4.0,0.0,4.0,4.0,7.0,10.0,0.0,0.0,0.0,0.0,0.0},{3.0,6.0,3.0,9.0,6.0,3.0,9.0,6.0,3.0,0.0,0.0,0.0,0.0,0.0},
					{20.0,20.0,4.0,20.0,4.0,4.0,20.0,20.0,20.0,2.0,3.0,4.50,7.0,10.0}};
Warp warp;


  
void T96(int Iopt, double *ParMod, double Ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double Pdyn, Pdyn0, Dst, ByIMF, BzIMF, Qx, Qy, Qz, Eps10, Am0, S0, X00, Dsig, DelIMFx, DelIMFy, sps, pps;
	double Depr, Bt, Theta, ct, st, Eps, FactEps, FactPd, RCampl, Tampl2, Tampl3, B1ampl, B2ampl, Reconn;
	double Xappa, Xappa3, ys, zs, FactIMF, OIMFx, OIMFy, OIMFz, RIMFampl, xx, yy, zz, X0, Am, Rho2, Asq, xmxm;
	double Axx0, Aro, Sigma, BxRC, ByRC, BzRC, BxT2, ByT2, BzT2, BxT3, ByT3, BzT3, CFx, CFy, CFz;
	double R1x, R1y, R1z, R2x, R2y, R2z, RIMFx, RIMFyS, RIMFzS, RIMFy, RIMFz, Fx, Fy, Fz, Fint, Fext;
	double A[] = {1.162,22.344,18.50,2.602,6.903,5.287,0.5790,0.4462,0.7850};
	Pdyn0 = 2.0;
	Eps10 = 3630.7;
	Am0 = 70.0;
	S0 = 1.08;
	X00 = 5.48;
	Dsig = 0.005;
	DelIMFx = 20.0;
	DelIMFy = 10.0;
	
	Pdyn = ParMod[0];
	Dst = ParMod[1];
	ByIMF = ParMod[2];
	BzIMF = ParMod[3];
	
	sps = sin(Ps);
	pps = Ps;
	Depr = 0.8*Dst - 13.0*sqrt(Pdyn);
	
	Bt = sqrt(ByIMF*ByIMF + BzIMF*BzIMF);
	
	if (ByIMF == 0.0  && BzIMF == 0.0) {
		Theta = 0.0;
	} else {
		Theta = atan2(ByIMF,BzIMF);
		if (Theta <= 0.0 ) {
			Theta+= 2*M_PI;
		}
	}
	
	ct = cos(Theta);
	st = sin(Theta);
		
	Eps = 718.5*sqrt(Pdyn)*Bt*sin(Theta/2.0);
	
	FactEps = Eps/Eps10-1.0;
	FactPd = sqrt(Pdyn/Pdyn0)-1.0;
	
	RCampl = -A[0]*Depr;
	Tampl2 = A[1] + A[2]*FactPd + A[3]*FactEps;
	Tampl3 = A[4] + A[5]*FactPd;
	B1ampl = A[6] + A[7]*FactEps;
	B2ampl = 20.0*B1ampl;
	
	Reconn = A[8];
	
	Xappa = pow((Pdyn/Pdyn0),0.14);
	Xappa3 = pow(Xappa,3.0);
	ys = y*ct - z*st;
	zs = z*ct + y*st;
	FactIMF = exp(x/DelIMFx - pow((ys/DelIMFy),2.0));
	
	OIMFx = 0.0;
	OIMFy = Reconn*ByIMF*FactIMF;
	OIMFz = Reconn*BzIMF*FactIMF;
	
	RIMFampl = Reconn*Bt;
	
	xx = x*Xappa;
	yy = y*Xappa;
	zz = z*Xappa;
	pps = Ps;
	
	X0 = X00/Xappa;
	Am = Am0/Xappa;
	Rho2 = y*y + z*z;
	Asq = Am*Am;
	xmxm = Am + x - X0;
	if (xmxm < 0.0) {
		xmxm = 0.0;
	}
	Axx0 = xmxm * xmxm;
	Aro = Asq + Rho2;
	Sigma = sqrt((Aro + Axx0 + sqrt(pow((Aro + Axx0),2.0) - 4.0*Asq*Axx0))/(2.0*Asq));
	if (Sigma < (S0+Dsig)) {
		DipShld(pps,xx,yy,zz,&CFx,&CFy,&CFz);
		TailRC96(sps,xx,yy,zz,&BxRC,&ByRC,&BzRC,&BxT2,&ByT2,&BzT2,&BxT3,&ByT3,&BzT3);
		Birk1Tot(pps,xx,yy,zz,&R1x,&R1y,&R1z);
		Birk2Tot(pps,xx,yy,zz,&R2x,&R2y,&R2z);
		Intercon(xx,ys*Xappa,zs*Xappa,&RIMFx,&RIMFyS,&RIMFzS);
		RIMFy = RIMFyS*ct + RIMFzS*st;
		RIMFz = RIMFzS*ct - RIMFyS*st;
		
		Fx = CFx*Xappa3 + RCampl*BxRC + Tampl2*BxT2 + Tampl3*BxT3 + B1ampl*R1x + B2ampl*R2x + RIMFampl*RIMFx;
		Fy = CFy*Xappa3 + RCampl*ByRC + Tampl2*ByT2 + Tampl3*ByT3 + B1ampl*R1y + B2ampl*R2y + RIMFampl*RIMFy;
		Fz = CFz*Xappa3 + RCampl*BzRC + Tampl2*BzT2 + Tampl3*BzT3 + B1ampl*R1z + B2ampl*R2z + RIMFampl*RIMFz;
/*		printf("C components\n");
		printf("CF: %10.5f %10.5f %10.5f\n",CFx,CFy,CFz);
		printf("RC: %10.5f %10.5f %10.5f\n",BxRC,ByRC,BzRC);
		printf("T2: %10.5f %10.5f %10.5f\n",BxT2,ByT2,BzT2);
		printf("T3: %10.5f %10.5f %10.5f\n",BxT3,ByT3,BzT3);
		printf("R1: %10.5f %10.5f %10.5f\n",R1x,R1y,R1z);//has error
		printf("R2: %10.5f %10.5f %10.5f\n",R2x,R2y,R2z);//fixed
		printf("IMF: %10.5f %10.5f %10.5f\n",RIMFx,RIMFyS,RIMFzS); 
		printf("F: %10.5f %10.5f %10.5f\n",Fx,Fy,Fz);
		
		printf("Xappa:     %10.5f\n",Xappa);
		printf("RCampl:    %10.5f\n",RCampl);
		printf("Tampl2:    %10.5f\n",Tampl2);
		printf("Tampl3:    %10.5f\n",Tampl3);
		printf("B1ampl:    %10.5f\n",B1ampl);
		printf("B2ampl:    %10.5f\n",B2ampl);
		printf("RIMFampl:  %10.5f\n",RIMFampl);*/
		if (Sigma < (S0-Dsig)) {
			*Bx = Fx;
			*By = Fy;
			*Bz = Fz;
		} else { 
			Fint = 0.5*(1.0 - (Sigma-S0)/Dsig);
			Fext = 0.5*(1.0 + (Sigma-S0)/Dsig);
			
			Dipole(Ps,x,y,z,&Qx,&Qy,&Qz);
			*Bx = (Fx + Qx)*Fint + OIMFx*Fext - Qx;
			*By = (Fy + Qy)*Fint + OIMFy*Fext - Qy;
			*Bz = (Fz + Qz)*Fint + OIMFz*Fext - Qz;
		} 
	} else {
		Dipole(Ps,x,y,z,&Qx,&Qy,&Qz);
		*Bx = OIMFx-Qx;
		*By = OIMFy-Qy;
		*Bz = OIMFz-Qz;
	}
	
	
}

void DipShld(double Ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double A1[] = {0.24777,-27.003,-0.46815,7.0637,-1.5918,-0.90317E-01,57.522,13.757,2.0100,10.458,4.5798,2.1695};
	double A2[] = {-0.65385,-18.061,-0.40457,-5.0995,1.2846,0.78231E-01,39.592,13.291,1.9970,10.062,4.5140,2.1558};
	double cps, sps, Hx, Hy, Hz, Fx, Fy, Fz;
	cps = cos(Ps);
	sps = sin(Ps);
	
	CylHarm(A1,x,y,z,&Hx,&Hy,&Hz);
	CylHar1(A2,x,y,z,&Fx,&Fy,&Fz);
	
	*Bx = Hx*cps + Fx*sps;
	*By = Hy*cps + Fy*sps;
	*Bz = Hz*cps + Fz*sps;
}

void CylHarm(double *A, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double Rho, SinFi, CosFi, SinFi2, Si2Co2, Dzeta, Xj0, Xj1, Xexp, Xksi, Brho, Bphi;
	int i;
	Rho = sqrt(y*y + z*z);
	if (Rho < 1e-8) {
		SinFi = 1.0;
		CosFi = 0.0;
		Rho = 1.0e-8;
	} else { 
		SinFi = z/Rho;
		CosFi = y/Rho;
	}
	SinFi2 = SinFi*SinFi;
	Si2Co2 = SinFi2 - (CosFi*CosFi);
		
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
		
	for (i=0;i<3;i++) {
		Dzeta = Rho/A[i+6];
		Xj0 = Bes(Dzeta,0);
		Xj1 = Bes(Dzeta,1);
		Xexp = exp(x/A[i+6]);
		*Bx = *Bx - A[i]*Xj1*Xexp*SinFi;
		*By = *By + A[i]*(2.0*Xj1/Dzeta - Xj0)*Xexp*SinFi*CosFi;
		*Bz = *Bz + A[i]*(Xj1/Dzeta*Si2Co2 - Xj0*SinFi2)*Xexp;
	}	
	
	for (i=3;i<6;i++) {
		Dzeta = Rho/A[i+6];
		Xksi = x/A[i+6];
		Xj0 = Bes(Dzeta,0);
		Xj1 = Bes(Dzeta,1);
		Xexp = exp(Xksi);
		Brho = (Xksi*Xj0 - (Dzeta*Dzeta + Xksi - 1.0)*Xj1/Dzeta)*Xexp*SinFi; 
		Bphi = (Xj0 + Xj1/Dzeta * (Xksi-1.0))*Xexp*CosFi;
		*Bx = *Bx + A[i]*(Dzeta*Xj0 + Xksi*Xj1)*Xexp*SinFi;
		*By = *By + A[i]*(Brho*CosFi - Bphi*SinFi);
		*Bz = *Bz + A[i]*(Brho*SinFi + Bphi*CosFi);
	}
}

void CylHar1(double *A, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double Rho, SinFi, CosFi, SinFi2, Si2Co2, Dzeta, Xj0, Xj1, Xexp, Xksi, Brho, Bphi;
	int i;
	Rho = sqrt(y*y + z*z);
	if (Rho < 1e-8) {
		SinFi = 1.0;
		CosFi = 0.0;
		Rho = 1.0e-8;
	} else { 
		SinFi = z/Rho;
		CosFi = y/Rho;
	}
		
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
		
	for (i=0;i<3;i++) {
		Dzeta = Rho/A[i+6];
		Xksi = x/A[i+6];
		Xj0 = Bes(Dzeta,0);
		Xj1 = Bes(Dzeta,1);
		Xexp = exp(Xksi);
		Brho = Xj1*Xexp;
		*Bx = *Bx - A[i]*Xj0*Xexp;
		*By = *By + A[i]*Brho*CosFi;
		*Bz = *Bz + A[i]*Brho*SinFi;
	}	
	
	for (i=3;i<6;i++) {
		Dzeta = Rho/A[i+6];
		Xksi = x/A[i+6];
		Xj0 = Bes(Dzeta,0);
		Xj1 = Bes(Dzeta,1);
		Xexp = exp(Xksi);
		Brho = (Dzeta*Xj0 + Xksi*Xj1)*Xexp; 
		*Bx = *Bx + A[i]*(Dzeta*Xj1 - Xj0*(Xksi+1.0))*Xexp;
		*By = *By + A[i]*Brho*CosFi;
		*Bz = *Bz + A[i]*Brho*SinFi;
	}	
}

double Bes(double x, int k) {
	if (k == 0) { 
		return Bes0(x);
	}
	if (k == 1) {
		return Bes1(x);
	}	
	
	if (x == 0.0) {
		return 0.0;
	}
	
	double G, Xjn, Xjnm1, Xjnp1, Sum, bes; 
	int N;
	
	G = 2.0/x;
	if (x > k) {
		N = 1;
		Xjn = Bes1(x);
		Xjnm1 = Bes0(x);
		
		while (1) {
			Xjnp1 = G*N*Xjn-Xjnm1;
			N++;
			if (N >= k) {
				return Xjnp1;
			}
			Xjnm1 = Xjn;
			Xjn = Xjnp1;
		}
	} else {
		N = 24;
		Xjn = 1.0;
		Xjnp1 = 0.0l;
		Sum = 0.0;
		
		while (1) {
			if ((N % 2) == 0) {
				Sum+=Xjn;
			}
			
			Xjnm1 = G*N*Xjn-Xjnp1;
			N--;
			
			Xjnp1 = Xjn;
			Xjn = Xjnm1;
			
			if (N == k) {
				bes = Xjn;
			}
			if (fabs(Xjn) > 1.0e5) {
				Xjnp1 = Xjnp1*1.0e-5;
				Xjn = Xjn*1.0e-5;
				Sum = Sum*1.0e-5;
				if (N <= k){
					bes = bes*1.0e-5;
				}
			}
			if (N == 0) {
				break;
			}
		}
		Sum = Xjn+2.0*Sum;
		bes = bes/Sum;
		return bes;		
	}
		
}

double Bes0(double x) {
	if (fabs(x) < 3.0) {
		double X32 = pow((x/3.0),2.0);
		return 1.0-X32*(2.2499997-X32*(1.26562080-X32*(0.31638660-X32*(0.04444790-X32*(0.00394440-X32*0.000210)))));
	} else {
		double XD3, F0, T0;
		XD3 = 3.0/x;
		F0=0.797884560-XD3*(0.000000770+XD3*(0.005527400+XD3*(0.000095120-XD3*(0.001372370-XD3*(0.000728050-XD3*0.000144760)))));
        T0=x-0.785398160-XD3*(0.041663970+XD3*(0.000039540-XD3*(0.002625730-XD3*(0.000541250+XD3*(0.000293330-XD3*0.000135580)))));
        return F0/sqrt(x)*cos(T0);		
	}
}

double Bes1(double x) {
	if (fabs(x) < 3.0) {
		double X32 = pow((x/3.0),2.0), BES1XM1;
		BES1XM1=0.50-X32*(0.562499850-X32*(0.210935730-X32*(0.039542890-X32*(0.004433190-X32*(0.000317610-X32*0.000011090)))));
		return BES1XM1 * x;
	} else {
		double XD3, F1, T1;
		XD3 = 3.0/x;
        F1=0.797884560+XD3*(0.000001560+XD3*(0.016596670+XD3*(0.000171050-XD3*(0.002495110-XD3*(0.001136530-XD3*0.000200330)))));
        T1=x-2.356194490+XD3*(0.124996120+XD3*(0.00005650-XD3*(0.006378790-XD3*(0.00074348+XD3*(0.00079824-XD3*0.000291660)))));
        return F1/sqrt(x)*cos(T1);		
	}	
}

void Intercon(double x, double y, double z, double *Bx, double *By, double *Bz) {
	double A[] = {-8.411078731,5932254.951,-9073284.93,-11.68794634,6027598.824,-9218378.368,-6.508798398,-11824.42793,18015.66212,7.99754043,13.9669886,90.24475036,16.75728834,1015.645781,1553.493216};
	double rp[3], rr[3], p[3], r[3], cypi, sypi, szrk,czrk,sqpr, epr,Hx, Hy, Hz;
	int i, j, l;
	p[0] = A[9];
	p[1] = A[10];
	p[2] = A[11];
	r[0] = A[12];
	r[1] = A[13];
	r[2] = A[14];
	
	for (i=0;i<3;i++) {
		rp[i] = 1.0/p[i];
		rr[i] = 1.0/r[i];
		
	}
	
	l = 0;
	
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
	
	for (i=0;i<3;i++) {
		cypi = cos(y*rp[i]);
		sypi = sin(y*rp[i]);
		for (j=0;j<3;j++) {
			szrk = sin(z*rr[j]);
			czrk = cos(z*rr[j]);
			sqpr = sqrt(rp[i]*rp[i] + rr[j]*rr[j]);
			epr = exp(x*sqpr);
			Hx = -sqpr*epr*cypi*szrk;
			Hy = rp[i]*epr*sypi*szrk;
			Hz = -rr[j]*epr*cypi*czrk;
			
			*Bx = *Bx + A[l]*Hx;
			*By = *By + A[l]*Hy;
			*Bz = *Bz + A[l]*Hz;
			l++;
		}
	}
}

void TailRC96(double SPS, double x, double y, double z, double *BxRC, double *ByRC, double *BzRC, double *BxT2, double *ByT2, double *BzT2, double *BxT3, double *ByT3, double *BzT3) {
	double ARC[] = {-3.087699646,3.516259114,18.81380577,-13.95772338,-5.497076303,0.1712890838,2.392629189,-2.728020808,-14.79349936,
					11.08738083,4.388174084,0.2492163197E-01,0.7030375685,-0.7966023165,-3.835041334,2.642228681,-0.2405352424,-0.7297705678,
					-0.3680255045,0.1333685557,2.795140897,-1.078379954,0.8014028630,0.1245825565,0.6149982835,-0.2207267314,-4.424578723,
					1.730471572,-1.716313926,-0.2306302941,-0.2450342688,0.8617173961E-01,1.54697858,-0.6569391113,-0.6537525353,0.2079417515,
					12.75434981,11.37659788,636.4346279,1.752483754,3.604231143,12.83078674,7.412066636,9.434625736,676.7557193,1.701162737,
					3.580307144,14.64298662};
	double ATail2[] = {0.8747515218,-.9116821411,2.209365387,-2.159059518,-7.059828867,5.924671028,-1.916935691,1.996707344,-3.877101873,
					3.947666061,11.38715899,-8.343210833,1.194109867,-1.244316975,3.73895491,-4.406522465,-20.66884863,3.020952989,
					0.2189908481,-0.09942543549,-0.927225562,0.1555224669,0.6994137909,-0.08111721003,-0.7565493881,0.4686588792,4.266058082,
					-0.3717470262,-3.920787807,0.02298569870,0.7039506341,-0.5498352719,-6.675140817,0.8279283559,-2.234773608,-1.622656137,
					5.187666221,6.802472048,39.13543412,2.784722096,6.979576616,25.71716760,4.495005873,8.068408272,93.47887103,4.158030104,
					9.313492566,57.18240483};
	double ATail3[] = {-19091.95061,-3011.613928,20582.16203,4242.918430,-2377.091102,-1504.820043,19884.04650,2725.150544,-21389.04845,
					-3990.475093,2401.610097,1548.171792,-946.5493963,490.1528941,986.9156625,-489.3265930,-67.99278499,8.711175710,
					-45.15734260,-10.76106500,210.7927312,11.41764141,-178.0262808,0.7558830028,339.3806753,9.904695974,69.50583193,
					-118.0271581,22.85935896,45.91014857,-425.6607164,15.47250738,118.2988915,65.58594397,-201.4478068,-14.57062940,
					19.69877970,20.30095680,86.45407420,22.50403727,23.41617329,48.48140573,24.61031329,123.5395974,223.5367692,39.50824342,
					65.83385762,266.2948657};
	double Rh, Dr, G, D0, Deltady, Dr2, C11, C12, C1, SPSC1, R, Sq1, Sq2,C, Cs, Wfac, W, Ws, Dddy;
	Rh = 9.0;
	Dr = 4.0;
	G = 10.0;
	D0 = 2.0;
	Deltady = 10.0;
	
	Dr2 = Dr*Dr;
	C11 = sqrt(pow(1.0+Rh,2.0) + Dr2);
	C12 = sqrt(pow(1.0-Rh,2.0) + Dr2);
	C1 = C11 - C12;
	SPSC1 = SPS/C1;
	warp.rps = 0.5*(C11+C12)*SPS;
	
	R = sqrt(x*x + y*y + z*z);
	Sq1 = sqrt(pow(R + Rh,2.0) + Dr2);
	Sq2 = sqrt(pow(R - Rh,2.0) + Dr2);
	C = Sq1 - Sq2;
	Cs = (R+Rh)/Sq1 - (R-Rh)/Sq2;
	warp.spss = SPSC1/R*C;
	warp.cpss = sqrt(1.0- pow(warp.spss,2.0));
	warp.dpsrr = SPS/(R*R)*(Cs*R-C)/sqrt(pow(R*C1,2.0) - pow(C*SPS,2.0));
	
	Wfac = y/(pow(y,4.0)+1.0e4);
	W = Wfac*pow(y,3.0);
	Ws = 4.0e4*y*pow(Wfac,2.0);
	warp.warp = G*SPS*W;
	warp.xs = x*warp.cpss - z*warp.spss;
	warp.zsww = z*warp.cpss + x*warp.spss;
	warp.zs = warp.zsww + warp.warp;
	
	warp.dxsx = warp.cpss - x*warp.zsww*warp.dpsrr;
	warp.dxsy = -y*warp.zsww*warp.dpsrr;
	warp.dxsz = -warp.spss - z*warp.zsww*warp.dpsrr;
	warp.dzsx = warp.spss + x*warp.xs*warp.dpsrr;
	warp.dzsy = warp.xs*y*warp.dpsrr + G*SPS*Ws;
	warp.dzsz = warp.cpss + warp.xs*z*warp.dpsrr;
	
	warp.d = D0 + Deltady*pow(y/20.0,2.0);
	Dddy = Deltady*y*0.005;
	
	warp.dzetas = sqrt(warp.zs*warp.zs + warp.d*warp.d);
	
	warp.ddzetadx = warp.zs * warp.dzsx/warp.dzetas;
	warp.ddzetady = (warp.zs*warp.dzsy + warp.d*Dddy)/warp.dzetas;
	warp.ddzetadz = warp.zs*warp.dzsz/warp.dzetas;


	double Wx, Wy, Wz, Hx, Hy, Hz;
	
	
	ShlCar3x3(ARC,x,y,z,SPS,&Wx,&Wy, &Wz);
	RingCurr96(x,y,z,&Hx,&Hy,&Hz);
	*BxRC = Wx + Hx;
	*ByRC = Wy + Hy;
	*BzRC = Wz + Hz;
	
	ShlCar3x3(ATail2,x,y,z,SPS,&Wx,&Wy, &Wz);
	TailDisk(x,y,z,&Hx,&Hy,&Hz);
	*BxT2 = Wx + Hx;
	*ByT2 = Wy + Hy;
	*BzT2 = Wz + Hz;	
	
	ShlCar3x3(ATail3,x,y,z,SPS,&Wx,&Wy, &Wz);
	Tail87(x,z,&Hx,&Hz);
	*BxT3 = Wx + Hx;
	*ByT3 = Wy;
	*BzT3 = Wz + Hz;	
		
	return;
}

void RingCurr96(double x, double y, double z, double *Bx, double *By, double *Bz) {
	double F[] = {569.8953660,-1603.3869930};
	double Beta[] = {2.7221880,3.7668750};
	double D0, Deltadx, Xd, Xldx, dzsy, Xxd, Fdx, dddx, D, dzetas, Rhos;
	double ddzetadx, ddzetady, ddzetadz, drhosdx, drhosdy, drhosdz;
	double S1, S2, Bi, dS1ddz, dS2ddz, dS1drhos, dS2drhos, dS1dx, dS1dy, dS1dz, dS2dx, dS2dy, dS2dz; 
	double S1tS2, S1pS2, S1pS2sq, Fac1, Fac2, As, Term1, dAsdS1, dAsdS2, dAsdx, dAsdy, dAsdz;
	int i;
	D0 = 2.0;
	Deltadx = 0.0;
	Xd = 0.0;
	Xldx = 4.0;
	
	dzsy = warp.xs*y*warp.dpsrr;
	Xxd = x-Xd;
	Fdx = 0.5*(1.0+Xxd/sqrt(Xxd*Xxd + Xldx*Xldx));
	dddx = Deltadx*0.5*Xldx*Xldx/pow(sqrt(Xxd*Xxd + Xldx*Xldx),3.0);
	D = D0 + Deltadx*Fdx;
	dzetas = sqrt(pow(warp.zsww,2.0) + D*D);
	Rhos = sqrt(warp.xs*warp.xs + y*y);
	
	ddzetadx = (warp.zsww*warp.dzsx + D*dddx)/dzetas;
	ddzetady = warp.zsww*dzsy/dzetas;
	ddzetadz = warp.zsww*warp.dzsz/dzetas;
	
	if (Rhos < 1.0e-5) {
		drhosdx = 0.0;
		drhosdy = y/fabs(y);
		drhosdz = 0.0;
	} else {
		drhosdx = warp.xs*warp.dxsx/Rhos;
		drhosdy = (warp.xs*warp.dxsy + y)/Rhos;
		drhosdz = warp.xs*warp.dxsz/Rhos;
	}
	
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
	
	for (i=0;i<2;i++) {
		Bi = Beta[i];
		S1 = sqrt(pow(dzetas+Bi,2.0) + pow(Rhos+Bi,2.0));
		S2 = sqrt(pow(dzetas+Bi,2.0) + pow(Rhos-Bi,2.0));
		dS1ddz = (dzetas+Bi)/S1;
		dS2ddz = (dzetas+Bi)/S2;
		dS1drhos = (Rhos+Bi)/S1;
		dS2drhos = (Rhos-Bi)/S2;
		
		
		dS1dx = dS1ddz*ddzetadx + dS1drhos*drhosdx;
		dS1dy = dS1ddz*ddzetady + dS1drhos*drhosdy;
		dS1dz = dS1ddz*ddzetadz + dS1drhos*drhosdz;

		dS2dx = dS2ddz*ddzetadx + dS2drhos*drhosdx;
		dS2dy = dS2ddz*ddzetady + dS2drhos*drhosdy;
		dS2dz = dS2ddz*ddzetadz + dS2drhos*drhosdz;
		
	
		S1tS2 = S1*S2;
		S1pS2 = S1+S2;
		S1pS2sq = S1pS2*S1pS2;
		Fac1 = sqrt(S1pS2sq - pow(2.0*Bi,2.0));
		As = Fac1/(S1tS2*S1pS2sq);
		Term1 = 1.0/(S1tS2*S1pS2*Fac1);
		Fac2 = As/S1pS2sq;
		
		dAsdS1 = Term1-Fac2/S1*(S2*S2+S1*(3.0*S1+4.0*S2));
		dAsdS2 = Term1-Fac2/S2*(S1*S1+S2*(3.0*S2+4.0*S1));
		
		dAsdx = dAsdS1*dS1dx + dAsdS2*dS2dx;
		dAsdy = dAsdS1*dS1dy + dAsdS2*dS2dy;
		dAsdz = dAsdS1*dS1dz + dAsdS2*dS2dz;
		
		*Bx = *Bx + F[i]*((2.0*As + y*dAsdy)*warp.spss - warp.xs*dAsdz + As*warp.dpsrr*(y*y*warp.cpss + z*warp.zsww));
		*By = *By - F[i]*y*(As*warp.dpsrr*warp.xs + dAsdz*warp.cpss + dAsdx*warp.spss);
		*Bz = *Bz + F[i]*((2.0*As + y*dAsdy)*warp.cpss + warp.xs*dAsdx - As*warp.dpsrr*(x*warp.zsww + y*y*warp.spss));

	}
	return;
}

void TailDisk(double x, double y, double z, double *Bx, double *By, double *Bz) {
	double F[] = {-745796.73380,1176470.1410,-444610.5290,-57508.010280};
	double Beta[] = {7.92500000,8.08500000,8.47125000,27.895000};
	double Xshift, Rhos, drhosdx, drhosdy, drhosdz;
	double ddzetadx, ddzetady, ddzetadz;
	double S1, S2, Bi, dS1ddz, dS2ddz, dS1drhos, dS2drhos, dS1dx, dS1dy, dS1dz, dS2dx, dS2dy, dS2dz; 
	double S1tS2, S1pS2, S1pS2sq, Fac1, Fac2, As, Term1, dAsdS1, dAsdS2, dAsdx, dAsdy, dAsdz;
	int i;
	
	Xshift = 4.5;
	Rhos = sqrt(pow((warp.xs-Xshift),2.0) + y*y);
	if (Rhos < 1.0e-5) {
		drhosdx = 0.0;
		drhosdy = y/fabs(y);
		drhosdz = 0.0;
	} else {
		drhosdx = (warp.xs-Xshift)*warp.dxsx/Rhos;
		drhosdy = ((warp.xs-Xshift)*warp.dxsy + y)/Rhos;
		drhosdz = (warp.xs-Xshift)*warp.dxsz/Rhos;
	}
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
	
	for (i=0;i<4;i++) {
		Bi = Beta[i];
		S1 = sqrt(pow(warp.dzetas+Bi,2.0) + pow(Rhos+Bi,2.0));
		S2 = sqrt(pow(warp.dzetas+Bi,2.0) + pow(Rhos-Bi,2.0));
		dS1ddz = (warp.dzetas+Bi)/S1;
		dS2ddz = (warp.dzetas+Bi)/S2;
		dS1drhos = (Rhos+Bi)/S1;
		dS2drhos = (Rhos-Bi)/S2;
		
		dS1dx = dS1ddz*warp.ddzetadx + dS1drhos*drhosdx;
		dS1dy = dS1ddz*warp.ddzetady + dS1drhos*drhosdy;
		dS1dz = dS1ddz*warp.ddzetadz + dS1drhos*drhosdz;

		dS2dx = dS2ddz*warp.ddzetadx + dS2drhos*drhosdx;
		dS2dy = dS2ddz*warp.ddzetady + dS2drhos*drhosdy;
		dS2dz = dS2ddz*warp.ddzetadz + dS2drhos*drhosdz;
		
		S1tS2 = S1*S2;
		S1pS2 = S1+S2;
		S1pS2sq = S1pS2*S1pS2;
		Fac1 = sqrt(S1pS2sq - pow(2.0*Bi,2.0));
		As = Fac1/(S1tS2*S1pS2sq);
		Term1 = 1.0/(S1tS2*S1pS2*Fac1);
		Fac2 = As/S1pS2sq;
		
		dAsdS1 = Term1-Fac2/S1*(S2*S2+S1*(3.0*S1+4.0*S2));
		dAsdS2 = Term1-Fac2/S2*(S1*S1+S2*(3.0*S2+4.0*S1));
		
		dAsdx = dAsdS1*dS1dx + dAsdS2*dS2dx;
		dAsdy = dAsdS1*dS1dy + dAsdS2*dS2dy;
		dAsdz = dAsdS1*dS1dz + dAsdS2*dS2dz;
		
		*Bx = *Bx + F[i]*((2.0*As + y*dAsdy)*warp.spss - (warp.xs-Xshift)*dAsdz + As*warp.dpsrr*(y*y*warp.cpss + z*warp.zsww));
		*By = *By - F[i]*y*(As*warp.dpsrr*warp.xs + dAsdz*warp.cpss + dAsdx*warp.spss);
		*Bz = *Bz + F[i]*((2.0*As + y*dAsdy)*warp.cpss + (warp.xs-Xshift)*dAsdx - As*warp.dpsrr*(x*warp.zsww + y*y*warp.spss));

	}
	return;
}

void Tail87(double x, double z, double *Bx, double *Bz) {
	double dd, hpi, rt, xn, x1, x2, b0, b1, b2, xn21, xnr, adln;
	dd = 3.0;
	hpi =  M_PI/2.0;
	rt = 40.0;
	xn = -10.0;
	x1 = -1.261;
	x2 = -0.663;
	b0 = 0.391734;
	b1 = 5.89715;
	b2 = 24.6833;
	xn21 = 76.37;
	xnr = -0.1071;
	adln = 0.13238005;
	
	double zs, zp, zm, xnx, xnx2, xc1, xc2, xc22, d2, xr2, xc12, b20, b2p, b2m, b, bp, bm;
	double xa1, xap1, xam1, xa2, xap2, xam2, f, fp, fm, xln1, xlnp1, xlnm1, xln2, xlnp2, xlnm2;
	double aln, s0, s0p, s0m, s1, s1p, s1m, s2, s2p, s2m, g1, g1p, g1m, g2, g2p, g2m, xna, xnap, xnam;
	zs = z-warp.rps+warp.warp;
	zp = z-rt;
	zm = z+rt;
	
	xnx = xn - x;
	xnx2 = xnx * xnx;
	xc1 = x - x1;
	xc2 = x - x2;
	xc22 = xc2 * xc2;
	xr2 = xc2 * xnr;
	xc12 = xc1 * xc1;
	d2 = dd*dd;
	b20 = zs*zs + d2;
	b2p = zp*zp + d2;
	b2m = zm*zm + d2;
	b = sqrt(b20);
	bp = sqrt(b2p);
	bm = sqrt(b2m);
	xa1 = xc12+b20;
	xap1 = xc12+b2p;
	xam1 = xc12+b2m;
	xa2 = 1.0f/(xc22+b20);
	xam2 = 1.0f/(xc22+b2m);
	xap2 = 1.0f/(xc22+b2p);
	xna =xnx2+b20;
	xnap =xnx2+b2p;
	xnam =xnx2+b2m;
	f = b20 - xc22;
	fp = b2p - xc22;
	fm = b2m - xc22;
	xln1 = log(xn21/xna);
	xlnp1 = log(xn21/xnap);
	xlnm1 = log(xn21/xnam);
	xln2 = xln1 + adln;
	xlnp2 = xlnp1 + adln;
	xlnm2 = xlnm1 + adln;
	aln = 0.25*(xlnp1 + xlnm1 - 2.0*xln1);
	s0 = (atan(xnx/b) + hpi)/b;
	s0p = (atan(xnx/bp) + hpi)/bp;
	s0m = (atan(xnx/bm) + hpi)/bm;
	s1 = (xln1*0.5 + xc1*s0)/xa1;
	s1p = (xlnp1*0.5 + xc1*s0p)/xap1;
	s1m = (xlnm1*0.5 + xc1*s0m)/xam1;
	s2 = (xc2*xa2*xln2 - xnr - f*xa2*s0)*xa2;
	s2p = (xc2*xap2*xlnp2 - xnr - fp*xap2*s0p)*xap2;
	s2m = (xc2*xam2*xlnm2 - xnr - fm*xam2*s0m)*xam2;
	g1 = (b20*s0 - 0.5*xc1*xln1)/xa1;
	g1p = (b2p*s0p - 0.5*xc1*xlnp1)/xap1;
	g1m = (b2m*s0m - 0.5*xc1*xlnm1)/xam1;
	g2 = ((0.5*f*xln2 + 2.0*s0*b20*xc2)*xa2 + xr2)*xa2;
	g2p = ((0.5*fp*xlnp2 + 2.0*s0p*b2p*xc2)*xap2 + xr2)*xap2;
	g2m = ((0.5*fm*xlnm2 + 2.0*s0m*b2m*xc2)*xam2 + xr2)*xam2;
	*Bx = b0*(zs*s0 - 0.5*(zp*s0p + zm*s0m)) + b1*(zs*s1 - 0.5*(zp*s1p + zm*s1m)) + b2*(zs*s2-0.5*(zp*s2p + zm*s2m));
	*Bz = b0*aln + b1*(g1 - 0.5*(g1p+g1m)) + b2*(g2-0.5*(g2p+g2m));
}

void ShlCar3x3(double *A, double x, double y, double z, double SPS, double *Bx, double *By, double *Bz) {
	double CPS, S3PS, p, q, cypi, cyqi, sypi, syqi, r, s, szrk, czsk, czrk, szsk, sqpr, sqqs, epr, eqs;
	double Dx, Dy, Dz;
	CPS = sqrt(1.0 - SPS*SPS);
	S3PS = 4.0*CPS*CPS - 1.0;
	int l, m, k, i, n;
	
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
	l = 0;
	
	for (m=0;m<2;m++) {
		for (i=0;i<3;i++) {
			p = A[36+i];
			q = A[42+i];
			cypi = cos(y/p);
			sypi = sin(y/p);
			cyqi = cos(y/q);
			syqi = sin(y/q);
			for (k=0;k<3;k++) {
				r = A[39+k];
				s = A[45+k];
				szrk = sin(z/r);
				czsk = cos(z/s);
				czrk = cos(z/r);
				szsk = sin(z/s);
				sqpr = sqrt(1.0/pow(p,2.0) + 1.0/pow(r,2.0));
				sqqs = sqrt(1.0/pow(q,2.0) + 1.0/pow(s,2.0));
				epr = exp(x*sqpr);
				eqs = exp(x*sqqs);
				for (n=0;n<2;n++) {
					if (m == 0) {
						if (n == 0) {
							Dx = -sqpr*epr*cypi*szrk;
							Dy = epr/p*sypi*szrk;
							Dz = -epr/r*cypi*czrk;
						} else {
							Dx = Dx*CPS;
							Dy = Dy*CPS;
							Dz = Dz*CPS;
						}
					} else {
						if (n == 0) {
							Dx = -SPS*sqqs*eqs*czsk*cyqi;
							Dy = SPS*eqs/q*syqi*czsk;
							Dz = SPS*eqs/s*cyqi*szsk;
						} else {
							Dx = Dx*S3PS;
							Dy = Dy*S3PS;
							Dz = Dz*S3PS;
						}
					}
					*Bx = *Bx + A[l]*Dx;
					*By = *By + A[l]*Dy;
					*Bz = *Bz + A[l]*Dz;
					l++;
				}
			}
		}
	}
}

void Birk1Tot(double Ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double C1[] = {	-0.911582E-03,-0.376654E-02,-0.727423E-02,-0.270084E-02,
					-0.123899E-02,-0.154387E-02,-0.340040E-02,-0.191858E-01,
					-0.518979E-01,0.635061E-01,0.440680,-0.396570,0.561238E-02,
					0.160938E-02,-0.451229E-02,-0.251810E-02,-0.151599E-02,
					-0.133665E-02,-0.962089E-03,-0.272085E-01,-0.524319E-01,
					0.717024E-01,0.523439,-0.405015,-89.5587,23.2806 };
	double C2[] = {	6.04133,0.305415,0.606066E-02,0.128379E-03,-0.179406E-04,
					1.41714,-27.2586,-4.28833,-1.30675,35.5607,8.95792,0.961617E-03,
					-0.801477E-03,-0.782795E-03,-1.65242,-16.5242,-5.33798,0.424878E-03,
					0.331787E-03,-0.704305E-03,0.844342E-03,0.953682E-04,0.886271E-03,
					25.1120,20.9299,5.14569,-44.1670,-51.0672,-1.87725,20.2998,
					48.7505,-2.97415,3.35184,-54.2921,-0.838712,-10.5123,70.7594,
					-4.94104,0.106166E-03,0.465791E-03,-0.193719E-03,10.8439,-29.7968,
					8.08068,.463507E-03,-0.224475E-04,0.177035E-03,-0.317581E-03,
					-0.264487E-03,0.102075E-03,7.71390,10.1915,-4.99797,-23.1114,
					-29.2043,12.2928,10.9542,33.6671,-9.3851,0.174615E-03,-0.789777E-06,
					0.686047E-03,0.460104E-04,-0.345216E-02,0.221871E-02,0.110078E-01,
					-0.661373E-02,0.249201E-02,0.343978E-01,-0.193145E-05,0.493963E-05,
					-0.535748E-04,0.191833E-04,-0.100496E-03,-0.210103E-03,-0.232195E-02,
					0.315335E-02,-0.134320E-01,-0.263222E-01};
	
	double XltDay = 78.0, XltNght = 70.0;
	double Dtet0 = 0.034906;
	
	double TnoonN, TnoonS, DtetDN, Dr2, SPS, R2, R, R3, RmRh, RpRh, Sqm, Sqp, C, Q, Spsas, Cpsas, Xas, Zas, Pas,
			Tas, Stas, F, Tet0, Dtet, TetR1N, TetR1S, D1[3][26], D2[3][79], Xi[4], T01, T02, Sqr,
			St01as, St02as, Ct01as, Ct02as, Xas1, Y1, Zas1, X1, Z1, Bx1, By1, Bz1, Xas2, Y2, Zas2, X2, Z2, Bx2,
			By2, Bz2, ss, Ds, Frac, Bsx, Bsy, Bsz;
				
	int i, Loc;
	
	TnoonN = (90.0-XltDay)*M_PI/180.0;
	TnoonS = M_PI - TnoonN;
	DtetDN = (XltDay-XltNght)*M_PI/180.0;
	Dr2 = pow(rhdr.Dr,2.0);
	
	SPS = sin(Ps);
	R2 = x*x + y*y + z*z;
	R = sqrt(R2);
	R3 = R*R2;
	
	RmRh = R - rhdr.Rh;
	RpRh = R + rhdr.Rh;
	Sqm = sqrt(RmRh*RmRh + Dr2);
	Sqp = sqrt(RpRh*RpRh + Dr2);
	C = Sqp-Sqm;
	Q = sqrt(pow(rhdr.Rh+1.0,2.0)+Dr2) - sqrt(pow(rhdr.Rh-1.0,2.0)+Dr2);
	Spsas = SPS/R * C/Q;
	Cpsas = sqrt(1.0f - pow(Spsas,2.0));
	Xas = x*Cpsas - z*Spsas;
	Zas = x*Spsas + z*Cpsas;
	
	if (Xas != 0.0 || y !=0.0) {
		Pas = atan2(y,Xas);
	} else {
		Pas = 0.0;
	}
	
	Tas = atan2(sqrt(Xas*Xas + y*y),Zas);
	Stas = sin(Tas);
	F = Stas/pow((pow(Stas,6.0)*(1.0-R3)+R3),0.1666666667f);
	
	Tet0 = asin(F);
	if (Tas > M_PI/2.0) {
		Tet0 = M_PI-Tet0;
	}
	Dtet = DtetDN*pow(sin(Pas*0.5),2.0);
	TetR1N = TnoonN+Dtet;
	TetR1S = TnoonS-Dtet;
	
	if (Tet0 < (TetR1N-Dtet0) || Tet0 > (TetR1S+Dtet0)) {
		Loc = 1;
	}
	if (Tet0 > (TetR1N+Dtet0) && Tet0 < (TetR1S-Dtet0)) {
		Loc = 2;
	}
	if (Tet0 >= (TetR1N-Dtet0) && Tet0 <= (TetR1N+Dtet0)) {
		Loc = 3;
	}
	if (Tet0 >= (TetR1S-Dtet0) && Tet0 <= (TetR1S+Dtet0)) {
		Loc = 4;
	}

	if (Loc == 1) {
		Xi[0] = x;
		Xi[1] = y;
		Xi[2] = z;
		Xi[3] = Ps;
		DipLoop1(Xi,D1);
		*Bx = 0.0;
		*By = 0.0;
		*Bz = 0.0;
		for (i=0;i<26;i++) {
			*Bx = *Bx + C1[i]*D1[0][i];
			*By = *By + C1[i]*D1[1][i];
			*Bz = *Bz + C1[i]*D1[2][i];
		}
	}
	
	if (Loc == 2) {
		Xi[0] = x;
		Xi[1] = y;
		Xi[2] = z;
		Xi[3] = Ps;
		ConDip1(Xi,D2);
		*Bx = 0.0;
		*By = 0.0;
		*Bz = 0.0;
		for (i=0;i<79;i++) {
			*Bx = *Bx + C2[i]*D2[0][i];
			*By = *By + C2[i]*D2[1][i];
			*Bz = *Bz + C2[i]*D2[2][i];
		}
	}	
	
	if (Loc == 3) {
		T01 = TetR1N-Dtet0;
		T02 = TetR1N+Dtet0;
		Sqr = sqrt(R);
		St01as = Sqr/pow(R3 + 1.0/pow(sin(T01),6.0)-1.0,0.1666666667f);
		St02as = Sqr/pow(R3 + 1.0/pow(sin(T02),6.0)-1.0,0.1666666667f);
		Ct01as = sqrt(1.0 - St01as*St01as);
		Ct02as = sqrt(1.0 - St02as*St02as);
		Xas1 = R*St01as*cos(Pas);
		Y1 = R*St01as*sin(Pas);
		Zas1 = R*Ct01as;
		X1 = Xas1*Cpsas + Zas1*Spsas;
		Z1 = -Xas1*Spsas + Zas1*Cpsas;
		
		Xi[0] = X1;
		Xi[1] = Y1;
		Xi[2] = Z1;
		Xi[3] = Ps;
		DipLoop1(Xi,D1);
		Bx1 = 0.0;
		By1 = 0.0;
		Bz1 = 0.0;
		for (i=0;i<26;i++) {
			Bx1 = Bx1 + C1[i]*D1[0][i];
			By1 = By1 + C1[i]*D1[1][i];
			Bz1 = Bz1 + C1[i]*D1[2][i];
		}
		
		Xas2 = R*St02as*cos(Pas);
		Y2 = R*St02as*sin(Pas);
		Zas2 = R*Ct02as;
		X2 = Xas2*Cpsas + Zas2*Spsas;
		Z2 = -Xas2*Spsas + Zas2*Cpsas;
		
		Xi[0] = X2;
		Xi[1] = Y2;
		Xi[2] = Z2;
		Xi[3] = Ps;
		ConDip1(Xi,D2);
		Bx2 = 0.0;
		By2 = 0.0;
		Bz2 = 0.0;
		for (i=0;i<79;i++) {
			Bx2 = Bx2 + C2[i]*D2[0][i];
			By2 = By2 + C2[i]*D2[1][i];
			Bz2 = Bz2 + C2[i]*D2[2][i];
		}		
		
		ss = sqrt(pow(X2 - X1,2.0) + pow(Y2 - Y1,2.0) + pow(Z2 - Z1,2.0));
		Ds = sqrt(pow(x - X1,2.0) + pow(y - Y1,2.0) + pow(z - Z1,2.0));
		Frac = Ds/ss;
		*Bx = Bx1*(1.0-Frac) + Bx2*Frac;
		*By = By1*(1.0-Frac) + By2*Frac;
		*Bz = Bz1*(1.0-Frac) + Bz2*Frac;
	}
	
	if (Loc == 4) {
		T01 = TetR1N-Dtet0;
		T02 = TetR1N+Dtet0;
		Sqr = sqrt(R);
		St01as = Sqr/pow(R3 + 1.0/pow(sin(T01),6.0)-1.0,0.1666666667);
		St02as = Sqr/pow(R3 + 1.0/pow(sin(T02),6.0)-1.0,0.1666666667);
		Ct01as = -sqrt(1.0 - St01as*St01as);
		Ct02as = -sqrt(1.0 - St02as*St02as);
		Xas1 = R*St01as*cos(Pas);
		Y1 = R*St01as*sin(Pas);
		Zas1 = R*Ct01as;
		X1 = Xas1*Cpsas + Zas1*Spsas;
		Z1 = -Xas1*Spsas + Zas1*Cpsas;
		
		Xi[0] = X1;
		Xi[1] = Y1;
		Xi[2] = Z1;
		Xi[3] = Ps;
		ConDip1(Xi,D2);
		Bx1 = 0.0;
		By1 = 0.0;
		Bz1 = 0.0;
		for (i=0;i<79;i++) {
			Bx1 = Bx1 + C2[i]*D2[0][i];
			By1 = By1 + C2[i]*D2[1][i];
			Bz1 = Bz1 + C2[i]*D2[2][i];
		}
		
		Xas2 = R*St02as*cos(Pas);
		Y2 = R*St02as*sin(Pas);
		Zas2 = R*Ct02as;
		X2 = Xas2*Cpsas + Zas2*Spsas;
		Z2 = -Xas2*Spsas + Zas2*Cpsas;
		
		Xi[0] = X2;
		Xi[1] = Y2;
		Xi[2] = Z2;
		Xi[3] = Ps;
		DipLoop1(Xi,D1);
		Bx2 = 0.0;
		By2 = 0.0;
		Bz2 = 0.0;
		for (i=0;i<26;i++) {
			Bx2 = Bx2 + C1[i]*D1[0][i];
			By2 = By2 + C1[i]*D1[1][i];
			Bz2 = Bz2 + C1[i]*D1[2][i];
		}		
		
		ss = sqrt(pow(X2 - X1,2.0) + pow(Y2 - Y1,2.0) + pow(Z2 - Z1,2.0));
		Ds = sqrt(pow(z - X1,2.0) + pow(y - Y1,2.0) + pow(z - Z1,2.0));
		Frac = Ds/ss;
		*Bx = Bx1*(1.0-Frac) + Bx2*Frac;
		*By = By1*(1.0-Frac) + By2*Frac;
		*Bz = Bz1*(1.0-Frac) + Bz2*Frac;
	}	
	
	Birk1Shld(Ps,x,y,z,&Bsx,&Bsy,&Bsz);
	//printf("Birk1    : %lf %lf %lf\n",*Bx,*By,*Bz);
	//printf("Birk1Shld: %lf %lf %lf\n",Bsx,Bsy,Bsz);
	*Bx = *Bx + Bsx;
	*By = *By + Bsy;
	*Bz = *Bz + Bsz;
}

void DipLoop1(double *Xi, double D[3][26]) {

	double X, Y, Z, Ps, SPS, R, R2, RMRH, RPRH, DR2, SQM, SQP, C, Q, SPSAS, CPSAS;
	double XD, YD, ZD, BX1X, BY1X, BZ1X, BX1Y, BY1Y, BZ1Y, BX1Z, BY1Z, BZ1Z;
	double BX2X, BY2X, BZ2X, BX2Y, BY2Y, BZ2Y, BX2Z, BY2Z, BZ2Z;
	double XOCT1,YOCT1,ZOCT1,BXOCT1,BYOCT1,BZOCT1,XOCT2,YOCT2,ZOCT2,BX,BY,BZ;
	int i;
	X = Xi[0];
	Y = Xi[1];
	Z = Xi[2];
	Ps = Xi[3];
	SPS = sin(Ps);

	for (i=0;i<12;i++) {
		R2=pow(coord11.xx[i]*loopdip1.DipX,2.0)+pow(coord11.yy[i]*loopdip1.DipY,2.0);
		R=sqrt(R2);
		RMRH=R-rhdr.Rh;
		RPRH=R+rhdr.Rh;
		DR2=rhdr.Dr*rhdr.Dr;
		SQM=sqrt(RMRH*RMRH+DR2);
		SQP=sqrt(RPRH*RPRH+DR2);
		C=SQP-SQM;
		Q=sqrt(pow(rhdr.Rh+1.0,2.0)+DR2)-sqrt(pow(rhdr.Rh-1.0,2.0)+DR2);
		SPSAS=SPS/R*C/Q;
		CPSAS=sqrt(1.0-SPSAS*SPSAS);
		XD= (coord11.xx[i]*loopdip1.DipX)*CPSAS;
		YD= (coord11.yy[i]*loopdip1.DipY);
		ZD=-(coord11.xx[i]*loopdip1.DipX)*SPSAS;
		//printf("%d %f %f %f %f %f\n",i,XD,YD,ZD,coord11.xx[i],coord11.yy[i]);
		DipXYZ(X-XD,Y-YD,Z-ZD,&BX1X,&BY1X,&BZ1X,&BX1Y,&BY1Y,&BZ1Y,&BX1Z,&BY1Z,&BZ1Z);
        if (fabs(YD) > 1.0e-10) {
			DipXYZ(X-XD,Y+YD,Z-ZD,&BX2X,&BY2X,&BZ2X,&BX2Y,&BY2Y,&BZ2Y,&BX2Z,&BY2Z,&BZ2Z);
		} else {
			BX2X=0.0;
			BY2X=0.0;
			BZ2X=0.0;
	
			BX2Z=0.0;
			BY2Z=0.0;
			BZ2Z=0.0;
		}
		D[0][i]=BX1Z+BX2Z;
		D[1][i]=BY1Z+BY2Z;
		D[2][i]=BZ1Z+BZ2Z;
		D[0][i+12]=(BX1X+BX2X)*SPS;
		D[1][i+12]=(BY1X+BY2X)*SPS;
		D[2][i+12]=(BZ1X+BZ2X)*SPS;
	}
	R2=pow((loopdip1.XCentre[0]+loopdip1.Radius[0]),2.0);
	R=sqrt(R2);
	RMRH=R-rhdr.Rh;
	RPRH=R+rhdr.Rh;
	DR2=pow(rhdr.Dr,2);
	SQM=sqrt(RMRH*RMRH+DR2);
	SQP=sqrt(RPRH*RPRH+DR2);
	C=SQP-SQM;
	Q=sqrt(pow(rhdr.Rh+1.0,2.0)+DR2)-sqrt(pow(rhdr.Rh-1.0,2.0)+DR2);
	SPSAS=SPS/R*C/Q;
	CPSAS=sqrt(1.0-SPSAS*SPSAS);
	XOCT1= X*CPSAS-Z*SPSAS;
	YOCT1= Y;
	ZOCT1= X*SPSAS+Z*CPSAS;

	CrossLP(XOCT1,YOCT1,ZOCT1,&BXOCT1,&BYOCT1,&BZOCT1,loopdip1.XCentre[0],loopdip1.Radius[0],loopdip1.Tilt);
	D[0][24]=BXOCT1*CPSAS+BZOCT1*SPSAS;
	D[1][24]=BYOCT1;
	D[2][24]=-BXOCT1*SPSAS+BZOCT1*CPSAS;

	R2=pow(loopdip1.Radius[1]-loopdip1.XCentre[1],2.0);
	R=sqrt(R2);
	RMRH=R-rhdr.Rh;
	RPRH=R+rhdr.Rh;
	DR2=rhdr.Dr*rhdr.Dr;
	SQM=sqrt(RMRH*RMRH+DR2);
	SQP=sqrt(RPRH*RPRH+DR2);
	C=SQP-SQM;
	Q=sqrt(pow(rhdr.Rh+1.0,2.0)+DR2)-sqrt(pow(rhdr.Rh-1.0,2.0)+DR2);
	SPSAS=SPS/R*C/Q;
	CPSAS=sqrt(1.0-SPSAS*SPSAS);
	XOCT2= X*CPSAS-Z*SPSAS -loopdip1.XCentre[1];
	YOCT2= Y;
	ZOCT2= X*SPSAS+Z*CPSAS;
	Circle(XOCT2,YOCT2,ZOCT2,loopdip1.Radius[1],&BX,&BY,&BZ);
	D[0][25] =  BX*CPSAS+BZ*SPSAS;
	D[1][25] =  BY;
	D[2][25] = -BX*SPSAS+BZ*CPSAS;	
}

void Circle(double x, double y, double z, double Rl, double *Bx, double *By, double *Bz) {
	double K, Rho, Rho2, R22, R2, R12, R32, XK2, XK2S, DL, E, BRho;
	
	Rho2 = x*x + y*y;
	Rho = sqrt(Rho2);
	R22 = z*z + pow((Rho+Rl),2.0);
	R2 = sqrt(R22);
	R12 = R22 - 4.0*Rho*Rl;
	R32 = 0.5*(R12+R22);
	XK2 = 1.0 - (R12/R22);
	XK2S = 1.0 - XK2;
	DL = log(1.0/XK2S);
	K = 1.386294361120+XK2S*(0.096663442590+XK2S*(0.03590092383+XK2S*(0.03742563713+XK2S*0.01451196212))) +DL*(0.50+XK2S*(0.124985935970+XK2S*(0.068802485760+XK2S*(0.033283553460+XK2S*0.004417870120))));
	E = 1.0+XK2S*(0.443251414630+XK2S*(0.06260601220+XK2S*(0.047573835460+XK2S*0.017365064510))) +DL*XK2S*(0.24998368310+XK2S*(0.092001800370+XK2S*(0.040696975260+XK2S*0.005264496390)));
	
	if (Rho > 1.0e-6) {
		BRho = z/(Rho2*R2)*(R32/R12*E - K);
	} else {
		BRho = M_PI*Rl/R2*(Rl-Rho)/R12*z/(R32-Rho2);
	}
	*Bx = BRho*x;
	*By = BRho*y;
	*Bz = (K-E*(R32-2.0*Rl*Rl)/R12)/R2;
}

void CrossLP(double x, double y, double z, double *Bx, double *By, double *Bz, double Xc, double Rl, double Al) {
	double Cal, Sal, y1, z1, y2, z2, Bx1, By1, Bz1, Bx2, By2, Bz2;
	Cal = cos(Al);
	Sal = sin(Al);
	
	y1 = y*Cal - z*Sal;
	z1 = y*Sal + z*Cal;
	y2 = y*Cal + z*Sal;
	z2 = -y*Sal + z*Cal;
	Circle(x-Xc,y1,z1,Rl,&Bx1,&By1,&Bz1);
	Circle(x-Xc,y2,z2,Rl,&Bx2,&By2,&Bz2);
	*Bx = Bx1 + Bx2;
	*By = (By1 + By2)*Cal + (Bz1-Bz2)*Sal;
	*Bz =-(By1 - By2)*Sal + (Bz1+Bz2)*Cal;
}

void DipXYZ(double x, double y, double z, double *Bxx, double *Byx, double *Bzx, double *Bxy, double *Byy, double *Bzy, double *Bxz, double *Byz, double *Bzz) {
	double x2, y2, z2, r2, Xmr5, Xmr53;
	
	x2 = x*x;
	y2 = y*y;
	z2 = z*z;
	r2 = x2 + y2 + z2;
	
	Xmr5 = 30574.0/(r2*r2*sqrt(r2));
	Xmr53 = 3.0*Xmr5;
	*Bxx = Xmr5*(3.0*x2-r2);
	*Byx = Xmr53*x*y;
	*Bzx = Xmr53*x*z;
	
	*Bxy = *Byx;
	*Byy = Xmr5*(3.0*y2-r2);
	*Bzy = Xmr53*y*z;
	
	*Bxz = *Bzx;
	*Byz = *Bzy;
	*Bzz = Xmr5*(3.0*z2-r2);
	
	//printf("DIPXYZ\n");
	//printf("B*X %lf %lf %lf\n",*Bxx,*Byx,*Bzx);
	//printf("B*Y %lf %lf %lf\n",*Bxy,*Byy,*Bzy);
	//printf("B*Z %lf %lf %lf\n",*Bxz,*Byz,*Bzz);
	
}

void ConDip1(double *Xi, double D[3][79]) {
	double X, Y, Z, Ps, SPS, CPS, CF[5], SF[5], RO, RO2, XSM, ZSM, R, R2, C, S, CH, SH, TNH, CNH;
	double YD, XD, ZD,BX1X,BY1X,BZ1X,BX1Y,BY1Y,BZ1Y,BX1Z,BY1Z,BZ1Z;
	double BX2X,BY2X,BZ2X,BX2Y,BY2Y,BZ2Y,BX2Z,BY2Z,BZ2Z;
	double BX3X,BY3X,BZ3X,BX3Y,BY3Y,BZ3Y,BX3Z,BY3Z,BZ3Z;
	double BX4X,BY4X,BZ4X,BX4Y,BY4Y,BZ4Y,BX4Z,BY4Z,BZ4Z, BT, BF, BXSM, BZSM, BY;
	int M, I, IX, IY, IZ;

	X = Xi[0];
	Y = Xi[1];
	Z = Xi[2];
	Ps = Xi[3];
	SPS = sin(Ps);
	CPS = cos(Ps);
	//printf("Xi: %lf %lf %lf %lf %lf %lf\n",X,Y,Z,Ps,SPS,CPS);

	XSM = X*CPS - Z*SPS  - dx1.dx;
	ZSM = Z*CPS + X*SPS;
	RO2 = XSM*XSM + Y*Y;
	RO = sqrt(RO2);

	CF[0] = XSM/RO;
	SF[0] = Y/RO;

	CF[1] =    CF[0]*CF[0] - SF[0]*SF[0];
	SF[1] =2.0*SF[0]*CF[0];
	CF[2] =    CF[1]*CF[0] - SF[1]*SF[0];
	SF[2] =    SF[1]*CF[0] + CF[1]*SF[0];
	CF[3] =    CF[2]*CF[0] - SF[2]*SF[0];
	SF[3] =    SF[2]*CF[0] + CF[2]*SF[0];
	CF[4] =    CF[3]*CF[0] - SF[3]*SF[0];
	SF[4] =    SF[3]*CF[0] + CF[3]*SF[0];

	//printf("CF: %lf %lf %lf %lf %lf\n",CF[0],CF[1],CF[2],CF[3],CF[4]);
	//printf("SF: %lf %lf %lf %lf %lf\n",SF[0],SF[1],SF[2],SF[3],SF[4]);

	R2 = RO2 + ZSM*ZSM;
	R = sqrt(R2);
	C = ZSM/R;
	S = RO/R;
	CH = sqrt(0.5*(1.0+C));
	SH = sqrt(0.5*(1.0-C));
	TNH = SH/CH;
	CNH = 1.0/TNH;

	for (M=0;M<5;M++) { //0 - 4
		BT = (M+1)*CF[M]/(R*S)*(pow(TNH,M+1.0) + pow(CNH,M+1.0));
		BF =-0.5*(M+1)*SF[M]/R*(pow(TNH,M)/pow(CH,2.0) - pow(CNH,M)/pow(SH,2.0));
		BXSM = BT*C*CF[0] - BF*SF[0];
		BY = BT*C*SF[0] + BF*CF[0];
		BZSM = -BT*S;

		D[0][M] = BXSM*CPS + BZSM*SPS;
		D[1][M] = BY;
		D[2][M] =-BXSM*SPS + BZSM*CPS;
		//printf("D: %lf %lf %lf\n",D[0][M],D[1][M],D[2][M]); //these appear to be correct
	}
	XSM = X*CPS - Z*SPS;
	ZSM = Z*CPS + X*SPS;

	for (I=0;I<9;I++) { //5 - 31 and 32 - 58
        if (I == 2 || I == 4 || I == 5) {
			XD = coord21.xx[I]*dx1.ScaleIn;
			YD = coord21.yy[I]*dx1.ScaleIn;
        } else {
			XD = coord21.xx[I]*dx1.ScaleOut;
			YD = coord21.yy[I]*dx1.ScaleOut;
		}

		ZD =  coord21.zz[I];
		//printf("I: %d %lf %lf %lf\n",I+1,XD,YD,ZD); //these seem right
		DipXYZ(XSM-XD,Y-YD,ZSM-ZD,&BX1X,&BY1X,&BZ1X,&BX1Y,&BY1Y,&BZ1Y,&BX1Z,&BY1Z,&BZ1Z);
		DipXYZ(XSM-XD,Y+YD,ZSM-ZD,&BX2X,&BY2X,&BZ2X,&BX2Y,&BY2Y,&BZ2Y,&BX2Z,&BY2Z,&BZ2Z);
		DipXYZ(XSM-XD,Y-YD,ZSM+ZD,&BX3X,&BY3X,&BZ3X,&BX3Y,&BY3Y,&BZ3Y,&BX3Z,&BY3Z,&BZ3Z);
		DipXYZ(XSM-XD,Y+YD,ZSM+ZD,&BX4X,&BY4X,&BZ4X,&BX4Y,&BY4Y,&BZ4Y,&BX4Z,&BY4Z,&BZ4Z);

		IX=(I+1)*3+3;
		IY=IX+1;
		IZ=IY+1;
	
	//	printf("I   : %d %d %d %d\n",I+1,IX,IY,IZ); //same

		D[0][IX-1]=(BX1X+BX2X-BX3X-BX4X)*CPS+(BZ1X+BZ2X-BZ3X-BZ4X)*SPS;
		D[1][IX-1]= BY1X+BY2X-BY3X-BY4X;
		D[2][IX-1]=(BZ1X+BZ2X-BZ3X-BZ4X)*CPS-(BX1X+BX2X-BX3X-BX4X)*SPS;

		D[0][IY-1]=(BX1Y-BX2Y-BX3Y+BX4Y)*CPS+(BZ1Y-BZ2Y-BZ3Y+BZ4Y)*SPS;
		D[1][IY-1]= BY1Y-BY2Y-BY3Y+BY4Y;
		D[2][IY-1]=(BZ1Y-BZ2Y-BZ3Y+BZ4Y)*CPS-(BX1Y-BX2Y-BX3Y+BX4Y)*SPS;

		D[0][IZ-1]=(BX1Z+BX2Z+BX3Z+BX4Z)*CPS+(BZ1Z+BZ2Z+BZ3Z+BZ4Z)*SPS;
		D[1][IZ-1]= BY1Z+BY2Z+BY3Z+BY4Z;
		D[2][IZ-1]=(BZ1Z+BZ2Z+BZ3Z+BZ4Z)*CPS-(BX1Z+BX2Z+BX3Z+BX4Z)*SPS;

		IX=IX+27;
		IY=IY+27;
		IZ=IZ+27;

	//	printf("I+27: %d %d %d %d\n",I+1,IX,IY,IZ); //same

		D[0][IX-1]=SPS*((BX1X+BX2X+BX3X+BX4X)*CPS+(BZ1X+BZ2X+BZ3X+BZ4X)*SPS);
		D[1][IX-1]=SPS*(BY1X+BY2X+BY3X+BY4X);
		D[2][IX-1]=SPS*((BZ1X+BZ2X+BZ3X+BZ4X)*CPS-(BX1X+BX2X+BX3X+BX4X)*SPS);

		D[0][IY-1]=SPS*((BX1Y-BX2Y+BX3Y-BX4Y)*CPS+(BZ1Y-BZ2Y+BZ3Y-BZ4Y)*SPS);
		D[1][IY-1]=SPS*(BY1Y-BY2Y+BY3Y-BY4Y);
		D[2][IY-1]=SPS*((BZ1Y-BZ2Y+BZ3Y-BZ4Y)*CPS-(BX1Y-BX2Y+BX3Y-BX4Y)*SPS);

		D[0][IZ-1]=SPS*((BX1Z+BX2Z-BX3Z-BX4Z)*CPS+(BZ1Z+BZ2Z-BZ3Z-BZ4Z)*SPS);
		D[1][IZ-1]=SPS*(BY1Z+BY2Z-BY3Z-BY4Z);
		D[2][IZ-1]=SPS*((BZ1Z+BZ2Z-BZ3Z-BZ4Z)*CPS-(BX1Z+BX2Z-BX3Z-BX4Z)*SPS);
	}

	for (I=0;I<5;I++) { // 59 - 78
		ZD = coord21.zz[I+9];
		DipXYZ(XSM,Y,ZSM-ZD,&BX1X,&BY1X,&BZ1X,&BX1Y,&BY1Y,&BZ1Y,&BX1Z,&BY1Z,&BZ1Z);
		DipXYZ(XSM,Y,ZSM+ZD,&BX2X,&BY2X,&BZ2X,&BX2Y,&BY2Y,&BZ2Y,&BX2Z,&BY2Z,&BZ2Z);
		//printf("Pos: %d %lf %lf %lf %lf\n",I+1,XSM,Y,ZSM,ZD); //same
		//printf("Pos : %d\n",I+1);
		IX = 58 + (I+1)*2;
		IZ = IX + 1;
		//printf("I   : %d %d %d\n",I+1,IX,IZ);//same
		D[0][IX-1]=(BX1X-BX2X)*CPS+(BZ1X-BZ2X)*SPS;
		D[1][IX-1]=BY1X-BY2X;
		D[2][IX-1]=(BZ1X-BZ2X)*CPS-(BX1X-BX2X)*SPS;

		D[0][IZ-1]=(BX1Z+BX2Z)*CPS+(BZ1Z+BZ2Z)*SPS;
		D[1][IZ-1]=BY1Z+BY2Z;
		D[2][IZ-1]=(BZ1Z+BZ2Z)*CPS-(BX1Z+BX2Z)*SPS;

		IX = IX + 10;
		IZ = IZ + 10;
		//printf("I+10: %d %d %d\n",I+1,IX,IZ);//same
		D[0][IX-1]=SPS*((BX1X+BX2X)*CPS+(BZ1X+BZ2X)*SPS);
		D[1][IX-1]=SPS*(BY1X+BY2X);
		D[2][IX-1]=SPS*((BZ1X+BZ2X)*CPS-(BX1X+BX2X)*SPS);

		D[0][IZ-1]=SPS*((BX1Z-BX2Z)*CPS+(BZ1Z-BZ2Z)*SPS);
		D[1][IZ-1]=SPS*(BY1Z-BY2Z);
		D[2][IZ-1]=SPS*((BZ1Z-BZ2Z)*CPS-(BX1Z-BX2Z)*SPS);	
	}
	/*for (I=0;I<79;I++) {
		printf("I,D: %2d %9.4lf %9.4lf %9.4lf\n",I,D[0][I],D[1][I],D[2][I]);
	}*/
}

void Birk1Shld(double ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double A[] = {1.174198045,-1.463820502,4.840161537,-3.674506864,
		82.18368896,-94.94071588,-4122.331796,4670.278676,-21.54975037,
		26.72661293,-72.81365728,44.09887902,40.08073706,-51.23563510,
		1955.348537,-1940.971550,794.0496433,-982.2441344,1889.837171,
		-558.9779727,-1260.543238,1260.063802,-293.5942373,344.7250789,
		-773.7002492,957.0094135,-1824.143669,520.7994379,1192.484774,
		-1192.184565,89.15537624,-98.52042999,-0.8168777675E-01,
		0.4255969908E-01,0.3155237661,-0.3841755213,2.494553332,
		-0.6571440817E-01,-2.765661310,0.4331001908,0.1099181537,
		-0.6154126980E-01,-0.3258649260,0.6698439193,-5.542735524,
		0.1604203535,5.854456934,-0.8323632049,3.732608869,-3.130002153,
		107.0972607,-32.28483411,-115.2389298,54.45064360,-0.5826853320,
		-3.582482231,-4.046544561,3.311978102,-104.0839563,30.26401293,
		97.29109008,-50.62370872,-296.3734955,127.7872523,5.303648988,
		10.40368955,69.65230348,466.5099509,1.645049286,3.825838190,
		11.66675599,558.9781177,1.826531343,2.066018073,25.40971369,
		990.2795225,2.319489258,4.555148484,9.691185703,591.8280358};
	
	double *P1, *R1, *Q1, *S1, RP[4], RR[4], RQ[4], RS[4], CPS, SPS, S3PS, HX, HY, HZ;
	double CYPI, CYQI, SYPI, SYQI, SZRK, CZSK, CZRK, SZSK, SQPR, SQQS, EPR, EQS;
	int i, k, m, n, l;
	P1 = &A[64];
	R1 = &A[68];
	Q1 = &A[72];
	S1 = &A[76];
	
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
	
	CPS = cos(ps);
	SPS = sin(ps);
	S3PS = 4.0*CPS*CPS-1.0;
	
	for (i=0;i<4;i++) {
		RP[i] = 1.0/P1[i];
		RR[i] = 1.0/R1[i];
		RQ[i] = 1.0/Q1[i];
		RS[i] = 1.0/S1[i];
	}
	
	l = 0;
	for (m=0;m<2;m++) {
		for (i=0;i<4;i++) {
			CYPI=cos(y*RP[i]);
			CYQI=cos(y*RQ[i]);
			SYPI=sin(y*RP[i]);
			SYQI=sin(y*RQ[i]);			
			for (k=0;k<4;k++) {
				SZRK=sin(z*RR[k]);
				CZSK=cos(z*RS[k]);
				CZRK=cos(z*RR[k]);
				SZSK=sin(z*RS[k]);
				SQPR=sqrt(pow(RP[i],2.0)+pow(RR[k],2.0));
				SQQS=sqrt(pow(RQ[i],2.0)+pow(RS[k],2.0));
				EPR=exp(x*SQPR);
				EQS=exp(x*SQQS);
				for (n=0;n<2;n++) {
					if (m == 0) {
						if (n == 0) {
							HX=-SQPR*EPR*CYPI*SZRK;
							HY=RP[i]*EPR*SYPI*SZRK;
							HZ=-RR[k]*EPR*CYPI*CZRK;
                        } else {
							HX=HX*CPS;
							HY=HY*CPS;
							HZ=HZ*CPS;
                        }
					} else {
						if (n == 0) {
							HX=-SPS*SQQS*EQS*CYQI*CZSK;
							HY=SPS*RQ[i]*EQS*SYQI*CZSK;
							HZ=SPS*RS[k]*EQS*CYQI*SZSK;
                        } else {
							HX=HX*S3PS;
							HY=HY*S3PS;
							HZ=HZ*S3PS;
                       }
					}
					*Bx = *Bx + A[l]*HX;
					*By = *By + A[l]*HY;
					*Bz = *Bz + A[l]*HZ;
					l++;
				}
			}
		}
	}
}

void Birk2Tot(double ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double Wx, Wy, Wz, Hx, Hy, Hz;
	Birk2Shl(x,y,z,ps,&Wx,&Wy,&Wz);
	R2_Birk(x,y,z,ps,&Hx,&Hy,&Hz);
	//printf("%f %f %f %f %f %f\n",Wx, Wy, Wz, Hx, Hy, Hz);
	//Probem is with Hz
	*Bx = Wx + Hx;
	*By = Wy + Hy;
	*Bz = Wz + Hz;
}

void Birk2Shl(double x, double y, double z, double ps, double *Bx, double *By, double *Bz) {
	double A[] = {-111.6371348,124.5402702,110.3735178,-122.0095905,
			111.9448247,-129.1957743,-110.7586562,126.5649012,-0.7865034384,
			-0.2483462721,0.8026023894,0.2531397188,10.72890902,0.8483902118,
			-10.96884315,-0.8583297219,13.85650567,14.90554500,10.21914434,
			10.09021632,6.340382460,14.40432686,12.71023437,12.83966657};
	double *P, *R, *Q, *S, CPS, SPS, S3PS, DX, DY, DZ;
	double CYPI, CYQI, SYPI, SYQI, SZRK, CZSK, CZRK, SZSK, SQPR, SQQS, EPR, EQS;
	int i, k, m, n, l;
	P = &A[16];
	R = &A[18];
	Q = &A[20];
	S = &A[22];
	
	*Bx = 0.0;
	*By = 0.0;
	*Bz = 0.0;
	
	CPS = cos(ps);
	SPS = sin(ps);
	S3PS = 4.0*CPS*CPS-1.0;
	
	l = 0;
	for (m=0;m<2;m++) {
		for (i=0;i<2;i++) {
			CYPI=cos(y/P[i]);
			CYQI=cos(y/Q[i]);
			SYPI=sin(y/P[i]);
			SYQI=sin(y/Q[i]);			
			for (k=0;k<2;k++) {
				SZRK=sin(z/R[k]);
				CZSK=cos(z/S[k]);
				CZRK=cos(z/R[k]);
				SZSK=sin(z/S[k]);
				SQPR=sqrt(1.0/pow(P[i],2.0)+1.0/pow(R[k],2.0));
				SQQS=sqrt(1.0/pow(Q[i],2.0)+1.0/pow(S[k],2.0));
				EPR=exp(x*SQPR);
				EQS=exp(x*SQQS);
				for (n=0;n<2;n++) {
					if (m == 0) {
						if (n == 0) {
							DX=-SQPR*EPR*CYPI*SZRK;
							DY=EPR/P[i]*SYPI*SZRK;
							DZ=-EPR/R[k]*CYPI*CZRK;
                        } else {
							DX=DX*CPS;
							DY=DY*CPS;
							DZ=DZ*CPS;
                        }
					} else {
						if (n == 0) {
							DX=-SPS*SQQS*EQS*CYQI*CZSK;
							DY=SPS*EQS/Q[i]*SYQI*CZSK;
							DZ=SPS*EQS/S[k]*CYQI*SZSK;
                        } else {
							DX=DX*S3PS;
							DY=DY*S3PS;
							DZ=DZ*S3PS;
                       }
					}
					*Bx = *Bx + A[l]*DX;
					*By = *By + A[l]*DY;
					*Bz = *Bz + A[l]*DZ;
					l++;
				}
			}
		}
	}
}

void R2_Birk(double x, double y, double z, double ps, double *Bx, double *By, double *Bz) {
	double PSI, CPS, SPS, DELARG=0.03, DELARG1=0.015, XSM, ZSM, XKS;
	double BXSM, BZSM, BXSM1, BY1, BZSM1, BXSM2, BY2, BZSM2, F2, F1;
	PSI=ps;
	CPS=cos(ps);
	SPS=sin(ps);

	XSM=x*CPS-z*SPS;
	ZSM=z*CPS+x*SPS;
	
	XKS=XKSI(XSM,y,ZSM);
	if (XKS < -(DELARG+DELARG1)) {
		//printf("a\n");
		R2Outer(XSM,y,ZSM,&BXSM,By,&BZSM);
		BXSM = -BXSM*0.02;
		*By = -(*By)*0.02;
		BZSM = -BZSM*0.02;
	}
	
	if (XKS >= -(DELARG+DELARG1) && XKS < -DELARG+DELARG1) {
		//printf("b\n");
		R2Outer(XSM,y,ZSM,&BXSM1,&BY1,&BZSM1);
		R2Sheet(XSM,y,ZSM,&BXSM2,&BY2,&BZSM2);
        F2=-0.02*TKSI(XKS,-DELARG,DELARG1);
        F1=-0.02-F2;
        BXSM=BXSM1*F1+BXSM2*F2;
        *By=BY1*F1+BY2*F2;
        BZSM=BZSM1*F1+BZSM2*F2;	
	}
	
	if (XKS >= -DELARG+DELARG1 && XKS < DELARG-DELARG1) {
		//printf("c\n");
		R2Sheet(XSM,y,ZSM,&BXSM,By,&BZSM);
		BXSM=-BXSM*0.02;
		*By=-(*By)*0.02;
		BZSM=-BZSM*0.02;
	}

	if (XKS >= DELARG-DELARG1 && XKS < DELARG+DELARG1) {
		//printf("d\n");
		R2Inner(XSM,y,ZSM,&BXSM1,&BY1,&BZSM1);
		R2Sheet(XSM,y,ZSM,&BXSM2,&BY2,&BZSM2);
        F1=-0.02*TKSI(XKS,DELARG,DELARG1);
        F2=-0.02-F1;
        BXSM=BXSM1*F1+BXSM2*F2;
        *By=BY1*F1+BY2*F2;
        BZSM=BZSM1*F1+BZSM2*F2;
	}
	
	if (XKS >= DELARG+DELARG1) {
		//printf("e\n");
		R2Inner(XSM,y,ZSM,&BXSM,By,&BZSM);
		BXSM=-BXSM*0.02;
		*By=-(*By)*0.02;
		BZSM=-BZSM*0.02;
	}
	
	*Bx = BXSM*CPS + BZSM*SPS;
	*Bz = BZSM*CPS - BXSM*SPS;
}

void R2Inner(double x, double y, double z, double *Bx, double *By, double *Bz) {
	double PL[] = {154.185,-2.12446,0.601735E-01,-0.153954E-02,0.355077E-04,29.9996,262.886,99.9132};
	double PN[] = {-8.1902,6.5239,5.504,7.7815,0.8573,3.0986,0.0774,-0.038};
	double CBX[5], CBY[5], CBZ[5],DBX8,DBY8,DBZ8,DBX6,DBY6,DBZ6,DBX7,DBY7,DBZ7;
	BConic(x,y,z,CBX,CBY,CBZ,5);//wrong in here somewhere
	//printf("CBX: %f %f %f %f %f\n",CBX[0],CBX[1],CBX[2],CBX[3],CBX[4]);
	//printf("CBY: %f %f %f %f %f\n",CBY[0],CBY[1],CBY[2],CBY[3],CBY[4]);
	//printf("CBZ: %f %f %f %f %f\n",CBZ[0],CBZ[1],CBZ[2],CBZ[3],CBZ[4]);
	Loops4(x,y,z,&DBX8,&DBY8,&DBZ8,PN[0],PN[1],PN[2],PN[3],PN[4],PN[5]);
	//printf("8: %f %f %f \n",DBX8, DBY8, DBZ8);

	DipDistR(x-PN[6],y,z,&DBX6,&DBY6,&DBZ6,0);
	DipDistR(x-PN[7],y,z,&DBX7,&DBY7,&DBZ7,1);
	//printf("7: %f %f %f \n",DBX7, DBY7, DBZ7);
	//printf("6: %f %f %f \n",DBX6, DBY6, DBZ6);

	*Bx=PL[0]*CBX[0]+PL[1]*CBX[1]+PL[2]*CBX[2]+PL[3]*CBX[3]+PL[4]*CBX[4]+PL[5]*DBX6+PL[6]*DBX7+PL[7]*DBX8;
	*By=PL[0]*CBY[0]+PL[1]*CBY[1]+PL[2]*CBY[2]+PL[3]*CBY[3]+PL[4]*CBY[4]+PL[5]*DBY6+PL[6]*DBY7+PL[7]*DBY8;
	*Bz=PL[0]*CBZ[0]+PL[1]*CBZ[1]+PL[2]*CBZ[2]+PL[3]*CBZ[3]+PL[4]*CBZ[4]+PL[5]*DBZ6+PL[6]*DBZ7+PL[7]*DBZ8;
	
}

void BConic(double x, double y, double z, double *CBx, double *CBy, double *CBz, int Nmax) {
	double RO2, RO, CFM1, SFM1, R2, R, C, S, CH, SH, TNHM1, CNHM1, TNH, CNH;
	double CFM, SFM, TNHM, CNHM, BT, BF, CF, SF;
	int M;
	
	RO2=x*x+y*y;
	RO=sqrt(RO2);

	CF=x/RO;
	SF=y/RO;
	CFM1=1.0;
	SFM1=0.0;

	R2=RO2+z*z;
	R=sqrt(R2);
	C=z/R;
	S=RO/R;
	CH=sqrt(0.50*(1.0+C));
	SH=sqrt(0.50*(1.0-C));
	TNHM1=1.0;
	CNHM1=1.0;
	TNH=SH/CH;
	CNH=1.0/TNH;
	
	for (M=0;M<Nmax;M++) {
		CFM=CFM1*CF-SFM1*SF;
		SFM=CFM1*SF+SFM1*CF;
		CFM1=CFM;
		SFM1=SFM;
        TNHM=TNHM1*TNH;
        CNHM=CNHM1*CNH;
		BT=(M+1)*CFM/(R*S)*(TNHM+CNHM);
		BF=-0.5*(M+1)*SFM/R*(TNHM1/pow(CH,2.0)-CNHM1/pow(SH,2.0));
		TNHM1=TNHM;
		CNHM1=CNHM;
		CBx[M]=BT*C*CF-BF*SF;
		CBy[M]=BT*C*SF+BF*CF;
		CBz[M]=-BT*S;
	}	
}

void DipDistR(double x, double y, double z, double *Bx, double *By, double *Bz, int Mode) {
	double X2, RHO2, R2, R3;
	
	X2=x*x;
	RHO2=X2+y*y;
	R2=RHO2+z*z;
	R3=R2*sqrt(R2);

	if (Mode == 0) {
		*Bx=z/pow(RHO2,2.0)*(R2*(y*y-X2)-RHO2*X2)/R3;
		*By=-x*y*z/pow(RHO2,2.0)*(2.0*R2+RHO2)/R3;
		*Bz=x/R3;
	} else {
		*Bx=z/pow(RHO2,2.0)*(y*y-X2);
		*By=-2.0*x*y*z/pow(RHO2,2.0);
		*Bz=x/RHO2;
	}
}

void R2Outer(double x, double y, double z, double *Bx, double *By, double *Bz) {
	double PL[] = {-34.105,-2.00019,628.639,73.4847,12.5162};
	double PN[] = {0.55,0.694,0.0031,1.55,2.8,0.1375,-0.7,0.2,0.9625,
					-2.994,2.925,-1.775,4.3,-0.275,2.7,0.4312,1.55};
	double DBX1,DBY1,DBZ1,DBX2,DBY2,DBZ2,DBX3,DBY3,DBZ3,DBX4,DBY4,DBZ4,DBX5,DBY5,DBZ5;

	CrossLP(x,y,z,&DBX1,&DBY1,&DBZ1,PN[0],PN[1],PN[2]);
	CrossLP(x,y,z,&DBX2,&DBY2,&DBZ2,PN[3],PN[4],PN[5]);
	CrossLP(x,y,z,&DBX3,&DBY3,&DBZ3,PN[6],PN[7],PN[8]);
	Circle(x-PN[9],y,z,PN[10],&DBX4,&DBY4,&DBZ4);
	Loops4(x,y,z,&DBX5,&DBY5,&DBZ5,PN[11],PN[12],PN[13],PN[14],PN[15],PN[16]);
	*Bx=PL[0]*DBX1+PL[1]*DBX2+PL[2]*DBX3+PL[3]*DBX4+PL[4]*DBX5;
	*By=PL[0]*DBY1+PL[1]*DBY2+PL[2]*DBY3+PL[3]*DBY4+PL[4]*DBY5;
	*Bz=PL[0]*DBZ1+PL[1]*DBZ2+PL[2]*DBZ3+PL[3]*DBZ4+PL[4]*DBZ5;
	
}

void Loops4(double x, double y, double z, double *Bx, double *By, double *Bz, double Xc, double Yc, double Zc, double R, double Theta, double Phi) {
	
	double CT, ST, CP, SP, XS, YSS, ZS, XSS, ZSS, BXS, BXSS, BYS, BZSS, BX1, BY1, BZ1, BX2, BY2, BZ2, BX3, BY3, BZ3, BX4, BY4, BZ4;
	
	CT=cos(Theta);
	ST=sin(Theta);
	CP=cos(Phi);
	SP=sin(Phi);

	XS=(x-Xc)*CP+(y-Yc)*SP;
	YSS=(y-Yc)*CP-(x-Xc)*SP;
	ZS=z-Zc;
	XSS=XS*CT-ZS*ST;
	ZSS=ZS*CT+XS*ST;

	Circle(XSS,YSS,ZSS,R,&BXSS,&BYS,&BZSS);
	BXS=BXSS*CT+BZSS*ST;
	BZ1=BZSS*CT-BXSS*ST;
	BX1=BXS*CP-BYS*SP;
	BY1=BXS*SP+BYS*CP;

	XS=(x-Xc)*CP-(y+Yc)*SP;
	YSS=(y+Yc)*CP+(x-Xc)*SP;
	ZS=z-Zc;
	XSS=XS*CT-ZS*ST;
	ZSS=ZS*CT+XS*ST;

	Circle(XSS,YSS,ZSS,R,&BXSS,&BYS,&BZSS);
	BXS=BXSS*CT+BZSS*ST;
	BZ2=BZSS*CT-BXSS*ST;
	BX2=BXS*CP+BYS*SP;
	BY2=-BXS*SP+BYS*CP;

	XS=-(x-Xc)*CP+(y+Yc)*SP;
	YSS=-(y+Yc)*CP-(x-Xc)*SP;
	ZS=z+Zc;
	XSS=XS*CT-ZS*ST;
	ZSS=ZS*CT+XS*ST;

	Circle(XSS,YSS,ZSS,R,&BXSS,&BYS,&BZSS);
	BXS=BXSS*CT+BZSS*ST;
	BZ3=BZSS*CT-BXSS*ST;
	BX3=-BXS*CP-BYS*SP;
	BY3=BXS*SP-BYS*CP;

	XS=-(x-Xc)*CP-(y-Yc)*SP;
	YSS=-(y-Yc)*CP+(x-Xc)*SP;
	ZS=z+Zc;
	XSS=XS*CT-ZS*ST;
	ZSS=ZS*CT+XS*ST;
	
	Circle(XSS,YSS,ZSS,R,&BXSS,&BYS,&BZSS);
	BXS=BXSS*CT+BZSS*ST;
	BZ4=BZSS*CT-BXSS*ST;
	BX4=-BXS*CP+BYS*SP;
	BY4=-BXS*SP-BYS*CP;

	*Bx=BX1+BX2+BX3+BX4;
	*By=BY1+BY2+BY3+BY4;
	*Bz=BZ1+BZ2+BZ3+BZ4;
}

void R2Sheet(double x, double y, double z, double *Bx, double *By, double *Bz) {
	double PNONX[] = {-19.09690,-9.288280,-0.1296870,5.585940,22.50550,0.483750e-01,0.396953e-01,0.579023e-01};
	double PNONY[] = {-13.67500,-6.706250, 2.318750,11.40620,20.45620,0.478750e-01,0.363750e-01, 0.567500e-01};
	double PNONZ[] = {-16.71250,-16.46250,-0.16250,5.10,23.71250,0.355625e-01,0.318750e-01,0.538750e-01};
	double A[] = {8.071900,-7.395820,-7.623410,0.6846710,-13.56720,11.66810,
				13.1154,-0.8902170,7.787260,-5.383460,-8.087380,0.6093850,
				-2.704100, 3.537410,3.155490,-1.110690,-8.475550,0.2781220,
				2.735140,4.556250,13.11340,1.158480,-3.526480,-8.246980,
				-6.857100,-2.813690, 2.037950, 4.643830,2.493090,-1.220410,
				-1.674320,-0.4225260,-5.397960,7.103260,5.537300,-13.19180,
				4.678530,-7.603290,-2.530660, 7.763380, 5.601650,5.348160,
				-4.564410,7.059760,-2.627230,-0.5290780,1.420190,-2.939190,
				55.63380,-1.551810,39.83110,-80.65610,-46.96550,32.89250,
				-6.322960,19.78410,124.7310,10.43470,-30.75810,102.6800,
				-47.40370,-3.312780,9.371410,-50.02680,-533.3190,110.4260,
				1000.200,-1051.400, 1619.480,589.8550,-1462.730,1087.100,
				-1994.730,-1654.120,1263.330,-260.2100,1424.840,1255.710,
				-956.7330, 219.9460};
	double B[] = {-9.08427,10.6777,10.3288,-0.969987,6.45257,-8.42508,
				  -7.97464,1.41996,-1.92490,3.93575,2.83283,-1.48621,
				  0.244033,-0.757941,-0.386557,0.344566,9.56674,-2.5365,
				  -3.32916,-5.86712,-6.19625,1.83879,2.52772,4.34417,
				  1.87268,-2.13213,-1.69134,-.176379,-.261359,.566419,
				  0.3138,-0.134699,-3.83086,-8.4154,4.77005,-9.31479,
				  37.5715,19.3992,-17.9582,36.4604,-14.9993,-3.1442,
				  6.17409,-15.5519,2.28621,-0.891549e-2,-.462912,2.47314,
				  41.7555,208.614,-45.7861,-77.8687,239.357,-67.9226,
				  66.8743,238.534,-112.136,16.2069,-40.4706,-134.328,
				  21.56,-0.201725,2.21,32.5855,-108.217,-1005.98,
				  585.753,323.668,-817.056,235.750,-560.965,-576.892,
				  684.193,85.0275,168.394,477.776,-289.253,-123.216,
				  75.6501,-178.605};
	double C[] = {1167.61,-917.782,-1253.2,-274.128,-1538.75,1257.62,
				1745.07,113.479,393.326,-426.858,-641.1,190.833,
				-29.9435,-1.04881,117.125,-25.7663,-1168.16,910.247,
				1239.31,289.515,1540.56,-1248.29,-1727.61,-131.785,
				-394.577,426.163,637.422,-187.965,30.0348,0.221898,
				-116.68,26.0291,12.6804,4.84091,1.18166,-2.75946,
				-17.9822,-6.80357,-1.47134,3.02266,4.79648,0.665255,
				-0.256229,-0.857282e-1,-0.588997,0.634812e-1,0.164303,
				-0.15285,22.2524,-22.4376,-3.85595,6.07625,-105.959,
				-41.6698,0.378615,1.55958,44.3981,18.8521,3.19466,
				5.89142,-8.63227,-2.36418,-1.027,-2.31515,1035.38,
				2040.66,-131.881,-744.533,-3274.93,-4845.61,482.438,
				1567.43,1354.02,2040.47,-151.653,-845.012,-111.723,
				-265.343,-26.1171,216.632};
				
	double XKS, T1X, T2X, T3X, T1Y, T2Y, T3Y, T1Z, T2Z, T3Z, R, RHO, RHO2, C1P, S1P, C2P, S2P, C3P, S3P, C4P, S4P;
	double CT, ST, S[5];
	int i,j;

	XKS=XKSI(x,y,z);
	T1X=XKS/sqrt(pow(XKS,2)+pow(PNONX[5],2));
	T2X=pow(PNONX[6],3)/pow(sqrt(pow(XKS,2)+pow(PNONX[6],2)),3);
	T3X=XKS/pow(sqrt(pow(XKS,2)+pow(PNONX[7],2)),5) *3.493856*pow(PNONX[7],4);

	T1Y=XKS/sqrt(pow(XKS,2)+pow(PNONY[5],2));
	T2Y=pow(PNONY[6],3)/pow(sqrt(pow(XKS,2)+pow(PNONY[6],2)),3);
	T3Y=XKS/pow(sqrt(pow(XKS,2)+pow(PNONY[7],2)),5) *3.493856*pow(PNONY[7],4);

	T1Z=XKS/sqrt(pow(XKS,2)+pow(PNONZ[5],2));
	T2Z=pow(PNONZ[6],3)/pow(sqrt(pow(XKS,2)+pow(PNONZ[6],2)),3);
	T3Z=XKS/pow(sqrt(pow(XKS,2)+pow(PNONZ[7],2)),5) *3.493856*pow(PNONZ[7],4);
	
	
	RHO2=x*x + y*y;
	R=sqrt(RHO2+z*z);
	RHO=sqrt(RHO2);

	C1P=x/RHO;
	S1P=y/RHO;
	S2P=2.0*S1P*C1P;
	C2P=C1P*C1P-S1P*S1P;
	S3P=S2P*C1P+C2P*S1P;
	C3P=C2P*C1P-S2P*S1P;
	S4P=S3P*C1P+C3P*S1P;
	CT=z/R;
	ST=RHO/R;

	S[0]=Fexp(CT,PNONX[0]);
	S[1]=Fexp(CT,PNONX[1]);
	S[2]=Fexp(CT,PNONX[2]);
	S[3]=Fexp(CT,PNONX[3]);
	S[4]=Fexp(CT,PNONX[4]);	
	
	*Bx = 0.0;
	for (i=0;i<5;i++) {
		j = i*16;
		*Bx = *Bx + S[i]*((A[j]+A[j+1]*T1X+A[j+2]*T2X+A[j+3]*T3X)
				  + C1P*(A[j+4]+A[j+5]*T1X+A[j+6]*T2X+A[j+7]*T3X)
                  + C2P*(A[j+8]+A[j+9]*T1X+A[j+10]*T2X+A[j+11]*T3X)
                  + C3P*(A[j+12]+A[j+13]*T1X+A[j+14]*T2X+A[j+15]*T3X));
	}


	S[0]=Fexp(CT,PNONY[0]);
	S[1]=Fexp(CT,PNONY[1]);
	S[2]=Fexp(CT,PNONY[2]);
	S[3]=Fexp(CT,PNONY[3]);
	S[4]=Fexp(CT,PNONY[4]);
	
	*By = 0.0;
	for (i=0;i<5;i++) {
		j = i*16;
		*By = *By + S[i]*(S1P*(B[j]+B[j+1]*T1Y+B[j+2]*T2Y+B[j+3]*T3Y)
						+ S2P*(B[j+4]+B[j+5]*T1Y+B[j+6]*T2Y+B[j+7]*T3Y)
						+ S3P*(B[j+8]+B[j+9]*T1Y+B[j+10]*T2Y+B[j+11]*T3Y)
						+ S4P*(B[j+12]+B[j+13]*T1Y+B[j+14]*T2Y+B[j+15]*T3Y));
	}	
	
	S[0]=Fexp1(CT,PNONZ[0]);
	S[1]=Fexp1(CT,PNONZ[1]);
	S[2]=Fexp1(CT,PNONZ[2]);
	S[3]=Fexp1(CT,PNONZ[3]);
	S[4]=Fexp1(CT,PNONZ[4]);

	*Bz = 0.0;
	for (i=0;i<5;i++) {
		j = i*16;
		*Bz = *Bz + S[i]*((C[j]   + C[j+1]*T1Z + C[j+2]*T2Z + C[j+3]*T3Z)
					+ C1P*(C[j+4] + C[j+5]*T1Z + C[j+6]*T2Z + C[j+7]*T3Z)
					+ C2P*(C[j+8] + C[j+9]*T1Z + C[j+10]*T2Z+ C[j+11]*T3Z)
					+ C3P*(C[j+12]+ C[j+13]*T1Z+ C[j+14]*T2Z+ C[j+15]*T3Z));
	}
					
}

double XKSI(double x, double y, double z) {
	double A11A12,A21A22,A41A42,A51A52,A61A62,B11B12,B21B22,C61C62,C71C72,R0,DR,TNOON,DTETA, FGH, FGH32,FCHSG2;
	double DR2, X2, Y2, Z2, XY, XYZ, R2, R, R3, R4, XR, YR, ZR, PR, F, G, H, G2, SQFCHSG2, ALPHA, THETA, PHI;
	A11A12=0.305662;
	A21A22=-0.383593;
	A41A42=0.2677733;
	A51A52=-0.097656;
	A61A62=-0.636034;
	B11B12=-0.359862;
	B21B22=0.424706;
	C61C62=-0.126366;
	C71C72=0.292578;
	R0=1.21563;
	DR=7.50937;
	TNOON = 0.3665191;
	DTETA = 0.09599309;

	DR2=DR*DR;

	X2=x*x;
	Y2=y*y;
	Z2=z*z;
	XY=x*y;
	XYZ=XY*z;
	R2=X2+Y2+Z2;
	R=sqrt(R2);
	R3=R2*R;
	R4=R2*R2;
	XR=x/R;
	YR=y/R;
	ZR=z/R;

	if (R < R0) {
		PR = 0.0;
	} else {
		PR = sqrt(pow(R-R0,2)+DR2)-DR;
	}

	F=x+PR*(A11A12+A21A22*XR+A41A42*XR*XR+A51A52*YR*YR+A61A62*ZR*ZR);
	G=y+PR*(B11B12*YR+B21B22*XR*YR);
	H=z+PR*(C61C62*ZR+C71C72*XR*ZR);
	G2=G*G;

	FGH=F*F+G2+H*H;
	FGH32=pow(sqrt(FGH),3);
	FCHSG2=F*F+G2;

	if (FCHSG2 < 1.0e-5) {
		return -1.0;              
	}

	SQFCHSG2=sqrt(FCHSG2);
	ALPHA=FCHSG2/FGH32;
	THETA=TNOON+0.50*DTETA*(1.0-F/SQFCHSG2);
	PHI=pow(sin(THETA),2);

	return (ALPHA-PHI);
	
}

double Fexp(double S, double A) {
	if (A < 0.0) {
		return sqrt(-2.0*A*M_E)*S*expf(A*S*S);
	} else {
		return S*expf(A*(S*S-1.0));
	}
}

double Fexp1(double S, double A) {
	if (A < 0.0) {
		return expf(A*S*S);
	} else {
		return expf(A*(S*S-1.0));
	}
	
}

double TKSI( double xksi, double xks0, double Dxksi) {
	double tdz3, br3, tksii;
	tdz3 = 2.0*pow(Dxksi,3);
	
	if (xksi-xks0 < -Dxksi) { 
		tksii = 0.0;
	}
	if (xksi-xks0 >= Dxksi) {
		tksii = 1.0;
	}
	if (xksi >= xks0-Dxksi && xksi < xks0) {
		br3 = pow(xksi-xks0+Dxksi,3.0);
		tksii = 1.5*br3/(tdz3+br3);
	}
	if (xksi >= xks0 && xksi < xks0+Dxksi) {
		br3 = pow(xksi-xks0-Dxksi,3.0);
		tksii = 1.0+1.5*br3/(tdz3-br3);		
	}
	return tksii;
}

void Dipole(double ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	double SPS, CPS, PSI, P, U, V, T, Q;
	SPS=sin(ps);
	CPS=cos(ps);
	PSI=ps;
	P=x*x;
	U=z*z;
	V=3.0*z*x;
	T=y*y;
	Q=30574.0/pow(sqrt(P+T+U),5.0);
	*Bx=Q*((T+U-2.0*P)*SPS-V*CPS);
	*By=-3.0*y*Q*(x*SPS+z*CPS);
	*Bz=Q*((P+T-2.0*U)*CPS-V*SPS);	
}
