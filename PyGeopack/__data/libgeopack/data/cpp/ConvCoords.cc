#include "ConvCoords.h"

void SphtoCar(double R, double Theta, double Phi, double *X, double *Y, double *Z) {
	/* Convert spherical coordinates (theta and phi are in radians) to
	 * cartesian*/
	
	
	double SQ = R * sin(Theta);
	*X = SQ*cos(Phi);
	*Y = SQ*sin(Phi);
	*Z = R*cos(Theta);
	return;
} 

void CartoSph(double X, double Y, double Z, double *R, double *Theta, double *Phi) {
	/* Converts cartesian coordinates to spherical, where theta and phi
	 * are in radians */
	double SQ = X*X + Y*Y;
	*R = sqrt(SQ + Z*Z);
	if (SQ == 0.0) {
		*Phi = 0.0;
		if (Z < 0) {
			*Theta = M_PI;
		} else {
			*Theta = 0.0;
		}
	} else {
		SQ = sqrt(SQ);
		*Phi = atan2(Y,X);
		*Theta = atan2(SQ,Z);
		if (*Phi < 0.0) {
			*Phi = *Phi + 2*M_PI;
		}
	}
}

void BSphtoCar(double Theta, double Phi, double Br, double Btheta, double Bphi, double *Bx, double *By, double *Bz) {
	/* Calculates cartesian magnetic field components from local 
	 * spherical coordinates*/
	double S, C, SF, CF, Be;
	S = sin(Theta);
	C = cos(Theta);
	SF = sin(Phi);
	CF = cos(Phi);
	Be = Br*S + Btheta*C;
	*Bx = Be*CF - Bphi*SF;
	*By = Be*SF + Bphi*CF;
	*Bz = Br*C - Btheta*S;
	return;
}

void BCartoSph(double X, double Y, double Z, double Bx, double By, double Bz, double *Br, double *Btheta, double *Bphi) {
	/* Converts cartesian magnetic field components to spherical ones */
	
	double Rho2, R, Rho, Cphi, Sphi, CT, ST;
	Rho2 = X*X + Y*Y;
	R = sqrt(Rho2+Z*Z);
	Rho = sqrt(Rho2);

	if (Rho != 0.0) {
		Cphi = X/Rho;
		Sphi = Y/Rho;
	} else {
		Cphi = 1.0;
		Sphi = 0.0;
	}

	CT = Z/R;
	ST = Rho/R;

	*Br = (X*Bx + Y*By + Z*Bz)/R;
	*Btheta = (Bx*Cphi + By*Sphi)*CT - Bz*ST;
	*Bphi = By*Cphi - Bx*Sphi;
	return;
}


void GSWtoGSE(double XGSW, double YGSW, double ZGSW,double *XGSE, double *YGSE, double *ZGSE) {
	/* Converts GSW to GSE coordinates */
	
	*XGSE = XGSW*GP1.E11 + YGSW*GP1.E12 + ZGSW*GP1.E13;
	*YGSE = XGSW*GP1.E21 + YGSW*GP1.E22 + ZGSW*GP1.E23;
	*ZGSE = XGSW*GP1.E31 + YGSW*GP1.E32 + ZGSW*GP1.E33;
	return;
}


void GSEtoGSW(double XGSE, double YGSE, double ZGSE,double *XGSW, double *YGSW, double *ZGSW) {
	/* Converts GSE to GSW coordinates */
	
	*XGSW = XGSE*GP1.E11 + YGSE*GP1.E21 + ZGSE*GP1.E31;
	*YGSW = XGSE*GP1.E12 + YGSE*GP1.E22 + ZGSE*GP1.E32;
	*ZGSW = XGSE*GP1.E13 + YGSE*GP1.E23 + ZGSE*GP1.E33;
	return;
}

void GEOtoMAG(double XGEO, double YGEO, double ZGEO,double *XMAG, double *YMAG, double *ZMAG) {
	/* Converts GEO to MAG coordinates */
	
	*XMAG = XGEO*GP1.CTCL + YGEO*GP1.CTSL - ZGEO*GP1.ST0;
	*YMAG = YGEO*GP1.CL0  - XGEO*GP1.SL0;
	*ZMAG = XGEO*GP1.STCL + YGEO*GP1.STSL + ZGEO*GP1.CT0;
	return;
}

void MAGtoGEO(double XMAG, double YMAG, double ZMAG,double *XGEO, double *YGEO, double *ZGEO) {
	/* Converts MAG to GEO coordinates */
	
	*XGEO = XMAG*GP1.CTCL - YMAG*GP1.SL0 + ZMAG*GP1.STCL;
	*YGEO = XMAG*GP1.CTSL + YMAG*GP1.CL0 + ZMAG*GP1.STSL;
	*ZGEO = ZMAG*GP1.CT0  - XMAG*GP1.ST0;
	return;
}


void GEItoGEO(double XGEI, double YGEI, double ZGEI,double *XGEO, double *YGEO, double *ZGEO) {
	/* Converts GEI to GEO coordinates */
	
	*XGEO = XGEI*GP1.CGST + YGEI*GP1.SGST;
	*YGEO = YGEI*GP1.CGST - XGEI*GP1.SGST;
	*ZGEO = ZGEI;
	return;
}

void GEOtoGEI(double XGEO, double YGEO, double ZGEO,double *XGEI, double *YGEI, double *ZGEI) {
	/* Converts GEO to GEI coordinates */
	
	*XGEI = XGEO*GP1.CGST - YGEO*GP1.SGST;
	*YGEI = YGEO*GP1.CGST + XGEO*GP1.SGST;
	*ZGEI = ZGEO;
	return;
}

void MAGtoSM(double XMAG, double YMAG, double ZMAG,double *XSM, double *YSM, double *ZSM) {
	/* Converts MAG to SM coordinates */
	
	*XSM = XMAG*GP1.CFI - YMAG*GP1.SFI;
	*YSM = XMAG*GP1.SFI + YMAG*GP1.CFI;
	*ZSM = ZMAG;
	return;
}

void SMtoMAG(double XSM, double YSM, double ZSM,double *XMAG, double *YMAG, double *ZMAG) {
	/* Converts SM to MAG coordinates */
	
	*XMAG = XSM*GP1.CFI + YSM*GP1.SFI;
	*YMAG = YSM*GP1.CFI - XSM*GP1.SFI;
	*ZMAG = ZSM;
	return;
}

void SMtoGSW(double XSM, double YSM, double ZSM,double *XGSW, double *YGSW, double *ZGSW) {
	/* Converts SM to GSW coordinates */
	
	*XGSW = XSM*GP1.CPS + ZSM*GP1.SPS;
	*YGSW = YSM;
	*ZGSW = ZSM*GP1.CPS - XSM*GP1.SPS;
	return;
}

void GSWtoSM(double XGSW, double YGSW, double ZGSW, double *XSM, double *YSM, double *ZSM) {
	/* Converts GSW to SM coordinates */
	
	*XSM = XGSW*GP1.CPS - ZGSW*GP1.SPS;
	*YSM = YGSW;
	*ZSM = XGSW*GP1.SPS + ZGSW*GP1.CPS;
	return;
}

void GEOtoGSW(double XGEO, double YGEO, double ZGEO,double *XGSW, double *YGSW, double *ZGSW) {
	/* Converts GEO to GSW coordinates */
	
	*XGSW = GP1.A11*XGEO + GP1.A12*YGEO + GP1.A13*ZGEO;
	*YGSW = GP1.A21*XGEO + GP1.A22*YGEO + GP1.A23*ZGEO;
	*ZGSW = GP1.A31*XGEO + GP1.A32*YGEO + GP1.A33*ZGEO;
	return;
}

void GSWtoGEO(double XGSW, double YGSW, double ZGSW,double *XGEO, double *YGEO, double *ZGEO) {
	/* Converts GSW to GEO coordinates */
	
	*XGEO = GP1.A11*XGSW + GP1.A21*YGSW + GP1.A31*ZGSW;
	*YGEO = GP1.A12*XGSW + GP1.A22*YGSW + GP1.A32*ZGSW;
	*ZGEO = GP1.A13*XGSW + GP1.A23*YGSW + GP1.A33*ZGSW;
	return;
}

void GEODtoGEO(double H,double Xmu, double *R, double *Theta) {
	/* Converts Geodetic altitude (H) and latitude (Xmu) to
	 * geocentric R and Theta */
	
	double R_eq = 6378.137, Beta = 6.73949674228E-3, Tol = 1.0e-6;
	double CosXmu, SinXmu, Den, CosLam, SinLam, Rs, Z, X;
	CosXmu = cos(Xmu);
	SinXmu = sin(Xmu);
	Den = sqrt(pow(CosXmu,2.0) + pow((SinXmu/(1.0 + Beta)),2.0));
	CosLam = CosXmu/Den;
	SinLam = SinXmu/(Den*(1.0 + Beta));
	Rs = R_eq/sqrt(1.0 + Beta*pow(SinLam,2.0));
	X = Rs*CosLam + H*CosXmu;
	Z = Rs*SinLam + H*SinXmu;
	*R = sqrt(X*X + Z*Z);
	*Theta = acos(Z/(*R));	
	return;
}


void GEOtoGEOD(double R, double Theta,double *H,double *Xmu) {
	/* Converts geocentric R and Theta to
	 * Geodetic altitude (H) and latitude (Xmu).*/
	
	double R_eq = 6378.137, Beta = 6.73949674228E-3, Tol = 1.0e-6;
	double Phi, Phi1, SP, DPhi, Arg, Xmus, Rs, CosFims, X, Z, RR;
	int n = 0;
	Phi = M_PI/2.0 - Theta;
	Phi1 = Phi;
	while ((n < 100) && (abs(DPhi) > Tol)) {
		SP = sin(Phi1);
		Arg = SP*(1.0 + Beta)/sqrt(1.0 + Beta*(2.0 + Beta)*SP*SP);
		Xmus = asin(Arg);
		Rs = R_eq/sqrt(1.0 + Beta*pow(sin(Phi1),2.0));
		CosFims = cos(Phi1 - Xmus);
		*H = sqrt(pow((Rs*CosFims),2.0) + R*R - Rs*Rs) - Rs*CosFims;
		Z = Rs*sin(Phi1) + (*H)*sin(Xmus);
		X = Rs*cos(Phi1) + (*H)*cos(Xmus);
		RR = sqrt(X*X + Z*Z);
		DPhi = asin(Z/RR) - Phi;
		Phi1 = Phi1 - DPhi;
		n++;
	}
	*Xmu = Xmus;	
	return;
}




void GSEtoGSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		GSEtoGSW(Xin[i],Yin[i],Zin[i],&Xout[i],&Yout[i],&Zout[i]);
	}
	return;
}

void GSEtoGSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoGSMArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void GSMtoGSEArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		GSWtoGSE(Xin[i],Yin[i],Zin[i],&Xout[i],&Yout[i],&Zout[i]);
	}
	return;
}

void GSMtoGSEUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSMtoGSEArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void GSMtoSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		GSWtoSM(Xin[i],Yin[i],Zin[i],&Xout[i],&Yout[i],&Zout[i]);
	}
	return;
}

void GSMtoSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSMtoSMArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void SMtoGSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		SMtoGSW(Xin[i],Yin[i],Zin[i],&Xout[i],&Yout[i],&Zout[i]);
	}
	return;
}

void SMtoGSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	SMtoGSMArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void GSEtoSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X[n],Y[n],Z[n], Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		GSEtoGSW(Xin[i],Yin[i],Zin[i],&X[i],&Y[i],&Z[i]);
		GSWtoSM(X[i],Y[i],Z[i],&Xout[i],&Yout[i],&Zout[i]);
	}

	return;
}

void GSEtoSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoSMArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void GSEtoMAGArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X[n],Y[n],Z[n], Xt, Yt, Zt, Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		GSEtoGSW(Xin[i],Yin[i],Zin[i],&X[i],&Y[i],&Z[i]);
		GSWtoSM(X[i],Y[i],Z[i],&Xt,&Yt,&Zt);
		SMtoMAG(Xt,Yt,Zt,&Xout[i],&Yout[i],&Zout[i]);
	}

	return;
}

void GSEtoMAGUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoMAGArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void SMtoGSEArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X[n],Y[n],Z[n], Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		SMtoGSW(Xin[i],Yin[i],Zin[i],&X[i],&Y[i],&Z[i]);
		GSWtoGSE(X[i],Y[i],Z[i],&Xout[i],&Yout[i],&Zout[i]);
	}

	return;
}

void SMtoGSEUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	SMtoGSEArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void MAGtoGSEArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X,Y,Z, Xt, Yt, Zt, Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		MAGtoSM(Xin[i],Yin[i],Zin[i],&Xt,&Yt,&Zt);
		SMtoGSW(Xt,Yt,Zt,&X,&Y,&Z);
		GSWtoGSE(X,Y,Z,&Xout[i],&Yout[i],&Zout[i]);
	}

	return;
}

void MAGtoGSEUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MAGtoGSEArray(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}



void MLONtoMLT(double *MLon, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLT) {

	double X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;


	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(MLon[i]*M_PI/180.0);
		Y0 = sin(MLon[i]*M_PI/180.0);
		Z0 = 0.0;
		MAGtoSM(X0,Y0,Z0,&X1,&Y1,&Z1);
		SMtoGSW(X1,Y1,Z1,&X0,&Y0,&Z0);
		MLT[i] =  atan2(-Y0,-X0)*180.0/(M_PI*15.0);
	}

	return;
}

void MLTtoMLON(double *MLT, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon) {

	double X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;


	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(MLT[i]*M_PI/12.0);
		Y0 = sin(MLT[i]*M_PI/12.0);
		Z0 = 0.0;
		GSWtoSM(X0,Y0,Z0,&X1,&Y1,&Z1);
		SMtoMAG(X1,Y1,Z1,&X0,&Y0,&Z0);
		MLon[i] =  atan2(-Y0,-X0)*180.0/(M_PI);
	}

	return;
}

void MLONtoMLTUT(double *MLon, int n, int date,double UT, double *MLT) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MLONtoMLT(MLon,n,Year,DayNo,Hr,Mn,Sc,MLT);
	
	
}

void MLTtoMLONUT(double *MLT, int n, int date,double UT, double *MLon) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MLTtoMLON(MLT,n,Year,DayNo,Hr,Mn,Sc,MLon);
}

void GEOtoMAGArray(double *Lon, double *Lat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon, double *MLat) {

	double X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(Lon[i]*M_PI/180.0)*cos(Lat[i]*M_PI/180.0);
		Y0 = sin(Lon[i]*M_PI/180.0)*cos(Lat[i]*M_PI/180.0);
		Z0 = sin(Lat[i]*M_PI/180.0);
		GEOtoMAG(X0,Y0,Z0,&X1,&Y1,&Z1);
		MLon[i] = atan2(Y1,X1)*180.0/M_PI;
		MLat[i] = asin(Z1)*180.0/M_PI;
	}

	
	return;
}


void GEOtoMAGUTArray(double *Lon, double *Lat, int n, int date,double UT, double *MLon, double *MLat) {

	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GEOtoMAGArray(Lon,Lat,n,Year,DayNo,Hr,Mn,Sc,MLon,MLat);
	
	
	return;
}


void MAGtoGEOArray(double *MLon, double *MLat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Lon, double *Lat) {

	double X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	Recalc(Year,DayNo,Hr,Mn,Sc,Vx,Vy,Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(MLon[i]*M_PI/180.0)*cos(MLat[i]*M_PI/180.0);
		Y0 = sin(MLon[i]*M_PI/180.0)*cos(MLat[i]*M_PI/180.0);
		Z0 = sin(MLat[i]*M_PI/180.0);
		MAGtoGEO(X0,Y0,Z0,&X1,&Y1,&Z1);
		Lon[i] = atan2(Y1,X1)*180.0/M_PI;
		Lat[i] = asin(Z1)*180.0/M_PI;
	}
	
}


void MAGtoGEOUTArray(double *MLon, double *MLat, int n, int date,double UT, double *Lon, double *Lat) {

	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MAGtoGEOArray(MLon,MLat,n,Year,DayNo,Hr,Mn,Sc,Lon,Lat);
}


