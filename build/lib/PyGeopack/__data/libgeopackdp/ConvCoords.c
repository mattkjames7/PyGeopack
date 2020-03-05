#include "ConvCoords.h"

void GSEtoGSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;


	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		gswgse_08_(&Xout[i],&Yout[i],&Zout[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
	}
	return;
}

void GSEtoGSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoGSM(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void GSMtoGSE(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		gswgse_08_(&Xin[i],&Yin[i],&Zin[i],&Xout[i],&Yout[i],&Zout[i],&dirp);
	}
	return;
}

void GSMtoGSEUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSMtoGSE(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void GSMtoSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		smgsw_08_(&Xout[i],&Yout[i],&Zout[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
	}
	return;
}

void GSMtoSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSMtoSM(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void SMtoGSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;


	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&Xout[i],&Yout[i],&Zout[i],&dirp);
	}
	return;
}

void SMtoGSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	SMtoGSM(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void GSEtoSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X[n],Y[n],Z[n];
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		gswgse_08_(&X[i],&Y[i],&Z[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
		smgsw_08_(&Xout[i],&Yout[i],&Zout[i],&X[i],&Y[i],&Z[i],&dirn);
	}
	return;
}

void GSEtoSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoSM(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void GSEtoMAG(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X[n],Y[n],Z[n], Xt, Yt, Zt;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		gswgse_08_(&X[i],&Y[i],&Z[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
		smgsw_08_(&Xt,&Yt,&Zt,&X[i],&Y[i],&Z[i],&dirn);
		magsm_08_(&Xout[i],&Yout[i],&Zout[i],&Xt,&Yt,&Zt,&dirn);
	}
	return;
}

void GSEtoMAGUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoMAG(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void SMtoGSE(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X[n],Y[n],Z[n];
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&X[i],&Y[i],&Z[i],&dirp);
		gswgse_08_(&X[i],&Y[i],&Z[i],&Xout[i],&Yout[i],&Zout[i],&dirp);
	}
	return;
}

void SMtoGSEUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	SMtoGSE(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void MAGtoGSE(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) {

	double X,Y,Z, Xt, Yt, Zt;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		magsm_08_(&Xin[i],&Yin[i],&Zin[i],&Zt,&Yt,&Zt,&dirp);
		smgsw_08_(&Xt,&Yt,&Zt,&X,&Y,&Z,&dirp);
		gswgse_08_(&X,&Y,&Z,&Xout[i],&Yout[i],&Zout[i],&dirp);	
	}
	return;
}

void MAGtoGSEUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MAGtoGSE(Xin,Yin,Zin,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}



void MLONtoMLT(double *MLon, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLT) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(MLon[i]*M_PI/180.0);
		Y0 = sin(MLon[i]*M_PI/180.0);
		Z0 = 0.0;
		magsm_08_(&X0,&Y0,&Z0,&X1,&Y1,&Z1,&dirp);
		smgsw_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirp);
		MLT[i] =  atan2f(-Y0,-X0)*180.0/(M_PI*15.0);
	}
	return;
}

void MLTtoMLON(double *MLT, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(MLT[i]*M_PI/12.0);
		Y0 = sin(MLT[i]*M_PI/12.0);
		Z0 = 0.0;
		smgsw_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirn);
		magsm_08_(&X0,&Y0,&Z0,&X1,&Y1,&Z1,&dirn);
	
		MLon[i] =  atan2f(-Y0,-X0)*180.0/(M_PI);
	}
	return;
}

void MLONtoMLTUT(double *MLon, int n, double Vx, double Vy, double Vz, int date,float UT, double *MLT) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MLONtoMLT(MLon,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,MLT);
	
	
}

void MLTtoMLONUT(double *MLT, int n, double Vx, double Vy, double Vz, int date,float UT, double *MLon) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MLTtoMLON(MLT,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,MLon);
}

void GEOtoMAG(double *Lon, double *Lat, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon, double *MLat) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int i;
	int dirp = 1, dirn = -1;
	
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(Lon[i]*M_PI/180.0)*cos(Lat[i]*M_PI/180.0);
		Y0 = sin(Lon[i]*M_PI/180.0)*cos(Lat[i]*M_PI/180.0);
		Z0 = sin(Lat[i]*M_PI/180.0);
		geomag_08_(&X0,&Y0,&Z0,&X1,&Y1,&Z1,&dirp);
		MLon[i] = atan2f(Y1,X1)*180.0/M_PI;
		MLat[i] = asin(Z1)*180.0/M_PI;
	}
	
	
	return;
}


void GEOtoMAGUT(double *Lon, double *Lat, int n, double Vx, double Vy, double Vz, int date,float UT, double *MLon, double *MLat) {

	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GEOtoMAG(Lon,Lat,n,Vx,Vy,Vz,Year,DayNo,Hr,Mn,Sc,MLon,MLat);
	
	
	return;
}


void MAGtoGEO(double *MLon, double *MLat, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Lon, double *Lat) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int i;
	int dirp = 1, dirn = -1;
	
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		X0 = cos(MLon[i]*M_PI/180.0)*cos(MLat[i]*M_PI/180.0);
		Y0 = sin(MLon[i]*M_PI/180.0)*cos(MLat[i]*M_PI/180.0);
		Z0 = sin(MLat[i]*M_PI/180.0);
		geomag_08_(&X0,&Y0,&Z0,&X1,&Y1,&Z1,&dirn);
		Lon[i] = atan2f(Y1,X1)*180.0/M_PI;
		Lat[i] = asin(Z1)*180.0/M_PI;
	}
	
}


void MAGtoGEOUT(double *MLon, double *MLat, int n, double Vx, double Vy, double Vz, int date,float UT, double *Lon, double *Lat) {

	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);

	/*Get velocity if needed*/
	if (isnan(Vx)) { 
		GetSWVelocity(date,UT,NULL,&Vx,&Vy,&Vz);
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MAGtoGEO(MLon,MLat,Vx,Vy,Vz,n,Year,DayNo,Hr,Mn,Sc,Lon,Lat);
}


