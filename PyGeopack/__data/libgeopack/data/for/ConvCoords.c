#include "ConvCoords.h"

void GSEtoGSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		gswgse_08_(&Xout[i],&Yout[i],&Zout[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
	}
	return;
}

void GSEtoGSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoGSM(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void GSMtoGSE(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		gswgse_08_(&Xin[i],&Yin[i],&Zin[i],&Xout[i],&Yout[i],&Zout[i],&dirp);
	}
	return;
}

void GSMtoGSEUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSMtoGSE(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void GSMtoSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		smgsw_08_(&Xout[i],&Yout[i],&Zout[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
	}
	return;
}

void GSMtoSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSMtoSM(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void SMtoGSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float Vx=-400.0, Vy=0.0, Vz=0.0;
	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	for (i=0;i<n;i++) {
		smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&Xout[i],&Yout[i],&Zout[i],&dirp);
	}
	return;
}

void SMtoGSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	SMtoGSM(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void GSEtoSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float X[n],Y[n],Z[n], Vx=-400.0, Vy=0.0, Vz=0.0;
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

void GSEtoSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoSM(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}

void GSEtoMAG(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float X[n],Y[n],Z[n], Xt, Yt, Zt, Vx=-400.0, Vy=0.0, Vz=0.0;
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

void GSEtoMAGUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GSEtoMAG(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void SMtoGSE(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float X[n],Y[n],Z[n], Vx=-400.0, Vy=0.0, Vz=0.0;
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

void SMtoGSEUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	SMtoGSE(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}


void MAGtoGSE(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) {

	float X,Y,Z, Xt, Yt, Zt, Vx=-400.0, Vy=0.0, Vz=0.0;
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

void MAGtoGSEUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MAGtoGSE(Xin,Yin,Zin,n,Year,DayNo,Hr,Mn,Sc,Xout,Yout,Zout);
}



void MLONtoMLT(float *MLon, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *MLT) {

	float X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
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

void MLTtoMLON(float *MLT, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *MLon) {

	float X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
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

void MLONtoMLTUT(float *MLon, int n, int date,float UT, float *MLT) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MLONtoMLT(MLon,n,Year,DayNo,Hr,Mn,Sc,MLT);
	
	
}

void MLTtoMLONUT(float *MLT, int n, int date,float UT, float *MLon) {
	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MLTtoMLON(MLT,n,Year,DayNo,Hr,Mn,Sc,MLon);
}

void GEOtoMAG(float *Lon, float *Lat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *MLon, float *MLat) {

	float X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
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


void GEOtoMAGUT(float *Lon, float *Lat, int n, int date,float UT, float *MLon, float *MLat) {

	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	GEOtoMAG(Lon,Lat,n,Year,DayNo,Hr,Mn,Sc,MLon,MLat);
	
	
	return;
}


void MAGtoGEO(float *MLon, float *MLat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Lon, float *Lat) {

	float X0,Y0,Z0,X1,Y1,Z1, Vx=-400.0, Vy=0.0, Vz=0.0;
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


void MAGtoGEOUT(float *MLon, float *MLat, int n, int date,float UT, float *Lon, float *Lat) {

	int Year, DayNo, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(UT,&Hr,&Mn,&Sc);
	
	MAGtoGEO(MLon,MLat,n,Year,DayNo,Hr,Mn,Sc,Lon,Lat);
}


