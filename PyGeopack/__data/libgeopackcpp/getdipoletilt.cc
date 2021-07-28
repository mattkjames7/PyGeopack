#include "getdipoletilt.h"

double GetDipoleTilt(int Year, int Doy, int Hr, int Mn, double Vx, double Vy, double Vz) {
	double psi;
	int Sc = 0;

	recalc_08_(&Year,&Doy,&Hr,&Mn,&Sc,&Vx,&Vy,&Vz);

	psi = getpsi_();
	return psi;
}

double GetDipoleTiltUT(int Date, float ut, double Vx, double Vy, double Vz) {
	int Year, Doy, Hr, Mn, Sc;
	double Ms, utd = (double) ut, vx, vy, vz;
	/*convert date into Year and DayNo*/
	DayNo(1,&Date,&Year,&Doy);

	TData->GetSWVelocity(1,&Date,&ut,&Vx,&Vy,&Vz,&vx,&vy,&vz);
	
	if (isnan(vx)) {
		vx = -428.0;
	}
	if (isnan(vy)) {
		vy = -1.4;
	}
	if (isnan(vz)) {
		vz = 0.0;
	}
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);	
		
	return GetDipoleTilt(Year,Doy,Hr,Mn,vx,vy,vz);
		
}
