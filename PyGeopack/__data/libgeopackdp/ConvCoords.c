#include "ConvCoords.h"


/***********************************************************************
 * NAME : 			void GSEtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSE coordinate (R_E)
 * 		double	Yin		y GSE coordinate (R_E)
 * 		double	Zin		z GSE coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSM coordinate (R_E)
 * 		double	*Yout	y GSM coordinate (R_E)
 * 		double	*Zout	z GSM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GSM coordinates
 * 
 * ********************************************************************/
void GSEtoGSM(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;


	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

/***********************************************************************
 * NAME : 			void GSEtoGSM(	*Xin, *Yin, *Zin, n, *Vx, *Vy, *Vz, 
 * 									*Date, *ut, recalc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	*Xin	Array of x GSE coordinates (R_E)
 * 		double	*Yin	Array of y GSE coordinates (R_E)
 * 		double	*Zin	Array of z GSE coordinates (R_E)
 * 		int		n		Number of vectors to convert
 * 		double 	*Vx		Array of x-component of SW velocity in km/s
 * 		double 	*Vy		Array of y-component of SW velocity in km/s
 * 		double 	*Vz		Array of z-component of SW velocity in km/s
 * 		int 	*Date	Array of dates, format yyyymmdd
 * 		float 	*ut		Array of times in hours
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSM coordinate (R_E)
 * 		double	*Yout	y GSM coordinate (R_E)
 * 		double	*Zout	z GSM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Converts Dates into years and day numbers
 * 		[2] Converts UT hours to hours, minutes and seconds
 * 		[3] Finds appropriate SW velocity vectors if none are defined
 * 		[4] Calls GSEtoGSM wrapper to convert coordinates
 * 
 * ********************************************************************/
void GSEtoGSMUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {
	
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GSEtoGSM(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
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


