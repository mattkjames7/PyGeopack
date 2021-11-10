#include "ConvCoords.h"

ConvFunc ConvFuncs[6][6];
bool ConvFuncsLoaded = false;
const char *CoordAbr[6];
/***********************************************************************
 * NAME : 			void GSEtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
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
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;


	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
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
	
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSEtoGSM,Xout,Yout,Zout);
}

/***********************************************************************
 * NAME : 			void GSMtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to GSE coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
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
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to GSE coordinates
 * 
 * ********************************************************************/
void GSMtoGSE(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc,
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);

	return;
}

void GSMtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {
						
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSMtoGSE,Xout,Yout,Zout);
	
}

/***********************************************************************
 * NAME : 			void GSMtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to SM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
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
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to SM coordinates
 * 
 * ********************************************************************/
void GSMtoSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);

	return;
}

void GSMtoSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz,
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSMtoSM,Xout,Yout,Zout);
}

/***********************************************************************
 * NAME : 			void SMtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
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
 * 		[2] Calls FORTRAN code to convert from SM to GSM coordinates
 * 
 * ********************************************************************/
void SMtoGSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int dirp = 1, dirn = -1;


	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	
	/* do the coordinate conversion */
	smgsw_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void SMtoGSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz, 
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&SMtoGSM,Xout,Yout,Zout);
	
}

/***********************************************************************
 * NAME : 			void GSEtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to SM coordinates
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
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GSM coordinates
 * 		[3] Calls FORTRAN code to convert from GSM to SM coordinates
 * 
 * ********************************************************************/
void GSEtoSM(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&X,&Y,&Z,&Xin,&Yin,&Zin,&dirn);
	smgsw_08_(Xout,Yout,Zout,&X,&Y,&Z,&dirn);
	
	return;
}

void GSEtoSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz, 
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSEtoSM,Xout,Yout,Zout);
}

/***********************************************************************
 * NAME : 			void GSEtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to MAG coordinates
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
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GSM coordinates
 * 		[3] Calls FORTRAN code to convert from GSM to SM coordinates
 * 		[4] Calls FORTRAN code to convert from SM to MAG coordinates
 * 
 * ********************************************************************/
void GSEtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z,Xt,Yt,Zt;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&X,&Y,&Z,&Xin,&Yin,&Zin,&dirn);
	smgsw_08_(&Xt,&Yt,&Zt,&X,&Y,&Z,&dirn);
	magsm_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void GSEtoMAGUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut,
					double *Xout, double *Yout, double *Zout) {
						
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSEtoMAG,Xout,Yout,Zout);
}

/***********************************************************************
 * NAME : 			void SMtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
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
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to GSM coordinates
 * 		[3] Calls FORTRAN code to convert from GSM to GSE coordinates
 * 
 * ********************************************************************/
void SMtoGSE(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(&Xin,&Yin,&Zin,&X,&Y,&Z,&dirp);
	gswgse_08_(&X,&Y,&Z,Xout,Yout,Zout,&dirp);

	return;
}

void SMtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz,
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&SMtoGSE,Xout,Yout,Zout);
	
}

/***********************************************************************
 * NAME : 			void MAGtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to GSE coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
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
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to SM coordinates
 * 		[3] Calls FORTRAN code to convert from SM to GSM coordinates
 * 		[4] Calls FORTRAN code to convert from GSM to GSE coordinates
 * 
 * ********************************************************************/
void MAGtoGSE(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z,Xt,Yt,Zt;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	magsm_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	smgsw_08_(&Xt,&Yt,&Zt,&X,&Y,&Z,&dirp);
	gswgse_08_(&X,&Y,&Z,Xout,Yout,Zout,&dirp);	
	
	return;
}

void MAGtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {
						
	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&MAGtoGSE,Xout,Yout,Zout);
	
}


/***********************************************************************
 * NAME : 			void MLONtoMLT(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MLON to MLT coordinates
 * 
 * INPUTS : 
 * 		double	MLon	Magnetic longitude (degrees)
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
 * 		double	*MLT	Magnetic Local Time (hours)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to SM coordinates
 * 
 * ********************************************************************/
void MLONtoMLT(	double MLon, double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, double *MLT) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	X0 = cos(MLon*M_PI/180.0);
	Y0 = sin(MLon*M_PI/180.0);
	Z0 = 0.0;
	magsm_08_(&X0,&Y0,&Z0,&X1,&Y1,&Z1,&dirp);
	//smgsw_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirp);
	//MLT[0] =  atan2f(-Y0,-X0)*180.0/(M_PI*15.0);
	MLT[0] =  fmod(atan2f(-Y1,-X1)*180.0/(M_PI*15.0) + 24.0,24.0);
	
	return;
}

/***********************************************************************
 * NAME : 			void MLTtoMLON(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MLT to MLON coordinates
 * 
 * INPUTS : 
 * 		double	MLT	Magnetic Local Time (hours)
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
 * 		double	*MLon	Magnetic longitude (degrees)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to MAG coordinates
 * 
 * ********************************************************************/
void MLTtoMLON(	double MLT, double Vx, double Vy, double Vz, int recalc, 
				int Year, int DyNo, int Hr, int Mn, int Sc, double *MLon) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	X0 = cos(MLT*M_PI/12.0);
	Y0 = sin(MLT*M_PI/12.0);
	Z0 = 0.0;
	//smgsw_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirn);
	magsm_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirn);
	//MLon[0] =  atan2f(-Y0,-X0)*180.0/(M_PI);
	MLon[0] =  atan2f(-Y1,-X1)*180.0/(M_PI);
	
	return;
}

void MLONtoMLTUT(	double *MLon, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, double *MLT) {
						
	int Year, DyNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp, Ms, utd; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DayNo(1,&Date[i],&Year,&DyNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			utd = (double) ut[i];
			DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		CheckV(Date[i],ut[i],Vx[i],Vy[i],Vz[i],&vx,&vy,&vz);
		
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MLONtoMLT(	MLon[i],vx,vy,vz,recalc,
					Year,DyNo,Hr,Mn,Sc,
					&MLT[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	

	
}

void MLTtoMLONUT(	double *MLT, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, double *MLon) {
						
	int Year, DyNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp, Ms, utd; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DayNo(1,&Date[i],&Year,&DyNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			utd = (double) ut[i];
			DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);
						
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		CheckV(Date[i],ut[i],Vx[i],Vy[i],Vz[i],&vx,&vy,&vz);
		
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MLTtoMLON(	MLT[i],vx,vy,vz,recalc,
					Year,DyNo,Hr,Mn,Sc,
					&MLon[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	
}


/***********************************************************************
 * NAME : 			void GEOtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to MAG coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
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
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEO to MAG coordinates
 * 
 * ********************************************************************/
void GEOtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geomag_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEOtoMAG_LL(	double Lon, double Lat, 
					double Vx, double Vy, double Vz, int recalc,
					int Year, int DyNo, int Hr, int Mn, int Sc, 
					double *MLon, double *MLat) {

	double X0,Y0,Z0,X1,Y1,Z1;

	/* convert to cartesian */
	X0 = cos(Lon*M_PI/180.0)*cos(Lat*M_PI/180.0);
	Y0 = sin(Lon*M_PI/180.0)*cos(Lat*M_PI/180.0);
	Z0 = sin(Lat*M_PI/180.0);
	
	/* convert coordinates */
	GEOtoMAG(	X0,Y0,Z0,Vx,Vy,Vz,recalc,
				DyNo,Year,Hr,Mn,Sc,
				&X1,&Y1,&Z1);
	
	/* convert back to lat and lon*/			
	MLon[0] = atan2f(Y1,X1)*180.0/M_PI;
	MLat[0] = asin(Z1)*180.0/M_PI;
	
	return;
}


void GEOtoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEOtoMAG,Xout,Yout,Zout);
}


void GEOtoMAGUT_LL(	double *Lon, double *Lat, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, 
					double *MLon, double *MLat) {

	int Year, DyNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp, utd, Ms; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DayNo(1,&Date[i],&Year,&DyNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			utd = (double) ut[i];
			DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);
						
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		CheckV(Date[i],ut[i],Vx[i],Vy[i],Vz[i],&vx,&vy,&vz);
		
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GEOtoMAG_LL(	Lon[i],Lat[i],
						vx,vy,vz,recalc,
						Year,DyNo,Hr,Mn,Sc,
						&MLon[i],&MLat[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}

/***********************************************************************
 * NAME : 			void MAGtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to GEO coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
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
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to GEO coordinates
 * 
 * ********************************************************************/
void MAGtoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geomag_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

void MAGtoGEO_LL(	double MLon, double MLat, 
					double Vx, double Vy, double Vz, int recalc,
					int Year, int DyNo, int Hr, int Mn, int Sc, 
					double *Lon, double *Lat) {

	double X0,Y0,Z0,X1,Y1,Z1;

	/* convert to cartesian */
	X0 = cos(MLon*M_PI/180.0)*cos(MLat*M_PI/180.0);
	Y0 = sin(MLon*M_PI/180.0)*cos(MLat*M_PI/180.0);
	Z0 = sin(MLat*M_PI/180.0);
	
	/* convert coordinates */
	MAGtoGEO(	X0,Y0,Z0,Vx,Vy,Vz,recalc,
				DyNo,Year,Hr,Mn,Sc,
				&X1,&Y1,&Z1);
	
	/* convert back to lat and lon*/			
	Lon[0] = atan2f(Y1,X1)*180.0/M_PI;
	Lat[0] = asin(Z1)*180.0/M_PI;
	
	return;
}



void MAGtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&MAGtoGEO,Xout,Yout,Zout);
}


void MAGtoGEOUT_LL(	double *MLon, double *MLat, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, 
					double *Lon, double *Lat) {

	int Year, DyNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp, Ms, utd; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DayNo(1,&Date[i],&Year,&DyNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			utd = (double) ut[i];
			DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);
						
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		CheckV(Date[i],ut[i],Vx[i],Vy[i],Vz[i],&vx,&vy,&vz);
		
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MAGtoGEO_LL(	MLon[i],MLat[i],
						vx,vy,vz,recalc,
						Year,DyNo,Hr,Mn,Sc,
						&Lon[i],&Lat[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}






/***********************************************************************
 * NAME : 			void GEItoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEI to GEO coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEI coordinate (R_E)
 * 		double	Yin		y GEI coordinate (R_E)
 * 		double	Zin		z GEI coordinate (R_E)
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
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEI to GEO coordinates
 * 
 * ********************************************************************/
void GEItoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEItoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEItoGEO,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEOtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to GEI coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
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
 * 		double	*Xout	x GEI coordinate (R_E)
 * 		double	*Yout	y GEI coordinate (R_E)
 * 		double	*Zout	z GEI coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEO to GEI coordinates
 * 
 * ********************************************************************/
void GEOtoGEI(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

void GEOtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEOtoGEI,Xout,Yout,Zout);
}






/***********************************************************************
 * NAME : 			void GSMtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to GEO coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
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
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to GEO coordinates
 * 
 * ********************************************************************/
void GSMtoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geogsw_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

void GSMtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSMtoGEO,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEOtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
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
 * 		[2] Calls FORTRAN code to convert from GEO to GSM coordinates
 * 
 * ********************************************************************/
void GEOtoGSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geogsw_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEOtoGSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEOtoGSM,Xout,Yout,Zout);
}







/***********************************************************************
 * NAME : 			void GSEtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GEO coordinates
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
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GEO coordinates
 * 
 * ********************************************************************/
void GSEtoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&Xt,&Yt,&Zt,&Xin,&Yin,&Zin,&dirn);
	geogsw_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void GSEtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSEtoGEO,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEOtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to GSE coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
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
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEO to GSE coordinates
 * 
 * ********************************************************************/
void GEOtoGSE(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geogsw_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	gswgse_08_(&Xt,&Yt,&Zt,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEOtoGSEUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEOtoGSE,Xout,Yout,Zout);
}






/***********************************************************************
 * NAME : 			void SMtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting SM to GEO coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
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
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to GEO coordinates
 * 
 * ********************************************************************/
void SMtoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	geogsw_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void SMtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&SMtoGEO,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEOtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to SM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
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
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEO to SM coordinates
 * 
 * ********************************************************************/
void GEOtoSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geogsw_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	smgsw_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void GEOtoSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEOtoSM,Xout,Yout,Zout);
}




/***********************************************************************
 * NAME : 			void GSEtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GEI coordinates
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
 * 		double	*Xout	x GEI coordinate (R_E)
 * 		double	*Yout	y GEI coordinate (R_E)
 * 		double	*Zout	z GEI coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GEI coordinates
 * 
 * ********************************************************************/
void GSEtoGEI(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt0, Yt0, Zt0, Xt1, Yt1, Zt1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&Xt0,&Yt0,&Zt0,&Xin,&Yin,&Zin,&dirn);
	geogsw_08_(&Xt1,&Yt1,&Zt1,&Xt0,&Yt0,&Zt0,&dirn);
	geigeo_08_(Xout,Yout,Zout,&Xt1,&Yt1,&Zt1,&dirn);
	
	return;
}

void GSEtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSEtoGEI,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEItoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEI to GSE coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEI coordinate (R_E)
 * 		double	Yin		y GEI coordinate (R_E)
 * 		double	Zin		z GEI coordinate (R_E)
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
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEI to GSE coordinates
 * 
 * ********************************************************************/
void GEItoGSE(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt0, Yt0, Zt0, Xt1, Yt1, Zt1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,&Xt0,&Yt0,&Zt0,&dirp);
	geogsw_08_(&Xt0,&Yt0,&Zt0,&Xt1,&Yt1,&Zt1,&dirp);
	gswgse_08_(&Xt1,&Yt1,&Zt1,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEItoGSEUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEItoGSE,Xout,Yout,Zout);
}


/***********************************************************************
 * NAME : 			void GSMtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to GEI coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
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
 * 		double	*Xout	x GEI coordinate (R_E)
 * 		double	*Yout	y GEI coordinate (R_E)
 * 		double	*Zout	z GEI coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to GEI coordinates
 * 
 * ********************************************************************/
void GSMtoGEI(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geogsw_08_(&Xt,&Yt,&Zt,&Xin,&Yin,&Zin,&dirn);
	geigeo_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void GSMtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSMtoGEI,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEItoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEI to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEI coordinate (R_E)
 * 		double	Yin		y GEI coordinate (R_E)
 * 		double	Zin		z GEI coordinate (R_E)
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
 * 		[2] Calls FORTRAN code to convert from GEI to GSM coordinates
 * 
 * ********************************************************************/
void GEItoGSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	geogsw_08_(&Xt,&Yt,&Zt,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEItoGSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEItoGSM,Xout,Yout,Zout);
}


/***********************************************************************
 * NAME : 			void SMtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting SM to GEI coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
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
 * 		double	*Xout	x GEI coordinate (R_E)
 * 		double	*Yout	y GEI coordinate (R_E)
 * 		double	*Zout	z GEI coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to GEI coordinates
 * 
 * ********************************************************************/
void SMtoGEI(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt0, Yt0, Zt0, Xt1, Yt1, Zt1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(&Xin,&Yin,&Zin,&Xt0,&Yt0,&Zt0,&dirp);
	geogsw_08_(&Xt1,&Yt1,&Zt1,&Xt0,&Yt0,&Zt0,&dirn);
	geigeo_08_(Xout,Yout,Zout,&Xt1,&Yt1,&Zt1,&dirn);
	
	return;
}

void SMtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&SMtoGEI,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEItoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEI to SM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEI coordinate (R_E)
 * 		double	Yin		y GEI coordinate (R_E)
 * 		double	Zin		z GEI coordinate (R_E)
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
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEI to SM coordinates
 * 
 * ********************************************************************/
void GEItoSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt0, Yt0, Zt0, Xt1, Yt1, Zt1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,&Xt0,&Yt0,&Zt0,&dirp);
	geogsw_08_(&Xt0,&Yt0,&Zt0,&Xt1,&Yt1,&Zt1,&dirp);
	smgsw_08_(Xout,Yout,Zout,&Xt1,&Yt1,&Zt1,&dirn);
	
	return;
}

void GEItoSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEItoSM,Xout,Yout,Zout);
}









/***********************************************************************
 * NAME : 			void MAGtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to GEI coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
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
 * 		double	*Xout	x GEI coordinate (R_E)
 * 		double	*Yout	y GEI coordinate (R_E)
 * 		double	*Zout	z GEI coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to GEI coordinates
 * 
 * ********************************************************************/
void MAGtoGEI(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geomag_08_(&Xt,&Yt,&Zt,&Xin,&Yin,&Zin,&dirn);
	geigeo_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void MAGtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&MAGtoGEI,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GEItoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEI to MAG coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEI coordinate (R_E)
 * 		double	Yin		y GEI coordinate (R_E)
 * 		double	Zin		z GEI coordinate (R_E)
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
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEI to MAG coordinates
 * 
 * ********************************************************************/
void GEItoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	geomag_08_(&Xt,&Yt,&Zt,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEItoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GEItoMAG,Xout,Yout,Zout);
}




/***********************************************************************
 * NAME : 			void MAGtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
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
 * 		[2] Calls FORTRAN code to convert from MAG to GSM coordinates
 * 
 * ********************************************************************/
void MAGtoGSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	magsm_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	smgsw_08_(&Xt,&Yt,&Zt,Xout,Yout,Zout,&dirp);
	
	return;
}

void MAGtoGSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&MAGtoGSM,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void GSMtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to MAG coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
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
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to MAG coordinates
 * 
 * ********************************************************************/
void GSMtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	double Xt, Yt, Zt;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(&Xt,&Yt,&Zt,&Xin,&Yin,&Zin,&dirn);
	magsm_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void GSMtoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&GSMtoMAG,Xout,Yout,Zout);
}





/***********************************************************************
 * NAME : 			void MAGtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to SM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
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
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to SM coordinates
 * 
 * ********************************************************************/
void MAGtoSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	magsm_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void MAGtoSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&MAGtoSM,Xout,Yout,Zout);
}



/***********************************************************************
 * NAME : 			void SMtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DyNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting SM to MAG coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
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
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to MAG coordinates
 * 
 * ********************************************************************/
void SMtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DyNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	magsm_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

void SMtoMAGUT(		double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	XXXtoYYYUT(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,&SMtoMAG,Xout,Yout,Zout);

}

void XXXtoYYYUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, ConvFuncHMS CF,
					double *Xout, double *Yout, double *Zout) {
	
	int Year, DyNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc = 0;
	double vx = 0.0, vy = 0.0, vz = 0.0, vxp = -1.0, vyp = -1.0, vzp = -1.0, Ms, utd; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DayNo(1,&Date[i],&Year,&DyNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			utd = (double) ut[i];
			DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);
						
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		CheckV(Date[i],ut[i],Vx[i],Vy[i],Vz[i],&vx,&vy,&vz);
		
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		CF(			Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DyNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}	



void _PopulateConvFuncs() {
	
	if (!ConvFuncsLoaded) {
		CoordAbr[0] = "GSE";
		CoordAbr[1] = "GSM";
		CoordAbr[2] = "SM";
		CoordAbr[3] = "GEO";
		CoordAbr[4] = "MAG";
		CoordAbr[5] = "GEI";
		
		
		ConvFuncs[0][1] = &GSEtoGSMUT;
		ConvFuncs[0][2] = &GSEtoSMUT;
		ConvFuncs[0][3] = &GSEtoGEOUT;
		ConvFuncs[0][4] = &GSEtoMAGUT;
		ConvFuncs[0][5] = &GSEtoGEIUT;

		ConvFuncs[1][0] = &GSMtoGSEUT;
		ConvFuncs[1][2] = &GSMtoSMUT;
		ConvFuncs[1][3] = &GSMtoGEOUT;
		ConvFuncs[1][4] = &GSMtoMAGUT;
		ConvFuncs[1][5] = &GSMtoGEIUT;

		ConvFuncs[2][0] = &SMtoGSEUT;
		ConvFuncs[2][1] = &SMtoGSMUT;
		ConvFuncs[2][3] = &SMtoGEOUT;
		ConvFuncs[2][4] = &SMtoMAGUT;
		ConvFuncs[2][5] = &SMtoGEIUT;

		ConvFuncs[3][0] = &GEOtoGSEUT;
		ConvFuncs[3][1] = &GEOtoGSMUT;
		ConvFuncs[3][2] = &GEOtoSMUT;
		ConvFuncs[3][4] = &GEOtoMAGUT;
		ConvFuncs[3][5] = &GEOtoGEIUT;

		ConvFuncs[4][0] = &MAGtoGSEUT;
		ConvFuncs[4][1] = &MAGtoGSMUT;
		ConvFuncs[4][2] = &MAGtoSMUT;
		ConvFuncs[4][3] = &MAGtoGEOUT;
		ConvFuncs[4][5] = &MAGtoGEIUT;

		ConvFuncs[5][0] = &GEItoGSEUT;
		ConvFuncs[5][1] = &GEItoGSMUT;
		ConvFuncs[5][2] = &GEItoSMUT;
		ConvFuncs[5][3] = &GEItoGEOUT;
		ConvFuncs[5][4] = &GEItoMAGUT;
	}
	
	ConvFuncsLoaded = true;

}


void ConvCoords(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vxin, double *Vyin, double *Vzin, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout,
					const char *CoordIn, const char *CoordOut) {
	
	/* check that the function pointer array has been populated */			
	_PopulateConvFuncs();
	
	/* Get SW Velocity if NULL provided */
	double *Vx, *Vy, *Vz;
	bool delvx = false, delvy = false, delvz = false;
	if (Vxin == NULL) {
		Vx = new double[n];
		delvx = true;
		TData->GetVx(n,Date,ut,Vx);
	} else {
		Vx = Vxin;
	}
	if (Vyin == NULL) {
		Vy = new double[n];
		delvy = true;
		TData->GetVy(n,Date,ut,Vy);
	} else {
		Vy = Vyin;
	}
	if (Vzin == NULL) {
		Vz = new double[n];
		delvz = true;
		TData->GetVz(n,Date,ut,Vz);
	} else {
		Vz = Vzin;
	}
	
	/* get the indices for input and output functions */
	int i, iIn = -1, iOut = -1;
	for(i=0;i<6;i++) {
		if ((iIn > -1) && (iOut > -1)) {
			break;
		}
		if (iIn == -1) {
			if (strcmp(CoordAbr[i],CoordIn) == 0) {
				iIn = i;
			}
		}
		if (iOut == -1) {
			if (strcmp(CoordAbr[i],CoordOut) == 0) {
				iOut = i;
			}
		}
	}
	if (iIn != iOut) {
		/* Get the function */
		ConvFunc CF = ConvFuncs[iIn][iOut];
		
		/* Call the function */
		CF(Xin,Yin,Zin,n,Vx,Vy,Vz,Date,ut,Xout,Yout,Zout);
	} else {
		/* copy the values straight across */
		for (i=0;i<n;i++) {
			Xout[i] = Xin[i];
			Yout[i] = Yin[i];
			Zout[i] = Zin[i];
		}
	}	
	
	if (delvx) {
		delete[] Vx;
	}
	if (delvy) {
		delete[] Vy;
	}
	if (delvz) {
		delete[] Vz;
	}
	
}
