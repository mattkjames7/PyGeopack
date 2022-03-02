#include <stdio.h>
#define _USE_MATH_DEFINES
#include <math.h>
#include "DateTimeTools.h"
#include "libgeopack.h"

/* this will be used as a pointer to each of the ***to***UT functions*/
typedef void (*ConvFunc) (double*,double*,double*,int,double*,double*,double*,
						int*,float*,double*,double*,double*);


/* This 2D array will be used to provide a conversion function 
 * Annoyingly, it has 36 elements which need filling manually! */
ConvFunc ConvFuncs[6][6];
bool ConvFuncsLoaded = false;
/* Indices:
 * GSE = 0
 * GSM = 1
 * SM = 2
 * GEO = 3
 * MAG = 4
 * GEI = 5 
 * Where the first dimension is the input and the second corresponds to 
 * to the output coordinates */
void _PopulateConvFuncs();

/* array of coordinate system abbreviations */
const char *CoordAbr[6];

/* Coordinate conversion  function */
void ConvCoords(double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout,
					const char *CoordIn, const char *CoordOut);


/***********************************************************************
 * GSEtoGSM
 * 
 * Wrapper for converting GSE to GSM coordinates
 * ********************************************************************/
void GSEtoGSM(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

/***********************************************************************
 * GSEtoGSMUT
 * 
 * Wrapper for converting GSE to GSM coordinates
 * ********************************************************************/
void GSEtoGSMUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);

void GSMtoGSE(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc,
				double *Xout, double *Yout, double *Zout);
	
void GSMtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);

void GSMtoSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSMtoSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz,
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout);

void SMtoGSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void SMtoGSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz, 
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout);

void GSEtoSM(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSEtoSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz, 
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout);

void GSEtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSEtoMAGUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut,
					double *Xout, double *Yout, double *Zout);

void SMtoGSE(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void SMtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz,
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout);

void MAGtoGSE(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void MAGtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);

void MLONtoMLT(	double MLon, double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, double *MLT);

void MLONtoMLTUT(	double *MLon, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, double *MLT);

void MLTtoMLON(	double MLT, double Vx, double Vy, double Vz, int recalc, 
				int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon);

void MLTtoMLONUT(	double *MLT, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, double *MLon);

void GEOtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEOtoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);

void GEOtoMAG_LL(	double Lon, double Lat, 
					double Vx, double Vy, double Vz, int recalc,
					int Year, int DayNo, int Hr, int Mn, int Sc, 
					double *MLon, double *MLat);

void GEOtoMAGUT_LL(	double *Lon, double *Lat, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, 
					double *MLon, double *MLat);
											
void MAGtoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void MAGtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);
				
void MAGtoGEO_LL(	double MLon, double MLat, 
					double Vx, double Vy, double Vz, int recalc,
					int Year, int DayNo, int Hr, int Mn, int Sc, 
					double *Lon, double *Lat);

void MAGtoGEOUT_LL(	double *MLon, double *MLat, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, 
					double *Lon, double *Lat);






/***********************************************************************
 * NAME : 			void GEItoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);
				
void GEItoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);


/***********************************************************************
 * NAME : 			void GEOtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEOtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);




/***********************************************************************
 * NAME : 			void GSMtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);
				
void GSMtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);


/***********************************************************************
 * NAME : 			void GEOtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEOtoGSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);






/***********************************************************************
 * NAME : 			void GSEtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSEtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void GEOtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEOtoGSEUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);






/***********************************************************************
 * NAME : 			void SMtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void SMtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);


/***********************************************************************
 * NAME : 			void GEOtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEOtoSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);




/***********************************************************************
 * NAME : 			void GSEtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSEtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void GEItoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEItoGSEUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);


/***********************************************************************
 * NAME : 			void GSMtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSMtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void GEItoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEItoGSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);


/***********************************************************************
 * NAME : 			void SMtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void SMtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void GEItoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEItoSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);









/***********************************************************************
 * NAME : 			void MAGtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void MAGtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void GEItoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GEItoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);




/***********************************************************************
 * NAME : 			void MAGtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void MAGtoGSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void GSMtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void GSMtoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);





/***********************************************************************
 * NAME : 			void MAGtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void MAGtoSMUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);



/***********************************************************************
 * NAME : 			void SMtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
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
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout);

void SMtoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout);


