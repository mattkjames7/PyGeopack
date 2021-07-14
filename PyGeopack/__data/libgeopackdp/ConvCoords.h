#include <stdio.h>
#include <math.h>
#include "DateTimeTools.h"
#include "libgeopack.h"

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
