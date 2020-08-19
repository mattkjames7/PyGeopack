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
