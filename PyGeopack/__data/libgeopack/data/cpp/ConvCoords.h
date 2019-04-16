#ifndef __ConvCoords_h__
#define __ConvCoords_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"
#include "Recalc.h"
#include "DateTimeTools.h"
using namespace std;


void SphtoCar(double R, double Theta, double Phi, double *X, double *Y, double *Z);
void CartoSph(double X, double Y, double Z, double *R, double *Theta, double *Phi);
void BSphtoCar(double Theta, double Phi, double Br, double Btheta, double Bphi, double *Bx, double *By, double *Bz);
void BCartoSph(double X, double Y, double Z, double Bx, double By, double Bz, double *Br, double *Btheta, double *Bphi);
void GSWtoGSE(double XGSW, double YGSW, double ZGSW,double *XGSE, double *YGSE, double *ZGSE);
void GSEtoGSW(double XGSE, double YGSE, double ZGSE,double *XGSW, double *YGSW, double *ZGSW);
void GEOtoMAG(double XGEO, double YGEO, double ZGEO,double *XMAG, double *YMAG, double *ZMAG);
void MAGtoGEO(double XMAG, double YMAG, double ZMAG,double *XGEO, double *YGEO, double *ZGEO);
void GEItoGEO(double XGEI, double YGEI, double ZGEI,double *XGEO, double *YGEO, double *ZGEO);
void GEOtoGEI(double XGEO, double YGEO, double ZGEO,double *XGEI, double *YGEI, double *ZGEI);
void MAGtoSM(double XMAG, double YMAG, double ZMAG,double *XSM, double *YSM, double *ZSM);
void SMtoMAG(double XSM, double YSM, double ZSM,double *XMAG, double *YMAG, double *ZMAG);
void SMtoGSW(double XSM, double YSM, double ZSM,double *XGSW, double *YGSW, double *ZGSW);
void GSWtoSM(double XGSW, double YGSW, double ZGSW, double *XSM, double *YSM, double *ZSM);
void GEOtoGSW(double XGEO, double YGEO, double ZGEO,double *XGSW, double *YGSW, double *ZGSW);
void GSWtoGEO(double XGSW, double YGSW, double ZGSW,double *XGEO, double *YGEO, double *ZGEO);
void GEODtoGEO(double H,double Xmu, double *R, double *Theta);
void GEOtoGEOD(double R, double Theta,double *H,double *Xmu);




void GSEtoGSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void GSMtoGSEArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void GSMtoSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void SMtoGSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void GSEtoSMArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) ;
void GSEtoMAGArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void SMtoGSEArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void MAGtoGSEArray(double *Xin, double *Yin, double *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void MLONtoMLT(double *MLon, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLT);
void MLTtoMLON(double *MLT, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon);
void GEOtoMAGArray(double *Lon, double *Lat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon, double *MLat);
void MAGtoGEOArray(double *MLon, double *MLat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, double *Lon, double *Lat);

void GSEtoGSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void GSMtoGSEUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void GSMtoSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void SMtoGSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void GSEtoSMUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void GSEtoMAGUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void SMtoGSEUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void MAGtoGSEUTArray(double *Xin, double *Yin, double *Zin, int n, int date, double UT, double *Xout, double *Yout, double *Zout);
void MLONtoMLTUT(double *MLon, int n, int date,double UT, double *MLT);
void MLTtoMLONUT(double *MLT, int n, int date,double UT, double *MLon);
void GEOtoMAGUTArray(double *Lon, double *Lat, int n, int date,double UT, double *MLon, double *MLat);
void MAGtoGEOUTArray(double *MLon, double *MLat, int n, int date,double UT, double *Lon, double *Lat);

#endif
