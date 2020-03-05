#include <stdio.h>
#include <math.h>
#include "DateTimeTools.h"
#include "libgeopack.h"

void GSEtoGSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void GSMtoGSE(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void GSMtoSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void SMtoGSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void GSEtoSM(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout) ;
void GSEtoMAG(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void SMtoGSE(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void MAGtoGSE(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Xout, double *Yout, double *Zout);
void MLONtoMLT(double *MLon, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLT);
void MLTtoMLON(double *MLT, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon);
void GEOtoMAG(double *Lon, double *Lat, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon, double *MLat);
void MAGtoGEO(double *MLon, double *MLat, int n, double Vx, double Vy, double Vz, int Year, int DayNo, int Hr, int Mn, int Sc, double *Lon, double *Lat);

//CtypeStart
//PyFunc iiiiiiooo
void GSEtoGSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void GSMtoGSEUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void GSMtoSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void SMtoGSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void GSEtoSMUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void GSEtoMAGUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void SMtoGSEUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiiiiooo
void MAGtoGSEUT(double *Xin, double *Yin, double *Zin, int n, double Vx, double Vy, double Vz, int date, float UT, double *Xout, double *Yout, double *Zout);
//PyFunc iiiio
void MLONtoMLTUT(double *MLon, int n, double Vx, double Vy, double Vz, int date,float UT, double *MLT);
//PyFunc iiiio
void MLTtoMLONUT(double *MLT, int n, double Vx, double Vy, double Vz, int date,float UT, double *MLon);
//PyFunc iiiiioo
void GEOtoMAGUT(double *Lon, double *Lat, int n, double Vx, double Vy, double Vz, int date,float UT, double *MLon, double *MLat);
//PyFunc iiiiioo
void MAGtoGEOUT(double *MLon, double *MLat, int n, double Vx, double Vy, double Vz, int date,float UT, double *Lon, double *Lat);
//CtypeStop
