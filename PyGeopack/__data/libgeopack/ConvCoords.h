#include <stdio.h>
#include <math.h>
#include "DateTimeTools.h"
#include "libgeopack.h"

void GSEtoGSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void GSMtoGSE(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void GSMtoSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void SMtoGSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void GSEtoSM(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout) ;
void GSEtoMAG(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void SMtoGSE(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void MAGtoGSE(float *Xin, float *Yin, float *Zin, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Xout, float *Yout, float *Zout);
void MLONtoMLT(float *MLon, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *MLT);
void MLTtoMLON(float *MLT, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *MLon);
void GEOtoMAG(float *Lon, float *Lat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *MLon, float *MLat);
void MAGtoGEO(float *MLon, float *MLat, int n, int Year, int DayNo, int Hr, int Mn, int Sc, float *Lon, float *Lat);

//CtypeStart
//PyFunc iiiiiiooo
void GSEtoGSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void GSMtoGSEUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void GSMtoSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void SMtoGSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void GSEtoSMUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void GSEtoMAGUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void SMtoGSEUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiiiiooo
void MAGtoGSEUT(float *Xin, float *Yin, float *Zin, int n, int date, float UT, float *Xout, float *Yout, float *Zout);
//PyFunc iiiio
void MLONtoMLTUT(float *MLon, int n, int date,float UT, float *MLT);
//PyFunc iiiio
void MLTtoMLONUT(float *MLT, int n, int date,float UT, float *MLon);
//PyFunc iiiiioo
void GEOtoMAGUT(float *Lon, float *Lat, int n, int date,float UT, float *MLon, float *MLat);
//PyFunc iiiiioo
void MAGtoGEOUT(float *MLon, float *MLat, int n, int date,float UT, float *Lon, float *Lat);
//CtypeStop
