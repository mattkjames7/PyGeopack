#ifndef __libgeopack_h__
#define __libgeopack_h__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "DateTimeTools.h"
#include <math.h>
#include <stdbool.h>

typedef struct {
	int n;
	int *Date;
	float *ut;
	int *Year;
	int *DayNo;
	int *Hr;
	int *Mn;
	float *Bx;
	float *By;
	float *Bz;
	float *Vx;
	float *Vy;
	float *Vz;
	float *Den;
	float *Temp;
	float *SymH;
	int *IMFFlag;
	int *ISWFlag;
	float *Tilt;
	float *Pdyn;
	float *W1;
	float *W2;
	float *W3;
	float *W4;
	float *W5;
	float *W6;
	float *G1;
	float *G2;
	float *Kp;
	int *MonthInds;
	int nMonth;
	int minYr;
	int minMn;
} TSD;

extern TSD TSData;

typedef struct {
	float iopt;
	float parmod[10];
	float tilt;
	float Vx, Vy, Vz;
} CustParam;

CustParam CustP;

typedef void (*ModelFuncPtr)(int*,double*,double*,double*,double*,double*,double*,double*,double*);
typedef void (*InternalFuncPtr) (double*,double*,double*,double*,double*,double*);


extern const double Re;

char DataFile[256];

#define min(a,b) ({ \
	__typeof__ (a) _a = (a);\
	__typeof__ (b) _b = (b);\
	_a < _b ? _a : _b;})

#define max(a,b) ({ \
	__typeof__ (a) _a = (a);\
	__typeof__ (b) _b = (b);\
	_a > _b ? _a : _b;})
	

#endif


void LoadTSData();
void FreeTSData();
void SetCustParam(int iopt, float *parmod, float tilt, float Vx, float Vy, float Vz);
void GetModelParams(int Date, float ut, const char *Model, int *iopt, double *parmod, double *tilt, double *Vx, double *Vy, double *Vz);
void Init(const char *filename);
double GetDipoleTilt(int Year, int Doy, int Hr, int Mn, double Vx, double Vy, double Vz);
double GetDipoleTiltUT(int Date, float ut, double Vx, double Vy, double Vz);
void GetSWVelocity(int Date, float ut, const char *Model, double *Vx, double *Vy, double * Vz);
bool WithinMP(double x, double y, double z, double Bz, double Pdyn);

void FillInKp(int nk, int *kDate, float *kut0, float *kut1, float *kp, int n, int *Date, float *ut, float *kpout);
void FindIntervals(int n, float *SymH, float *Bz, int *SWflag, int *IMFflag, int *ni, int *ibeg, int *iend);
void CalculateW(int n, float *SymH, float *Bz, int *SWflag, int *IMFflag, float *V, float *Den, float *W1, float *W2, float *W3, float *W4, float *W5, float *W6);
void CalculateG(int n, float *By, float *Bz, float *V, bool *good, float *G1, float *G2);

void PopulateMonthInds();
int MonthStartInd(int Date);
float InterpParam(float *x, int Date, float ut);
void DummyFunc(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);

extern void geomag_08_(double *xgeo, double *ygeo, double *zgeo, double *xmag, double *ymag, double *zmag, int *j);
extern void gswgse_08_(double *xgsw, double *ygsw, double *zgsw, double *xgse, double *ygse, double *zgse, int *j);
extern void smgsw_08_(double *xsm, double *ysm, double *zsm, double *xgsw, double *ygsw, double *zgsw, int *j);
extern void magsm_08_(double *xmag, double *ymag, double *zmag, double *xsm, double *ysm, double *zsm, int *j);
extern void geogsw_08_(double *xgeo, double *ygeo, double *zgeo, double *xgsw, double *ygsw, double *zgsw, int *j);
extern void recalc_08_(int *iyear, int *iday, int *ihour, int *min, int *isec, double *vgsex, double *vgsey, double *vgsez);
extern void trace_08_(double *xi, double *yi, double *zi, double *dir, double *dsmax, double *err, double *rlim, double *r0, int *iopt, double *parmod, ModelFuncPtr ModelFunc, InternalFuncPtr IntFunc, double *xf, double *yf, double *zf, double *xx, double *yy, double *zz, int *L, int *Lmax);
extern void igrf_gsw_08_(double *xgsw, double *ygsw, double *zgsw, double *hxgsw, double *hygsw, double *hzgsw);
extern double getpsi_();
extern void t89c_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void t96_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void t01_01_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void t04_s_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void listwintervals_(int *n, float *SymH, float *Bz, int *SWflag, int *IMFflag, int *ni, int *ibeg, int *iend);
extern void calculatew_(int *ni, int *ibeg, int *iend, int *n, float *Bz, float *V, float *Den, float *W1, float *W2, float *W3, float *W4, float *W5, float *W6);
