#ifndef __libgeopack_h__
#define __libgeopack_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DateTimeTools.h"
#include <string.h>

/* Data structure to store recalc parameters*/
typedef struct GEOPACK1 {
	double ST0;
	double CT0;
	double SL0;
	double CL0;
	double CTCL;
	double STCL;
	double CTSL;
	double STSL;
	double SFI;
	double CFI;
	double SPS;
	double CPS;
	double DS3;
	double CGST;
	double SGST;
	double PSI;
	double A11;
	double A21;
	double A31;
	double A12;
	double A22;
	double A32;
	double A13;
	double A23;
	double A33;
	double E11;
	double E21;
	double E31;
	double E12;
	double E22;
	double E32;
	double E13;
	double E23;
	double E33;
} GPPar1 ;

extern GPPar1 GP1;


/* Data structure to store each set of parameters*/
typedef struct IGRFParam {
	int Year;
	double g[105];
	double h[105];
	double rec[105];
	int n[105];
	int m[105];
} IGRFP;

/* at the time of writing there are 25 sets of parameters, plus one set
 * of secular variation parameters (I believe these are a yearly change)*/
extern IGRFP IGRFParams[25];
extern IGRFP IGRFCurr;

#include "Recalc.h"


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
	int iopt;
	double parmod[10];
	float tilt;
	float Vx, Vy, Vz;
} CustParam;

extern CustParam CustP;

/* define the magnetic field functions typedef so we can pass them to the geopack routines*/

typedef void (*ModelFuncPtr)(int,double*,double,double,double,double,double*,double*,double*);
typedef void (*InternalFuncPtr) (double,double,double,double*,double*,double*);


extern const float Re;

extern char DataFile[256];

#endif

extern "C" {
	void LoadTSData();
	void FreeTSData();
	void SetCustParam(int iopt, double *parmod, float tilt, float Vx, float Vy, float Vz);
	void GetModelParams(int Date, float ut, const char *Model, int *iopt, double *parmod, float *tilt, float *Vx, float *Vy, float *Vz);	
	void Init(const char *filename, const char *igrffile);
	void GetGeopackParams(double *gp0, double *gp1);
	void SetGeopackParams(double *gp0, double *gp1);
}

void PopulateMonthInds();
int MonthStartInd(int Date);
double InterpParam(float *x, int Date, float ut);
void DummyFunc(int iopt, double *parmod, double ps, double x, double y, double z, double *bx, double *by, double *bz);
