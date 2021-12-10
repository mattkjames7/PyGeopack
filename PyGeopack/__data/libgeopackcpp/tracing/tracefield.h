#ifndef __TRACEFIELD_H__
#define __TRACEFIELD_H__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../fortran/geopack.h"
#include "../libdatetime/hhmm.h"
#include "../libdatetime/DayNo.h"
#include "../withinmp.h"
#include "tracefieldline.h"
#include "../modelfield.h"
#include "fieldlinedist.h"
#include "fieldliner.h"
#include "fieldlinernorm.h"
#include "tracefootprints.h"
#include "converttracecoords.h"
#include "calculatehalpha.h"
#include "../recalc.h"
#include "trace.h"
#endif

typedef struct TraceCFG {
	double alt; 
	int MaxLen; 
	double DSMax; 
	bool Verbose;
	int TraceDir;	
} TraceCFG;

//typedef struct SimpleTrace {
	//int n;
	//int *nstep;
	//double **x;
	//double **y;
	//double **z;
	//double **Bx;
	//double **By;
	//double **Bz;
//} SimpleTrace;

//typedef struct FullTrace {
	//int n;
	//SimpleTrace GSM;
	//SimpleTrace GSE;
	//SimpleTrace SM;
	//ModelCFG mcfg;
	//TraceCFG tcfg;
//} FullTrace;

extern "C" {

	void TraceField(int n, double *xin, double *yin, double *zin,
					int *Date, float *ut, const char *Model,
					int *iopt, double **parmod, 
					double *Vx, double *Vy, double *Vz,
					double alt, int MaxLen, double DSMax, 
					bool Verbose, int TraceDir,
					const char *CoordIn, int *nstep,
					double **xgsm, double **ygsm, double **zgsm, 
					double **bxgsm, double **bygsm, double **bzgsm,
					double **xgse, double **ygse, double **zgse, 
					double **bxgse, double **bygse, double **bzgse,
					double **xsm, double **ysm, double **zsm, 
					double **bxsm, double **bysm, double **bzsm,
					double **s, double **r, double **rnorm, double **FP,
					int nalpha, double *alpha, double *halpha);

	void TraceFieldOld (	double *Xin, double *Yin, double *Zin, int n, 
						int *Date, float *ut, const char *Model, 
						int *iopt, double **parmod, 
						double *Vx, double *Vy, double *Vz,
						const char *CoordIn, const char *CoordOut, 
						double alt, int MaxLen, double DSMax, 
						bool Verbose, int TraceDir,
						double **Xout, double **Yout, double **Zout, 
						double **s, double **R, double **Rnorm, 
						int nalpha, double *alpha, double **halpha,
						double **Bx, double **By, double **Bz, 
						int *nstep, double **FP);
						
}



						
			

//TraceCFG GetTraceCFG(	double alt, int MaxLen, double DSMax, 
						//bool Verbose, int TraceDir);

//SimpleTrace SimpleFieldTrace( double *Xin, double *Yin, double *Zin, int n, 
						//int *Date, float *ut, const char *Model, 
						//int *iopt, double **parmod, 
						//double *Vx, double *Vy, double *Vz,
						//const char *CoordIn, const char *CoordOut, 
						//double alt, int MaxLen, double DSMax, 
						//bool Verbose, int TraceDir );
						
//SimpleTrace SimpleFieldTrace( double *Xin, double *Yin, double *Zin, int n, 
								//ModelCFG mcfg, TraceCFG tcfg);
