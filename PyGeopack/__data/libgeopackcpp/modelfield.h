#ifndef __MODELFIELD_H__
#define __MODELFIELD_H__
#include <stdio.h>
#include <math.h>
#include "fortran/geopack.h"
#include <string.h>
#include "libdatetime/DayNo.h"
#include "libdatetime/hhmm.h"
#include "withinmp.h"
#include "dummyfunc.h"
#include "recalc.h"
using namespace std;

typedef struct ModelCFG{
	int n;
	int *Date;
	float *ut;
	bool SameTime;
	ModelFuncPtr model;
	int *iopt;
	double **parmod;
	double *Vx;
	double *Vy;
	double *Vz;
	const char *CoordIn;
	const char *CoordOut;
	bool WithinMPOnly;
} ModelCFG;
#endif

/***********************************************************************
 * ModelField
 * 
 * Calculates the magnetic field model for an array of times and
 * positions.
 * ********************************************************************/
extern "C" {
	void ModelField(	int n, double *Xin, double *Yin, double *Zin,  
						int *Date, float *ut, bool SameTime,
						const char *Model, int *iopt, double **parmod,
						double *Vx, double *Vy, double *Vz,
						const char *CoordIn, const char *CoordOut, bool WithinMPOnly, 
						double *Bx, double *By, double *Bz);
	
	void ModelFieldWrap(int n, double *Xin, double *Yin, double *Zin,  
						int *Date, float *ut, bool SameTime,
						const char *Model, int *iopt, double **parmod,
						double *Vx, double *Vy, double *Vz,
						const char *CoordIn, const char *CoordOut, bool WithinMPOnly,
						double *Bx, double *By, double *Bz);
	
	
}

ModelCFG GetModelCFG(	int n, int *Date, float *ut, bool SameTime,
						const char *Model, int *iopt, double **parmod,
						double *Vx, double *Vy, double *Vz,
						const char *CoordIn, const char *CoordOut, bool WithinMPOnly); 

void ModelFieldNew(	int n, double *Xin, double *Yin, double *Zin, 
					ModelCFG cfg,double *Bx, double *By, double *Bz);
