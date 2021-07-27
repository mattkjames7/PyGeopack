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
#endif
using namespace std;

/***********************************************************************
 * ModelField
 * 
 * Calculates the magnetic field model for an array of times and
 * positions.
 * ********************************************************************/
void ModelField(	int n, double *Xin, double *Yin, double *Zin,  
					int *Date, float *ut, bool SameTime,
					const char *Model, int *iopt, double **parmod,
					double *Vx, double *Vy, double *Vz,
					const char *CoordIn, const char *CoordOut, 
					double *Bx, double *By, double *Bz);

