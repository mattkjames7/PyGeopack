#ifndef __ModelField_h__
#define __ModelField_h__
#include <stdio.h>
#include <math.h>
#include "libgeopack.h"
#endif


/***********************************************************************
 * ModelField
 * 
 * Calculates the magnetic field model for an array of times and
 * positions.
 * ********************************************************************/
void ModelField(	double *Xin, double *Yin, double *Zin, int n, 
					int *Date, float *ut, int SameTime, 
			const char *Model,	int CoordIn, int CoordOut, int WithinMPOnly,
			double *Bx, double *By, double *Bz);

