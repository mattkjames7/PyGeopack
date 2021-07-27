#ifndef __GETMAGEQUATORFP_H__
#define __GETMAGEQUATORFP_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../tools/argmax.h"
#include "../fortran/geopack.h"
#include "fieldlinemidpoint.h"
#endif



void GetMagEquatorFP(	double *x, double *y, double *z, 
						double *s, double *R, int n, 
						double *Lshell, double *MltE);

