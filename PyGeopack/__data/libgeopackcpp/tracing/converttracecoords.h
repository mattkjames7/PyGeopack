#ifndef __CONVERTTRACECOORDS_H__
#define __CONVERTTRACECOORDS_H__
#include <stdio.h>
#include <stdlib.h>
#include "../fortran/geopack.h"
#include <string.h>
#endif


void ConvertTraceCoords(int nstep, const char *CoordOut, 
						double *x, double *y, double *z, 
						double *Bx, double *By, double *Bz);
