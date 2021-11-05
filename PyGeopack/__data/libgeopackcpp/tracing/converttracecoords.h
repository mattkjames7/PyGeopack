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

void ConvertTraceCoords(int nstep, const char *CoordOut, 
						double *x0, double *y0, double *z0, 
						double *x1, double *y1, double *z1, 
						double *Bx0, double *By0, double *Bz0,
						double *Bx1, double *By1, double *Bz1);
