#ifndef __INTERPTRACECLOSESTPOS_H__
#define __INTERPTRACECLOSESTPOS_H__
#include <stdio.h>
#include <stdlib.h>
#include "../spline/spline.h"
#include <math.h>
#endif
void interptraceClosestPos(	int n, double *x, double *y, double *z,
						double *bx, double *by, double *bz,
						int n0, double *x0, double *y0, double *z0, double *s0,
						int n1, double *x1, double *y1, double *z1, double *s1,
						double *xc0, double *yc0, double *zc0,
						double *xc1, double *yc1, double *zc1 );

void interpOptimum(	double x, double y, double z,
					double bx, double by, double bz,
					int is0, 
					Spline Sx, Spline Sy, Spline Sz,
					double *xc, double *yc, double *zc);
						
int ClosestS(double x, double y, double z,
				int nt, double *xt, double *yt, double *zt,
				double *st);
				
double AngleDiff( 	double s,								/* current position along the field line */
					Spline Sx, Spline Sy, Spline Sz,	/* Splines converting s to a  vector */
					double x, double y, double z,		/* this is the position along the original field line */
					double bx, double by, double bz);
					
					
bool OptimizePos(	double x, double y, double z,
					double bx, double by, double bz,
					double s0, 
					Spline Sx, Spline Sy, Spline Sz,
					double *xc, double *yc, double *zc);
