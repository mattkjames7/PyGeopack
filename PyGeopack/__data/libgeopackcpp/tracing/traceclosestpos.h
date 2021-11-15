#ifndef __TRACECLOSESTPOS_H__
#define __TRACECLOSESTPOS_H__
#include <stdio.h>
#include <stdlib.h>
#include "trace.h"
#include "../matrix/matrixarray.h"
#include "../matrix/rotmatrix.h"
#include "../matrix/matrix.h"
#include "../spline/spline.h"

#endif
class Trace;

void dbg();

void TraceClosestPos(	MatrixArray &R,
						int n, double *x, double *y, double *z,
						int n0, double *x0, double *y0, double *z0,
						int n1, double *x1, double *y1, double *z1,
						double *xc0, double *yc0, double *zc0,
						double *xc1, double *yc1, double *zc1);
					
void _ClosestPos(	int i, Matrix &R,
					int n, double *x, double *y, double *z,
					int n0, double *x0, double *y0, double *z0,
					int n1, double *x1, double *y1, double *z1,
					double *xc0, double *yc0, double *zc0,
					double *xc1, double *yc1, double *zc1);

void _RotateBack(	double xi, double yi, double zi,
					double Px, double Py, double Pz,
					Matrix &R,
					double *xo, double *yo, double *zo);
				
void _RotateTrace(	int n, double *x, double *y, double *z, 
					double Px, double Py, double Pz,
					Matrix &R,
					double *rx, double *ry, double *rz);
					
void _Closest4Pos(	double Prx, double Pry, double Prz,
					double *rx, double *ry, double *rz, int n,
					int *nc, double *cx, double *cy, double *cz);

void _ClosestPosSpline(int nc, double *cx, double *cy, double *cz,
						double *xc, double *yc, double *zc);
