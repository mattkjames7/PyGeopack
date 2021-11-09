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

void TraceClosestPos(Trace T, Trace T0, Trace T1, int I,
					MatrixArray &R,
					double *xc0, double *yc0, double *zc0,
					double *xc1, double *yc1, double *zc1) ;
					
void _ClosestPos(int i, int I, Matrix &R, Trace T, Trace T0, Trace T1,
				double *xc0, double *yc0, double *zc0,
				double *xc1, double *yc1, double *zc1);
				
void _RotateTrace(	int n, Trace T, int I, 
					double Px, double Py, double Pz,
					Matrix &R,
					double *rx, double *ry, double *rz);
					
void _Closest4Pos(	double Prx, double Pry, double Prz,
					double *rx, double *ry, double *rz, int n,
					int *nc, double *cx, double *cy, double *cz);

void _ClosestPosSpline(int nc, double *cx, double *cy, double *cz,
						double *xc, double *yc, double *zc);
