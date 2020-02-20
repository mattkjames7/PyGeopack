#ifndef __TraceRoutines_h__
#define __TraceRoutines_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ModelField.h"
#include "libgeopack.h"
#include <stdbool.h>

#endif


void argmax(double *x, int n, double *xmx, int *Imx);
void FieldLineDistance(double *x, double *y, double *z, int n, double *s);
void FieldLineMidPoint(double *x, double *y, double *z, double *s, int n, double *xm, double *ym, double *zm);
void FieldLineR(double *x, double *y, double *z, int n, double *R);
void FieldLineRnorm(double *R, int n, double Lshell, double *Rnorm);
void GetMagEquatorFP(double *x, double *y, double *z, double *s, double *R, int n, double *Lshell, double *MltE);
double linterp(double x0, double x1, double y0, double y1, const double xt);
void CartToSpherical(double x, double y, double z, double *r, double *theta, double *phi);
void TraceField(double *Xin, double *Yin, double *Zin, int n, 
				int *Date, float *ut, const char *Model, int CoordIn, int CoordOut, 
				double alt, int MaxLen, double DSMax, 
				double *Xout, double *Yout, double *Zout, 
				double *s, double *R, double *Rnorm,
				double *Bx, double *By, double *Bz, 
				int *nstep, double *FP, bool Verbose);
void ConvertTraceCoords(int nstep, int CoordOut, double *x, double *y, double *z, double *Bx, double *By, double *Bz);
void MagLatLonLT(double x, double y, double z, double *lat, double *lon, double *lt);	
void GeoLatLonLT(double x, double y, double z, double *lat, double *lon, double *lt);
void TraceFootprints(double *x, double *y, double *z, double *s, double *R, int nstep, double xfn, double yfn, double zfn, 
					double xfs, double yfs, double zfs, double alt, double *FP, int MaxLen);
void ReverseElements(double *x, int n);
void TraceFieldLine(double x0, double y0, double z0, int iopt, double *parmod, ModelFuncPtr ModelFunc,double alt, int MaxLen, double DSMax, 
				double *xfn, double *yfn, double *zfn, double *xfs, double *yfs, double *zfs, double *x, double *y, double *z, int *nstep);				


