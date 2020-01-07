#ifndef __TraceRoutines_h__
#define __TraceRoutines_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ModelField.h"
#include "libgeopack.h"
#include <stdbool.h>

#endif

//CtypeStart
//PyFunc iiiiiiiiiiiioooooooooooooooooooooo
/*void TraceField(float *Xin, float *Yin, float *Zin, int n, int Date, float ut, const char *Model, int CoordIn, int CoordOut,
				float alt, int MaxLen, float DSMax, float *Xout, float *Yout, float *Zout,
				float *Bx, float *By, float *Bz, int *nstep, float *GlatN, float *GlatS, float *MlatN, float *MlatS,
				float *GlonN, float *GlonS, float *MlonN, float *MlonS,float *GltN, float *GltS, float *MltN,
				float *MltS, float *Lshell, float *MltE, float *FlLen);*/
//CtypeStop

void NorthSouthFLs(double flx[],double fly[],double flz[], double *R, int N, double **Nflx, double **Nfly, double **Nflz, double **NR, int *nn, double **Sflx, double **Sfly, double **Sflz, double **SR, int *ns);
double linterp(double x0, double x1, double y0, double y1, const double xt);
void EqFootprint(double *Nflx, double *Nfly, double *Nflz, int nN, double *Sflx, double *Sfly, double *Sflz, int nS, double *lshell, double *mlte);
void CartToSpherical(double x, double y, double z, double *r, double *theta, double *phi);
double CalculateFieldLineLength(double *x, double *y, double *z, int n);

void TraceFootprints(double *x, double *y, double *z, int nstep, double xfn, double yfn, double zfn, double xfs, double yfs, double zfs, double alt,double *MltN, double *MlatN, double *MlonN,
					double *GltN, double *GlatN, double *GlonN, double *MltS, double *MlatS, double *MlonS, double *GltS, double *GlatS,double *GlonS, double *Lshell, double *MltE, double *FlLen, int MaxLen);
void ReverseElements(double *x, int n);
void TraceFieldLine(double x0, double y0, double z0, int iopt, double *parmod, ModelFuncPtr ModelFunc,double alt, int MaxLen, double DSMax, double *xfn, double *yfn, double *zfn, double *xfs, double *yfs, double *zfs, double *x, double *y, double *z, int *nstep);
void ConvertTraceCoords(int nstep, int CoordOut, double *x, double *y, double *z, double *Bx, double *By, double *Bz);

void TraceField(double *Xin, double *Yin, double *Zin, int n, int *Date, float *ut, const char *Model, int CoordIn, int CoordOut, 
				double alt, int MaxLen, double DSMax, double *Xout, double *Yout, double *Zout,
				double *Bx, double *By, double *Bz, int *nstep, double *GlatN, double *GlatS, double *MlatN, double *MlatS,
				double *GlonN, double *GlonS, double *MlonN, double *MlonS,double *GltN, double *GltS, double *MltN,
				double *MltS, double *Lshell, double *MltE, double *FlLen, bool Verbose);
