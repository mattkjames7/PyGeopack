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

void NorthSouthFLs(float flx[],float fly[],float flz[], float *R, int N, float **Nflx, float **Nfly, float **Nflz, float **NR, int *nn, float **Sflx, float **Sfly, float **Sflz, float **SR, int *ns);
float linterp(float x0, float x1, float y0, float y1, const float xt);
void EqFootprint(float *Nflx, float *Nfly, float *Nflz, int nN, float *Sflx, float *Sfly, float *Sflz, int nS, float *lshell, float *mlte);
void CartToSpherical(float x, float y, float z, float *r, float *theta, float *phi);
float CalculateFieldLineLength(float *x, float *y, float *z, int n);

void TraceFootprints(float *x, float *y, float *z, int nstep, float xfn, float yfn, float zfn, float xfs, float yfs, float zfs, float alt,float *MltN, float *MlatN, float *MlonN,
					float *GltN, float *GlatN, float *GlonN, float *MltS, float *MlatS, float *MlonS, float *GltS, float *GlatS,float *GlonS, float *Lshell, float *MltE, float *FlLen, int MaxLen);
void ReverseElements(float *x, int n);
void TraceFieldLine(float x0, float y0, float z0, int iopt, float *parmod, ModelFuncPtr ModelFunc,float alt, int MaxLen, float DSMax, float *xfn, float *yfn, float *zfn, float *xfs, float *yfs, float *zfs, float *x, float *y, float *z, int *nstep);
void ConvertTraceCoords(int nstep, int CoordOut, float *x, float *y, float *z, float *Bx, float *By, float *Bz);

void TraceField(float *Xin, float *Yin, float *Zin, int n, int *Date, float *ut, const char *Model, int CoordIn, int CoordOut, 
				float alt, int MaxLen, float DSMax, float *Xout, float *Yout, float *Zout,
				float *Bx, float *By, float *Bz, int *nstep, float *GlatN, float *GlatS, float *MlatN, float *MlatS,
				float *GlonN, float *GlonS, float *MlonN, float *MlonS,float *GltN, float *GltS, float *MltN,
				float *MltS, float *Lshell, float *MltE, float *FlLen, bool Verbose);
