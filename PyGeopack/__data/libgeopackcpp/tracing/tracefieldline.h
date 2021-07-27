#ifndef __TRACEFIELDLINE_H__
#define __TRACEFIELDLINE_H__
#include <stdio.h>
#include <stdlib.h>
#include "../fortran/geopack.h"
#include "../tools/reverseelements.h"
#endif

void TraceFieldLine(double x0, double y0, double z0, 
					int iopt, double *parmod, 
					ModelFuncPtr ModelFunc,
					double alt, int MaxLen, double DSMax, int TraceDir,
					double *xfn, double *yfn, double *zfn, 
					double *xfs, double *yfs, double *zfs, 
					double *x, double *y, double *z, 
					int *nstep);
