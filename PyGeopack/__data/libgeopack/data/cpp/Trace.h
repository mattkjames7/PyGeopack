#ifndef __Trace_h__
#define __Trace_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"
#include "RHand.h"
#include "Step.h"

void Trace(double XI, double YI, double ZI, double Dir, double DsMax, double Err, double Rlim, double R0, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName, double *XF, double *YF, double *ZF, double *XX, double *YY, double *ZZ, int *L, int LMax);
#endif
