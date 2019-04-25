#ifndef __Step_h__
#define __Step_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"
#include "RHand.h"

void Step(double *X, double *Y, double *Z, double *Ds, double DsMax, double ErrIn, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName);
#endif
