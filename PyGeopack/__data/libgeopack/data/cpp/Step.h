#ifndef __Step_h__
#define __Step_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"
#include "RHand.h"

void Step(float *X, float *Y, float *Z, float *Ds, float DsMax, float ErrIn, float *Iopt, float *ParMod, ExFun ExName, InFun InName);
#endif
