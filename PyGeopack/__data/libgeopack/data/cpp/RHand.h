#ifndef __RHand_h__
#define __RHand_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"

void RHand(double X, double Y, double Z, double *R1, double *R2, double *R3, int Iopt, double *ParMod, ModelFuncPtr ExName, InternalFuncPtr InName);
#endif
