#ifndef __Magnetopause_h__
#define __Magnetopause_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"
void ShueEtAlMP(double XN_PD, double Vel, double BzIMF, double XGSW, double YGSW, double ZGSW, double *XMGNP, double *YMGNP, double *ZMGNP, double *Dist, int *Id);
void T96MP(double XN_PD, double Vel, double XGSW, double YGSW, double ZGSW, double *XMGNP, double *YMGNP, double *ZMGNP, double *Dist, int *ID);

#endif
