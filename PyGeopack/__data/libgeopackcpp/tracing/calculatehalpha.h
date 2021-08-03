#ifndef __CALCULATEHALPHA_H__
#define __CALCULATEHALPHA_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../fortran/geopack.h"
#include "tracefieldline.h"
#endif
using namespace std;


void CalculateHalphas(	int nalpha, double *alpha, int nstep,
						double *x, double *y, double *z,
						double *Bx, double *By, double *Bz,
						ModelFuncPtr ModelFunc, int iopt, double *parmod,
						double alt, int MaxLen, double DSMax, 
						double xfe, double yfe, double zfe,
						double *halpha);

void CalculateHalpha(	double alpha, int nstep,
						double *x, double *y, double *z,
						double *Bx, double *By, double *Bz,
						ModelFuncPtr ModelFunc, int iopt, double *parmod,
						double alt, int MaxLen, double DSMax, 
						double xfe, double yfe, double zfe,
						double *halpha);
