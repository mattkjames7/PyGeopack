#ifndef __TRACEFIELD_H__
#define __TRACEFIELD_H__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../fortran/geopack.h"
#include "../libdatetime/hhmm.h"
#include "../libdatetime/DayNo.h"
#include "../withinmp.h"
#include "tracefieldline.h"
#include "../modelfield.h"
#include "fieldlinedist.h"
#include "fieldliner.h"
#include "fieldlinernorm.h"
#include "tracefootprints.h"
#include "converttracecoords.h"
#endif

extern "C" {

	void TraceField(double *Xin, double *Yin, double *Zin, int n, 
					int *Date, float *ut, const char *Model, 
					int *iopt, double **parmod,
					double *Vx, double *Vy, double *Vz,
					const char *CoordIn, const char *CoordOut, 
					double alt, int MaxLen, double DSMax, 
					bool Verbose, int TraceDir,
					double *Xout, double *Yout, double *Zout, 
					double *s, double *R, double *Rnorm, 
					int nalpha, double *alpha, double **halpha,
					double *Bx, double *By, double *Bz, 
					int *nstep, double **FP);
}
