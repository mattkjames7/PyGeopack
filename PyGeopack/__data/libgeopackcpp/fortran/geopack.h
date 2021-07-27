#ifndef __GEOPACK_H__
#define __GEOPACK_H__
#include <stdio.h>
#include <stdlib.h>

#endif
using namespace std;

/* Model function pointer templates */	
typedef void (*ModelFuncPtr)(int*,double*,double*,double*,double*,double*,double*,double*,double*);
typedef void (*InternalFuncPtr) (double*,double*,double*,double*,double*,double*);
	
/* this file contains the prototypes for the FORTRAN functions in
 * geopack and the Tsyganenko models */
	
	
/* function prototypes for coordinate system conversion */
extern void geigeo_08_(double *xgei, double *ygei, double *zgei, double *xgeo, double *ygeo, double *zgeo, int *j);
extern void geomag_08_(double *xgeo, double *ygeo, double *zgeo, double *xmag, double *ymag, double *zmag, int *j);
extern void gswgse_08_(double *xgsw, double *ygsw, double *zgsw, double *xgse, double *ygse, double *zgse, int *j);
extern void smgsw_08_(double *xsm, double *ysm, double *zsm, double *xgsw, double *ygsw, double *zgsw, int *j);
extern void magsm_08_(double *xmag, double *ymag, double *zmag, double *xsm, double *ysm, double *zsm, int *j);
extern void geogsw_08_(double *xgeo, double *ygeo, double *zgeo, double *xgsw, double *ygsw, double *zgsw, int *j);

/* prototype for initializing model parameters and rotation matrices */
extern void recalc_08_(int *iyear, int *iday, int *ihour, int *min, int *isec, double *vgsex, double *vgsey, double *vgsez);

/* The trace wrapper function */
extern void trace_08_(double *xi, double *yi, double *zi, double *dir, double *dsmax, double *err, double *rlim, double *r0, int *iopt, double *parmod, ModelFuncPtr ModelFunc, InternalFuncPtr IntFunc, double *xf, double *yf, double *zf, double *xx, double *yy, double *zz, int *L, int *Lmax);

/* IGRF Model function */
extern void igrf_gsw_08_(double *xgsw, double *ygsw, double *zgsw, double *hxgsw, double *hygsw, double *hzgsw);

/* a function to returnt he dipole tilt */
extern double getpsi_();

/* different model functions */
extern void t89c_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void t96_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void t01_01_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);
extern void t04_s_(int *iopt, double *parmod, double *ps, double *x, double *y, double *z, double *bx, double *by, double *bz);


//extern void listwintervals_(int *n, float *SymH, float *Bz, int *SWflag, int *IMFflag, int *ni, int *ibeg, int *iend);
//extern void calculatew_(int *ni, int *ibeg, int *iend, int *n, float *Bz, float *V, float *Den, float *W1, float *W2, float *W3, float *W4, float *W5, float *W6);

	

