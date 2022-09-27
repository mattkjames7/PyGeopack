#include "tracefieldline.h"

void TraceFieldLine(double x0, double y0, double z0, 
					int iopt, double *parmod, 
					ModelFuncPtr ModelFunc,
					double alt, int MaxLen, double DSMax, int TraceDir,
					double *xfn, double *yfn, double *zfn, 
					double *xfs, double *yfs, double *zfs, 
					double *x, double *y, double *z, 
					int *nstep) {
	
	/*calculate the radial distance of the stopping altitude*/
    const double Re = 6371.2;
    double R = (alt + Re)/Re;
    
	
	int N = 0, M = 0;
	double dir;
	double Err = 0.0001;
	double Rlim = 1000.0;
	int Nmax;

	/*next we need to trace backwards along the field line towards the north pole*/
	if (TraceDir == 0) {
		Nmax = MaxLen/2 - 2;
		dir = -1.0;
	} else if (TraceDir == 1) {
		Nmax = MaxLen - 1;
		dir = -1.0;
	} else {
		Nmax = 0;
	}
	if (Nmax > 0) {
		trace_08_(&x0,&y0,&z0,&dir,&DSMax,&Err,&Rlim,&R,&iopt,parmod,
					ModelFunc,&igrf_gsw_08_,xfn,yfn,zfn,x,y,z,&N,&Nmax);
	} else {
		xfn[0] = 0.0;
		yfn[0] = 0.0;
		zfn[0] = 0.0;
	}
	
	/*reverse array elements*/
	if (N > 1) {
		ReverseElements(x,N);
		ReverseElements(y,N);
		ReverseElements(z,N);
	}

	/*now for the southern part of the field line*/
	if (TraceDir == 0) {
		Nmax = MaxLen - N;
		dir = 1.0;
		N -= 1;
	} else if (TraceDir == -1) {
		Nmax = MaxLen - 1;
		dir = 1.0;
	} else {
		Nmax = 0;
	}
	if (Nmax > 1) {
		trace_08_(&x0,&y0,&z0,&dir,&DSMax,&Err,&Rlim,&R,&iopt,parmod,
					ModelFunc,&igrf_gsw_08_,xfs,yfs,zfs,
					&x[N],&y[N],&z[N],&M,&Nmax);
	} else {
		xfs[0] = 0.0;
		yfs[0] = 0.0;
		zfs[0] = 0.0;
	}
	
	nstep[0] = N + M;
}
