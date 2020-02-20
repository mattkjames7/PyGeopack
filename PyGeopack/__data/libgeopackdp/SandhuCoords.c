#include "SandhuCoords.h"


void CalculateR(double *x, double *y, double *z, int n, double *r, int *rmaxI) {
	/*******************************************************************
	 * Calculate the radial distance of each point along each trace and 
	 * note the index of the maximum radial distance.
	 * 
	 * ****************************************************************/
	
	int i;
	rmaxI[0] = 0;
	for (i=0;i<n;i++) {
		r[i] = sqrt(x[i]*x[i] + y[i]*y[i] + z[i]*z[i]);
		if (r[i] > r[rmaxI[0]]) {
			rmaxI[0] = i;
		}
	}
}

void sandhucoordsf_(double *x, double *y, double *z, int *n, int *date, float *ut, int *verbose, double *l, double *m, double *r) {
	/*******************************************************************
	 * Call this function from Fortran.
	 * 
	 * ****************************************************************/

	SandhuCoords(x,y,z,n[0],date[0],ut[0],verbose[0],l,m,r);
}
	 


void SandhuCoords(double *Xin, double *Yin, double *Zin, int n, int Date, float ut, int Verbose, double *L, double *M, double *Rnorm) {
	/*******************************************************************
	 * Calculate the coordinates used for the Sandhu et al models using
	 * T96 magnetic field traces.
	 * 
	 * ****************************************************************/
	/* Trace parameters */
	const char *Model = "T96";
	int CoordIn = 3;
	int CoordOut = 3;
	double alt = 100.0;
	int MaxLen = 1000;
	double DSMax = 1.0;
	
	/* Convert Date and ut into arrays */
	int *dates;
	float *uts;
	dates = (int*) malloc(n*sizeof(int)); 
	uts = (float*) malloc(n*sizeof(float)); 
	int i;
	for (i=0;i<n;i++) {
		dates[i] = Date;
		uts[i] = ut;
	}
	
	
	/* Create some temporary arrays to store the output of the tracing*/
	int *nstep;
	double *Xout, *Yout, *Zout, *Bx, *By, *Bz, *FP, *s, *R, *Rn;/*GlatN, *GlatS;
	double *MlatN, *MlatS, *GlonN, *GlonS, *MlonN, *MlonS,*GltN, *GltS;
	double *MltN, *MltS, *Lshell, *MltE, *FlLen;*/
	
	
	Xout = (double*) malloc(n*MaxLen*sizeof(double));
	Yout = (double*) malloc(n*MaxLen*sizeof(double));
	Zout = (double*) malloc(n*MaxLen*sizeof(double));
	Bx = (double*) malloc(n*MaxLen*sizeof(double));
	By = (double*) malloc(n*MaxLen*sizeof(double));
	Bz = (double*) malloc(n*MaxLen*sizeof(double));
	s = (double*) malloc(n*MaxLen*sizeof(double));
	R = (double*) malloc(n*MaxLen*sizeof(double));
	Rn = (double*) malloc(n*MaxLen*sizeof(double));
	nstep = (int*) malloc(n*sizeof(int));
	/* FP replaces all of the footprint longitude, latitudes etc in the 
	 * following order:
	 * GlatN,GlatS,MlatN,MlatS,GlonN,GlonS,MlonN,MlonS
	 * GltN,GltS,MltN,MltS,Lshell,MltE,FlLen
	 * */
	FP = (double*) malloc(n*15*sizeof(double));
/*
	GlatN = (double*) malloc(n*sizeof(double));
	GlatS = (double*) malloc(n*sizeof(double));
	MlatN = (double*) malloc(n*sizeof(double));
	MlatS = (double*) malloc(n*sizeof(double));
	GlonN = (double*) malloc(n*sizeof(double));
	GlonS = (double*) malloc(n*sizeof(double));
	MlonN = (double*) malloc(n*sizeof(double));
	MlonS = (double*) malloc(n*sizeof(double));
	GltN = (double*) malloc(n*sizeof(double));
	GltS = (double*) malloc(n*sizeof(double));
	MltN = (double*) malloc(n*sizeof(double));
	MltS = (double*) malloc(n*sizeof(double));
	Lshell = (double*) malloc(n*sizeof(double));
	MltE = (double*) malloc(n*sizeof(double));
	FlLen = (double*) malloc(n*sizeof(double));*/
	
	

	/* Perform the traces */
	TraceField(Xin,Yin,Zin,n,dates,uts,Model,CoordIn,CoordOut,alt,MaxLen,
			DSMax,Xout,Yout,Zout,s,R,Rn,Bx,By,Bz,nstep,FP, (bool) Verbose);

	/* Temporary stuff for R */
			
	/* Now to loop through each trace and calculate L, M and Rnorm */
	for (i=0;i<n;i++) {
		/* Check for a good trace first of all (>0 steps and footprints in both hemispheres */
		if ((nstep[i] > 0) && (isfinite(FP[i*15+12]))) {
		
			/* Here we set L = Rmax (alternatively you could set L = Lshell 
			 * from above -  that would be R at Z=0*/
			L[i] = FP[i*15+12];
			
			/* Local time (in hours, hopefully) of the field line, again at where Rmax is, not Z=0 */
			M[i] = FP[i*15+13];
			
			/* now for Rnorm */
			Rnorm[i] = sqrt(Xin[i]*Xin[i] + Yin[i]*Yin[i] + Zin[i]*Zin[i])/L[i];
		} else { 
			L[i] = NAN;
			M[i] = NAN;
			Rnorm[i] = NAN;
		}
	}

	/* Free some memory */
	free(R);
	free(Xout);
	free(Yout);
	free(Zout);
	free(Bx);
	free(By);
	free(Bz);
	free(nstep);
/*	free(GlatN);
	free(GlatS);
	free(MlatN);
	free(MlatS);
	free(GlonN);
	free(GlonS);
	free(MlonN);
	free(MlonS);
	free(GltN);
	free(GltS);
	free(MltN);
	free(MltS);
	free(Lshell);
	free(MltE);
	free(FlLen);*/
	free(FP);
	free(Rn);
	free(s);
	free(dates);
	free(uts);	
}
