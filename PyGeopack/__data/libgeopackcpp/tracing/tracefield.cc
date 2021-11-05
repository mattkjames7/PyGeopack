#include "tracefield.h"

/* this could be come a wrapper function where I/O is converted to
 * a few typedefs */
void TraceField(double *Xin, double *Yin, double *Zin, int n, 
				int *Date, float *ut, const char *Model, 
				int *iopt, double **parmod, 
				double *Vx, double *Vy, double *Vz,
				const char *CoordIn, const char *CoordOut, 
				double alt, int MaxLen, double DSMax, 
				bool Verbose, int TraceDir,
				double **Xout, double **Yout, double **Zout, 
				double **s, double **R, double **Rnorm, 
				int nalpha, double *alpha, double **halpha,
				double **Bx, double **By, double **Bz, 
				int *nstep, double **FP) {

	int dirp = 1, dirn = -1;

	/* move all of the declarations here, before the loop*/
	int i, j;
	ModelFuncPtr ModelFunc;
	double xfn,yfn,zfn,xfs,yfs,zfs;
	double xfe,yfe,zfe;
	double X[n], Y[n], Z[n];
	bool update, inMP;

	/*get model function and parmod*/
	if (strcmp(Model,"T89") == 0){
		ModelFunc = &t89c_;
	} else if (strcmp(Model,"T96") == 0) {
		ModelFunc = &t96_;
	} else if (strcmp(Model,"T01") == 0) {
		ModelFunc = &t01_01_;
	} else if (strcmp(Model,"TS05") == 0) {
		ModelFunc = &t04_s_;
	} else if (strcmp(Model,"IGRF") == 0) {
		ModelFunc = &DummyFunc;
	} else { 
		printf("Model %s not found\n",Model);
		return;
	}

	for (i=0;i<n;i++) {
		if (Verbose) {
			printf("\rTracing field line %d of %d (%6.2f)%%",i+1,n,((float) (i+1)*100.0)/n);
		}

		/* call recalc */
		Recalc(Date[i],ut[i],Vx[i],Vy[i],Vz[i]);
		
			
		/*Convert input coordinates to GSM*/
		if (strcmp(CoordIn,"GSE") == 0) {
			/*GSE in*/
			gswgse_08_(&X[i],&Y[i],&Z[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
		} else if (strcmp(CoordIn,"SM") == 0) {
			/*SM in*/
			smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&X[i],&Y[i],&Z[i],&dirp);
		} else {
			/*GSM in*/
			X[i] = Xin[i];
			Y[i] = Yin[i];
			Z[i] = Zin[i];
		}
		
		/*Check if the point is within the MP*/
		inMP = WithinMP(X[i],Y[i],Z[i],parmod[i][3],parmod[i][0]);
		
		if (inMP) {

			/* perform trace */
			TraceFieldLine(X[i],Y[i],Z[i],iopt[i],parmod[i],ModelFunc,alt,
							MaxLen,DSMax,TraceDir,
							&xfn,&yfn,&zfn,&xfs,&yfs,&zfs,
							Xout[i],Yout[i],Zout[i],&nstep[i]);

			/*get B vectors along trace*/
			ModelField(nstep[i],Xout[i],Yout[i],Zout[i],
						&Date[i],&ut[i],true,Model,&iopt[i],&parmod[i],
						&Vx[i],&Vy[i],&Vz[i],"GSM","GSM",
						Bx[i],By[i],Bz[i]);

			/* Get the distance along the field line*/
			FieldLineDist(nstep[i],Xout[i],Yout[i],Zout[i],s[i]);

			/* Get the radius of each point */
			FieldLineR(nstep[i],Xout[i],Yout[i],Zout[i],R[i]);

			/* find trace footprints */
			TraceFootprints(nstep[i],ut[i],Xout[i],
						Yout[i],Zout[i],s[i],
						R[i],xfn,yfn,zfn,xfs,yfs,zfs,
						&xfe,&yfe,&zfe,
						alt,FP[i],MaxLen,TraceDir);

			/* Get the Rnorm of each point */
			FieldLineRnorm(nstep[i],R[i],FP[i][12],Rnorm[i]);
			
			/* now to try and calculate halpha */
			if ((nalpha > 0) & (TraceDir == 0)) {
				CalculateHalphas(nalpha,alpha,nstep[i],
						Xout[i],Yout[i],Zout[i],Bx[i],By[i],Bz[i],
						ModelFunc,iopt[i],parmod[i],alt,MaxLen,DSMax, 
						xfe,yfe,zfe,
						halpha[i]);
			}

		} else {
			/*fill with NaN*/
			nstep[i] = 0;
			for (j=0;j<15;j++) {
				FP[i][j] = NAN;
			}
		}
							
	}


	if (Verbose) {
		printf("\n");
	}	
	/*Convert everything to the desired output coords*/
	for (i=0;i<n;i++) {
		ConvertTraceCoords(nstep[i],CoordOut,Xout[i],Yout[i],Zout[i],
							Bx[i],By[i],Bz[i]);
	}
	


}

TraceCFG GetTraceCFG(	double alt, int MaxLen, double DSMax, 
						bool Verbose, int TraceDir) { 
	TraceCFG tcfg = {alt,MaxLen,DSMax,Verbose,TraceDir};

	return tcfg;
}

SimpleTrace SimpleFieldTrace( double *Xin, double *Yin, double *Zin, int n, 
						int *Date, float *ut, const char *Model, 
						int *iopt, double **parmod, 
						double *Vx, double *Vy, double *Vz,
						const char *CoordIn, const char *CoordOut, 
						double alt, int MaxLen, double DSMax, 
						bool Verbose, int TraceDir ) {
							
	ModelCFG mcfg = GetModelCFG(n,Date,ut,true,Model,iopt,parmod,Vx,Vy,
								Vz,CoordIn,CoordOut);
	
	TraceCFG tcfg = GetTraceCFG(alt,MaxLen,DSMax,Verbose,TraceDir);
	
	return SimpleFieldTrace(Xin,Yin,Zin,n,mcfg,tcfg);
}
						
//SimpleTrace SimpleFieldTrace( double *Xin, double *Yin, double *Zin, int n, 
								//ModelCFG mcfg, TraceCFG tcfg) {
	
	///* create a simple trace object */
	//SimpleTrace T;
	//T.n = n;
	
	
	
	///* loop through each trace */
	//for (i=0;i<n;i++) {
//}
	
							
//}								
	
