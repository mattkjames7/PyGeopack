#include "TraceRoutines.h"

void argmax(double *x, int n, double *xmx, int *Imx) {
	/*finds the index of the maximum within an array*/
	int i;
	xmx[0] = 0.0;
	Imx[0] = 0;
	for (i=0;i<n;i++) {
		if (x[i] > xmx[0]) {
			xmx[0] = x[i];
			Imx[0] = i;
		}
	}
}

void FieldLineDistance(double *x, double *y, double *z, int n, double *s) {
	/*******************************************************************
	 * Calculate the distance along the field line.
	 * 
	 * ****************************************************************/
	int i;
	double ds, dx, dy, dz;
	s[0] = 0.0;
	for (i=1;i<n;i++) {
		dx = x[i] - x[i-1];
		dy = y[i] - y[i-1];
		dz = z[i] - z[i-1];
		ds = sqrt(dx*dx + dy*dy + dz*dz);
		s[i] = s[i-1] + ds;
	}
}

void FieldLineMidPoint(double *x, double *y, double *z, double *s, int n, 
	double *xm, double *ym, double *zm) {
	/*******************************************************************
	 * Calculate the coordinates of the mid point along a field line.
	 * 
	 * ****************************************************************/
	int i, i0, i1;
	double sm, ds, m;
	sm = s[n-1]/2.0;
	/* find the midpoint indices*/
	for (i=0;i<n-1;i++) {
		if ((s[i] <= sm) && (s[i+1] > sm)) {
			i0 = i;
			i1 = i + 1;
			break;
		}
	}
	
	/* now interpolate x*/
	xm[0] = linterp(s[i0],s[i1],x[i0],x[i1],sm);
	ym[0] = linterp(s[i0],s[i1],y[i0],y[i1],sm);
	zm[0] = linterp(s[i0],s[i1],z[i0],z[i1],sm);
	

} 

void FieldLineR(double *x, double *y, double *z, int n, double *R) {
	/*******************************************************************
	 * Calculate R along the field line.
	 * 
	 * ****************************************************************/
	int i;
	
	/*calculate R*/
	for (i=0;i<n;i++) {
		R[i] = sqrt(x[i]*x[i] + y[i]*y[i] + z[i]*z[i]);
	}

}

void FieldLineRnorm(double *R, int n, double Lshell, double *Rnorm) {
	/*******************************************************************
	 * Calculate R norm along the field line.
	 * 
	 * ****************************************************************/
	int i;
	
	for (i=0;i<n;i++) {
		Rnorm[i] = R[i]/Lshell;
	}
	
}

void GetMagEquatorFP(double *x, double *y, double *z, double *s, double *R, int n, double *Lshell, double *MltE) {
	/*******************************************************************
	 * This function will replace the old "NorthSouthFLs" and find the 
	 * point farthest away from the planet, unless that point is a 
	 * significant distance away from the SM x-y plane in the dayside.
	 * 
	 * ****************************************************************/
	int i, Imx, dirn=-1;
	double Rmx, rho, xt, yt, zt, a;
	



	/*find the maximum R*/
	argmax(R,n,&Rmx,&Imx);

	/*Convert to SM */
	smgsw_08_(&xt,&yt,&zt,&x[Imx],&y[Imx],&z[Imx],&dirn);

	if (x[Imx] < 0.0) {
		/* if we are on the night side, just use the furthest point */
		Lshell[0] = R[Imx];
		MltE[0] = fmod(atan2(-yt,-xt)*12.0/M_PI + 24.0,24.0);
	} else {
		/*convert to SM and check that it is within 10 degrees of SM x-y plane*/
		
		rho = sqrt(xt*xt + yt*yt);
		a = acos(rho/Rmx)*180.0/M_PI;
		if (a > 10.0) {
			/* at this point use the midpoint along the field line */
			FieldLineMidPoint(x,y,z,s,n,&xt,&yt,&zt);
			Lshell[0] = sqrt(xt*xt + yt*yt + zt*zt);
			MltE[0] = fmod(atan2(-yt,-xt)*12.0/M_PI + 24.0,24.0);
			
		} else { 
			/*just use the largest R*/
			Lshell[0] = R[Imx];
			MltE[0] = fmod(atan2(-yt,-xt)*12.0/M_PI + 24.0,24.0);	
		}
	}
	
}


void NorthSouthFLs(double flx[],double fly[],double flz[], double *R, int N, double **Nflx, double **Nfly, double **Nflz, double **NR, int *nn, double **Sflx, double **Sfly, double **Sflz, double **SR, int *ns) {
	int i,cn = 0, cs = 0;
	while (flz[cn] >= 0 && isfinite(flz[cn]) && cn < N) {
		cn++;
	}

	(*nn)=cn;
	if (cn > 0) {
		*Nflx = malloc(cn*sizeof(double));
		*Nfly = malloc(cn*sizeof(double));
		*Nflz = malloc(cn*sizeof(double));
		*NR = malloc(cn*sizeof(double));
		for (i=0;i<cn;i++) {
			(*Nflx)[i]=flx[i];
			(*Nfly)[i]=fly[i];
			(*Nflz)[i]=flz[i];
			(*NR)[i]=R[i];
		}
	} else { 
		*Nflx=NULL;
		*Nfly=NULL;
		*Nflz=NULL;
	}
	
	i=cn+1;
	while (flz[i] < 0 && isfinite(flz[i]) && i < N) {
		cs++;
		i++;
	}
	(*ns)=cs;
	if (cs > 0) {
		*Sflx = malloc(cs*sizeof(double));
		*Sfly = malloc(cs*sizeof(double));
		*Sflz = malloc(cs*sizeof(double));
		*SR = malloc(cs*sizeof(double));

		for (i=0;i<cs;i++) {
			(*Sflx)[i]=flx[(cn+cs-1)-i];
			(*Sfly)[i]=fly[(cn+cs-1)-i];
			(*Sflz)[i]=flz[(cn+cs-1)-i];
			(*SR)[i]=R[(cn+cs-1)-i];
		}
	} else { 
		*Sflx=NULL;
		*Sfly=NULL;
		*Sflz=NULL;
	}	
		
}

double linterp(double x0, double x1, double y0, double y1, const double xt) {
	double m;
	m = (y1 - y0)/(x1 - x0);
	return m*(xt - x0) + y0;
}

void EqFootprint(double *Nflx, double *Nfly, double *Nflz, int nN, double *Sflx, double *Sfly, double *Sflz, int nS, double *lshell, double *mlte) {

	if (nN > 0 && nS > 0) {
		double fpx, fpy;
		fpx = linterp(Sflz[nS-1],Nflz[nN-1],Nflx[nN-1],Sflx[nS-1],0.0);
		fpy = linterp(Sflz[nS-1],Nflz[nN-1],Nfly[nN-1],Sfly[nS-1],0.0);
		*lshell = sqrt(pow(fpx,2.0) + pow(fpy,2.0));
		*mlte = fmod((M_PI + atan2(fpy,fpx)) , (2*M_PI)) * (180.0/M_PI)/15.0;
	} else { 
		*lshell = NAN;
		*mlte = NAN;
	}
	return;
}

/*convert cartesian to spherical polar coordinates*/	
void CartToSpherical(double x, double y, double z, double *r, double *theta, double *phi) {
	
	double sq = powf(x,2) + powf(y,2);
	*r = sqrtf(sq + powf(z,2));
	if (sq > 0.0) {
		sq = sqrt(sq);
		*phi = atan2f(y,x);
		*theta = atan2f(sq,z);
	} else {
		*phi = 0.0;
		if (z < 0.0) {
			*theta = M_PI;
		} else {
			*theta = 0.0;
		}
	}		
	return;
}

double CalculateFieldLineLength(double *x, double *y, double *z, int n) {
	int i;
	double len=0.0;
	for (i=0;i<n-1;i++) {
		len += sqrtf(powf((x[i]-x[i+1]),2.0) + powf((y[i]-y[i+1]),2.0) + powf((z[i]-z[i+1]),2.0));
	}
	return len;
}


void TraceField(double *Xin, double *Yin, double *Zin, int n, 
				int *Date, float *ut, const char *Model, int CoordIn, int CoordOut, 
				double alt, int MaxLen, double DSMax, 
				double *Xout, double *Yout, double *Zout, 
				double *s, double *R, double *Rnorm,
				double *Bx, double *By, double *Bz, 
				int *nstep, double *FP, bool Verbose) {
/*				double *GlatN, double *GlatS, double *MlatN, double *MlatS,
				double *GlonN, double *GlonS, double *MlonN, double *MlonS,double *GltN, double *GltS, double *MltN,
				double *MltS, double *Lshell, double *MltE, double *FlLen, bool Verbose) {
	*/
	/*******************************************************************
	 * FP is an array containing the following:
	 * 		GlatN,GlatS,MlatN,MlatS,GlonN,GlonS,MlonN,MlonS
	 * 		GltN,GltS,MltN,MltS,Lshell,MltE,FlLen (n*15 elements)
	 * 
	 * 
	 * ****************************************************************/
	
	
	int dirp = 1, dirn = -1;
	/*Check that TSData has been loaded*/
	if (TSData.n == 0) {
		LoadTSData();
	} 

	/* move all of the declarations here, before the loop*/
	int Year, DayNo, Hr, Mn, Sc, i, j;
	ModelFuncPtr ModelFunc;
	double xfn,yfn,zfn,xfs,yfs,zfs;
	int iopt;
	double parmod[10], tilt, Vx, Vy, Vz;
	double X[n], Y[n], Z[n];
	bool update, inMP;
	
	/*get model function and parmod*/
	if ((strcmp(Model,"T89") == 0) || (strcmp(Model,"T89c") == 0)){
		ModelFunc = &t89c_;
	} else if ((strcmp(Model,"T96") == 0) || (strcmp(Model,"T96c") == 0)) {
		ModelFunc = &t96_;
	} else if ((strcmp(Model,"T01") == 0) || (strcmp(Model,"T01c") == 0)) {
		ModelFunc = &t01_01_;
	} else if ((strcmp(Model,"TS05") == 0) || (strcmp(Model,"TS05c") == 0)) {
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
		/*convert date into Year and DayNo*/
		DateToYearDayNo(Date[i],&Year,&DayNo);
		
		/*convert decimal UT to Hr, Mn, Sc*/
		DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
	
		update = false;
		if (i == 0) {
			update = true;
		} else if ((Date[i] != Date[i-1]) || (ut[i] != ut[i-1])) {
			update = true;
		}

		if (update) {
			/*get params and recalc08*/
			GetModelParams(Date[i],ut[i],Model,&iopt,parmod,&tilt,&Vx,&Vy,&Vz);
			recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
			tilt = getpsi_();
		}
		/*Convert input coordinates to GSM*/
		

		switch (CoordIn) {
			case 1:
				/*GSE in*/
				gswgse_08_(&X[i],&Y[i],&Z[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
				break;
			case 2:
				/*GSM in*/
				X[i] = Xin[i];
				Y[i] = Yin[i];
				Z[i] = Zin[i];
				break;
			case 3:
				/*SM in*/
				smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&X[i],&Y[i],&Z[i],&dirp);
				break;
			default:
				printf("Input coordinate type not recognised\n");
				return;	
				break;	
		}
		
		/*Check if the point is within the MP*/
		inMP = WithinMP(X[i],Y[i],Z[i],parmod[3],parmod[0]);
		
		
		if (inMP) {
			/* perform trace */
			TraceFieldLine(X[i],Y[i],Z[i],iopt,parmod,ModelFunc,alt,MaxLen,DSMax,&xfn,&yfn,&zfn,&xfs,&yfs,&zfs,&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&nstep[i]);

			/*get B vectors along trace*/
			ModelField(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],Date[i],ut[i],Model,2,2,&Bx[i*MaxLen],&By[i*MaxLen],&Bz[i*MaxLen]);

			/* Get the distance along the field line*/
			FieldLineDistance(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],&s[i*MaxLen]);

			/* Get the radius of each point */
			FieldLineR(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],&R[i*MaxLen]);

			/* find trace footprints */
			TraceFootprints(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&s[i*MaxLen],&R[i*MaxLen],nstep[i],xfn,yfn,zfn,xfs,yfs,zfs,alt,&FP[i*15],MaxLen);

			/* Get the Rnorm of each point */
			FieldLineRnorm(&R[i*MaxLen],nstep[i],FP[i*15+12],&Rnorm[i*MaxLen]);
							
		} else {
			/*fill with NaN*/
			nstep[i] = 0;
			for (j=0;j<15;j++) {
				FP[i*15+j] = NAN;
			}
		}
							
	}


	if (Verbose) {
		printf("\n");
	}	
	/*Convert everything to the desired output coords*/
	for (i=0;i<n;i++) {
		ConvertTraceCoords(nstep[i],CoordOut,&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&Bx[i*MaxLen],&By[i*MaxLen],&Bz[i*MaxLen]);
	}
	

	

}

void ConvertTraceCoords(int nstep, int CoordOut, double *x, double *y, double *z, double *Bx, double *By, double *Bz) {
	int dirp = 1, dirn = -1, i;
	double xtmp, ytmp, ztmp;
	/*now to convert the vectors to the desired output coordinates*/
	switch (CoordOut) {
		case 1:
			/*GSE Out*/
			for (i=0;i<nstep;i++) {
				gswgse_08_(&Bx[i],&By[i],&Bz[i],&xtmp,&ytmp,&ztmp,&dirp);
				Bx[i] = xtmp;
				By[i] = ytmp;
				Bz[i] = ztmp;
				gswgse_08_(&x[i],&y[i],&z[i],&xtmp,&ytmp,&ztmp,&dirp);
				x[i] = xtmp;
				y[i] = ytmp;
				z[i] = ztmp;
			}
			break;
		case 2:
			/*GSM Out:
			 * Do nothing - model already outputs GSM*/
			
			break;
		case 3:
			/*SM Out*/
			for (i=0;i<nstep;i++) {
				smgsw_08_(&xtmp,&ytmp,&ztmp,&Bx[i],&By[i],&Bz[i],&dirn);
				Bx[i] = xtmp;
				By[i] = ytmp;
				Bz[i] = ztmp;
				smgsw_08_(&xtmp,&ytmp,&ztmp,&x[i],&y[i],&z[i],&dirn);
				x[i] = xtmp;
				y[i] = ytmp;
				z[i] = ztmp;
			}
			break;
		default:
			printf("Output coordinate type not recognised\n");
			return;	
			break;	
	}	

}

void MagLatLonLT(double x, double y, double z, double *lat, double *lon, double *lt) {
	int dirp = 1;
	int dirn = -1;
	double X1, Y1, Z1, X2, Y2, Z2;
	double r, theta, phi;
	
	/*convert GSW to SM*/
	smgsw_08_(&X1,&Y1,&Z1,&x,&y,&z,&dirn);
	
	/* convert SM to MAG */
	magsm_08_(&X2,&Y2,&Z2,&X1,&Y1,&Z1,&dirn);
	
	/* Calculate the spherical coordinate */
	CartToSpherical(X2,Y2,Z2,&r,&theta,&phi);	
	
	/* latitude */
	lat[0] = 90.0 - (theta*180.0/M_PI);
	
	/* longitude */
	lon[0] = phi*180.0/M_PI;
	
	/* local time */	
	lt[0] = fmod(atan2(-Y1,-X1)*12.0/M_PI + 24.0,24.0);
}

void GeoLatLonLT(double x, double y, double z, double *lat, double *lon, double *lt) {
	int dirp = 1;
	int dirn = -1;
	double X1, Y1, Z1;
	double r, theta, phi;
	
	/*convert GSW to SM*/
	geogsw_08_(&X1,&Y1,&Z1,&x,&y,&z,&dirn);

	/* Calculate the spherical coordinate */
	CartToSpherical(X1,Y1,Z1,&r,&theta,&phi);	
	
	/* latitude */
	lat[0] = 90.0 - (theta*180.0/M_PI);
	
	/* longitude */
	lon[0] = phi*180.0/M_PI;
	
	/* local time */	
	lt[0] = fmod(atan2(-Y1,-X1)*12.0/M_PI + 24.0,24.0);
}



void TraceFootprints(double *x, double *y, double *z, double *s, double *R, int nstep, double xfn, double yfn, double zfn, 
					double xfs, double yfs, double zfs, double alt, double *FP, int MaxLen) {


	double MaxR = (Re + alt)/Re + 0.01;
	double RFN, RFS;
	RFN = sqrt(powf(xfn,2.0) + powf(yfn,2.0) + powf(zfn,2.0));
	RFS = sqrt(powf(xfs,2.0) + powf(yfs,2.0) + powf(zfs,2.0));
	
	double MlatN,MlatS,GlatN,GlatS;
	double MlonN,MlonS,GlonN,GlonS;
	double MltN,MltS,MltE,GltN,GltS;
	double FlLen,Lshell;

	/* Calculate the lat, long and lt of the northern footprint*/
	if (RFN <= MaxR) {
		GeoLatLonLT(xfn,yfn,zfn,&GlatN,&GlonN,&GltN);
		MagLatLonLT(xfn,yfn,zfn,&MlatN,&MlonN,&MltN);
	} else {
		MltN = NAN;
		MlatN = NAN;
		MlonN = NAN;
		GltN = NAN;
		GlatN = NAN;
		GlonN = NAN;			
	}

	/* Calculate the lat, long and lt of the southern footprint*/ 
	if (RFS <= MaxR) {
		GeoLatLonLT(xfs,yfs,zfs,&GlatS,&GlonS,&GltS);
		MagLatLonLT(xfs,yfs,zfs,&MlatS,&MlonS,&MltS);
	} else {
		MltS = NAN;
		MlatS = NAN;
		MlonS = NAN;
		GltS = NAN;
		GlatS = NAN;
		GlonS = NAN;		
	}

	if ((RFN <= MaxR) && (RFS <= MaxR)) {
		/* Calculate the position of the equatorial footprint */
		GetMagEquatorFP(x,y,z,s,R,nstep,&Lshell,&MltE);
		FlLen = s[nstep-1];
	} else {
		/* Don't provide FlLen and Lshell and MltE if the field line is open*/
		Lshell = NAN;
		MltE = NAN;
		FlLen = NAN;
	}
		
	/* place all the footprints in the output array*/
	FP[0] = GlatN;
	FP[1] = GlatS;
	FP[2] = MlatN;
	FP[3] = MlatS;
	FP[4] = GlonN;
	FP[5] = GlonS;
	FP[6] = MlonN;
	FP[7] = MlonS;
	FP[8] = GltN;
	FP[9] = GltS;
	FP[10] = MltN;
	FP[11] = MltS;
	FP[12] = Lshell;
	FP[13] = MltE;
	FP[14] = FlLen;
	
}

void ReverseElements(double *x, int n) {
	double tmp[n];
	int i;
	for (i=0;i<n;i++) {
		tmp[i] = x[i];
	}
	for (i=0;i<n;i++) {
		x[i] = tmp[n-i-1];
	}
	return;
}

void TraceFieldLine(double x0, double y0, double z0, int iopt, double *parmod, ModelFuncPtr ModelFunc,double alt, int MaxLen, double DSMax, double *xfn, double *yfn, double *zfn, double *xfs, double *yfs, double *zfs, double *x, double *y, double *z, int *nstep) {
	
	/*calculate the radial distance of the stopping altitude*/
    double R = (alt + Re)/Re;
    
	/*next we need to trace backwards along the field line towards the north pole*/
	int N = 0, M = 0;
	double dir = -1.0;
	double Err = 0.0001;
	double Rlim = 1000.0;
	int Nmax = MaxLen/2 - 2;
	trace_08_(&x0,&y0,&z0,&dir,&DSMax,&Err,&Rlim,&R,&iopt,parmod,ModelFunc,&igrf_gsw_08_,xfn,yfn,zfn,x,y,z,&N,&Nmax);

	/*reverse array elements*/
	ReverseElements(x,N);
	ReverseElements(y,N);
	ReverseElements(z,N);
	
	/*now for the southern part of the field line*/
	Nmax = MaxLen - N;
	dir = 1.0;
	trace_08_(&x0,&y0,&z0,&dir,&DSMax,&Err,&Rlim,&R,&iopt,parmod,ModelFunc,&igrf_gsw_08_,xfs,yfs,zfs,&x[N-1],&y[N-1],&z[N-1],&M,&Nmax);
	
	nstep[0] = N + M - 1;
}
