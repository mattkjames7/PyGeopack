#include "TraceRoutines.h"

void NorthSouthFLs(float flx[],float fly[],float flz[], float *R, int N, float **Nflx, float **Nfly, float **Nflz, float **NR, int *nn, float **Sflx, float **Sfly, float **Sflz, float **SR, int *ns) {
	int i,cn = 0, cs = 0;
	while (flz[cn] >= 0 && isfinite(flz[cn]) && cn < N) {
		cn++;
	}

	(*nn)=cn;
	if (cn > 0) {
		*Nflx = malloc(cn*sizeof(float));
		*Nfly = malloc(cn*sizeof(float));
		*Nflz = malloc(cn*sizeof(float));
		*NR = malloc(cn*sizeof(float));
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
		*Sflx = malloc(cs*sizeof(float));
		*Sfly = malloc(cs*sizeof(float));
		*Sflz = malloc(cs*sizeof(float));
		*SR = malloc(cs*sizeof(float));

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

float linterp(float x0, float x1, float y0, float y1, const float xt) {
	float m;
	m = (y1 - y0)/(x1 - x0);
	return m*(xt - x0) + y0;
}

void EqFootprint(float *Nflx, float *Nfly, float *Nflz, int nN, float *Sflx, float *Sfly, float *Sflz, int nS, float *lshell, float *mlte) {

	if (nN > 0 && nS > 0) {
		float fpx, fpy;
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
void CartToSpherical(float x, float y, float z, float *r, float *theta, float *phi) {
	
	float sq = powf(x,2) + powf(y,2);
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

float CalculateFieldLineLength(float *x, float *y, float *z, int n) {
	int i;
	float len=0.0;
	for (i=0;i<n-1;i++) {
		len += sqrtf(powf((x[i]-x[i+1]),2.0) + powf((y[i]-y[i+1]),2.0) + powf((z[i]-z[i+1]),2.0));
	}
	return len;
}


//void TraceField(float *Xin, float *Yin, float *Zin, int n, int Date, float ut, const char *Model, int CoordIn, int CoordOut, 
				//float alt, int MaxLen, float DSMax, float *Xout, float *Yout, float *Zout,
				//float *Bx, float *By, float *Bz, int *nstep, float *GlatN, float *GlatS, float *MlatN, float *MlatS,
				//float *GlonN, float *GlonS, float *MlonN, float *MlonS,float *GltN, float *GltS, float *MltN,
				//float *MltS, float *Lshell, float *MltE, float *FlLen) {
	
	//int dirp = 1, dirn = -1;
	///*Check that TSData has been loaded*/
	//if (TSData.n == 0) {
		//LoadTSData();
	//} 
	
	//int Year, DayNo, Hr, Mn, Sc, i;
	///*convert date into Year and DayNo*/
	//DateToYearDayNo(Date,&Year,&DayNo);
	
	///*convert decimal UT to Hr, Mn, Sc*/
	//DecUTToHHMMSS(ut,&Hr,&Mn,&Sc);

	//ModelFuncPtr ModelFunc;
	
	///*get model function and parmod*/
	//if ((strcmp(Model,"T89") == 0) || (strcmp(Model,"T89c") == 0)){
		//ModelFunc = &t89c_;
	//} else if ((strcmp(Model,"T96") == 0) || (strcmp(Model,"T96c") == 0)) {
		//ModelFunc = &t96_;
	//} else if ((strcmp(Model,"T01") == 0) || (strcmp(Model,"T01c") == 0)) {
		//ModelFunc = &t01_01_;
	//} else if ((strcmp(Model,"TS05") == 0) || (strcmp(Model,"TS05c") == 0)) {
		//ModelFunc = &t04_s_;
	//} else if (strcmp(Model,"IGRF") == 0) {
		//ModelFunc = &DummyFunc;
	//} else { 
		//printf("Model %s not found\n",Model);
		//return;
	//}
	
	
	///*get params and recalc08*/
	//int iopt;
	//float parmod[10], tilt, Vx, Vy, Vz;
	//GetModelParams(Date,ut,Model,&iopt,parmod,&tilt,&Vx,&Vy,&Vz);
	//recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	
	///*Convert input coordinates to GSM*/
	//float X[n], Y[n], Z[n];
	//switch (CoordIn) {
		//case 1:
			///*GSE in*/
			//for (i=0;i<n;i++) {
				//gswgse_08_(&X[i],&Y[i],&Z[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
			//}
			//break;
		//case 2:
			///*GSM in*/
			//for (i=0;i<n;i++) {
				//X[i] = Xin[i];
				//Y[i] = Yin[i];
				//Z[i] = Zin[i];
			//}
			//break;
		//case 3:
			///*SM in*/
			//for (i=0;i<n;i++) {
				//smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&X[i],&Y[i],&Z[i],&dirp);
			//}
			//break;
		//default:
			//printf("Input coordinate type not recognised\n");
			//return;	
			//break;	
	//}
	
	//float xfn,yfn,zfn,xfs,yfs,zfs;
	//for (i=0;i<n;i++) {
		///* perform trace */
		//TraceFieldLine(X[i],Y[i],Z[i],iopt,parmod,ModelFunc,alt,MaxLen,DSMax,&xfn,&yfn,&zfn,&xfs,&yfs,&zfs,&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&nstep[i]);
		
		///*get B vectors along trace*/
		//ModelField(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],Date,ut,Model,2,2,&Bx[i*MaxLen],&By[i*MaxLen],&Bz[i*MaxLen]);
		
		///* find trace footprints */
		//TraceFootprints(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],xfn,yfn,zfn,xfs,yfs,zfs,alt,&MltN[i],&MlatN[i],&MlonN[i],&GltN[i],&GlatN[i],&GlonN[i],
						//&MltS[i],&MlatS[i],&MlonS[i],&GltS[i],&GlatS[i],&GlonS[i],&Lshell[i],&MltE[i],&FlLen[i],MaxLen);
						
	//}
	
	///*Convert everything to the desired output coords*/
	//for (i=0;i<n;i++) {
		//ConvertTraceCoords(nstep[i],CoordOut,&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&Bx[i*MaxLen],&By[i*MaxLen],&Bz[i*MaxLen]);
	//}
	

	

//}

void TraceField(float *Xin, float *Yin, float *Zin, int n, int *Date, float *ut, const char *Model, int CoordIn, int CoordOut, 
				float alt, int MaxLen, float DSMax, float *Xout, float *Yout, float *Zout,
				float *Bx, float *By, float *Bz, int *nstep, float *GlatN, float *GlatS, float *MlatN, float *MlatS,
				float *GlonN, float *GlonS, float *MlonN, float *MlonS,float *GltN, float *GltS, float *MltN,
				float *MltS, float *Lshell, float *MltE, float *FlLen, bool Verbose) {
	
	int dirp = 1, dirn = -1;
	/*Check that TSData has been loaded*/
	if (TSData.n == 0) {
		LoadTSData();
	} 
	
	/* move all of the declarations here, before the loop*/
	int Year, DayNo, Hr, Mn, Sc, i;
	ModelFuncPtr ModelFunc;
	float xfn,yfn,zfn,xfs,yfs,zfs;
	int iopt;
	float parmod[10], tilt, Vx, Vy, Vz;
	float X[n], Y[n], Z[n];
	bool update;
	
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


		/* perform trace */
		TraceFieldLine(X[i],Y[i],Z[i],iopt,parmod,ModelFunc,alt,MaxLen,DSMax,&xfn,&yfn,&zfn,&xfs,&yfs,&zfs,&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&nstep[i]);
		
		/*get B vectors along trace*/
		ModelField(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],Date[i],ut[i],Model,2,2,&Bx[i*MaxLen],&By[i*MaxLen],&Bz[i*MaxLen]);
		
		/* find trace footprints */
		TraceFootprints(&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],nstep[i],xfn,yfn,zfn,xfs,yfs,zfs,alt,&MltN[i],&MlatN[i],&MlonN[i],&GltN[i],&GlatN[i],&GlonN[i],
						&MltS[i],&MlatS[i],&MlonS[i],&GltS[i],&GlatS[i],&GlonS[i],&Lshell[i],&MltE[i],&FlLen[i],MaxLen);
						
	}


	if (Verbose) {
		printf("\n");
	}	
	/*Convert everything to the desired output coords*/
	for (i=0;i<n;i++) {
		ConvertTraceCoords(nstep[i],CoordOut,&Xout[i*MaxLen],&Yout[i*MaxLen],&Zout[i*MaxLen],&Bx[i*MaxLen],&By[i*MaxLen],&Bz[i*MaxLen]);
	}
	

	

}

void ConvertTraceCoords(int nstep, int CoordOut, float *x, float *y, float *z, float *Bx, float *By, float *Bz) {
	int dirp = 1, dirn = -1, i;
	float xtmp, ytmp, ztmp;
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

void TraceFootprints(float *x, float *y, float *z, int nstep, float xfn, float yfn, float zfn, float xfs, float yfs, float zfs, float alt,float *MltN, float *MlatN, float *MlonN,
					float *GltN, float *GlatN, float *GlonN, float *MltS, float *MlatS, float *MlonS, float *GltS, float *GlatS,float *GlonS, float *Lshell, float *MltE, float *FlLen, int MaxLen) {
	int i;			
	float r,theta,phi,MaxR = (Re + alt)/Re + 0.01, X, Y, Z, X2, Y2, Z2, XSM[MaxLen], YSM[MaxLen], ZSM[MaxLen];
	int dirp = 1, dirn = -1;
	float RFN, RFS;
	RFN = sqrt(powf(xfn,2.0) + powf(yfn,2.0) + powf(zfn,2.0));
	RFS = sqrt(powf(xfs,2.0) + powf(yfs,2.0) + powf(zfs,2.0));

	if (RFN <= MaxR) {
		*MltN = atan2f(-yfn,-xfn)*180.0/(M_PI*15.0);
		if (*MltN < 0.0) {
			(*MltN) += 24.0;
		}
		smgsw_08_(&X,&Y,&Z,&xfn,&yfn,&zfn,&dirn);
		magsm_08_(&X2,&Y2,&Z2,&X,&Y,&Z,&dirn);
		CartToSpherical(X2,Y2,Z2,&r,&theta,&phi);
		*MlatN = 90.0 - (theta*180.0/M_PI);
		*MlonN = phi*180.0/M_PI;

		gswgse_08_(&xfn,&yfn,&zfn,&X,&Y,&Z,&dirp);
		*GltN = atan2f(-Y,-X)*180.0/(M_PI*15.0);
		if (*GltN < 0.0) {
			(*GltN) += 24.0;
		}
			
		geogsw_08_(&X2,&Y2,&Z2,&xfn,&yfn,&zfn,&dirn);
		CartToSpherical(X2,Y2,Z2,&r,&theta,&phi);
		*GlatN = 90.0 - (theta*180.0/M_PI);
		*GlonN = phi*180.0/M_PI;		
	} else {
		*MltN = NAN;
		*MlatN = NAN;
		*MlonN = NAN;
		*GltN = NAN;
		*GlatN = NAN;
		*GlonN = NAN;			
	}
	//printf("here %d\n",h++);
	if (RFS <= MaxR) {
		*MltS = atan2f(-yfs,-xfs)*180.0/(M_PI*15.0);
		if (*MltS < 0.0) {
			(*MltS) += 24.0;
		}
		smgsw_08_(&X,&Y,&Z,&xfs,&yfs,&zfs,&dirn);
		magsm_08_(&X2,&Y2,&Z2,&X,&Y,&Z,&dirn);
		CartToSpherical(X2,Y2,Z2,&r,&theta,&phi);
		*MlatS = 90.0 - (theta*180.0/M_PI);
		*MlonS = phi*180.0/M_PI;	
		
		gswgse_08_(&xfs,&yfs,&zfs,&X,&Y,&Z,&dirp);
		*GltS = atan2f(-Y,-X)*180.0/(M_PI*15.0);
		if (*GltS < 0.0) {
			(*GltS) += 24.0;
		}
			
		geogsw_08_(&X2,&Y2,&Z2,&xfs,&yfs,&zfs,&dirn);
		CartToSpherical(X2,Y2,Z2,&r,&theta,&phi);
		*GlatS = 90.0 - (theta*180.0/M_PI);
		*GlonS = phi*180.0/M_PI;	
	} else {
		*MltS = NAN;
		*MlatS = NAN;
		*MlonS = NAN;
		*GltS = NAN;
		*GlatS = NAN;
		*GlonS = NAN;		
	}


	
	/*SM coordinates are probably best for the equatorial plane footprint as this would be rotated with dipole axis*/
	float R[MaxLen];
	for (i=0;i<=nstep;i++) {
		smgsw_08_(&XSM[i],&YSM[i],&ZSM[i],&x[i],&y[i],&z[i],&dirn);
		R[i] = sqrt(XSM[i]*XSM[i] + YSM[i]*YSM[i] + ZSM[i]*ZSM[i]);
	}
	/* First of all, determine which parts of the field lines are north and south of the equatorial plane, if any */
	
	
	float *Nflx,*Nfly,*Nflz,*NR,*Sflx,*Sfly,*Sflz,*SR;
	int nN, nS;
	
	NorthSouthFLs(XSM,YSM,ZSM,R,nstep,&Nflx,&Nfly,&Nflz,&NR,&nN,&Sflx,&Sfly,&Sflz,&SR,&nS);	
	/* Find equatorial footprint */
	EqFootprint(Nflx, Nfly, Nflz, nN, Sflx, Sfly, Sflz, nS, Lshell, MltE);		

	if (!isnan(*MlatN) && !isnan(*MlatS)) {
		*FlLen = CalculateFieldLineLength(XSM,YSM,ZSM,nstep);
	} else {
		*FlLen = NAN;
	}

	if (nN > 0) {
		free(Nflx);
		free(Nfly);
		free(Nflz);
		free(NR);
	}
	if (nS > 0) {
		free(Sflx);
		free(Sfly);
		free(Sflz);
		free(SR);
	}

	return;
}

void ReverseElements(float *x, int n) {
	float tmp[n];
	int i;
	for (i=0;i<n;i++) {
		tmp[i] = x[i];
	}
	for (i=0;i<n;i++) {
		x[i] = tmp[n-i-1];
	}
	return;
}

void TraceFieldLine(float x0, float y0, float z0, int iopt, float *parmod, ModelFuncPtr ModelFunc,float alt, int MaxLen, float DSMax, float *xfn, float *yfn, float *zfn, float *xfs, float *yfs, float *zfs, float *x, float *y, float *z, int *nstep) {
	
	/*calculate the radial distance of the stopping altitude*/
    float R = (alt + Re)/Re;
    
	/*next we need to trace backwards along the field line towards the north pole*/
	int N = 0, M = 0;
	float dir = -1.0;
	float Err = 0.0001;
	float Rlim = 1000.0;
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
