#include "libgeopack.h"

const float Re = 6371.2;
TSD TSData = {.n = 0};

void LoadTSData() {
	FILE *f = fopen(DataFile,"rb");
	if (f == NULL) {
		return;
	}
	int n;
	fread(&n,sizeof(int),1,f);
	TSData.n = n;
	TSData.Date = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.Date,sizeof(int),TSData.n,f);
	TSData.ut = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.ut,sizeof(float),TSData.n,f);
	TSData.Year = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.Year,sizeof(int),TSData.n,f);
	TSData.DayNo = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.DayNo,sizeof(int),TSData.n,f);
	TSData.Hr = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.Hr,sizeof(int),TSData.n,f);
	TSData.Mn = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.Mn,sizeof(int),TSData.n,f);
	TSData.Bx = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Bx,sizeof(float),TSData.n,f);
	TSData.By = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.By,sizeof(float),TSData.n,f);
	TSData.Bz = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Bz,sizeof(float),TSData.n,f);
	TSData.Vx = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Vx,sizeof(float),TSData.n,f);
	TSData.Vy = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Vy,sizeof(float),TSData.n,f);
	TSData.Vz = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Vz,sizeof(float),TSData.n,f);	
	TSData.Den = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Den,sizeof(float),TSData.n,f);
	TSData.Temp = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Temp,sizeof(float),TSData.n,f);
	TSData.SymH = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.SymH,sizeof(float),TSData.n,f);
	TSData.IMFFlag = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.IMFFlag,sizeof(int),TSData.n,f);
	TSData.ISWFlag = (int *) malloc(sizeof(int)*TSData.n);
	fread(TSData.ISWFlag,sizeof(int),TSData.n,f);
	TSData.Tilt = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Tilt,sizeof(float),TSData.n,f);
	TSData.Pdyn = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Pdyn,sizeof(float),TSData.n,f);
	TSData.W1 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.W1,sizeof(float),TSData.n,f);	
	TSData.W2 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.W2,sizeof(float),TSData.n,f);
	TSData.W3 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.W3,sizeof(float),TSData.n,f);
	TSData.W4 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.W4,sizeof(float),TSData.n,f);
	TSData.W5 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.W5,sizeof(float),TSData.n,f);
	TSData.W6 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.W6,sizeof(float),TSData.n,f);
	TSData.G1 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.G1,sizeof(float),TSData.n,f);	
	TSData.G2 = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.G2,sizeof(float),TSData.n,f);	
	TSData.Kp = (float *) malloc(sizeof(float)*TSData.n);
	fread(TSData.Kp,sizeof(float),TSData.n,f);		
	fclose(f);
	
	PopulateMonthInds();
}

void PopulateMonthInds() {
	int minYr, minMn, maxYr, maxMn, tmp,tmpYr,tmpMn;
	minYr = TSData.Year[0];
	minMn = (TSData.Date[0] % 10000)/100;
	maxYr = TSData.Year[TSData.n-1];
	maxMn = (TSData.Date[TSData.n-1] % 10000)/100;	
	
	TSData.minYr = minYr;
	TSData.minMn = minMn;
	TSData.nMonth = 12*(maxYr-minYr) + maxMn - minMn + 1;
	TSData.MonthInds = (int *) malloc(sizeof(int)*TSData.nMonth);
	
	tmpYr = minYr;
	tmpMn = minMn;
	
	int i, j, p = 0;
	for (i=0;i<TSData.nMonth;i++) {
		tmp = tmpYr*10000 + tmpMn*100;
		for (j=p;j<TSData.n;j++) {
			if (TSData.Date[j] >= tmp) {
				p = j;
				TSData.MonthInds[i] = j;
				tmpMn++;
				if (tmpMn > 12) {
					tmpMn = 1;
					tmpYr++;
				}
				break;
			}
		}
	}		
}

int MonthStartInd(int Date) {
	int yr, mn, ind;
	yr = Date / 10000;
	mn = (Date % 10000)/100;
	ind = (yr - TSData.minYr)*12 + mn - TSData.minMn;

	if (ind >= TSData.nMonth){
		ind = TSData.nMonth-1;
	}
	return TSData.MonthInds[ind];
}

void FreeTSData() {
	if (TSData.n > 0) {
		free(TSData.Date);
		free(TSData.ut);
		free(TSData.Year);
		free(TSData.DayNo);
		free(TSData.Hr);
		free(TSData.Mn);
		free(TSData.Bx);
		free(TSData.By);
		free(TSData.Bz);
		free(TSData.Vx);
		free(TSData.Vy);
		free(TSData.Vz);
		free(TSData.Den);
		free(TSData.Temp);
		free(TSData.SymH);
		free(TSData.IMFFlag);
		free(TSData.ISWFlag);
		free(TSData.Tilt);
		free(TSData.Pdyn);
		free(TSData.W1);
		free(TSData.W2);
		free(TSData.W3);
		free(TSData.W4);
		free(TSData.W5);
		free(TSData.W6);
		free(TSData.G1);
		free(TSData.G2);
		free(TSData.Kp);
		free(TSData.MonthInds);
		TSData.n = 0;
	}
}

float InterpParam(float *x, int Date, float ut) {
	/*First get the start ind for searching for this date*/
	int ind = MonthStartInd(Date);
	int i, i0, i1;
	/*now start search for nearest two indices*/
	if ((TSData.Date[ind] > Date) || ((TSData.Date[ind] == Date) && (TSData.ut[ind] > ut))){
		//if the index corresponds to a time after the current time then set i0 to the time index before
		i0 = ind - 1;
		if (i0 < 0) {
			i0 = 0;
		}
		i1 = i0+1;
	} else {
		i = ind;
		while ((i < TSData.n - 1) && ((TSData.Date[i] < Date) || ((TSData.Date[i] == Date) && (TSData.ut[i] <= ut)))) {
			i++;
		}
		i0 = i-1;
		i1 = i;
	}	

	/*now to calculate time differences between two points and the first point witht he requested time*/
	float dt, dtp;
	dt = TimeDifference(TSData.Date[i0],TSData.ut[i0],TSData.Date[i1],TSData.ut[i1]);
	dtp = TimeDifference(TSData.Date[i0],TSData.ut[i0],Date,ut);
	/*now the linear interpolation*/
	float out, m, c;
	m = (x[i1]-x[i0])/dt;
	c = x[i0];
	out = m*dtp + c;
	return out;
}

void GetModelParams(int Date, float ut, const char *Model, int *iopt, float *parmod, float *tilt, float *Vx, float *Vy, float *Vz) {
	/*fall back if the data are still not loaded*/
	if (TSData.n == 0) {
		iopt[0] = 1;
		parmod[0] = 2.0;
		parmod[1] = 0.0;
		parmod[2] = 0.0;
		parmod[3] = 0.0;
		parmod[4] = 0.0;
		parmod[5] = 0.0;
		parmod[6] = 0.0;
		parmod[7] = 0.0;
		parmod[8] = 0.0;
		parmod[9] = 0.0;
		Vx[0] = -400.0;
		Vy[0] = 0.0;
		Vz[0] = 0.0;
		tilt[0] = 0.0;
	} else {
	
		/*Dipole tilt*/
		tilt[0] = InterpParam(TSData.Tilt,Date,ut);

		/*Vx, Vy, Vz*/
		Vx[0] = InterpParam(TSData.Vx,Date,ut);
		Vy[0] = InterpParam(TSData.Vy,Date,ut);
		Vz[0] = InterpParam(TSData.Vz,Date,ut);	
		/*The easiest one is T89 - just need Kp*/
		if ((strcmp(Model,"T89") == 0) || (strcmp(Model,"T89c") == 0)) {
			iopt[0] = (int) InterpParam(TSData.Kp,Date,ut) + 1;
			if (iopt[0] > 7) {
				iopt[0] = 7;
			} else if (iopt[0] < 1) {
				iopt[0] = 1;
			}
	//		return;
		/* Then T96 is Pdyn, Dst (SymH in this case), By, Bz */
		} else if ((strcmp(Model,"T96") == 0) || (strcmp(Model,"T96c") == 0)) {
			parmod[0] = InterpParam(TSData.Pdyn,Date,ut);
			parmod[1] = InterpParam(TSData.SymH,Date,ut);
			parmod[2] = InterpParam(TSData.By,Date,ut);
			parmod[3] = InterpParam(TSData.Bz,Date,ut);
	//		return;
		/* Next T01 which uses: Pdyn, Dst, By, Bz, G1, G2*/
		} else if ((strcmp(Model,"T01") == 0) || (strcmp(Model,"T01c") == 0)) {
			parmod[0] = InterpParam(TSData.Pdyn,Date,ut);
			parmod[1] = InterpParam(TSData.SymH,Date,ut);
			parmod[2] = InterpParam(TSData.By,Date,ut);
			parmod[3] = InterpParam(TSData.Bz,Date,ut);
			parmod[4] = InterpParam(TSData.G1,Date,ut);
			parmod[5] = InterpParam(TSData.G2,Date,ut);
	//		return;
		/*TS05: Pdyn, Dst, By, Bz, W1, W2, W3, W4, W5, W6*/
		} else if ((strcmp(Model,"TS05") == 0) || (strcmp(Model,"TS05c") == 0)) {
			parmod[0] = InterpParam(TSData.Pdyn,Date,ut);
			parmod[1] = InterpParam(TSData.SymH,Date,ut);
			parmod[2] = InterpParam(TSData.By,Date,ut);
			parmod[3] = InterpParam(TSData.Bz,Date,ut);
			parmod[4] = InterpParam(TSData.W1,Date,ut);
			parmod[5] = InterpParam(TSData.W2,Date,ut);		
			parmod[6] = InterpParam(TSData.W3,Date,ut);
			parmod[7] = InterpParam(TSData.W4,Date,ut);
			parmod[8] = InterpParam(TSData.W5,Date,ut);
			parmod[9] = InterpParam(TSData.W6,Date,ut);	
	//		return;
		} 
	}
	if (strchr(Model,'c') != NULL) {
		//In this bit we have chosen a custom model, so will use the custom params stored in CustP
		//So, setting CustP needs to be done first!
		if (!isnan(CustP.tilt)) {
			//if we set CustP.tilt = NaN then we use the default interpolated value
			//otherwise we set it to a specific value
			tilt[0] = CustP.tilt;
		}
		if ((CustP.iopt > 0) && (CustP.iopt < 8)) {
			iopt[0] = CustP.iopt;
		}
		if (!isnan(CustP.Vx)) {
			Vx[0] = CustP.Vx;
		}
		if (!isnan(CustP.Vy)) {
			Vy[0] = CustP.Vy;
		}
		if (!isnan(CustP.Vz)) {
			Vz[0] = CustP.Vz;
		}
		int i;
		for (i=0;i<10;i++) {
			if (!isnan(CustP.parmod[i])) {
				parmod[i] = CustP.parmod[i];
			}
		}
	}
	
	/* Do a final parameter check*/
	if (isnan(tilt[0])) {
		tilt[0] = GetDipoleTiltUT(Date,ut,Vx[0],Vy[0],Vz[0]);
	}
	if ((iopt[0] <= 0) || (iopt[0] >= 8)) {
		iopt[0] = 1;
	}
	if (isnan(Vx[0])) {
		Vx[0] = -400.0;
	}
	if (isnan(Vy[0])) {
		Vy[0] = 0.0;
	}
	if (isnan(Vz[0])) {
		Vz[0] = 0.0;
	}
	if (isnan(parmod[0])) { 
		parmod[0] = 2.0;
	}
	int i;
	for (i=1;i<10;i++) {
		if (isnan(parmod[i])) {
			parmod[i] = 0.0;
		}
	}	
}


void DummyFunc(int *iopt, float *parmod, float *ps, float *x, float *y, float *z, float *bx, float *by, float *bz) {
	bx[0] = 0.0;
	by[0] = 0.0;
	bz[0] = 0.0;
	return;
}

void SetCustParam(int iopt, float *parmod, float tilt, float Vx, float Vy, float Vz) {
	CustP.iopt = iopt;
	int i;
	for (i=0;i<10;i++) {
		CustP.parmod[i] = parmod[i];
	}
	CustP.tilt = tilt;
	CustP.Vx = Vx;
	CustP.Vy = Vy;
	CustP.Vz = Vz;
}


void Init(const char *filename) {
	strcpy(DataFile,filename);
	if (TSData.n == 0) {
		LoadTSData();	
	}
}


float GetDipoleTilt(int Year, int Doy, int Hr, int Mn, float Vx, float Vy, float Vz) {
	float psi, vx0 = -400.0, vy0 = 0.0, vz0 = 0.0;
	int Sc = 0;
	if (isnanf(Vx)) {
		recalc_08_(&Year,&Doy,&Hr,&Mn,&Sc,&vx0,&vy0,&vz0);
	} else {
		recalc_08_(&Year,&Doy,&Hr,&Mn,&Sc,&Vx,&Vy,&Vz);
	}
	psi = getpsi_();
	return psi;
}

float GetDipoleTiltUT(int Date, float ut, float Vx, float Vy, float Vz) {
	int Year, Doy, Hr, Mn, Sc;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(Date,&Year,&Doy);
		
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(ut,&Hr,&Mn,&Sc);	
		
	return GetDipoleTilt(Year,Doy,Hr,Mn,Vx,Vy,Vz);
		
}

void FindIntervals(int n, float *SymH, float *Bz, int *SWflag, int *IMFflag, int *ni, int *ibeg, int *iend) {
	/*
	 *  This procedure will scan through the OMNI data looking for 2h
	 * quiet periods (to get the baseline) followed by continuous good 
	 * data using Tsyganenko's Fortran code.
	 * 
	 */ 
	listwintervals_(&n,SymH,Bz,SWflag,IMFflag,ni,ibeg,iend);
	
	
}


void CalculateW(int n, float *SymH, float *Bz, int *SWflag, int *IMFflag, float *V, float *Den, float *W1, float *W2, float *W3, float *W4, float *W5, float *W6) {
	/***
	 *	This code links to Tsyganenko's Fortran code which scans for the
	 * useable intervals in the OMNI data, then calculates the 6 W 
	 * parameters used for the T04/05 (I'm not sure what it's called) 
	 * model. They don't seem to come out the same as the ones listed on 
	 * Tsyganenko's website, so use with caution!
	 * 
	 * 
	 */
	
	/* Firstly we need to calculate the number of intervals, use an arbitrary number*/
	int ni = 0;
	int *ibeg = (int*) malloc(10000*sizeof(int));
	int *iend = (int*) malloc(10000*sizeof(int));
	
	/*use the Fortran code to find the intervals*/
	FindIntervals(n,SymH,Bz,SWflag,IMFflag,&ni,ibeg,iend);
	
	/*Calculate the W parameters*/
	calculatew_(&ni,ibeg,iend,&n,Bz,V,Den,W1,W2,W3,W4,W5,W6);	
	
	/*free the malloced variables*/
	free(ibeg);
	free(iend);

}


void FillInKp(int nk, int *kDate, float *kut0, float *kut1, float *kp, int n, int *Date, float *ut, float *kpout) {
	/*******************************************************************
	 * This procedure should fill in the Kp indices for the Tsyganenko 
	 * T89 model. It will loop through each data point until it finds 
	 * the correct date and time range.
	 * 
	 * ****************************************************************/ 
	
	/* i is the index of the current output data, p is the index of the kp data*/
	int i, p;
	bool outofrange = false;
	p = 0;
	for (i=0;i<n;i++) {
		printf("\rFilling in Kp %d of %d",i+1,n);
		while ((Date[i] < kDate[p]) || ((Date[i] == kDate[p]) && (ut[i] < kut0[p]))) {
			p--;
			if (p < 0) {
				p = 0;
				outofrange = true;
				break;
			}
		}		
		while ((Date[i] > kDate[p]) || ((Date[i] == kDate[p]) && (ut[i] >= kut1[p]))) {
			p++;
			if (p >= nk) {
				p = nk-1;
				outofrange = true;
				break;
			}
		}
		if (!outofrange) {
			kpout[i] = kp[p];
		} else { 
			kpout[i] = NAN;
			outofrange = false;
		}
	}
	printf("\n");
}


void CalculateG(int n, float *By, float *Bz, float *V, bool *good, float *G1, float *G2) {
	/*******************************************************************
	 * In this function we are trying to calculate the G1 and G2 coefficients
	 * for the TS01 model (I think).
	 * 
	 * ****************************************************************/
	int i,j,u,i0,i1;
	/*calculate the clock angle, and Bs (B southward I think)*/
	float *CA = (float*) malloc(n*sizeof(float));
	float *Bs = (float*) malloc(n*sizeof(float));
	float *h = (float*) malloc(n*sizeof(float));
	float Bp;
	for (i=0;i<n;i++) {
		 CA[i] = atan2f(-By[i],Bz[i]);
		 Bp = sqrtf(By[i]*By[i] + Bz[i]*Bz[i]);
		 Bs[i] = fabsf(min(0.0,Bz[i]));
		 h[i] = powf(Bp/40.0,2.0)/(1.0 + Bp/40.0);
	}
		 
	/*now to get G1 and G2*/
	for (i=0;i<n;i++) {
		printf("\rCalculating G parameter %d of %d",i+1,n);
		i0 = max(0,i-11);
		i1 = i + 1;
		u = 0;
		G1[i] = 0.0;
		G2[i] = 0.0;
		for (j=i0;j<i1;j++) {
			if (good[j]) {
				G1[i] += V[j]*h[j]*powf(sinf(CA[j]/2.0),3.0);
				G2[i] += 0.005*V[j]*Bs[j];
				u++;
			}
		}
		if (u > 0) {
			G1[i]/=u;
			G2[i]/=u;
		} else { 
			G1[i] = 0.0;
			G2[i] = 0.0;
		}
	} 
	printf("\n");
	/*free the temporary arrays from memory*/
	free(CA);
	free(Bs);
	free(h);
	 
}
