#include "libgeopack.h"

const float Re = 6371.2;
TSD TSData = {.n = 0};

void LoadTSData() {
	printf("Reading Model Data\n");
	FILE *f = fopen(DataFile,"rb");
	if (f == NULL) {
		printf("Full file path: %s",DataFile);
		printf("File open failed!\n");
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
	printf("Done\n");
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
	if (strchr(Model,'c') != NULL) {
		//In this bit we have chosen a custom model, so will use the custom params stored in CustP
		//So, setting CustP needs to be done first!
		if (!isnan(CustP.tilt)) {
			//if we set CustP.tilt = NaN then we use the default interpolated value
			//otherwise we set it to a specific value
			tilt[0] = CustP.tilt;
		}
		iopt[0] = CustP.iopt;
		Vx[0] = CustP.Vx;
		Vy[0] = CustP.Vy;
		Vz[0] = CustP.Vz;
		int i;
		for (i=0;i<10;i++) {
			parmod[i] = CustP.parmod[i];
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


void GetGeopackParams(float *gp0, float *gp1){
	getgeopackparams_(gp0,gp1);
}

