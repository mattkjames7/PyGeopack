#include "tsygdata.h"

TsygData::TsygData(const char *fname) {
	
	/* initialize the number of elements */
	n_ = 0;
	loaded_ = false;
	
	/* call the function which reads the file */
	loaded_ = _LoadFile(fname);
	
	/* this bit is where one might call the populate months bit */
	PopulateMonthInds();
}

TsygData::~TsygData() {
	
	
	if (loaded_) {
		delete[] Date_;
		delete[] ut_;
		delete[] utc_;
		delete[] Year_;
		delete[] DayNo_;
		delete[] Hr_;
		delete[] Mn_;
		delete[] IMFFlag_;
		delete[] ISWFlag_;
		delete[] Bx_;
		delete[] By_;
		delete[] Bz_;
		delete[] Vx_;
		delete[] Vy_;
		delete[] Vz_;
		delete[] Den_;
		delete[] Temp_;
		delete[] SymH_;
		delete[] Tilt_;
		delete[] Pdyn_;
		delete[] W1_;
		delete[] W2_;
		delete[] W3_;
		delete[] W4_;
		delete[] W5_;
		delete[] W6_;
		delete[] G1_;
		delete[] G2_;
		delete[] Kp_;
	}
	
}

bool TsygData::_LoadFile(const char *fname) {
	
	/* open the file */
	FILE *f = fopen(fname,"rb");
	if (f == NULL) {
		return false;
	}
	
	/* get the number of records */
	fread(&n_,sizeof(int),1,f);

	/* load the rest of the data */
	_ReadInt(f,n_,&Date_);
	_ReadFloat(f,n_,&ut_);
	_ReadInt(f,n_,&Year_);
	_ReadInt(f,n_,&DayNo_);
	_ReadInt(f,n_,&Hr_);
	_ReadInt(f,n_,&Mn_);
	_ReadDouble(f,n_,&Bx_);
	_ReadDouble(f,n_,&By_);
	_ReadDouble(f,n_,&Bz_);
	_ReadDouble(f,n_,&Vx_);
	_ReadDouble(f,n_,&Vy_);
	_ReadDouble(f,n_,&Vz_);
	_ReadDouble(f,n_,&Den_);
	_ReadDouble(f,n_,&Temp_);
	_ReadDouble(f,n_,&SymH_);
	_ReadInt(f,n_,&IMFFlag_);
	_ReadInt(f,n_,&ISWFlag_);
	_ReadDouble(f,n_,&Tilt_);
	_ReadDouble(f,n_,&Pdyn_);
	_ReadDouble(f,n_,&W1_);
	_ReadDouble(f,n_,&W2_);
	_ReadDouble(f,n_,&W3_);
	_ReadDouble(f,n_,&W4_);
	_ReadDouble(f,n_,&W5_);
	_ReadDouble(f,n_,&W6_);
	_ReadDouble(f,n_,&G1_);
	_ReadDouble(f,n_,&G2_);
	_ReadDouble(f,n_,&Kp_);
	
	/* close the file */
	fclose(f);

	/* populate the utc array */
	utc_ = new double[n_];
	ContUT(n_,Date_,ut_,utc_);
	return true;
}

void TsygData::_ReadInt(FILE *f, int n, int **out) {
	*out = new int[n];
	fread(*out,sizeof(int),n,f);
}

void TsygData::_ReadFloat(FILE *f, int n, float **out) {
	*out = new float[n];
	fread(*out,sizeof(float),n,f);
}

void TsygData::_ReadDouble(FILE *f, int n, double **out) {
	*out = new double[n];
	float *tmp = new float[n];
	fread(tmp,sizeof(float),n,f);
	int i;
	for (i=0;i<n;i++) {
		out[0][i] = (double) tmp[i];
	}
	delete[] tmp;
}

void TsygData::PopulateMonthInds() {
	minYr_ = Year_[0];
	maxYr_ = Year_[n_-1];
	
	minMn_ = (Date_[0] % 10000)/100;
	maxMn_ = (Date_[n_-1] % 10000)/100;
	
	nMonth_ = 12*(maxYr_-minYr_) + maxMn_ - minMn_ + 1;
	MonthInds_ = new int[nMonth_];
	Monthutc_ = new double[nMonth_];

	int tmp, tmpYr, tmpMn;
	int i, j, p = 0;
	tmpYr = minYr_;
	tmpMn = minMn_;
	
	for (i=0;i<nMonth_;i++) {
		tmp = tmpYr*10000 + tmpMn*100;

		for (j=p;j<n_;j++) {
			if (Date_[j] >= tmp) {
				p = j;
				MonthInds_[i] = j;
				Monthutc_[i] = utc_[j];

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


int TsygData::_MonthStartInd(int Date) {

	int yr, mn, ind;
	yr = Date / 10000;
	mn = (Date % 10000)/100;
	ind = (yr - minYr_)*12 + mn - minMn_;

	if (ind >= nMonth_){
		ind = nMonth_-1;
	}

	return MonthInds_[ind];
}

double TsygData::InterpParam(double *x, int Date, float ut) {

	/*First get the start ind for searching for this date*/
	int ind = _MonthStartInd(Date);
	int i, i0, i1;
	double utc;

	ContUT(1,&Date,&ut,&utc);

	

	/* check if the time provided is within the range of the data */
	if (utc < utc_[0]) {
		/* this would be from before the start of hte data */
		return NAN;
	} else if (utc > utc_[n_-1]) {
		/* this would be afterwards */
		return NAN;
	}
	

	/*now start search for nearest two indices*/
	i = ind;
	while ((i < n_ - 1) && (utc_[i] < utc)) {
		i++;
	}
	i0 = i-1;
	i1 = i;


	/*now to calculate time differences between two points and the first point witht he requested time*/
	double dt, dtp;
	dt = utc_[i1] - utc_[i0];
	dtp = utc - utc_[i0];

	/*now the linear interpolation*/
	double out, m, c;
	m = (x[i1]-x[i0])/dt;
	c = x[i0];
	out = m*dtp + c;

	return out;
}


void TsygData::GetVx(int n, int *Date, float *ut, double *Vx) {
	
	int i;
	for (i=0;i<n;i++) {
		Vx[i] = InterpParam(Vx_,Date[i],ut[i]);
	}	
	
}

void TsygData::GetVy(int n, int *Date, float *ut, double *Vy) {
	
	int i;
	for (i=0;i<n;i++) {
		Vy[i] = InterpParam(Vy_,Date[i],ut[i]);
	}	
	
}

void TsygData::GetVz(int n, int *Date, float *ut, double *Vz) {
	
	int i;
	for (i=0;i<n;i++) {
		Vz[i] = InterpParam(Vz_,Date[i],ut[i]);
	}	
	
}

void TsygData::GetSWVelocity(int n, int *Date, float *ut,
				double *Vxin, double *Vyin, double *Vzin,
				double *Vx, double *Vy, double *Vz)  {
	int i;
	if (Vxin == NULL) {
		/* get Vx from the data file */
		for (i=0;i<n;i++) {
			Vx[i] = InterpParam(Vx_,Date[i],ut[i]);
		}
	} else { 
		/* copy it across */
		for (i=0;i<n;i++) {
			Vx[i] = Vxin[i];
			if (isnan(Vx[i])) {
				Vx[i] = InterpParam(Vx_,Date[i],ut[i]);
			}
		}
	}
	
	if (Vyin == NULL) {
		/* get Vx from the data file */
		for (i=0;i<n;i++) {
			Vy[i] = InterpParam(Vy_,Date[i],ut[i]);
		}
	} else { 
		/* copy it across */
		for (i=0;i<n;i++) {
			Vy[i] = Vyin[i];
			if (isnan(Vy[i])) {
				Vy[i] = InterpParam(Vy_,Date[i],ut[i]);
			}
		}
	}
	
	if (Vzin == NULL) {
		/* get Vx from the data file */
		for (i=0;i<n;i++) {
			Vz[i] = InterpParam(Vz_,Date[i],ut[i]);
		}
	} else { 
		/* copy it across */
		for (i=0;i<n;i++) {
			Vz[i] = Vzin[i];
			if (isnan(Vz[i])) {
				Vz[i] = InterpParam(Vz_,Date[i],ut[i]);
			}
		}
	}
		
		
}

void TsygData::GetModelParams(	int n, int *Date, float *ut, 
								const char *Model, int *iopt, double **parmod) {
	int i, j;
	if (strcmp(Model,"T89") == 0) {
		/* T89 - just need iopt = Kp + 1 */
		for (i=0;i<n;i++) {
			iopt[i] = (int) (InterpParam(Kp_,Date[i],ut[i]) + 1);
			if (iopt[i] < 1) {
				iopt[i] = 1;
			}
			if (iopt[i] > 7) {
				iopt[i] = 7;
			}
			for (j=0;j<10;j++) {
				parmod[i][j] = 0.0;
			}
		}
	} else if (strcmp(Model,"T96") == 0) {
		/* T96 - Pdyn,SymH, By, Bz */
		for (i=0;i<n;i++) {
			 iopt[i] = 0;
			 parmod[i][0] = InterpParam(Pdyn_,Date[i],ut[i]);
			 parmod[i][1] = InterpParam(SymH_,Date[i],ut[i]);
			 parmod[i][2] = InterpParam(By_,Date[i],ut[i]);
			 parmod[i][3] = InterpParam(Bz_,Date[i],ut[i]);
			 for (j=4;j<10;j++) {
				 parmod[i][j] = 0.0;
			}
		}
	} else if (strcmp(Model,"T01") == 0) {
		/* T01 - Pdyn,SymH, By, Bz */
		for (i=0;i<n;i++) {
			 iopt[i] = 0;
			 parmod[i][0] = InterpParam(Pdyn_,Date[i],ut[i]);
			 parmod[i][1] = InterpParam(SymH_,Date[i],ut[i]);
			 parmod[i][2] = InterpParam(By_,Date[i],ut[i]);
			 parmod[i][3] = InterpParam(Bz_,Date[i],ut[i]);
			 parmod[i][4] = InterpParam(G1_,Date[i],ut[i]);
			 parmod[i][5] = InterpParam(G2_,Date[i],ut[i]);
			 for (j=6;j<10;j++) {
				 parmod[i][j] = 0.0;
			}
		}
	} else if (strcmp(Model,"TS05") == 0) {
		/* TS05 - Pdyn,SymH, By, Bz */
		for (i=0;i<n;i++) {
			 iopt[i] = 0;
			 parmod[i][0] = InterpParam(Pdyn_,Date[i],ut[i]);
			 parmod[i][1] = InterpParam(SymH_,Date[i],ut[i]);
			 parmod[i][2] = InterpParam(By_,Date[i],ut[i]);
			 parmod[i][3] = InterpParam(Bz_,Date[i],ut[i]);
			 parmod[i][4] = InterpParam(W1_,Date[i],ut[i]);
			 parmod[i][5] = InterpParam(W2_,Date[i],ut[i]);
			 parmod[i][6] = InterpParam(W3_,Date[i],ut[i]);
			 parmod[i][7] = InterpParam(W4_,Date[i],ut[i]);
			 parmod[i][8] = InterpParam(W5_,Date[i],ut[i]);
			 parmod[i][9] = InterpParam(W6_,Date[i],ut[i]);
		}
	}
}
							


void TsygData::GetModelParams(int n, const char *Model,
							double *Kp, double *Pdyn, double *SymH,
							double *By, double *Bz, 
							double *G1, double *G2,
							double *W1, double *W2, double *W3,
							double *W4, double *W5, double *W6,
							int *iopt, double **parmod) {
	/* use this function to convert PDyn, SymH etc into iopt/parmod
	 * 
	 * use other functions to mix overidden parameters with interpolated ones first (they will be the input to this)*/
	int i, j;
	if (strcmp(Model,"T89") == 0) {
		/* T89 - just need iopt = Kp + 1 */
		for (i=0;i<n;i++) {
			iopt[i] = (int) Kp[i] + 1;
			if (iopt[i] < 1) {
				iopt[i] = 1;
			}
			if (iopt[i] > 7) {
				iopt[i] = 7;
			}
			for (j=0;j<10;j++) {
				parmod[i][j] = 0.0;
			}
		}
	} else if (strcmp(Model,"T96") == 0) {
		/* T96 - Pdyn,SymH, By, Bz */
		for (i=0;i<n;i++) {
			 iopt[i] = 0;
			 parmod[i][0] = Pdyn[i];
			 parmod[i][1] = SymH[i];
			 parmod[i][2] = By[i];
			 parmod[i][3] = Bz[i];
			 for (j=4;j<10;j++) {
				 parmod[i][j] = 0.0;
			}
		}
	} else if (strcmp(Model,"T01") == 0) {
		/* T01 - Pdyn,SymH, By, Bz */
		for (i=0;i<n;i++) {
			 iopt[i] = 0;
			 parmod[i][0] = Pdyn[i];
			 parmod[i][1] = SymH[i];
			 parmod[i][2] = By[i];
			 parmod[i][3] = Bz[i];
			 parmod[i][4] = G1[i];
			 parmod[i][5] = G2[i];
			 for (j=6;j<10;j++) {
				 parmod[i][j] = 0.0;
			}
		}
	} else if (strcmp(Model,"TS05") == 0) {
		/* TS05 - Pdyn,SymH, By, Bz */
		for (i=0;i<n;i++) {
			 iopt[i] = 0;
			 parmod[i][0] = Pdyn[i];
			 parmod[i][1] = SymH[i];
			 parmod[i][2] = By[i];
			 parmod[i][3] = Bz[i];
			 parmod[i][4] = W1[i];
			 parmod[i][5] = W2[i];
			 parmod[i][6] = W3[i];
			 parmod[i][7] = W4[i];
			 parmod[i][8] = W5[i];
			 parmod[i][9] = W6[i];
		}
	}
}

void TsygData::GetParameter(int n, int *Date, float *ut, 
						double *x, double *xin, double *xout) {
	
	int i;
	if (xin == NULL) {
		for (i=0;i<n;i++) {
			xout[i] = InterpParam(x,Date[i],ut[i]);
		}
	} else {
		for (i=0;i<n;i++) {
			xout[i] = xin[i];
		}
	}
}
	
