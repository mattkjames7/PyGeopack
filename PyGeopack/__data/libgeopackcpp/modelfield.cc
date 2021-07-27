#include "modelfield.h"


void ModelField(	int n, double *Xin, double *Yin, double *Zin,  
					int *Date, float *ut, bool SameTime,
					const char *Model, int *iopt, double **parmod,
					double *Vx, double *Vy, double *Vz,
					const char *CoordIn, const char *CoordOut, 
					double *Bx, double *By, double *Bz) {

	/* declare a bunch of variables to use */
	int CurrDate;
	double CurrUT;
	int Year, DyNo, Hr, Mn, Sc, i, ipar;
	int dirp = 1, dirn = -1;
	double X[n],Y[n],Z[n], tilt, Ms;
	double Bxgsm[n],Bygsm[n],Bzgsm[n];
	double intx, inty, intz, extx, exty, extz;
	bool inMP;

	/* these onese will tell us whether we need to run recalc or not */
	int pDate = -1;
	float put = -1.0;
	double pVx=-1.0, pVy=-1.0, pVz=-1.0;
	bool recalc;

	
	/*get model function and parmod*/
	ModelFuncPtr ModelFunc;
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

	
	/* start looping through all the positions/times here */
	for (i=0;i<n;i++) {
		
		/* this is if we use the same parameters for each vector */
		if (SameTime) {
			ipar = 0;
		} else {
			ipar = i;
		}


		/* Get the current date and time  */
		CurrDate = Date[ipar];
		CurrUT = (double) ut[ipar];
		

		recalc = false;
		/* check if the time is different to the previous iteration */
		if ((CurrDate != pDate) || (CurrUT != put)) {
			/*convert date into Year and DayNo*/
			DayNo(1,&CurrDate,&Year,&DyNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DectoHHMM(1,&CurrUT,&Hr,&Mn,&Sc,&Ms);
			
			/*set the flag to call recalc*/
			recalc = true;
		}


		if ((Vx[ipar] != pVx) || (Vy[ipar] != pVy) || (Vz[ipar] != pVz)) {
			recalc = true;
		}

		
		/*get params and recalc08*/
		if (recalc) {
			recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc, &Vx[ipar], &Vy[ipar], &Vz[ipar]);
			tilt = getpsi_();
		}

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
			



		/*check if we are within the magnetopause or not */
		inMP = WithinMP(X[i],Y[i],Z[i],parmod[ipar][3],parmod[ipar][0]);
		if (inMP) {
			/*call relevant model code*/
			igrf_gsw_08_(&X[i],&Y[i],&Z[i],&intx,&inty,&intz);
			ModelFunc(&iopt[ipar],parmod[ipar],&tilt,&X[i],&Y[i],&Z[i],&extx,&exty,&extz);
			Bxgsm[i] = intx + extx;
			Bygsm[i] = inty + exty;
			Bzgsm[i] = intz + extz;
		} else { 
			/* fill with NAN */
			Bxgsm[i] = NAN;
			Bygsm[i] = NAN;
			Bzgsm[i] = NAN;
		}

		/*now to convert the vectors to the desired output coordinates*/
		if (strcmp(CoordIn,"GSE") == 0) {
			/*GSE Out*/
			gswgse_08_(&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&Bx[i],&By[i],&Bz[i],&dirp);
		} else if (strcmp(CoordIn,"SM") == 0) {
			/*SM Out*/
			smgsw_08_(&Bx[i],&By[i],&Bz[i],&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&dirn);
		} else {
			/*GSM Out*/
			Bx[i] = Bxgsm[i];
			By[i] = Bygsm[i];
			Bz[i] = Bzgsm[i];
		}

	}

	return;
	
}


