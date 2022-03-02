#include "ModelField.h"


void ModelField(	double *Xin, double *Yin, double *Zin, int n, 
					int *Date, float *ut, int SameTime, 
					const char *Model,	int CoordIn, int CoordOut, 
					double *Bx, double *By, double *Bz) {

	/*Check that TSData has been loaded*/
	if (TSData.n == 0) {
		LoadTSData();
	} 

	/* declare a bunch of variables to use */
	int CurrDate;
	float CurrUT;
	int Year, DayNo, Hr, Mn, Sc, i;
	int dirp = 1, dirn = -1;
	int iopt;
	double parmod[10], tilt, Vx, Vy, Vz;
	double* X = (double*)malloc(n * sizeof(double));
	double* Y = (double*)malloc(n * sizeof(double));
	double* Z = (double*)malloc(n * sizeof(double));
	double* Bxgsm = (double*)malloc(n * sizeof(double));
	double* Bygsm = (double*)malloc(n * sizeof(double));
	double* Bzgsm = (double*)malloc(n * sizeof(double));
	double intx, inty, intz, extx, exty, extz;
	bool inMP;

	/* these onese will tell us whether we need to run recalc or not */
	int pDate = -1;
	float put = -1.0;
	double pVx=-1.0, pVy=-1.0, pVz=-1.0;
	bool recalc;

	
	/*get model function and parmod*/
	ModelFuncPtr ModelFunc;
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

	
	/* start looping through all the positions/times here */
	for (i=0;i<n;i++) {
		
		/* Get the current date and time - it may be an array or just a
		 * single scalar */
		if (SameTime) {
			CurrDate = Date[0];
			CurrUT = ut[0];
		} else {
			CurrDate = Date[i];
			CurrUT = ut[i];
		}

		recalc = 0;
		/* check if the time is different to the previous iteration */
		if ((CurrDate != pDate) || (CurrUT != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(CurrDate,&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(CurrUT,&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}


		/* check for a difference in the velocity */
		GetSWVelocity(CurrDate,CurrUT,NULL,&Vx,&Vy,&Vz);
		if ((Vx != pVx) || (Vy != pVy) || (Vz != pVz)) {
			recalc = 1;
		}

		
		/*get params and recalc08*/
		if (recalc) {
			GetModelParams(CurrDate,CurrUT,Model,&iopt,parmod,&tilt,&Vx,&Vy,&Vz);
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

		/*check if we are within the magnetopause or not */
		inMP = WithinMP(X[i],Y[i],Z[i],parmod[3],parmod[0]);
		if (inMP) {
			/*call relevant model code*/
			igrf_gsw_08_(&X[i],&Y[i],&Z[i],&intx,&inty,&intz);
			ModelFunc(&iopt,parmod,&tilt,&X[i],&Y[i],&Z[i],&extx,&exty,&extz);
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
		switch (CoordOut) {
			case 1:
				/*GSE Out*/
				gswgse_08_(&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&Bx[i],&By[i],&Bz[i],&dirp);
				break;
			case 2:
				/*GSM Out*/
				Bx[i] = Bxgsm[i];
				By[i] = Bygsm[i];
				Bz[i] = Bzgsm[i];
				break;
			case 3:
				/*SM Out*/
				smgsw_08_(&Bx[i],&By[i],&Bz[i],&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&dirn);
				break;
			default:
				printf("Output coordinate type not recognised\n");
				return;	
				break;	
		}	

	}
	free(X);
	free(Y);
	free(Z);
	free(Bxgsm);
	free(Bygsm);
	free(Bzgsm);

	return;
	
}


