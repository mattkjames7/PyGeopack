#include "modelfield.h"


void ModelField(	int n, double *Xin, double *Yin, double *Zin,  
					int *Date, float *ut, bool SameTime,
					const char *Model, int *iopt, double **parmod,
					double *Vx, double *Vy, double *Vz,
					const char *CoordIn, const char *CoordOut, 
					double *Bx, double *By, double *Bz) {

	/* declare a bunch of variables to use */
	int i, ipar;
	int dirp = 1, dirn = -1;
	double X[n],Y[n],Z[n], tilt;
	double Bxgsm[n],Bygsm[n],Bzgsm[n];
	double intx, inty, intz, extx, exty, extz;
	bool inMP;

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

		/* call recalc */
		Recalc(Date[ipar],ut[ipar],Vx[ipar],Vy[ipar],Vz[ipar],true);

		/* get the tilt */
		tilt = getpsi_();
		
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
		if (strcmp(CoordOut,"GSE") == 0) {
			/*GSE Out*/
			gswgse_08_(&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&Bx[i],&By[i],&Bz[i],&dirp);
		} else if (strcmp(CoordOut,"SM") == 0) {
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


ModelCFG GetModelCFG(	int n, int *Date, float *ut, bool SameTime,
						const char *Model, int *iopt, double **parmod,
						double *Vx, double *Vy, double *Vz,
						const char *CoordIn, const char *CoordOut) {
	
	
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
		printf("Model %s not found\nDefaulting to T96",Model);
		ModelFunc = &t96_;
	}	
	
	/* create a struct to store model config */
	ModelCFG cfg = {n,Date,ut,SameTime,ModelFunc,iopt,parmod,Vx,Vy,Vz,
			CoordIn,CoordOut};

	return cfg;
	
}

