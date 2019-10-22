#include "ModelField.h"


void ModelField(float *Xin, float *Yin, float *Zin, int n, int Date, float ut, const char *Model, int CoordIn, int CoordOut, float *Bx, float *By, float *Bz) {

	/*Check that TSData has been loaded*/
	if (TSData.n == 0) {
		LoadTSData();
	} 
	
	int Year, DayNo, Hr, Mn, Sc, i;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(Date,&Year,&DayNo);

	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(ut,&Hr,&Mn,&Sc);
	ModelFuncPtr ModelFunc;
	
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
	
	/*get params and recalc08*/
	int dirp = 1, dirn = -1;
	
	int iopt;
	float parmod[10], tilt, Vx, Vy, Vz;
	GetModelParams(Date,ut,Model,&iopt,parmod,&tilt,&Vx,&Vy,&Vz);
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	tilt = getpsi_();
	
	/*Convert input coordinates to GSM*/
	float X[n],Y[n],Z[n];
	switch (CoordIn) {
		case 1:
			/*GSE in*/
			for (i=0;i<n;i++) {
				gswgse_08_(&X[i],&Y[i],&Z[i],&Xin[i],&Yin[i],&Zin[i],&dirn);
			}
			break;
		case 2:
			/*GSM in*/
			for (i=0;i<n;i++) {
				X[i] = Xin[i];
				Y[i] = Yin[i];
				Z[i] = Zin[i];
			}
			break;
		case 3:
			/*SM in*/
			for (i=0;i<n;i++) {
				smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&X[i],&Y[i],&Z[i],&dirp);
			}
			break;
		default:
			printf("Input coordinate type not recognised\n");
			return;	
			break;	
	}

	/*call relevant model code*/
	float Bxgsm[n],Bygsm[n],Bzgsm[n];
	float intx, inty, intz, extx, exty, extz;
	for (i=0;i<n;i++) {
		igrf_gsw_08_(&X[i],&Y[i],&Z[i],&intx,&inty,&intz);
		ModelFunc(&iopt,parmod,&tilt,&X[i],&Y[i],&Z[i],&extx,&exty,&extz);
		Bxgsm[i] = intx + extx;
		Bygsm[i] = inty + exty;
		Bzgsm[i] = intz + extz;
	}
	
	/*now to convert the vectors to the desired output coordinates*/
	switch (CoordOut) {
		case 1:
			/*GSE Out*/
			for (i=0;i<n;i++) {
				gswgse_08_(&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&Bx[i],&By[i],&Bz[i],&dirp);
			}
			break;
		case 2:
			/*GSM Out*/
			for (i=0;i<n;i++) {
				Bx[i] = Bxgsm[i];
				By[i] = Bygsm[i];
				Bz[i] = Bzgsm[i];
			}
			break;
		case 3:
			/*SM Out*/
			for (i=0;i<n;i++) {
				smgsw_08_(&Bx[i],&By[i],&Bz[i],&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&dirn);
			}
			break;
		default:
			printf("Output coordinate type not recognised\n");
			return;	
			break;	
	}	
	return;
	
}


