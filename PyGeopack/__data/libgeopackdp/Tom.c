#include "Tom.h"

void tom_ (double *x, double *y, double *z, int *n, int *Date, float *ut, double *Bx, double *By, double *Bz) {
	/*******************************************************************
	 * This is the Tom96 model field
	 * 
	 * Inputs
	 * ======
	 * *x, *y, *z: Arrays of positions in SM coordinates (in R_e, not km)
	 * *n: the number of points in *x, *y, and *z.
	 * *Date: Single integer date in the format yyyymmdd
	 * *ut: Time in hours, e.g. for 16:45 ut = 16.75 
	 * 
	 * Outputs
	 * =======
	 * *Bx, *By, *Bz: 	Model field in SM coordinates
	 * 
	 ******************************************************************/

	_Tom96C(x,y,z,n[0],Date[0],ut[0],Bx,By,Bz);

}

void _Tom96C(double *Xin, double *Yin, double *Zin, int n, int Date, float ut, double *Bx, double *By, double *Bz) {
	int Year, DayNo, Hr, Mn, Sc, i;
	/*convert date into Year and DayNo*/
	DateToYearDayNo(Date,&Year,&DayNo);
	
	/*convert decimal UT to Hr, Mn, Sc*/
	DecUTToHHMMSS(ut,&Hr,&Mn,&Sc);
	ModelFuncPtr ModelFunc;
	
	/*get model function and parmod*/
	ModelFunc = &t96_;

	/*get params and recalc08*/
	int dirp = 1, dirn = -1;
	
	int iopt;
	double parmod[10], tilt, Vx, Vy, Vz;
	Vx = -400.0;
	Vy = 0.0;
	Vz = 0.0;
	recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	tilt = getpsi_();
	parmod[0] = 2.0;
	parmod[1] = 0.0;
	parmod[2] = 0.0;
	parmod[3] = 0.0;

	
	/*Convert input coordinates to GSM*/
	double X[n],Y[n],Z[n];
	/*SM in*/
	for (i=0;i<n;i++) {
		smgsw_08_(&Xin[i],&Yin[i],&Zin[i],&X[i],&Y[i],&Z[i],&dirp);
		//printf("new xyz: %f %f %f\n",Xin[i],Yin[i],Zin[i]);
	}

	/*call relevant model code*/
	double Bxgsm[n],Bygsm[n],Bzgsm[n];
	double intx, inty, intz, extx, exty, extz;
	for (i=0;i<n;i++) {
		igrf_gsw_08_(&X[i],&Y[i],&Z[i],&intx,&inty,&intz);
		ModelFunc(&iopt,parmod,&tilt,&X[i],&Y[i],&Z[i],&extx,&exty,&extz);
		Bxgsm[i] = intx + extx;
		Bygsm[i] = inty + exty;
		Bzgsm[i] = intz + extz;
	}
	
	/*now to convert the vectors to the desired output coordinates*/
	for (i=0;i<n;i++) {
		smgsw_08_(&Bx[i],&By[i],&Bz[i],&Bxgsm[i],&Bygsm[i],&Bzgsm[i],&dirn);
	}

	return;	
}
