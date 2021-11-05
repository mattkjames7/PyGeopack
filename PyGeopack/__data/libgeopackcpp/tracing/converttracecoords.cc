#include "converttracecoords.h"

void ConvertTraceCoords(int nstep, const char *CoordOut, 
						double *x, double *y, double *z, 
						double *Bx, double *By, double *Bz) {
	int dirp = 1, dirn = -1, i;
	double xtmp, ytmp, ztmp;
	/*now to convert the vectors to the desired output coordinates*/
	if (strcmp(CoordOut,"GSE") == 0) {
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
	} else if (strcmp(CoordOut,"GSM") == 0) {
		/*GSM Out:
		 * Do nothing - model already outputs GSM*/
	} else if (strcmp(CoordOut,"SM") == 0) {
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
	} else {
			printf("Output coordinate type not recognised\n");
	}	

}

void ConvertTraceCoords(int nstep, const char *CoordOut, 
						double *x0, double *y0, double *z0, 
						double *x1, double *y1, double *z1, 
						double *Bx0, double *By0, double *Bz0,
						double *Bx1, double *By1, double *Bz1) {
	int dirp = 1, dirn = -1, i;
	/*now to convert the vectors to the desired output coordinates*/
	if (strcmp(CoordOut,"GSE") == 0) {
		/*GSE Out*/
		for (i=0;i<nstep;i++) {
			gswgse_08_(&Bx0[i],&By0[i],&Bz0[i],&Bx1[i],&By1[i],&Bz1[i],&dirp);
			gswgse_08_(&x0[i],&y0[i],&z0[i],&x1[i],&y1[i],&z1[i],&dirp);
		}
	} else if (strcmp(CoordOut,"GSM") == 0) {
		/*GSM Out:
		 * copy - model already outputs GSM*/
		for (i=0;i<nstep;i++) {
			x1[i] = x0[i];
			y1[i] = y0[i];
			z1[i] = z0[i];
			Bx1[i] = Bx0[i];
			By1[i] = By0[i];
			Bz1[i] = Bz0[i];
		} 
	} else if (strcmp(CoordOut,"SM") == 0) {
		/*SM Out*/
		for (i=0;i<nstep;i++) {
			smgsw_08_(&Bx1[i],&By1[i],&Bz1[i],&Bx0[i],&By0[i],&Bz0[i],&dirn);
			smgsw_08_(&x1[i],&y1[i],&z1[i],&x0[i],&y0[i],&z0[i],&dirn);
		}
	} else {
			printf("Output coordinate type not recognised\n");
	}	

}
