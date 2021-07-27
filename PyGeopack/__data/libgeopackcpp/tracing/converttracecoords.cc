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
