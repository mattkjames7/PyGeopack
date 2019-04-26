#include "IGRFParams.h"

IGRFP IGRFParams[25];
IGRFP IGRFCurr;

void ReadIGRFParameters(const char *FileName) {
	/* This function will attempt to read all of the parameters from the 
	 * file downloaded from https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html*/
	
	int nl = 195, nc = 25;
	int i, j, n, m, p, pm;
	int StartYr = 1900;
	char line[1024];
	FILE *f = fopen(FileName,"r");
	//skip the first 4 lines
	for (i=0;i<4;i++) {
		fgets(line,1024,f);
	}
	
	//now to read each line of data
	char gh;
	p = 0;
	pm = -1;
	for (i=0;i<195;i++) {
		//read g/h column
		fscanf(f,"%c",&gh);

		//read in the n and m integers
		fscanf(f,"%d",&n);
		fscanf(f,"%d",&m);
		for (j=0;j<24;j++) {
			IGRFParams[j].n[p] = n;
			IGRFParams[j].m[p] = m;
		}
		
		//read in the res of the line
		for (j=0;j<24;j++) {
			if (gh == 'h') {
				fscanf(f,"%lf",&IGRFParams[j].h[p]);
			} else {
				fscanf(f,"%lf",&IGRFParams[j].g[p+1]);
			}
		}
		if (gh == 'h') {
			fscanf(f,"%lf\n",&IGRFParams[j].h[p]);
		} else {
			fscanf(f,"%lf\n",&IGRFParams[j].g[p+1]);
		}
		if (pm != m) {
			p++;
		}
		pm = m;
		
	}
	
	//set the years
	for (i=0;i<24;i++) {
		IGRFParams[i].Year = 1900 + i*5;
	}
	
}


void SetIGRFParams(int Year, int DayNo) {
	/* This will interpolate (where possible) the IGRF parameters loaded
	 * above for use in the model.
	 * */
	 
	float f0, f1;
	int i, j, i0, i1;

	if (Year < 1900) {
		/* before 1900 we shall use the IGRF data from 1900*/
		for (i=0;i<105;i++) {
			 IGRFCurr.n[i] = IGRFParams[0].n[i];
			 IGRFCurr.m[i] = IGRFParams[0].m[i];
			 IGRFCurr.g[i] = IGRFParams[0].g[i];
			 IGRFCurr.h[i] = IGRFParams[0].h[i];
		}
	} else if (Year < 2015) {
		/* Between 1900 and 2015, interpolate*/
		i0 = (Year - 1900)/5;
		i1 = i0 + 1;

		/* This bit is crude but it will do, works for geopack!*/
		f1 = (((float) Year) + (((float) (DayNo-1))/365.25) - IGRFParams[i0].Year)/5;
		f0 = 1.0 - f1;

		for (i=0;i<105;i++) {
			 IGRFCurr.n[i] = IGRFParams[i0].n[i];
			 IGRFCurr.m[i] = IGRFParams[i0].m[i];
			 IGRFCurr.g[i] = IGRFParams[i0].g[i]*f0 + IGRFParams[i1].g[i]*f1;
			 IGRFCurr.h[i] = IGRFParams[i0].h[i]*f0 + IGRFParams[i1].h[i]*f1;
		}		 
	} else {
		/*after 2015, extrapolate using secular variation*/
		i0 = 23;
		i1 = 24;
		f1 = (((float) Year) + ((float) (DayNo-1))/365.25 - IGRFParams[i0].Year);
		for (i=0;i<105;i++) {
			 IGRFCurr.n[i] = IGRFParams[i0].n[i];
			 IGRFCurr.m[i] = IGRFParams[i0].m[i];
			 IGRFCurr.g[i] = IGRFParams[i0].g[i] + IGRFParams[i1].g[i]*f1;
			 IGRFCurr.h[i] = IGRFParams[i0].h[i] + IGRFParams[i1].h[i]*f1;
		}		
	}
	
	/*calculate the recursion relation coefficients*/
	int n2;
	int mn;
	for (i=1;i<=14;i++) {
		n2 = 2*i - 1;
		n2 = n2*(n2-2);
		for (j=1;j<=i;j++) {
			mn = i*(i-1)/2 + j;
			IGRFCurr.rec[mn - 1] = ((float) ((i-j)*(i+j-2)))/((float) n2);
		}
	}
	
	/*multiply the g and h coefficients by the Schmidt normalization factors*/
	double s = 1.0, p, aa;
	int mnn;
	for (i=2;i<=14;i++) {
		mn = i*(i-1)/2 + 1;
		s = s*((double) (2*i-3))/((double) (i-1));
		IGRFCurr.g[mn-1] *= s;
		IGRFCurr.h[mn-1] *= s;
		p = s;
		for (j=2;j<=i;j++) {
			if (j == 2) {
				aa = 2.0;
			} else {
				aa = 1.0;
			}
			p = p*sqrt(aa*((double) (i-j+1))/((double) (i+j-2)));
			mnn = mn + j -1;
			IGRFCurr.g[mnn-1] *= p;
			IGRFCurr.h[mnn-1] *= p;
		}
	}
	
}
