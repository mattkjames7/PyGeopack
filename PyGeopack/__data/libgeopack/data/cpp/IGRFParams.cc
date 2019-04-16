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
		printf("%s\n",line);
	}
	
	//now to read each line of data
	char gh;
	p = 0;
	pm = -1;
	for (i=0;i<195;i++) {
		//read g/h column
		fscanf(f,"%c",&gh);
		printf("%d: %c\n",i,gh);
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
	
	
}


void SetIGRFParams(int Year, int DayNo) {
	/* This will interpolate (where possible) the IGRF parameters loaded
	 * above for use in the model.
	 * */
	 
	float f0, f1;
	int i, i0, i1;
	 
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
		f1 = (((float) Year) + ((float) (DayNo-1))/365.25 - IGRFParams[i0].Year)/5;
		f0 = 1.0 - f1;
		for (i=0;i<105;i++) {
			 IGRFCurr.n[i] = IGRFParams[0].n[i];
			 IGRFCurr.m[i] = IGRFParams[0].m[i];
			 IGRFCurr.g[i] = IGRFParams[0].g[i];
			 IGRFCurr.h[i] = IGRFParams[0].h[i];
		}		 
	} else {
		/*after 2015, extrapolate using secular variation*/
		
	}
		  
	 
	 
	
}
