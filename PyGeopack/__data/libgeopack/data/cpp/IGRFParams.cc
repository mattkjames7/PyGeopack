#include "IGRFParams.h"

IGRFP IGRFParams[25];


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
