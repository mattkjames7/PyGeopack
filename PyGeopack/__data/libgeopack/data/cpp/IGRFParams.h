#ifndef __IGRFParams_h__
#define __IGRFParams_h__
#include <stdio.h>
#include <stdlib.h>
using namespace std;

/* Data structure to store each set of parameters*/
typedef struct IGRFParam {
	int Year;
	double g[105];
	double h[105];
	double rec[105];
	int n[105];
	int m[105];
} IGRFP;

/* at the time of writing there are 25 sets of parameters, plus one set
 * of secular variation parameters (I believe these are a yearly change*/
extern IGRFP IGRFParams[25];
	
void ReadIGRFParameters(const char *FileName);
#endif
