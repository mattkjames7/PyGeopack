#include <stdio.h>
#include <stdlib.h>
#include "IGRFParams.h"
#include "libgeopack.h"
using namespace std;
extern IGRFP IGRFParams[25];


int main() {
	ReadIGRFParameters("igrf12coeffs.txt");
	
	int i;
	for (i=0;i<10;i++) {
		printf("(n,m): (%d,%d), G: %lf, H: %lf\n",IGRFParams[0].n[i],IGRFParams[0].m[i],IGRFParams[22].g[i],IGRFParams[22].h[i]);
	}
	return 0;
}
