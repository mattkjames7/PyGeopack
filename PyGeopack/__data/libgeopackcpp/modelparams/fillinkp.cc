#include "fillinkp.h"


void FillInKp(	int nk, int *kDate, float *kut0, float *kut1, float *kp, 
				int n, int *Date, float *ut, float *kpout) {
	/*******************************************************************
	 * This procedure should fill in the Kp indices for the Tsyganenko 
	 * T89 model. It will loop through each data point until it finds 
	 * the correct date and time range.
	 * 
	 * ****************************************************************/ 
	
	/* i is the index of the current output data, p is the index of the kp data*/
	int i, p;
	bool outofrange = false;
	p = 0;
	for (i=0;i<n;i++) {
		printf("\rFilling in Kp %d of %d",i+1,n);
		while ((Date[i] < kDate[p]) || ((Date[i] == kDate[p]) && (ut[i] < kut0[p]))) {
			p--;
			if (p < 0) {
				p = 0;
				outofrange = true;
				break;
			}
		}		
		while ((Date[i] > kDate[p]) || ((Date[i] == kDate[p]) && (ut[i] >= kut1[p]))) {
			p++;
			if (p >= nk) {
				p = nk-1;
				outofrange = true;
				break;
			}
		}
		if (!outofrange) {
			kpout[i] = kp[p];
		} else { 
			kpout[i] = NAN;
			outofrange = false;
		}
	}
	printf("\n");
}
