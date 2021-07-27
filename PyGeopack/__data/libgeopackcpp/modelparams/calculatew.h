#ifndef __CALCULATEW_H__
#define __CALCULATEW_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#endif
using namespace std;

extern "C" {
	void FindIntervals(	int n, double *SymH, double *Bz, int *SWflag, 
						int *IMFflag, int *ni, int *ibeg, int *iend);

	void CalculateW(int n, double *SymH, double *Bz, int *SWflag, int *IMFflag, 
					double *V, double *Den, double *W1, double *W2, double *W3, 
					double *W4, double *W5, double *W6);
}
