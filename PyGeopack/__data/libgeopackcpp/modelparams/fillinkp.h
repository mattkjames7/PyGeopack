#ifndef __FILLINKP_H__
#define __FILLINKP_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#endif

extern "C" {
	void FillInKp(	int nk, int *kDate, float *kut0, float *kut1, float *kp, 
					int n, int *Date, float *ut, float *kpout);
}
