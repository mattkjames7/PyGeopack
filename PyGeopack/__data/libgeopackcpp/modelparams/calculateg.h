#ifndef __CALCULATEG_H__
#define __CALCULATEG_H__
#include <stdio.h>
#include <stdlib.h>
#include <algorithm>
#include <math.h>
#endif
using namespace std;

extern "C" {
	void CalculateG(int n, double *By, double *Bz, double *V, bool *good, double *G1, double *G2);
}
