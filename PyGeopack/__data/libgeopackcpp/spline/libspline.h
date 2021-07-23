#ifndef __LIBSPLINE_H__
#define __LIBSPLINE_H__
#include <stdio.h>
#include <stdlib.h>
#include "spline.h"
#endif

extern "C" {
	void spline(int n0, double *x0, double *y0, 
				int n1, double *x1, double *y1);
}
