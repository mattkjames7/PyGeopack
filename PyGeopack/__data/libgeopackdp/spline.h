#ifndef __SPLINE_H__
#define __SPLINE_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#endif


class Spline {
	public:
		Spline(int,double*,double*);
		~Spline();
		void Interpolate(int,double*,double*);
	private:
		int n_;
		double *a_, *b_, *c_, *d_;
		double *x_, *y_;
};
