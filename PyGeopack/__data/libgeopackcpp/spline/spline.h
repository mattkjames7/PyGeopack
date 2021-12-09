#ifndef __SPLINE_H__
#define __SPLINE_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>



class Spline {
	public:
		Spline(int,double*,double*);
		Spline(const Spline &);
		~Spline();
		void Interpolate(int,double*,double*);
	
		int n_;
		double *a_, *b_, *c_, *d_;
		double *x_, *y_;
		bool del_;
};
#endif
