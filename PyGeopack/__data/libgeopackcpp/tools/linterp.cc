#include "linterp.h"

double linterp(double x0, double x1, double y0, double y1, const double xt) {
	double m;
	m = (y1 - y0)/(x1 - x0);
	return m*(xt - x0) + y0;
}
