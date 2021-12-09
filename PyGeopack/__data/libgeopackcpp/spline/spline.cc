#include "spline.h"

Spline::Spline(int n, double *x, double *y) {

	/* set n_ equal to n-1 */
	n_ = n - 1;
	
	/* copy the x and y arrays */
	int i;
	x_ = new double[n_+1];
	y_ = new double[n_+1];

	for (i=0;i<n_+1;i++) {
		x_[i] = x[i];
		y_[i] = y[i];
	}

	/* allocate spline parameters */
	a_ = new double[n_+1];
	b_ = new double[n_+1];
	c_ = new double[n_+1];
	d_ = new double[n_+1];

	/* allocate temporary parameters
	 * https://en.wikipedia.org/w/index.php?title=Spline_%28mathematics%29&oldid=288288033#Algorithm_for_computing_natural_cubic_splines */
	double *a = new double[n];
	double *b = new double[n-1];
	double *c = new double[n];
	double *d = new double[n-1];
	double *h = new double[n-1];
	double *alpha = new double[n-1];
	double *mu = new double[n];
	double *l = new double[n];
	double *z = new double[n];

	/* fill a */
	for (i=0;i<n;i++) {
		a[i] = y[i];
	}

	/* fill h */
	for (i=0;i<n-1;i++) {
		h[i] = x[i+1] - x[i];
	}

	/* fill alpha */
	for (i=1;i<n-1;i++) {
		alpha[i] = (3.0/h[i])*(a[i+1] - a[i]) - (3.0/h[i-1])*(a[i] - a[i-1]);
	}

	/* calculate l, mu, z */
	l[0] = 0.0;
	z[0] = 0.0;
	mu[0] = 0.0;

	for (i=1;i<n-1;i++) {
		l[i] = 2.0*(x[i+1] - x[i-1]) - h[i-1]*mu[i-1];
		mu[i] = h[i]/l[i];
		z[i] = (alpha[i] - h[i-1]*z[i-1])/l[i]; 
	}

	l[n-1] = 1.0;
	z[n-1] = 0.0;
	c[n-1] = 0.0;

	/* calculate b, c and d */
	for(i=n-2;i>=0;i--) {
		c[i] = z[i] - mu[i]*c[i+1];
		b[i] = (a[i+1] - a[i])/h[i] - h[i]*(c[i+1] + 2*c[i])/3.0;
		d[i] = (c[i+1] - c[i])/(3*h[i]);
	}

	/* fill the member arrays */
	for (i=0;i<n-1;i++) {
		a_[i] = a[i];
		b_[i] = b[i];
		c_[i] = c[i];
		d_[i] = d[i];
	}

	/* delete temporary stuff */
	delete[] a;
	delete[] b;
	delete[] c;
	delete[] d;
	delete[] l;
	delete[] h;
	delete[] z;
	delete[] mu;
	delete[] alpha;
	
	del_ = true;

}

Spline::Spline(const Spline &obj) {
	/* set n_ equal to n-1 */
	n_ = obj.n_;
	
	del_ = false;
	x_ = obj.x_;
	y_ = obj.y_;
	a_ = obj.a_;
	b_ = obj.b_;
	c_ = obj.c_;
	d_ = obj.d_;

}

Spline::~Spline() {
	if (del_) {
		delete[] a_;
		delete[] b_;
		delete[] c_;
		delete[] d_;
		delete[] x_;
		delete[] y_;
	}
}

void Spline::Interpolate(int n, double *x, double *y) {

	/* create integer array to store indices */
	int *I = new int[n];

	/* determine which index to use for each x input */
	int i, j;
	for (i=0;i<n;i++) {
		if (isfinite(x[i])) {
			if (x[i] < x_[0]) {
				I[i] = 0;
			} else if (x[i] >= x_[n_]) {
				I[i] = n_ - 1;
			} else {
				for (j=0;j<n_;j++) {
					if ((x[i] >= x_[j]) && (x[i] < x_[j+1])) {
						I[i] = j;
						break;
					}
				}
			}
		} else {
			I[i] = -1;
		}
	}

	/* calculate the spline at each point */
	double dx, dx2, dx3;
	for (i=0;i<n;i++) {
		j = I[i];
		if (j < 0) {
			y[i] = NAN;
		} else {
			dx = x[i] - x_[j];
			dx2 = dx*dx;
			dx3 = dx2*dx;
			y[i] = a_[j] + b_[j]*dx + c_[j]*dx2 + d_[j]*dx3;
		}
	}
	
	delete[] I;

}
