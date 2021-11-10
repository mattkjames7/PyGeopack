#include "tracerotationmatrices.h"


MatrixArray TraceRotationMatrices(int n, double *xin, double *yin, double *zin) {

	/* create some arrays to store unit vectors in */
	double *Ax = new double[n];
	double *Ay = new double[n];
	double *Az = new double[n];
	double *Bx = new double[n];
	double *By = new double[n];
	double *Bz = new double[n];
	double A;

	/* fill them up and normalize A */
	int i;
	for (i=0;i<n;i++) {
		Ax[i] = xin[i];
		Ay[i] = yin[i];
		Az[i] = zin[i];
		A = sqrt(Ax[i]*Ax[i] + Ay[i]*Ay[i] + Az[i]*Az[i]);
		Ax[i]/= A;
		Ay[i]/= A;
		Az[i]/= A;
		
		Bx[i] = 0.0;
		By[i] = 0.0;
		Bz[i] = 1.0;
	}

	/*calculate the rotation matrices */
	int shapes[n*2];

	for (i=0;i<n;i++) {
		shapes[i*2] = 3;
		shapes[i*2 + 1] = 3;
	}

	MatrixArray R(n,shapes);
	GetRotationMatrices(n,Bx,By,Bz,Ax,Ay,Az,R);

	/* clean up allocated variables */
	delete[] Ax;
	delete[] Ay;
	delete[] Az;
	delete[] Bx;
	delete[] By;
	delete[] Bz;
	
	return R;
	
}
