#include "testrotate.h"

MatrixArray GetMatArr(int n) {
	
	int i;
	int shapes[n*2];
	for (i=0;i<n;i++) {
		shapes[i*2] = 3;
		shapes[i*2 + 1] = 3;
	}
	
	MatrixArray R(n,shapes);
	
	R.RandomInit(1.0);
	
	for (i=0;i<n;i++) {
		printf("i: %d\n",i);
		R.matrix[i]->PrintMatrix();
	}
	
	
	return R;
	
}

int main() {
	
	int n = 10;
	double *x = new double[n];
	double *y = new double[n];
	double *z = new double[n];
	
	int i;
	for (i=0;i<n;i++) {
		x[i] = -1.0 + i*0.3;
		y[i] = 8.0 - i*1.2;
		z[i] = 1.0 + i*0.2;
		printf("i = %d: [ %5.2f, %5.2f, %5.2f ]\n",i,x[i],y[i],z[i]);
	}
	
	MatrixArray R = GetMatArr(n);
	
	for (i=0;i<n;i++) {
		printf("i: %d\n",i);
		R.matrix[i]->PrintMatrix();
	}

	delete[] x;
	delete[] y;
	delete[] z;
}
