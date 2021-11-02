#include "libmatrix.h"

void RotMatrix(double *A, double *B, double *R) {
	
	Matrix r = Matrix(3,3);
	GetRotationMatrix(A,B,r);
	
	R[0] = r.data[0][0];
	R[1] = r.data[0][1];
	R[2] = r.data[0][2];
	R[3] = r.data[1][0];
	R[4] = r.data[1][1];
	R[5] = r.data[1][2];
	R[6] = r.data[2][0];
	R[7] = r.data[2][1];
	R[8] = r.data[2][2];
}


void RotMatrices(int n, double *Ax, double *Ay, double *Az,
				double *Bx, double *By, double *Bz,
				double *R) {

	int i, j, k, p;
	int shapes[n];

	for (i=0;i<n;i++) {
		shapes[i*2] = 3;
		shapes[i*2 + 1] = 3;
		printf("A[%d]: %f %f %f\n",i,Ax[i],Ay[i],Az[i]);
		printf("B[%d]: %f %f %f\n",i,Bx[i],By[i],Bz[i]);
	}
	MatrixArray r(n,shapes);
	
	GetRotationMatrices(n,Ax,Ay,Az,Bx,By,Bz,r);
	
	p = 0;
	for (i=0;i<n;i++) {
		r.matrix[i]->PrintMatrix("R:");
		for (j=0;j<3;j++) {
			for (k=0;k<3;k++) {
				R[p] = r.matrix[i]->data[j][k];
				p++;
			}
		}
	}

	delete[] R;
}
