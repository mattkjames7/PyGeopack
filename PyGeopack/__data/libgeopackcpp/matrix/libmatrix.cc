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
