#include "identity.h"

Matrix Identity(int n) {
	
	Matrix I = Matrix(n,n);
	I.FillZeros();
	int i;
	for (i=0;i<n;i++) {
		I.data[i][i] = 1.0;
	}
	return I;
	
}
