#include "rotmatrix.h"

/***********************************************************************
 * NAME : 			GetRotationMatrix(A,B,R)
 * 
 * DESCRIPTION : 	This function calculates a rotation matrix from
 * 					two vectors.
 * 
 * INPUTS : 
 * 			double	*A		Input vector
 * 			double	*B		Input vector
 *
 * OUTPUTS : 
 * 			Matrix	&R		Rotation matrix
 *
 * ********************************************************************/
void GetRotationMatrix(double *A, double *B, Matrix &R) {
	
	GetRotationMatrix(A[0],A[1],A[2],B[0],B[1],B[2],R);
	
	
}

/***********************************************************************
 * NAME : 			GetRotationMatrix(A,B,R)
 * 
 * DESCRIPTION : 	This function calculates a rotation matrix from
 * 					two vectors.
 * 
 * INPUTS : 
 * 			double	Ax		Input vector
 * 			double	Ay		Input vector
 * 			double	Az		Input vector
 * 			double	Bx		Input vector
 * 			double	By		Input vector
 * 			double	Bz		Input vector
 *
 * OUTPUTS : 
 * 			Matrix	&R		Rotation matrix
 *
 * ********************************************************************/
void GetRotationMatrix(double Ax, double Ay, double Az,
						double Bx, double By, double Bz, Matrix &R) {
	
	
	/* turn A and B into matrices */
	Matrix A = Matrix(3,1);
	Matrix B = Matrix(3,1);
	A.data[0][0] = Ax;
	A.data[1][0] = Ay;
	A.data[2][0] = Az;
	B.data[0][0] = Bx;
	B.data[1][0] = By;
	B.data[2][0] = Bz;
	

	/* temporary matrices */
	Matrix K0 = Matrix(3,3);
	Matrix K1 = Matrix(3,3);
	Matrix K = Matrix(3,3);
	Matrix K2 = Matrix(3,3);
	MatrixDot(B,A,false,true,K0);
	MatrixDot(A,B,false,true,K1);
	MatrixSubtract(K0,K1,false,false,K);
	MatrixDot(K,K,false,false,K2);

	/* sum everything */
	double IP = Ax*Bx + Ay*By + Az*Bz;
	IP = 1.0/(1.0 + IP);
	
	/* times by K2 */
	K2.TimesScalar(IP);
	
	/* identity matrix */
	Matrix I = Identity(3);
	
	/* final calculations */
	MatrixAdd(K,I,false,false,R);
	MatrixAdd(R,K2,false,false,R);
	
	
}

void GetRotationMatrices(int n, double *Ax, double *Ay, double *Az,
				double *Bx, double *By, double *Bz, MatrixArray &R) {
	
	int i;
	for (i=0;i<n;i++) {
		GetRotationMatrix(Ax[i],Ay[i],Az[i],Bx[i],By[i],Bz[i],*R.matrix[i]);	
	}
	
}
