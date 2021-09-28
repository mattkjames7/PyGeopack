#include "matrix.h"

/***********************************************************************
 * NAME : 			Matrix::Matrix(inshape)
 * 
 * DESCRIPTION : 	This is a constructor for a matrix object, each
 * 					element is set to zero.
 * 
 * INPUTS : 
 * 			int		*inshape	2-element integer array defining the 
 * 								shape of the matrix
 *
 * ********************************************************************/
Matrix::Matrix(int *inshape) {
	int i;
	Matrix::shape[0] = inshape[0];
	Matrix::shape[1] = inshape[1];
	Matrix::size = inshape[0]*inshape[1];
	Matrix::data = new double*[Matrix::shape[0]];
	for (i=0;i<Matrix::shape[0];i++) {
		Matrix::data[i] = new double[Matrix::shape[1]];
	}
	Matrix::DeleteData = true;
	Matrix::FillZeros();
}

/***********************************************************************
 * NAME : 			Matrix::Matrix(x,y)
 * 
 * DESCRIPTION : 	This is a constructor for a matrix object, each
 * 					element is set to zero.
 * 
 * INPUTS : 
 * 			int		x	length of first dimension of matrix
 * 			int		y	length of second dimension of matrix
 *
 * ********************************************************************/
Matrix::Matrix(int x, int y) {
	int i;
	Matrix::shape[0] = x;
	Matrix::shape[1] = y;
	Matrix::size = x*y;
	Matrix::data = new double*[Matrix::shape[0]];
	for (i=0;i<Matrix::shape[0];i++) {
		Matrix::data[i] = new double[Matrix::shape[1]];
	}
	Matrix::DeleteData = true;
	Matrix::FillZeros();
}

/***********************************************************************
 * NAME : 			Matrix::Matrix(inshape,matrix)
 * 
 * DESCRIPTION : 	This is a constructor for a matrix object, and is
 * 					will fill the matrix with existing data.
 * 
 * INPUTS : 
 * 			int		*inshape	2-element integer array defining the 
 * 								shape of the matrix
 * 			double 	**matrix	Data to fill the new array 
 *
 * ********************************************************************/
Matrix::Matrix(int *inshape, double **matrix) {
	int i;
	Matrix::shape[0] = inshape[0];
	Matrix::shape[1] = inshape[1];
	Matrix::size = inshape[0]*inshape[1];
	Matrix::data = matrix;
	Matrix::DeleteData = false;
}

/***********************************************************************
 * NAME : 			Matrix::Matrix(x,y,matrix)
 * 
 * DESCRIPTION : 	This is a constructor for a matrix object, each
 * 					element is set to zero.
 * 
 * INPUTS : 
 * 			int		x			length of first dimension of matrix
 * 			int		y			length of second dimension of matrix
 * 			double 	**matrix	Data to fill the new array 
 *
 * ********************************************************************/
Matrix::Matrix(int x, int y, double **matrix) {
	int i;
	Matrix::shape[0] = x;
	Matrix::shape[1] = y;
	Matrix::size = x*y;
	Matrix::data = matrix;
	Matrix::DeleteData = false;
}

/***********************************************************************
 * NAME : 			Matrix::Matrix(obj)
 * 
 * DESCRIPTION : 	This is a copy constructor for a matrix object.
 * 
 * INPUTS : 
 * 			int		const Matrix	The original object.	
 *
 * ********************************************************************/
Matrix::Matrix(const Matrix &obj) {
	printf("Matrix copy constructor called!");
	Matrix::shape[0] = obj.shape[0];
	Matrix::shape[1] = obj.shape[1];
	Matrix::size = obj.size;
	Matrix::data = new double*[Matrix::shape[0]];
	int i,j;
	for (i=0;i<Matrix::shape[0];i++) {
		Matrix::data[i] = new double[Matrix::shape[1]];
	}

	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) Matrix::shape[0])/maxthreads);
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] = obj.data[i][j];
			}
		}
	}
}


/***********************************************************************
 * NAME : 			Matrix::~Matrix()
 * 
 * DESCRIPTION : 	This is the destructor for a matrix object.
 * 
 * ********************************************************************/
 Matrix::~Matrix() {
	if (Matrix::DeleteData) {
		int i;
		for (i=0;i<Matrix::shape[0];i++) {
			delete Matrix::data[i];
		}
		delete[] Matrix::data;
	}
}


/***********************************************************************
 * NAME : 			Matrix::FillZeros()
 * 
 * DESCRIPTION : 	Fills every element in the matrix with zeros.
 * 
 * ********************************************************************/
void Matrix::FillZeros() {
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) Matrix::shape[0])/maxthreads);
	int i,j;
	#pragma omp parallel private(i,j)
	{
		
		#pragma omp for schedule(dynamic,chunk) 
		
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] = 0.0;
			}
		}
	}
}

/***********************************************************************
 * NAME : 			Matrix::TimesScalar(x)
 * 
 * DESCRIPTION : 	Multiplies every element in the matrix by a scalar 
 * 					value.
 * 
 * INPUTS : 
 * 		double 	x	Scalar to multiply by everything.
 * 
 * ********************************************************************/
void Matrix::TimesScalar(double x) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] *= x;
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			Matrix::DivideScalar(x)
 * 
 * DESCRIPTION : 	Divides every element in the matrix by a scalar 
 * 					value.
 * 
 * INPUTS : 
 * 		double 	x	Scalar to divide everything by.
 * 
 * ********************************************************************/
void Matrix::DivideScalar(double x) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] /= x;
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			Matrix::AddScalar(x)
 * 
 * DESCRIPTION : 	Adds every element in the matrix to a scalar 
 * 					value.
 * 
 * INPUTS : 
 * 		double 	x	Scalar to add to everything.
 * 
 * ********************************************************************/
void Matrix::AddScalar(double x) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] += x;
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			Matrix::SubtractScalar(x)
 * 
 * DESCRIPTION : 	Subtracts a scalar from every element in the matrix.
 * 
 * INPUTS : 
 * 		double 	x	Scalar to subtract from everything.
 * 
 * ********************************************************************/
void Matrix::SubtractScalar(double x) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] -= x;
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			Matrix::SubtractFromScalar(x)
 * 
 * DESCRIPTION : 	Subtracts every element in the matrix from a scalar.
 * 
 * INPUTS : 
 * 		double 	x	Scalar for everything to be subtracted from.
 * 
 * ********************************************************************/
void Matrix::SubtractFromScalar(double x) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] = x - Matrix::data[i][j];
			}
		}
	}
}	


/***********************************************************************
 * NAME : 			Matrix::PrintMatrix()
 * 
 * DESCRIPTION : 	Print out the contents of the matrix to the 
 * 					terminal, probably best not to do this with big 
 * 					ones!
 * 
 * ********************************************************************/
void Matrix::PrintMatrix() {
	int i,j;
	for (i=0;i<Matrix::shape[0];i++) {
		for (j=0;j<Matrix::shape[1];j++) {
			printf("%10.5f ",Matrix::data[i][j]);
		}
		printf("\n");
	}
}

/***********************************************************************
 * NAME : 			Matrix::PrintMatrix(str)
 * 
 * DESCRIPTION : 	Print out the contents of the matrix to the 
 * 					terminal, probably best not to do this with big 
 * 					ones!
 * 
 * INPUTS : 
 * 		const char 	*str	String to print prior to printing array.
 * 
 * ********************************************************************/
void Matrix::PrintMatrix(const char *str) {
	int i,j;
	printf("%s\n",str);
	printf("shape = (%d,%d)\n",Matrix::shape[0],Matrix::shape[1]);
	for (i=0;i<Matrix::shape[0];i++) {
		for (j=0;j<Matrix::shape[1];j++) {
			printf("%10.5f ",Matrix::data[i][j]);
		}
		printf("\n");
	}
}

/***********************************************************************
 * NAME : 			Matrix::CopyMatrix(m)
 * 
 * DESCRIPTION : 	Copy the contents from another matrix into this one.
 * 
 * INPUTS : 
 * 		Matrix 	m	The matrix from which data will be copied.
 * 
 * ********************************************************************/
void Matrix::CopyMatrix(Matrix &m) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] = m.data[i][j];
			}
		}
	}
}

/***********************************************************************
 * NAME : 			Matrix::FillMatrix(filldata)
 * 
 * DESCRIPTION : 	Copy the contents from a 2D array into this one.
 * 
 * INPUTS : 
 * 		double 		**filldata	The array from which data will be copied.
 * 
 * ********************************************************************/
void Matrix::FillMatrix(double **filldata) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] = filldata[i][j];
			}
		}
	}
}

/***********************************************************************
 * NAME : 			Matrix::FillMatrix(filldata)
 * 
 * DESCRIPTION : 	Copy the contents from a 2D array into this one.
 * 
 * INPUTS : 
 * 		float 		**filldata	The array from which data will be copied.
 * 
 * ********************************************************************/
void Matrix::FillMatrix(float **filldata) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				Matrix::data[i][j] = (double) filldata[i][j];
			}
		}
	}
}

/***********************************************************************
 * NAME : 			Matrix::FillMatrix(outdata)
 * 
 * DESCRIPTION : 	Copy the contents from this matrix into a 2D array.
 * 
 * OUTPUTS : 
 * 		double 		**outdata	The output array.
 * 
 * ********************************************************************/
void Matrix::ReturnMatrix(double **outdata) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				outdata[i][j] = Matrix::data[i][j];
			}
		}
	}
}

/***********************************************************************
 * NAME : 			Matrix::FillMatrix(outdata)
 * 
 * DESCRIPTION : 	Copy the contents from this matrix into a 2D array.
 * 
 * OUTPUTS : 
 * 		float 		**outdata	The output array.
 * 
 * ********************************************************************/
void Matrix::ReturnMatrix(float **outdata) {
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(dynamic) 
		for (i=0;i<Matrix::shape[0];i++) {
			for (j=0;j<Matrix::shape[1];j++) {
				outdata[i][j] = (float) Matrix::data[i][j];
			}
		}
	}
}
