#include "matrixmath.h"

/***********************************************************************
 * NAME : 			_Multab(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices (element-wise)
 * 					a x b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (n,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b - shape (n,m)
 *
 * ********************************************************************/
void _Multab(Matrix &a, Matrix&b, Matrix &out) {
	int *oshape = out.shape;
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) oshape[0])/maxthreads);
	#pragma omp parallel 
	{
		int i,j;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<oshape[0];i++) {
			for (j=0;j<oshape[1];j++) {
				out.data[i][j] = a.data[i][j] * b.data[i][j];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_MultaTb(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices (element-wise)
 * 					a.T x b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (n,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a.T x b - shape (n,m)
 *
 * ********************************************************************/
void _MultaTb(Matrix &a, Matrix&b, Matrix &out) {
	int *oshape = out.shape;
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) oshape[0])/maxthreads);
	#pragma omp parallel 
	{
		int i,j;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<oshape[0];i++) {
			for (j=0;j<oshape[1];j++) {
				out.data[i][j] = a.data[j][i] * b.data[i][j];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_MultabT(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices (element-wise)
 * 					a x b.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (m,n)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b.T - shape (n,m)
 *
 * ********************************************************************/
void _MultabT(Matrix &a, Matrix&b, Matrix &out) {
	int *oshape = out.shape;
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) oshape[0])/maxthreads);
	#pragma omp parallel 
	{
		int i,j;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<oshape[0];i++) {
			for (j=0;j<oshape[1];j++) {
				out.data[i][j] = a.data[i][j] * b.data[j][i];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_MultaTbT(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices (element-wise)
 * 					a.T x b.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (m,n)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a.T x b.T - shape (n,m)
 *
 * ********************************************************************/
void _MultaTbT(Matrix &a, Matrix&b, Matrix &out) {
	int *oshape = out.shape;
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) oshape[0])/maxthreads);
	#pragma omp parallel 
	{
		int i,j;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<oshape[0];i++) {
			for (j=0;j<oshape[1];j++) {
				out.data[i][j] = a.data[j][i] * b.data[j][i];
			}
		}
	}
}	


/***********************************************************************
 * NAME : 			MatrixMultiply(a,b,aT,bT,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices (element-wise).
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix
 * 			Matrix	&b	Second matrix
 * 			bool	aT	True if we are to transpose matrix a first
 * 			bool	bT	True if we are to transpose matrix b first
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix
 *
 * ********************************************************************/
void MatrixMultiply(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out) {
	if (not aT && not bT) {
		//no transpose
		_Multab(a,b,out);
	} else if (aT && not bT) {
		//only aT
		_MultaTb(a,b,out);
	} else if (not aT && bT) {
		//only bT
		_MultabT(a,b,out);
	} else {
		//both transposed
		_MultaTbT(a,b,out);
	}
}


/***********************************************************************
 * NAME : 			_Dotab(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices a x b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (m,l)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b - shape (n,l)
 *
 * ********************************************************************/
void _Dotab(Matrix &a, Matrix &b, Matrix &out) {
	int kdim;
	if (a.shape[1] == b.shape[0]) {
		kdim = a.shape[1];
	} else {
		printf("Warning! shape of input values should be a(i,j), b(j,k), this may produce strange results\n");
		if (a.shape[1] < b.shape[0]) {
			kdim = a.shape[1];
		} else {
			kdim = b.shape[0];
		}
	}
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) a.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j,k;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<a.shape[0];i++) {
			for (j=0;j<b.shape[1];j++) {
				tmp = 0.0;
				for (k=0;k<kdim;k++) {
					tmp += a.data[i][k] * b.data[k][j];
				}
				out.data[i][j] = tmp;
			}
		}
	}
}		

/***********************************************************************
 * NAME : 			_DotaTb(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices a.T x b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (m,l)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b - shape (n,l)
 *
 * ********************************************************************/
void _DotaTb(Matrix &a, Matrix &b, Matrix &out) {
	int kdim;
	if (a.shape[0] == b.shape[0]) {
		kdim = a.shape[0];
	} else {
		printf("Warning! shape of input values should be a(i,j), b(j,k), this may produce strange results\n");
		if (a.shape[0] < b.shape[0]) {
			kdim = a.shape[0];
		} else {
			kdim = b.shape[0];
		}
	}
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) a.shape[1])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j,k;
		double tmp;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<a.shape[1];i++) {
			for (j=0;j<b.shape[1];j++) {
				tmp = 0.0;
				for (k=0;k<kdim;k++) {
					tmp += a.data[k][i] * b.data[k][j];
				}
				out.data[i][j] = tmp;
			}
		}
	}
}		

/***********************************************************************
 * NAME : 			_DotabT(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices a x b.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (l,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b - shape (n,l)
 *
 * ********************************************************************/
void _DotabT(Matrix &a, Matrix &b, Matrix &out) {
	int kdim;
	if (a.shape[1] == b.shape[1]) {
		kdim = a.shape[1];
	} else {
		printf("Warning! shape of input values should be a(i,j), b(j,k), this may produce strange results\n");
		if (a.shape[1] < b.shape[1]) {
			kdim = a.shape[1];
		} else {
			kdim = b.shape[1];
		}
	}
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) a.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j,k;
		double tmp;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<a.shape[0];i++) {
			for (j=0;j<b.shape[0];j++) {
				tmp = 0.0;
				for (k=0;k<kdim;k++) {
					tmp += a.data[i][k] * b.data[j][k];
				}
				out.data[i][j] = tmp;
			}
		}
	}
}		

/***********************************************************************
 * NAME : 			_DotaTbT(a,b,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices a.T x b.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (l,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b - shape (n,l)
 *
 * ********************************************************************/
void _DotaTbT(Matrix &a, Matrix &b, Matrix &out) {
	int kdim;
	if (a.shape[0] == b.shape[1]) {
		kdim = a.shape[0];
	} else {
		printf("Warning! shape of input values should be a(i,j), b(j,k), this may produce strange results\n");
		if (a.shape[0] < b.shape[1]) {
			kdim = a.shape[0];
		} else {
			kdim = b.shape[1];
		}
	}
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) a.shape[1])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j,k;
		double tmp;
		#pragma omp for schedule(dynamic,chunk) 
		for (i=0;i<a.shape[1];i++) {
			for (j=0;j<b.shape[0];j++) {
				tmp = 0.0;
				for (k=0;k<kdim;k++) {
					tmp += a.data[k][i] * b.data[j][k];
				}
				out.data[i][j] = tmp;
			}
		}
	}
}		

/***********************************************************************
 * NAME : 			MatrixDot(a,b,aT,bT,out)
 * 
 * DESCRIPTION : 	This will multiply two matrices a x b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix 
 * 			Matrix	&b	Second matrix
 * 			bool	aT	True if we are to transpose matrix a first
 * 			bool	bT	True if we are to transpose matrix b first
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a x b
 *
 * ********************************************************************/
void MatrixDot(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out) {
	if (not aT && not bT) {
		//no transpose
		_Dotab(a,b,out);
	} else if (aT && not bT) {
		//only aT
		_DotaTb(a,b,out);
	} else if (not aT && bT) {
		//only bT
		_DotabT(a,b,out);
	} else {
		//both transposed
		_DotaTbT(a,b,out);
	}
}


/***********************************************************************
 * NAME : 			_Subab(a,b,out)
 * 
 * DESCRIPTION : 	This will subtract matrix b from matrix a.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (n,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a - b - shape (n,m)
 *
 * ********************************************************************/
void _Subab(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[i][j] - b.data[i][j];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_SubaTb(a,b,out)
 * 
 * DESCRIPTION : 	This will subtract matrix b from matrix a.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (n,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a.T - b - shape (n,m)
 *
 * ********************************************************************/
void _SubaTb(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[1])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[j][i] - b.data[i][j];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_SubabT(a,b,out)
 * 
 * DESCRIPTION : 	This will subtract matrix b.T from matrix a.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (m,n)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a - b.T - shape (n,m)
 *
 * ********************************************************************/
void _SubabT(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[i][j] - b.data[j][i];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_SubaTbT(a,b,out)
 * 
 * DESCRIPTION : 	This will subtract matrix b.T from matrix a.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (m,n)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a.T - b.T - shape (n,m)
 *
 * ********************************************************************/
void _SubaTbT(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[j][i] - b.data[j][i];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			MatrixSubtract(a,b,aT,bT,out)
 * 
 * DESCRIPTION : 	This will subtract matrix b from matrix a.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix
 * 			Matrix	&b	Second matrix
 * 			bool	aT	True if we are to transpose matrix a first
 * 			bool	bT	True if we are to transpose matrix b first
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a - b
 *
 * ********************************************************************/
void MatrixSubtract(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out) {
	if (not aT && not bT) {
		//no transpose
		_Subab(a,b,out);
	} else if (aT && not bT) {
		//only aT
		_SubaTb(a,b,out);
	} else if (not aT && bT) {
		//only bT
		_SubabT(a,b,out);
	} else {
		//both transposed
		_SubaTbT(a,b,out);
	}
}

/***********************************************************************
 * NAME : 			_Addab(a,b,out)
 * 
 * DESCRIPTION : 	This will add matrices a and b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (n,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a + b - shape (n,m)
 *
 * ********************************************************************/
void _Addab(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[i][j] + b.data[i][j];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_AddaTb(a,b,out)
 * 
 * DESCRIPTION : 	This will add matrices a.T and b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (n,m)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a.T + b - shape (n,m)
 *
 * ********************************************************************/
void _AddaTb(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[1])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[j][i] + b.data[i][j];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_AddabT(a,b,out)
 * 
 * DESCRIPTION : 	This will add matrices a and b.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (n,m)
 * 			Matrix	&b	Second matrix - shape (m,n)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a + b.T - shape (n,m)
 *
 * ********************************************************************/
void _AddabT(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[i][j] + b.data[j][i];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			_AddaTbT(a,b,out)
 * 
 * DESCRIPTION : 	This will add matrices a.T and b.T.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix - shape (m,n)
 * 			Matrix	&b	Second matrix - shape (m,n)
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a.T + b.T - shape (n,m)
 *
 * ********************************************************************/
void _AddaTbT(Matrix &a, Matrix &b, Matrix &out) {
    omp_set_num_threads(8);
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	#pragma omp parallel 
	{
		int i,j;
		double tmp;
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = a.data[j][i] + b.data[j][i];
			}
		}
	}
}	

/***********************************************************************
 * NAME : 			MatrixAdd(a,b,out)
 * 
 * DESCRIPTION : 	This will add matrices a and b.
 * 
 * INPUTS : 
 * 			Matrix	&a	First matrix
 * 			Matrix	&b	Second matrix
 * 			bool	aT	True if we are to transpose matrix a first
 * 			bool	bT	True if we are to transpose matrix b first
 *
 * OUTPUTS : 
 * 			Matrix 	&out	Output matrix = a + b
 *
 * ********************************************************************/
void MatrixAdd(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out) {
	if (not aT && not bT) {
		//no transpose
		_Addab(a,b,out);
	} else if (aT && not bT) {
		//only aT
		_AddaTb(a,b,out);
	} else if (not aT && bT) {
		//only bT
		_AddabT(a,b,out);
	} else {
		//both transposed
		_AddaTbT(a,b,out);
	}
}

/***********************************************************************
 * NAME : 			ApplyFunctionToMatrix(a,AF,out)
 * 
 * DESCRIPTION : 	This will apply an activation function to each 
 * 					element within a matrix.
 * 
 * INPUTS : 
 * 			Matrix	&a		Input matrix - shape (n,m)
 * 			DblFunc	AF		Activation function 
 * 							(see activationfunctions.h)
 *
 * OUTPUTS : 
 * 			Matrix 	&out		Output matrix = AF(a) - shape (n,m)
 *
 * ********************************************************************/
void ApplyFunctionToMatrix(Matrix &a, DblFunc AF, Matrix &out) {
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) out.shape[0])/(maxthreads*10));
	int i,j;
	#pragma omp parallel private(i,j)
	{

		#pragma omp for schedule(static,chunk) 
		for (i=0;i<out.shape[0];i++) {
			for (j=0;j<out.shape[1];j++) {
				out.data[i][j] = AF(a.data[i][j]);
			}
		}
	}	
}

/***********************************************************************
 * NAME : 			ApplyFunctionToMatrix(a,AF)
 * 
 * DESCRIPTION : 	This will apply an activation function to each 
 * 					element within a matrix.
 * 
 * INPUTS : 
 * 			Matrix	&a		Input AND output matrix - shape (n,m)
 * 							a = AF(a)
 * 			DblFunc	AF		Activation function 
 * 							(see activationfunctions.h)
 *
 *
 * ********************************************************************/
void ApplyFunctionToMatrix(Matrix &a, DblFunc AF) {
	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) a.shape[0])/(maxthreads*10));
	int i,j;
	#pragma omp parallel private(i,j)
	{

		#pragma omp for schedule(static,chunk) 
		for (i=0;i<a.shape[0];i++) {
			for (j=0;j<a.shape[1];j++) {
				a.data[i][j] = AF(a.data[i][j]);
			}
		}
	}	
}

/***********************************************************************
 * NAME : 			AddBiasVectorToMatrix(a,b)
 * 
 * DESCRIPTION : 	Adds a bias vector of shape (1,m) to a matrix of 
 * 					shape (n,m).
 * 
 * INPUTS : 
 * 			Matrix	&a		Input AND output matrix - shape (n,m)
 * 							a = a + b
 * 			Matrix	&b		Bias vector (Matrix) shape (1,m)
 *
 *
 * ********************************************************************/
void AddBiasVectorToMatrix(Matrix &a, Matrix &b) {

	int maxthreads = omp_get_max_threads();
	int chunk = ceil(((double) a.shape[0])/(maxthreads*10));
	int i,j;
	#pragma omp parallel private(i,j)
	{
		#pragma omp for schedule(static,chunk) 
		for (i=0;i<a.shape[0];i++) {
			for (j=0;j<a.shape[1];j++) {
				a.data[i][j] = a.data[i][j] + b.data[0][j];
			}
		}
	}		
}
