#include "matrixarray.h"



/***********************************************************************
 * NAME : 			MatrixArray::MatrixArray(nMat,MatShapes)
 * 
 * DESCRIPTION : 	This is a constructor for a matrix array object.
 * 
 * INPUTS : 
 * 			int		nMat		Number of matrices to be stored.
 * 			int		*MatShapes	Integer array containing the dimensions
 * 								of each matrix, e.g.
 * 								if nMat = 3, then the x and y dimensions
 * 								of each array are stored in MatShapes:
 * 								MatShapes = [x0,y0,x1,y1,x2,y2]
 *
 * ********************************************************************/
 MatrixArray::MatrixArray(int nMat, int *MatShapes) {
	int i;
	MatrixArray::n = nMat;
	MatrixArray::matrix = new Matrix*[MatrixArray::n];
	for (i=0;i<n*2;i+=2){
		MatrixArray::matrix[i/2] = new Matrix(MatShapes[i],MatShapes[i+1]);
	}
}

/***********************************************************************
 * NAME : 			MatrixArray::MatrixArray(memstart)
 * 
 * DESCRIPTION : 	This is a constructor for a matrix array object, it
 * 					will read the elements of each matrix from memory.
 * 					
 * 
 * INPUTS : 
 * 			unsigned char *memstart	This is the pointer to the start of
 * 									memory where the matrix array is
 * 									stored.
 * 									When using this method to load 
 * 									weights/biases of a neural network,
 * 									is is important to remember that
 * 									neither of these are at the start of
 * 									memory.
 *
 * ********************************************************************/
MatrixArray::MatrixArray(unsigned char **memstart) {
	
	/* create a pointer that we can move through the memory */
	unsigned char *p = *memstart;
	
	/* firstly, we need to work out the number of arrays */
	MatrixArray::n = ((int*) p)[0];
	p += sizeof(int);

	/* now we know that, we can create the empty matrix objects ready
	 * for initialization */
	int i, j, k, shape[2], ns, size;
	MatrixArray::matrix = new Matrix*[MatrixArray::n];
	
	/* loop through an read each on in - NOTE: the files are 32-bit
	 * floating points, but this object uses 64-bit floats.*/
	for (i=0;i<MatrixArray::n;i++) {
		/* start with reading the shape in */
		size = ((int*) p)[0];
		ns = ((int*) p)[1];
		if (ns == 1) {
			shape[1] = ((int*) p)[2];
			shape[0] = 1;
			p += 3*sizeof(int);
		} else {
			shape[0] = ((int*) p)[2];
			shape[1] = ((int*) p)[3];
			p += 4*sizeof(int);
		}
		
		
		/*create the matrix */
		MatrixArray::matrix[i] = new Matrix(shape);

		/* convert the data to doubles and copy it into the matrix object */
		for (j=0;j<shape[0];j++) {
			for (k=0;k<shape[1];k++) {
				MatrixArray::matrix[i]->data[j][k] = (double) ((float*) p)[0];
				p += sizeof(float);
			}
		}
	 }
	 /*update the initial pointer */
	 memstart[0] = p;
	
}


/***********************************************************************
 * NAME : 			MatrixArray::MatrixArray(const MatrixArray &obj)
 * 
 * DESCRIPTION : 	This is a copy constructor for a matrix array object.
 * 
 * ********************************************************************/
MatrixArray::MatrixArray(const MatrixArray &obj) {
	printf("Called Matrix array copy constructor\n");
}

/***********************************************************************
 * NAME : 			MatrixArray::~MatrixArray()
 * 
 * DESCRIPTION : 	This is a destructor for a matrix array object.
 * 
 * ********************************************************************/
MatrixArray::~MatrixArray() {
	int i;
	for (i=0;i<MatrixArray::n;i++) {
		delete MatrixArray::matrix[i];
	}
	delete[] MatrixArray::matrix;
}

/***********************************************************************
 * NAME : 			MatrixArray::RandomInit(range)
 * 
 * DESCRIPTION : 	This will fill each matrix with random values within
 * 					+/- range.
 * 
 * INPUTS : 
 * 			float 	range	The absolute range of the random values
 * 							which will fill each matrix.
 *
 * ********************************************************************/
void MatrixArray::RandomInit(float Range) {
	int i, j, k;
	
	for (i=0;i<MatrixArray::n;i++) {
		for (j=0;j<MatrixArray::matrix[i]->shape[0];j++) {
			for (k=0;k<MatrixArray::matrix[i]->shape[1];k++) {
				MatrixArray::matrix[i]->data[j][k] = Range*(2.0*((float) rand()) / ((float) RAND_MAX) - 1.0);
			}
		}
	}
}
