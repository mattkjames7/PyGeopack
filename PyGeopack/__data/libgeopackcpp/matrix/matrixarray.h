#ifndef __MATRIXARRAY_H__
#define __MATRIXARRAY_H__
#include <math.h>
#include <stdio.h>
#include <ctime>
#include <cstdlib>
#include "matrix.h"
using namespace std;

/***********************************************************************
 * NAME : Matrix
 * 
 * DESCRIPTION : The purpose of this object is to store a an array of
 * 				matrix objects. This object is particularly useful for
 * 				storing the weight/bias matrices of a neural network and
 * 				for use as temporary propagation matrices.
 * 
 * ********************************************************************/
class MatrixArray {
	public:
		/* constructor of the array */
		MatrixArray(int,int*);
		MatrixArray(unsigned char **memstart);
		
		/* copy constructor */
		MatrixArray(const MatrixArray &obj);
		
		/* destructor */
		~MatrixArray();
		
		/* random initialization of the array contained in this object */
		void RandomInit(float);
		
		/* this is the array of matrices contained within this object */
		int n;
		Matrix **matrix;
	private:

};

#endif
