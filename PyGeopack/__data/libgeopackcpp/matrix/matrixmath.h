#ifndef __MATRIXMATH_H__
#define __MATRIXMATH_H__
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "matrix.h"
#include <omp.h>
//#include "activationfunctions.h"
using namespace std;

/* this function pointer can be used to define a simple function which
 * accepts a double and returns a double */
typedef double (*DblFunc)(double);

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
void MatrixMultiply(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out);

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
void MatrixDot(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out);

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
void MatrixAdd(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out);

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
void MatrixSubtract(Matrix &a, Matrix &b, bool aT, bool bT, Matrix &out);

/***********************************************************************
 * NAME : 			ApplyFunctionToMatrix(a,AF,o)
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
 * 			Matrix 	&o		Output matrix = AF(a) - shape (n,m)
 *
 * ********************************************************************/
void ApplyFunctionToMatrix(Matrix &a, DblFunc AF, Matrix &o);

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
void ApplyFunctionToMatrix(Matrix &a, DblFunc AF);


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
void AddBiasVectorToMatrix(Matrix &a, Matrix &b);
#endif
