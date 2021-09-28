#ifndef __ROTMATRIX_H__
#define __ROTMATRIX_H__
#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include "matrixmath.h"
#include "matrixarray.h"
#include "identity.h"
#endif

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
void GetRotationMatrix(double *A, double *B, Matrix &R);

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
						double Bx, double By, double Bz, Matrix &R);

void GetRotationMatrices(int n, double *Ax, double *Ay, double *Az,
				double *Bx, double *By, double *Bz, MatrixArray &R);
