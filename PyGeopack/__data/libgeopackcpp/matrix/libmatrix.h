#ifndef __LIBMATRIX_H__
#define __LIBMATRIX_H__
#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include "matrixmath.h"
#include "matrixarray.h"
#include "identity.h"
#include "rotmatrix.h"

#endif

extern "C" {
	void RotMatrix(double *A, double *B, double *R) ;
	void RotMatrices(int n, double *Ax, double *Ay, double *Az,
				double *Bx, double *By, double *Bz,
				double *R) ;
}
