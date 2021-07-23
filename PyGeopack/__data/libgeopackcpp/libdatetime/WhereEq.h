#ifndef __WHEREEQ_H__
#define __WHEREEQ_H__
#include <stdio.h>
#include <stdlib.h>

using namespace std;


/***********************************************************************
 * NAME : 		void WhereEq(n,x,y,ni,ind)
 * 
 * DESCRIPTION : 	Scan through an arra, x, for instances where it is 
 * 					equal to y.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets
 * 			T		*x			Array to be scanned
 * 			T 		y			value to test
 *
 * OUTPUTS :
 * 			int		*ni			Number of indices
 * 			int		*ind		Array of indices
 * 
 * 
 * ********************************************************************/
/* create a template for a data type so that we can accept any data type */
template <typename T>
void WhereEq(int n, T *x, T y, int *ni, int *ind);


#include "WhereEq.cc"
#endif
