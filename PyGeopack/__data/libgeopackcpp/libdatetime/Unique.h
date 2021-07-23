#ifndef __UNIQUE_H__
#define __UNIQUE_H__
#include <stdio.h>
#include <stdlib.h>
#include "BubbleSort.h"
using namespace std;


/***********************************************************************
 * NAME : 			void Unique(n,x,nu,ux)
 * 
 * DESCRIPTION : 	Get a list of the unique values in an array.
 * 
 * INPUTS : 
 * 			int		n		Number of elements
 * 			T 		*x		Array of values
 *
 * OUTPUTS :
 * 			int		*nu		Number of unique dates found
 * 			T		*ux		Array of unique values from x
 * 
 * ********************************************************************/
template <typename T>
void Unique(int n, T *x, int *nu, T *ux);

#include "Unique.cc"
#endif
