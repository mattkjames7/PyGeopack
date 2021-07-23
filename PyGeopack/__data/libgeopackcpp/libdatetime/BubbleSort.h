#ifndef __BUBBLESORT_H__
#define __BUBBLESORT_H__
#include <stdio.h>
#include <stdlib.h>

using namespace std;

/***********************************************************************
 * NAME : 		void BubbleSort(n,x,y)
 * 
 * DESCRIPTION : 	Uses the buble sort algorithm to sort an array. NOTE
 * 					the datatype T is a tmeplate for all data types,
 * 					so it should accept int, float, double etc.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets
 * 			T		x			Array to be sorted
 *
 * OUTPUTS :
 * 			T		y			Sorted array
 * 
 * 
 * ********************************************************************/
/* create a template for a data type so that we can accept any data type */
template <typename T>
void BubbleSort(int n, T *x, T *y);


#include "BubbleSort.cc"
#endif
