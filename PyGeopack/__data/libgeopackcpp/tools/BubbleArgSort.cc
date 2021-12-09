#include "BubbleArgSort.h"



/***********************************************************************
 * NAME : 		void BubbleArgSort(n,x,I)
 * 
 * DESCRIPTION : 	Uses the buble sort algorithm to sort an array. NOTE
 * 					the datatype T is a tmeplate for all data types,
 * 					so it should accept int, float, double etc.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets
 * 			double	x			Array to be sorted
 *
 * OUTPUTS :
 * 			int		I			Sorted index array
 * 
 * 
 * ********************************************************************/
void BubbleArgSort(int n, double *x, int *I) {
	
	bool swapped = true;
	int i, p;
	double *y = new double[n];
	double tmp;
	int tmpI;
	
	/* copy each element of x into y */
	for (i=0;i<n;i++) {
		I[i] = i;
		y[i] = x[i];
	}	
	
	/* Check that we have enough elements for there not to be a 
	 * segmentation fault */
	if (n < 2) {
		return;
	}
	
	/* start sorting by swapping elements */
	p = n;
	while (!swapped) {
		swapped = false;
		for (i=1;i<p;i++) {
			if (y[i-1] > y[i]) {
				/* swap */
				tmp = y[i];
				tmpI = I[i];
				y[i] = y[i-1];
				I[i] = I[i-1];
				y[i-1] = tmp;
				I[i-1] = tmpI;
				swapped = true;
			}
		}
		p--;
	}
	
	delete[] y;
}

