//#include "BubbleSort.h"


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
template <typename T>
void BubbleSort(int n, T *x, T *y) {
	
	bool swapped = true;
	int i, p;
	T tmp;
	
	/* copy each element of x into y */
	for (i=0;i<n;i++) {
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
				y[i] = y[i-1];
				y[i-1] = tmp;
				swapped = true;
			}
		}
		p--;
	}
}
