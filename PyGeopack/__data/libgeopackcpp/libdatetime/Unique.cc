//#include "Unique.h"


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
void Unique(int n, T *x, int *nu, T *ux) {
	
	int i, p, pVal;
	p = 0;
	pVal = 0;
	
	/* sort the dates first */
	T *sx = new T[n];
	BubbleSort(n,x,sx);
	
	/* loop through sorted dates, adding a new one to the unique array
	 * when a differnet on is found */
	for (i=0;i<n;i++) {
		if ((sx[i] != pVal) || (i == 0)) {
			ux[p] = sx[i];
			pVal = sx[i];
			p++;
		}
	}
	nu[0] = p;
	
	/* delete the sorted array */
	delete[] sx;	
		
	
}
