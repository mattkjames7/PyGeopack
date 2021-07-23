//#include "WhereEq.h"

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
template <typename T>
void WhereEq(int n, T *x, T y, int *ni, int *ind) {
	
	int i,p;
	p = 0;
	for (i=0;i<n;i++) {
		if (y == x[i]) {
			ind[p] = i;
			p++;
		}
	}
	ni[0] = p;
}
	
