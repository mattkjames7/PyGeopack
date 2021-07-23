#include "WithinTimeRange.h"

/***********************************************************************
 * NAME : 		int WithinTimeRange(n,Date,ut,Date0,ut0,Date1,ut1,ni,ind)
 * 
 * DESCRIPTION : 	Locates the indices of all of the times within a 
 * 					defined time range.
 * 
 * INPUTS : 
 * 			int		n			Total number of elements
 * 			int		*Date		Date array in the format yyyymmdd
 * 			float	*ut			UT array, in decimal hours
 * 			int		Date0		Start date
 * 			float 	ut0			Start time
 * 			int		Date1		End date
 * 			float 	ut1			End time
 *
 *
 * OUTPUTS :
 * 			int		*ni			Number of elements within range
 * 			int 	*ind		Array of indices
 * 
 * ********************************************************************/
void WithinTimeRange(int n, int *Date, float *ut, 
						int Date0, float ut0,
						int Date1, float ut1,
						int *ni, int *ind){
						
	/* convert everything to utc */
	double *utc = new double[n];
	double utc0, utc1;
	ContUT(n,Date,ut,utc);
	ContUT(1,&Date0,&ut0,&utc0);
	ContUT(1,&Date1,&ut1,&utc1);
						
	/* scan throught he array to see which are within the time range */
	int i;
	int p = 0;
	
	for (i=0;i<n;i++) {
		if ((utc[i] >= utc0) && (utc[i] <= utc1)) {
			ind[p] = i;
			p++;
		}
	}
	ni[0] = p;
	
}
