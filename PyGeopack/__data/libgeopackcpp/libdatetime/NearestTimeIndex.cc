#include "NearestTimeIndex.h"

/***********************************************************************
 * NAME : 		int NearestTimeIndex(n,Date,ut,TestDate,Testut)
 * 
 * DESCRIPTION : 	Locates the index of the closest time/date.
 * 
 * INPUTS : 
 * 			int		n			Total number of elements
 * 			int		*Date		Date array in the format yyyymmdd
 * 			float	*ut			UT array, in decimal hours
 * 			int		TestDate	The date we are looking for
 * 			float 	Testut		The time we are looking for
 *
 *
 * RETURNS :
 * 			int		I			Index of the Date/ut arrays which is the
 * 								closest time.
 * 
 * ********************************************************************/
int NearestTimeIndex(int n, int *Date, float *ut, int TestDate, float Testut) {
	
	/* convert to ContUT first */
	double *utc = new double[n];
	ContUT(n,Date,ut,utc);
	double tutc;
	ContUT(1,&TestDate,&Testut,&tutc);	
	
	/* Locate index of nearest ContUT */
	double dt, dtp;
	dt = INFINITY;
	dtp = INFINITY;
	int i, I;
	I = -1;
	for (i=0;i<n;i++) {
		dt = fabs(utc[i] - tutc);
		if (dt < dtp) {
			I = i;
			dtp = dt;
		}
	}
	return I;
}

