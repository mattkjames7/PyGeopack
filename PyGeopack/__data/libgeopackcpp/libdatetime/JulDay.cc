#include "JulDay.h"



/***********************************************************************
 * NAME : 		void JulDay(n,Date,ut,JD)
 * 
 * DESCRIPTION : 	Calculates the Julian date from a date and time.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 *
 * OUTPUTS :
 * 			double 	*JD			Julian date.
 * 
 * ********************************************************************/
void JulDay(int n, int *Date, float *ut, double *JD) {
	
	/* split the date */
	int *yr = new int[n];
	int *mn = new int[n];
	int *dy = new int[n];
	DateSplit(n,Date,yr,mn,dy);
	
	
	/* formula from 
	 * https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html */
	int A, B, C, E, F;
	int i;
	for (i=0;i<n;i++) {
		A = yr[i]/100;
		B = A/4;
		C = 2 - A + B;
		E = (int) (365.25*((float) (yr[i] + 4716)));
		F = (int) (30.6001*((float) (mn[i] + 1)));
		JD[i] = C + dy[i] + E + F - 1524.5 + ((double) ut[i])/24.0;
	}
	delete [] yr;
	delete [] mn;
	delete [] dy;
}
