#include "JulDaytoDate.h"


/***********************************************************************
 * NAME : 		void JulDaytoDate(n,JD,Date,ut)
 * 
 * DESCRIPTION : 	Calculates the date from a Julian date and time.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			double 	*JD			Julian date.
 *
 * OUTPUTS :
 * 			int		*Date		Date array
 * 			float	*ut			Time array in decimal hours 			
 * 
 * ********************************************************************/
void JulDaytoDate(int n, double *JD, int *Date, float *ut) {
	
	/* split the date */
	int *yr = new int[n];
	int *mn = new int[n];
	int *dy = new int[n];

	
	/* formula from 
	 * https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html */
	int A, B, C, D, E, F, X, W, Z;
	double Q;
	int i;
	for (i=0;i<n;i++) {
		/* calculate the time first */
		ut[i] = 24.0*(fmod(JD[i]-0.5,1.0));
		
		Q = JD[i] + 0.5;
		Z = (int) Q;
		W = (Z - 1867216.25)/36524.25;
		X = W/4;
		A = Z + 1 + W - X;
		B = A + 1524;
		C = (B - 122.1)/365.25;
		D = 365.25*C;
		E = (B - D)/30.6001;
		F = 30.6001*E;
		dy[i] = B - D - F + (Q-Z);
		mn[i] = E - 1;
		if (mn[i] > 12) {
			mn[i] -= 12;
		}
		if ((mn[i] == 1) || (mn[i] == 2)) {
			yr[i] = C - 4715; 
		} else {
			yr[i] = C - 4716;
		}
	}
	DateJoin(n,yr,mn,dy,Date);

	
	delete [] yr;
	delete [] mn;
	delete [] dy;
}
