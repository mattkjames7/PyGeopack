#include "hhmm.h"

/***********************************************************************
 * NAME : 			void DectoHHMM(n,ut,hh,mm,ss,ms)
 * 
 * DESCRIPTION : 	Convert decimal hours to hours, minutes etc
 * 
 * INPUTS : 
 * 			int 	n		Number of elements in ut
 * 			double	*ut		Time array in decimal hours
 *
 * OUTPUTS :
 * 			int		*hh		hours
 * 			int		*mm		minutes
 * 			int		*ss		seconds
 * 			double	*ms		milliseconds
 * 
 * ********************************************************************/
void DectoHHMM(int n, double *ut, int *hh, int *mm, int *ss, double *ms) {
	int i;
	double rem;
	
	for (i=0;i<n;i++) {
		/* get the hours first and the remainder */
		hh[i] = floor(ut[i]);
		rem = ut[i] - (double) hh[i];
		
		/*work out the minutes */
		rem *= 60.0;
		mm[i] = floor(rem);
		rem = rem - (double) mm[i];
		
		/* now work out seconds */
		rem *= 60.0;
		ss[i] = floor(rem);
		rem = rem - (double) ss[i];
		
		/* milliseconds */
		ms[i] = rem*1000.0;
	}	
	
	
}

/***********************************************************************
 * NAME : 			void HHMMtoDec(n,hh,mm,ss,ms,dec)
 * 
 * DESCRIPTION : 	Convert time in hours minutes etc to decimal hours
 * 
 * INPUTS : 
 * 			int 	n		Number of elements
 * 			double	*hh		hours
 * 			double		*mm		minutes
 * 			double		*ss		seconds
 * 			double	*ms		milliseconds
 *
 * OUTPUTS :
 * 			double	*ut		Time array in decimal hours
 * 
 * ********************************************************************/
void HHMMtoDec(int n, double *hh, double *mm, double *ss, double *ms, double *ut){
	int i;
	
	for (i=0;i<n;i++) {
		ut[i] = ((double) hh[i]) + ((double) mm[i])*mn2hr + 
				((double) ss[i])*sc2hr + ((double) ms[i])*ms2hr;
	}
	
}
