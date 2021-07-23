#include "MidTime.h"

/***********************************************************************
 * NAME : 		void MidTime(Date0,ut0,Date1,ut1,Datem,utm)
 * 
 * DESCRIPTION : 	Calculates the midpoint between two times.
 * 
 * INPUTS : 
 * 			int		Date0		Starting date
 * 			float	ut0			Starting time in decimal hours
 * 			int 	Date1		Ending date
 * 			float 	ut1			Ending time in decimal hours
 *
 * OUTPUTS :
 * 			int		*Datem		Midpoint date
 * 			float	*utm		Midpoint time in decimal hours
 * 
 * ********************************************************************/
void MidTime(int Date0, float ut0, int Date1, float ut1,
			int *Datem, float *utm) {
	
	int i, ndays, nDayHalf;
	float dHour;
				
	/* Firstly, get the number of integer days betweeen Date0 and Date1 */
	ndays = DateDifference(Date0,Date1);
	
	/*check if Date0 == Date1 (it's easy this way) */
	if (ndays == 0) {
		Datem[0] = Date0;
		utm[0] = 0.5*(ut0 + ut1);
	} else {
		/* This is the slightly more complicated situation:
		 * Date0 != Date1, so we need to iterate PlusDay */
		dHour = ndays*24.0 + ut1 - ut0;
		nDayHalf = (int) ((dHour/2.0 + ut0)/24.0);
		Datem[0] = Date0;
		for (i=0;i<nDayHalf;i++) {
			Datem[0] = PlusDay(Datem[0]);
		}
		utm[0] = fmodf(dHour/2 + ut0 ,24.0);
	}
}
