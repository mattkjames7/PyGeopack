#include "TimeDifference.h"


/***********************************************************************
 * NAME : 			float TimeDifference(Date0,ut0,Date1,ut1)
 * 
 * DESCRIPTION : 	Calculates the number of days between two times
 * 					(result in days)
 * 
 * INPUTS : 
 * 			int		Date0		Starting date
 * 			float	ut0			Starting time in decimal hours
 * 			int 	Date1		Ending date
 * 			float 	ut1			Ending time in decimal hours
 *
 * RETURNS :
 *  		float	ndays		Number of days between start and end times
 * 
 * ********************************************************************/
float TimeDifference(int Date0, float ut0, int Date1, float ut1) { 
	
	int dd;
	float out;
	
	/* get the number of days difference first */
	dd = DateDifference(Date0,Date1);
	
	/* now the whole time difference */
	out = dd + (ut1 - ut0)/24.0;
	
	return out;
	
}
	
	
