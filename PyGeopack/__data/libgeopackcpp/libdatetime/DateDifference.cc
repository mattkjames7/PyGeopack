#include "DateDifference.h"

/***********************************************************************
 * NAME : 			int DateDifference(Date0,Date1)
 * 
 * DESCRIPTION : 	Calculates the number of whole days between two 
 * 					dates e.g. 
 * 					dd = DateDifference(20120101,20120103)
 * 					returns dd == 2
 * 
 * INPUTS : 
 * 			int		Date0		Starting date
 * 			int 	Date1		Ending date
 *
 * OUTPUTS :
 *  		int		ndays		Number of days from Date0 to Date1
 * 
 * ********************************************************************/
int DateDifference(int Date0, int Date1) { 
	
	int SD,ED,dir,ndays;
	
	/* Initialize ndays */
	ndays = 0;

	/* Check which order Date0 and Date1 are in */
	if (Date0 < Date1) {
		SD = Date0;
		ED = Date1;
		dir = 1;
	} else {
		ED = Date0;
		SD = Date1;
		dir = -1;
	}

	/* Now repeatedly add 1 day to SD until it is equal to ED */
	while (SD < ED) {
		SD = PlusDay(SD);
		ndays++;
	}
	
	return dir*ndays;
}
