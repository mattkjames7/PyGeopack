#include "LeapYear.h"


/***********************************************************************
 * NAME : 			void LeapYear(n,year,ly)
 * 
 * DESCRIPTION : 	Determine whether a year is a leap year
 * 
 * INPUTS : 
 * 			int 	n		Number of elements
 * 			int		*year	Array of years
 *
 * OUTPUTS :
 * 			bool	*ly		Array of boolean (true if is a leap year)
 * 
 * ********************************************************************/
void LeapYear(int n, int *year, bool *ly) {
	int i;
	for (i=0;i<n;i++) {
		if (((year[i] % 4) == 0 && (year[i] % 100) != 0) || (year[i] % 400) == 0) {
			ly[i] = true;
		} else {
			ly[i] = false;
		}
	}
}
