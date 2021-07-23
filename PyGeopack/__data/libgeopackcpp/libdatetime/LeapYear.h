#ifndef __LEAPYEAR_H__
#define __LEAPYEAR_H__
#include <stdio.h>
#include <stdlib.h>

#endif
using namespace std;

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
extern "C" {
	void LeapYear(int n, int *year, bool *ly);
}
