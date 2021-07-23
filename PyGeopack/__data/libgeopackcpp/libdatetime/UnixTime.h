#ifndef __UNIXTIME_H__
#define __UNIXTIME_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "LeapYear.h"
#include "Unique.h"
#include "DayNo.h"
#include "WhereEq.h"
#include "BubbleSort.h"

#endif
using namespace std;


/***********************************************************************
 * NAME : 		void PopulateYearUnixT()
 * 
 * DESCRIPTION : 	Calculates the unix time in hours since 00:00
 * 					on 19700101 for all years between 1950 and 2050
 * 
 * ********************************************************************/
void PopulateYearUnixT();

/***********************************************************************
 * NAME : 			double GetYearUTC(Year)
 * 
 * DESCRIPTION : 	Get the unix time at the beginning of a year.
 * 
 * INPUTS : 
 * 			int		Year	Year, obviously
 *
 * RETURNS :
 * 			double	unixt	Continuous time at the start of the year
 * 
 * ********************************************************************/
double GetYearUnixT(int Year);

/***********************************************************************
 * NAME : 		void UnixTime(n,Date,ut,unixt)
 * 
 * DESCRIPTION : 	Calculates the unix time in seconds since 00:00
 * 					on 19700101 for an array of dates and times. NOTE:
 * 					This algorithm will probably work best if dates and 
 * 					times are arranges in chronological order.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 *
 * OUTPUTS :
 * 			double 	*unix		Unix time in seconds since 19700101
 * 
 * ********************************************************************/
extern "C" {
	void UnixTime(int n, int *Date, float *ut, double *unixt);
}

/***********************************************************************
 * NAME : 		void UnixTimetoDate(n,Date,ut,unixt)
 * 
 * DESCRIPTION : 	Calculates the date and time from the unix
 * 					time given by ContUT,
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			double 	*utc		Unix time in hours since 00:00 on
 * 								19700101
 *
 * OUTPUTS :
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 * 
 * ********************************************************************/
extern "C" {
	void UnixTimetoDate(int n, double *unixt, int *Date, float *ut);
}
