#ifndef __CONTUT_H__
#define __CONTUT_H__
#include <stdio.h>
#include <stdlib.h>
#include "BubbleSort.h"
#include "DayNo.h"
#include "WhereEq.h"
#include "Unique.h"
#include <math.h>

#endif
using namespace std;






/***********************************************************************
 * NAME : 		void ContUT(n,Date,ut,utc)
 * 
 * DESCRIPTION : 	Calculates the continuous time in hours since 00:00
 * 					on 19500101 for an array of dates and times. NOTE:
 * 					This algorithm will probably work best if dates and 
 * 					times are arranges in chronological order.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 *
 * OUTPUTS :
 * 			double 	*utc		Continuous time in hours since 00:00 on
 * 								19500101
 * 
 * ********************************************************************/
extern "C" {
	void ContUT(int n, int *Date, float *ut, double *utc);
}

/***********************************************************************
 * NAME : 		void ContUTtoDate(n,Date,ut,utc)
 * 
 * DESCRIPTION : 	Calculates the date and time from the continuous
 * 					time given by ContUT,
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			double 	*utc		Continuous time in hours since 00:00 on
 * 								19500101
 *
 * OUTPUTS :
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 * 
 * ********************************************************************/
extern "C" {
	void ContUTtoDate(int n, double *utc, int *Date, float *ut);
}

/***********************************************************************
 * NAME : 			double GetYearUTC(Year)
 * 
 * DESCRIPTION : 	Get the utc at the beginning of a year.
 * 
 * INPUTS : 
 * 			int		Year	Year, obviously
 *
 * RETURNS :
 * 			double	utc		Continuous time at the start of the year
 * 
 * ********************************************************************/
double GetYearUTC(int Year);

/***********************************************************************
 * NAME : 		void PopulateYearUTC()
 * 
 * DESCRIPTION : 	Calculates the continuous time in hours since 00:00
 * 					on 19500101 for all years between 1950 and 2050
 * 
 * ********************************************************************/
void PopulateYearUTC();
