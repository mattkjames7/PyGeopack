#ifndef __TIMEDIFFERENCE_H__
#define __TIMEDIFFERENCE_H__
#include <stdio.h>
#include <stdlib.h>
#include "DateDifference.h"

#endif

using namespace std;

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
extern "C" {
	float TimeDifference(int Date0, float ut0, int Date1, float ut1);
}
