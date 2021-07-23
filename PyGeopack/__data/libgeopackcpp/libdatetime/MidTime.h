#ifndef __MIDTIME_H__
#define __MIDTIME_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DateDifference.h"
#include "PlusDay.h"

#endif
using namespace std;


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
extern "C" {
	void MidTime(int Date0, float ut0, int Date1, float ut1,
				int *Datem, float *utm);
}	
