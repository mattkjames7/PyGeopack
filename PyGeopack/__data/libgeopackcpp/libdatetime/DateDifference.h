#ifndef __DATEDIFFERENCE_H__
#define __DATEDIFFERENCE_H__
#include <stdio.h>
#include <stdlib.h>
#include "PlusDay.h"

#endif

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
extern "C" {
	int DateDifference(int Date0, int Date1);
}
