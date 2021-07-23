#ifndef __JULDAYTODATE_H__
#define __JULDAYTODATE_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DateJoin.h"
#endif
using namespace std;

/***********************************************************************
 * NAME : 		void JulDaytoDate(n,JD,Date,ut)
 * 
 * DESCRIPTION : 	Calculates the date from a Julian date and time.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			double 	*JD			Julian date.
 *
 * OUTPUTS :
 * 			int		*Date		Date array
 * 			float	*ut			Time array in decimal hours 			
 * 
 * ********************************************************************/
extern "C" {
	void JulDaytoDate(int n, double *JD, int *Date, float *ut);
}
