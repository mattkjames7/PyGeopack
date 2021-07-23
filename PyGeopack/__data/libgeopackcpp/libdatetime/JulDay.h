#ifndef __JULDAY_H__
#define __JULDAY_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DateSplit.h"
#endif
using namespace std;



/***********************************************************************
 * NAME : 		void JulDay(n,Date,ut,JD)
 * 
 * DESCRIPTION : 	Calculates the Julian date from a date and time.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 *
 * OUTPUTS :
 * 			double 	*JD			Julian date.
 * 
 * ********************************************************************/
extern "C" {
	void JulDay(int n, int *Date, float *ut, double *JD);
}
