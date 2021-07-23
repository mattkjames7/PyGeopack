#ifndef __WITHINTIMERANGE_H__
#define __WITHINTIMERANGE_H__
#include <stdio.h>
#include <stdlib.h>
#include "ContUT.h"
#endif
using namespace std;


/***********************************************************************
 * NAME : 		int WithinTimeRange(n,Date,ut,Date0,ut0,Date1,ut1,ni,ind)
 * 
 * DESCRIPTION : 	Locates the indices of all of the times within a 
 * 					defined time range.
 * 
 * INPUTS : 
 * 			int		n			Total number of elements
 * 			int		*Date		Date array in the format yyyymmdd
 * 			float	*ut			UT array, in decimal hours
 * 			int		Date0		Start date
 * 			float 	ut0			Start time
 * 			int		Date1		End date
 * 			float 	ut1			End time
 *
 *
 * OUTPUTS :
 * 			int		*ni			Number of elements within range
 * 			int 	*ind		Array of indices
 * 
 * ********************************************************************/
extern "C" {
	void WithinTimeRange(int n, int *Date, float *ut, 
						int Date0, float ut0,
						int Date1, float ut1,
						int *ni, int *ind);
}
