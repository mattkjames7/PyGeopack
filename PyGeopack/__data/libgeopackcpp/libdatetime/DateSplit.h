#ifndef __DATESPLIT_H__
#define __DATESPLIT_H__
#include <stdio.h>
#include <stdlib.h>

#endif
using namespace std;

/***********************************************************************
 * NAME : 			void SplitDate(Date,year,month,day)
 * 
 * DESCRIPTION : 	Convert a date of the format yyyymmdd to separate
 * 					integers: year, month and day
 * 
 * INPUTS : 
 * 			int		Date	Integer date in format yyyymmdd
 *
 * OUTPUTS :
 * 			int		*year	Integer year
 * 			int		*month	Integer month
 * 			int		*day	Integer day
 * 
 * ********************************************************************/
void SplitDate(int Date, int *year, int *month, int *day);

/***********************************************************************
 * NAME : 			void DateSplit(n,Date,year,month,day)
 * 
 * DESCRIPTION : 	Convert an array of dates in the format yyyymmdd to 
 * 					separate integers: year, month and day
 * 
 * INPUTS : 
 * 			int 	n
 * 			int		*Date	Integer date in format yyyymmdd
 *
 * OUTPUTS :
 * 			int		*year	Integer year
 * 			int		*month	Integer month
 * 			int		*day	Integer day
 * 
 * ********************************************************************/
extern "C" {
	void DateSplit(int n, int *Date, int *year, int *month, int *day);
}
