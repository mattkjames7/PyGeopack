#include "DateSplit.h"

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
void SplitDate(int Date, int *year, int *month, int *day) {
	year[0] = Date / 10000;
	month[0] = (Date % 10000) / 100;
	day[0] = Date % 100;
}

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
void DateSplit(int n, int *Date, int *year, int *month, int *day) {
	int i;
	
	for (i=0;i<n;i++) {
		SplitDate(Date[i],&year[i],&month[i],&day[i]);
	}
}
