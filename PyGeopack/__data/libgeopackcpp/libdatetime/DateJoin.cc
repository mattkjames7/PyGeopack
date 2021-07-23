#include "DateJoin.h"

/***********************************************************************
 * NAME : 			int JoinDate(year,month,day,Date)
 * 
 * DESCRIPTION : 	Join year month and day to a single dat integer in 
 * 					the format yyyymmdd.
 * 
 * INPUTS : 
 * 			int		year	Integer year
 * 			int		month	Integer month
 * 			int		day	Integer day
 *
 * RETURNS :
 * 			int		*Date	Integer date in format yyyymmdd
 * 
 * ********************************************************************/
int JoinDate(int year, int month, int day) {
	int Date;
	Date = year*10000 + month*100 + day;
	return Date;
}

/***********************************************************************
 * NAME : 			void DateJoin(n,year,month,day,Date)
 * 
 * DESCRIPTION : 	Join year month and day to a single dat integer in 
 * 					the format yyyymmdd.
 * 
 * INPUTS : 
 * 			int		n		Number of elements
 * 			int		*year	Integer years
 * 			int		*month	Integer months
 * 			int		*day	Integer days
 *
 * OUTPUTS :
 * 			int		*Date	Integer dates in format yyyymmdd
 * 
 * ********************************************************************/
void DateJoin(int n, int *year, int *month, int *day, int *Date) {
	int i;
	for (i=0;i<n;i++){
		Date[i] = JoinDate(year[i],month[i],day[i]);
	}
}
