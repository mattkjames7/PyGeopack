#ifndef __DATEJOIN_H__
#define __DATEJOIN_H__
#include <stdio.h>
#include <stdlib.h>

#endif
using namespace std;


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
int JoinDate(int year, int month, int day);

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
extern "C" {
	void DateJoin(int n, int *year, int *month, int *day, int *Date);
}
