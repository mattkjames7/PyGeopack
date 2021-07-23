#ifndef __DAYNO_H__
#define __DAYNO_H__
#include <stdio.h>
#include <stdlib.h>
#include "LeapYear.h"
#include "DateSplit.h"
#endif
using namespace std;


/***********************************************************************
 * NAME : 			void DayNo(n,Date,Year,DayNo)
 * 
 * DESCRIPTION : 	Work out the day numbers for an array of dates.
 * 
 * INPUTS : 
 * 			int		n		Number of dates
 * 			int		*Date	Integer dates in format yyyymmdd
 *
 * OUTPUTS :
 * 			int		*Year	output years
 * 			int		*DayNo	output day numbers
 * 
 * ********************************************************************/
extern "C" {
	void DayNo(int n, int *Date, int *Year, int *DayNo);
}
/***********************************************************************
 * NAME : 			void DayNotoDate(n,Year,DayNo,Date)
 * 
 * DESCRIPTION : 	Converts year and day number to dates in the format
 * 					yyyymmdd
 * 
 * INPUTS : 
 * 			int		n		Number of dates
 * 			int		*Year	years
 * 			int		*DayNo	day numbers
 *
 * OUTPUTS :
 *  		int		*Date	Integer dates in format yyyymmdd
 * 
 * ********************************************************************/
extern "C" {
	void DayNotoDate(int n, int *Year, int *Doy, int *Date);
}
