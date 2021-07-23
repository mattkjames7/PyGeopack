#include "PlusDay.h"

/***********************************************************************
 * NAME : 			void PlusDay(Date)
 * 
 * DESCRIPTION : 	Given a date in the format yyyymmdd, add a 
 * 					single day.
 * 
 * INPUTS : 
 * 			int		Date	Integer date in format yyyymmdd
 *
 * RETURNS :
 * 			int		Date	The day after the input date.
 * 
 * ********************************************************************/

int PlusDay(int Date) {
	int doy, tmp_year, out;
	bool ly;
	DayNo(1,&Date,&tmp_year,&doy);
	LeapYear(1,&tmp_year,&ly);
	if (((ly == 1) && (doy == 366)) || ((!ly) && (doy == 365))) {
		doy = 1;
		tmp_year++;
	} else {
		doy++;
	}
	DayNotoDate(1,&tmp_year,&doy,&out);
	return out;
}

