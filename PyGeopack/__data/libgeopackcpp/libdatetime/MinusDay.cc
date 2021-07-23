#include "MinusDay.h"

/***********************************************************************
 * NAME : 			void MinusDay(Date)
 * 
 * DESCRIPTION : 	Given a date in the format yyyymmdd, subtract a 
 * 					single day.
 * 
 * INPUTS : 
 * 			int		Date	Integer date in format yyyymmdd
 *
 * RETURNS :
 * 			int		Date	The day before the input date.
 * 
 * ********************************************************************/
int MinusDay(int Date) {
	int doy, tmp_year, out, new_doy;
	bool ly;
	DayNo(1,&Date,&tmp_year,&doy);
	
	if (doy == 1) {
		tmp_year = Date/10000 - 1;
		LeapYear(1,&tmp_year,&ly);
		if (ly) {
			new_doy=366;
		} else {
			new_doy=365;
		}
		DayNotoDate(1,&tmp_year,&new_doy,&out);
	} else {
		new_doy = doy-1;
		tmp_year = Date/10000;
		DayNotoDate(1,&tmp_year,&new_doy,&out);
	}
	return out;
}
