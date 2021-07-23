#include "DayNo.h"

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
void DayNo(int n, int *Date, int *Year, int *DayNo) {
	int Month, Day, i;
	bool ly;
	
	int months[13] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
	for (i=0;i<n;i++) {
		SplitDate(Date[i],&Year[i],&Month,&Day);
		LeapYear(1,&Year[i],&ly);

		if (ly && Month > 2) {
			DayNo[i] = months[Month-1] + Day + 1;
		} else {
			DayNo[i] = months[Month-1] + Day;
		}
	}
	
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
void DayNotoDate(int n, int *Year, int *DayNo, int *Date) {
	int monthsly[13] = {0,31,60,91,121,152,182,213,244,274,305,335,366};
	int monthsny[13] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
	int *months, i;
	int j, mn, dy;
	bool ly;
	
	for (i=0;i<n;i++) {
		LeapYear(1,&Year[i],&ly);
		if (ly) {
			months = monthsly;
		} else {
			months = monthsny;
		}
		
		if (DayNo[i] > months[12]) {
			Date[i] = Year[i]*10000 + 1231;	
		} else {
			
			j = 0;
			mn = 0;
			dy = DayNo[i];
			while ((DayNo[i] > months[j]) && (mn < 12)) {
				mn = j+1;
				dy = DayNo[i] - months[j];
				j++;
			}
			Date[i] = Year[i]*10000 + mn*100 + dy;
		}
	}
}
