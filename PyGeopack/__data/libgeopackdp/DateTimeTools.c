#include "DateTimeTools.h"

int LeapYear(int year) {
	if (((year % 4) == 0 && (year % 100) != 0) || (year % 400) == 0) {
		return 1;
	} else {
		return 0;
	}
}
		

void DateToYearDayNo(int Date, int *Year, int *DayNo) {
	int Month, Day;
	*Year = Date/10000;
	Month=(Date % 10000)/100;
	Day=Date % 100;
	int months[13] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
	if (LeapYear(*Year) == 1 && Month > 2) {
		*DayNo = months[Month-1] + Day + 1;
	} else {
		*DayNo = months[Month-1] + Day;
	}
	
	
}

void DecUTToHHMMSS(float UT, int *H, int *M, int *S) {
	*H = (int) UT;
	float Md = (60.0*(UT - *H));
	*M = (int) Md;
	*S = (int) (60.0*(Md - *M));
}

int DayNotoDate(int year, int doy) {
	int monthsly[13] = {0,31,60,91,121,152,182,213,244,274,305,335,366};
	int monthsny[13] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
	int *months;
	if (LeapYear(year) == 1) {
		months = monthsly;
	} else {
		months = monthsny;
	}
	
	if (doy > months[12]) {
		return year*10000 + 1231;	
	}
	int i, mn, dy;
	i=0;
	mn=0;
	dy=doy;
	while ((doy > months[i]) && (mn < 12)) {
		mn=i+1;
		dy=doy-months[i];
		i++;
	}
	return year*10000+mn*100+dy;
}

int PlusDay(int Date){
	int doy, tmp_year, ly;
	DateToYearDayNo(Date,&tmp_year,&doy);
	ly = LeapYear(tmp_year);
	if (((ly == 1) && (doy == 366)) || ((!ly) && (doy == 365))) {
		doy = 1;
		tmp_year++;
	} else {
		doy++;
	}
	return DayNotoDate(tmp_year,doy);
}

int DateDifference(int Date0, int Date1) {
	int SD, ED, dd, Dir;
	if (Date0 > Date1) {
		SD = Date1;
		ED = Date0;
		Dir = -1;
	} else { 
		SD = Date0;
		ED = Date1;
		Dir = 1;
	}
	
	dd = 0;
	while (SD < ED) {
		SD = PlusDay(SD);
		dd += Dir;
	}
	return dd;
}

float TimeDifference(int Date0, float ut0, int Date1, float ut1) {
	int dd = DateDifference(Date0,Date1);
	return ((float) dd) + (ut1 - ut0)/24.0;
}

