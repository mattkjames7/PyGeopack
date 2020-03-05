#ifndef __DateTimeTools_h__
#define __DateTimeTools_h__
#include <stdio.h>
int LeapYear(int year);
void DateToYearDayNo(int Date, int *Year, int *DayNo);
void DecUTToHHMMSS(float UT, int *H, int *M, int *S);
int DayNotoDate(int year, int doy);
int PlusDay(int Date);
int DateDifference(int Date0, int Date1);
float TimeDifference(int Date0, float ut0, int Date1, float ut1);
#endif
