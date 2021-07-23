#ifndef __MINUSDAY_H__
#define __MINUSDAY_H__
#include <stdio.h>
#include <stdlib.h>
#include "DayNo.h"
#include "LeapYear.h"
#endif
using namespace std;

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
extern "C" {
	int MinusDay(int Date);
}
