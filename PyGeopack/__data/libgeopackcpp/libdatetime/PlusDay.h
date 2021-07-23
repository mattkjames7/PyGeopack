#ifndef __PLUSDAY_H__
#define __PLUSDAY_H__
#include <stdio.h>
#include <stdlib.h>
#include "DayNo.h"
#include "LeapYear.h"

#endif
using namespace std;


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
extern "C" {
	int PlusDay(int Date);
}
