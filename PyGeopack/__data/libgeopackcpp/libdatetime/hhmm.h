#ifndef __HHMM_H__
#define __HHMM_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#endif
using namespace std;
const double mn2hr = 1.0/60.0;
const double sc2hr = 1.0/3600.0;
const double ms2hr = 1.0/3600000.0;

extern "C" {
/***********************************************************************
 * NAME : 			void DectoHHMM(n,ut,hh,mm,ss,ms)
 * 
 * DESCRIPTION : 	Convert decimal hours to hours, minutes etc
 * 
 * INPUTS : 
 * 			int 	n		Number of elements in ut
 * 			double	*ut		Time array in decimal hours
 *
 * OUTPUTS :
 * 			int		*hh		hours
 * 			int		*mm		minutes
 * 			int		*ss		seconds
 * 			double	*ms		milliseconds
 * 
 * ********************************************************************/
	void DectoHHMM(int n, double *ut, int *hh, int *mm, int *ss, double *ms);

/***********************************************************************
 * NAME : 			void HHMMtoDec(n,hh,mm,ss,ms,dec)
 * 
 * DESCRIPTION : 	Convert time in hours minutes etc to decimal hours
 * 
 * INPUTS : 
 * 			int 	n		Number of elements
 * 			double	*hh		hours
 * 			double		*mm		minutes
 * 			double		*ss		seconds
 * 			double	*ms		milliseconds
 *
 * OUTPUTS :
 * 			double	*ut		Time array in decimal hours
 * 
 * ********************************************************************/
	void HHMMtoDec(int n, double *hh, double *mm, double *ss, double *ms, double *ut);
}
