#include "ContUT.h"

double YearUTC[101];
bool PopYearUTC = false;


/***********************************************************************
 * NAME : 		void PopulateYearUTC()
 * 
 * DESCRIPTION : 	Calculates the continuous time in hours since 00:00
 * 					on 19500101 for all years between 1950 and 2050
 * 
 * ********************************************************************/
void PopulateYearUTC() {
	
	int i, Year, nDays;
	bool ly;
	
	/* Set the first one to zero */
	YearUTC[0] = 0.0;
	
	/* Calculate the number of days since the last index */
	for (i=0;i<100;i++) {
		Year = i + 1950;
		LeapYear(1,&Year,&ly);
		if (ly) {
			nDays = 366;
		} else {
			nDays = 365;
		}
		YearUTC[i+1] = YearUTC[i] + ((double) nDays)*24.0;
	}
	
	PopYearUTC = true;
}


/***********************************************************************
 * NAME : 			double GetYearUTC(Year)
 * 
 * DESCRIPTION : 	Get the utc at the beginning of a year.
 * 
 * INPUTS : 
 * 			int		Year	Year, obviously
 *
 * RETURNS :
 * 			double	utc		Continuous time at the start of the year
 * 
 * ********************************************************************/
double GetYearUTC(int Year) {
	
	/* Check that the global variable YearUTC is populated */
	if (!PopYearUTC) {
		PopulateYearUTC();
	}
	
	
	/* now get the starting utc */
	int i, nDays;
	bool ly;
	double utcYear;
	if ((Year >= 1950) & (Year <= 2050)) {
		utcYear = YearUTC[Year - 1950];
	} else if (Year < 1950) {
		utcYear = YearUTC[0];
		for (i=1949;i>=Year;i--) {
			LeapYear(1,&i,&ly);
			if (ly) {
				nDays = 366;
			} else {
				nDays = 365;
			}
			utcYear -= ((double) nDays)*24.0;
		}
	} else {
		utcYear = YearUTC[100];
		for (i=2050;i<Year;i++) {
			LeapYear(1,&i,&ly);
			if (ly) {
				nDays = 366;
			} else {
				nDays = 365;
			}
			utcYear += ((double) nDays)*24.0;
		}
	}
	return utcYear;
}


/***********************************************************************
 * NAME : 		void ContUT(n,Date,ut,utc)
 * 
 * DESCRIPTION : 	Calculates the continuous time in hours since 00:00
 * 					on 19500101 for an array of dates and times. NOTE:
 * 					This algorithm will probably work best if dates and 
 * 					times are arranges in chronological order.
 * 
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 *
 * OUTPUTS :
 * 			double 	*utc		Continuous time in hours since 00:00 on
 * 								19500101
 * 
 * ********************************************************************/
void ContUT(int n, int *Date, float *ut, double *utc) {
	
	/* copy the time across from ut to utc */
	int i;
	for (i=0;i<n;i++) {
		utc[i] = ut[i];
	}
	
	
	/* Get list of unique dates */
	int *uDate = new int[n];
	int nud;
	Unique(n,Date,&nud,uDate);
		
	/* get corresponding list of years and day numbers */
	int *yrs = new int[nud];
	int *dns = new int[nud];
	DayNo(nud,uDate,yrs,dns);
	
	/* loop through each unique date */
	int j;
	int *ind = new int[n];
	int ni;
	double utcDay, utcYear;
	for (i=0;i<nud;i++) {
		/* locate the instances of this particular date */
		WhereEq(n,Date,uDate[i],&ni,ind);
		
		/* Get the utc for the start of the year */
		utcYear = GetYearUTC(yrs[i]);
		
		/* now get the utc for the beginning of the day using the day number */
		utcDay = utcYear + ((double) (dns[i] - 1))*24.0;
		
		/*loop through each one */
		for (j=0;j<ni;j++) {
			utc[ind[j]] += utcDay;
		}
	}
	
	/* delete temporary arrays */
	delete [] ind;
	delete [] yrs;
	delete [] dns;
	delete [] uDate;
	
}

/***********************************************************************
 * NAME : 		void ContUTtoDate(n,Date,ut,utc)
 * 
 * DESCRIPTION : 	Calculates the date and time from the continuous
 * 					time given by ContUT,
 * INPUTS : 
 * 			int 	n			Number of elemenets in Date/ut arrays
 * 			double 	*utc		Continuous time in hours since 00:00 on
 * 								19500101
 *
 * OUTPUTS :
 * 			int		Date		Date array
 * 			float	ut			Time array in decimal hours
 * 
 * ********************************************************************/
void ContUTtoDate(int n, double *utc, int *Date, float *ut) {
	
	
	/* Start by getting the ut (from 0 - 24) and the remaining utc */
	int i, j;
	double *utcr = new double[n];
	double *utd = new double[n];

	for (i=0;i<n;i++) {
		utd[i] = fmod(utc[i],24.0);
		if (utc[i] < 0) {
			utd[i] += 24.0;
		}
		utcr[i] = utc[i] - utd[i];
		ut[i] = (float) utd[i];
	}
	
	/* find the unique values of utcr */
	double *uutcr = new double[n];
	int nu;

	Unique(n,utcr,&nu,uutcr);
	
	/* now loop through each one */
	int ni;
	int *ind = new int[n];
	int yr, dn, dt;
	double utcYear;
	for (i=0;i<nu;i++) {
		/* get all of the times with the same date */
		WhereEq(n,utcr,uutcr[i],&ni,ind);

		/* work out the year first */
		yr = 2000;
		utcYear = GetYearUTC(yr);
		if (utcYear <= uutcr[i]) {
			/* start from 2000 and loop forwards a year at a time */
			while (utcYear <= uutcr[i]) {
				yr++;
				utcYear = GetYearUTC(yr);
			}
			yr = yr -1;
			utcYear = GetYearUTC(yr);
		} else { 
			/* in this case, we need to loop backwards */
			while (utcYear > uutcr[i]) {
				yr--;
				utcYear = GetYearUTC(yr);
			}
		}

		/* we know the year, now to find the number of days */
		dn = (int) round((uutcr[i] - utcYear)/24.0 + 1.0);

		/* work out the date integer */
		DayNotoDate(1,&yr,&dn,&dt);

		/* Fill in the output array */
		for (j=0;j<ni;j++) {
			Date[ind[j]] = dt;
		}	
	}
	delete [] ind;
	delete [] uutcr;
	delete [] utcr;
	delete [] utd;
		
	
}
