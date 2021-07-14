#include "ConvCoords.h"


/***********************************************************************
 * NAME : 			void GSEtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSE coordinate (R_E)
 * 		double	Yin		y GSE coordinate (R_E)
 * 		double	Zin		z GSE coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSM coordinate (R_E)
 * 		double	*Yout	y GSM coordinate (R_E)
 * 		double	*Zout	z GSM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GSM coordinates
 * 
 * ********************************************************************/
void GSEtoGSM(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;


	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

/***********************************************************************
 * NAME : 			void GSEtoGSM(	*Xin, *Yin, *Zin, n, *Vx, *Vy, *Vz, 
 * 									*Date, *ut, recalc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	*Xin	Array of x GSE coordinates (R_E)
 * 		double	*Yin	Array of y GSE coordinates (R_E)
 * 		double	*Zin	Array of z GSE coordinates (R_E)
 * 		int		n		Number of vectors to convert
 * 		double 	*Vx		Array of x-component of SW velocity in km/s
 * 		double 	*Vy		Array of y-component of SW velocity in km/s
 * 		double 	*Vz		Array of z-component of SW velocity in km/s
 * 		int 	*Date	Array of dates, format yyyymmdd
 * 		float 	*ut		Array of times in hours
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSM coordinate (R_E)
 * 		double	*Yout	y GSM coordinate (R_E)
 * 		double	*Zout	z GSM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Converts Dates into years and day numbers
 * 		[2] Converts UT hours to hours, minutes and seconds
 * 		[3] Finds appropriate SW velocity vectors if none are defined
 * 		[4] Calls GSEtoGSM wrapper to convert coordinates
 * 
 * ********************************************************************/
void GSEtoGSMUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {
	
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GSEtoGSM(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}

/***********************************************************************
 * NAME : 			void GSMtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to GSE coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to GSE coordinates
 * 
 * ********************************************************************/
void GSMtoGSE(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc,
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);

	return;
}

void GSMtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {
						
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	

	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GSMtoGSE(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	
}

/***********************************************************************
 * NAME : 			void GSMtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSM to SM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSM coordinate (R_E)
 * 		double	Yin		y GSM coordinate (R_E)
 * 		double	Zin		z GSM coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSM to SM coordinates
 * 
 * ********************************************************************/
void GSMtoSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);

	return;
}

void GSMtoSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz,
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GSMtoSM(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}	
}

/***********************************************************************
 * NAME : 			void SMtoGSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSM coordinate (R_E)
 * 		double	*Yout	y GSM coordinate (R_E)
 * 		double	*Zout	z GSM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to GSM coordinates
 * 
 * ********************************************************************/
void SMtoGSM(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int dirp = 1, dirn = -1;


	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	
	/* do the coordinate conversion */
	smgsw_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void SMtoGSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz, 
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		SMtoGSM(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	
}

/***********************************************************************
 * NAME : 			void GSEtoSM(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to SM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSE coordinate (R_E)
 * 		double	Yin		y GSE coordinate (R_E)
 * 		double	Zin		z GSE coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x SM coordinate (R_E)
 * 		double	*Yout	y SM coordinate (R_E)
 * 		double	*Zout	z SM coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GSM coordinates
 * 		[3] Calls FORTRAN code to convert from GSM to SM coordinates
 * 
 * ********************************************************************/
void GSEtoSM(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&X,&Y,&Z,&Xin,&Yin,&Zin,&dirn);
	smgsw_08_(Xout,Yout,Zout,&X,&Y,&Z,&dirn);
	
	return;
}

void GSEtoSMUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz, 
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GSEtoSM(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}	
}

/***********************************************************************
 * NAME : 			void GSEtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to MAG coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GSE coordinate (R_E)
 * 		double	Yin		y GSE coordinate (R_E)
 * 		double	Zin		z GSE coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GSE to GSM coordinates
 * 		[3] Calls FORTRAN code to convert from GSM to SM coordinates
 * 		[4] Calls FORTRAN code to convert from SM to MAG coordinates
 * 
 * ********************************************************************/
void GSEtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z,Xt,Yt,Zt;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	gswgse_08_(&X,&Y,&Z,&Xin,&Yin,&Zin,&dirn);
	smgsw_08_(&Xt,&Yt,&Zt,&X,&Y,&Z,&dirn);
	magsm_08_(Xout,Yout,Zout,&Xt,&Yt,&Zt,&dirn);
	
	return;
}

void GSEtoMAGUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut,
					double *Xout, double *Yout, double *Zout) {
						
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GSEtoMAG(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}

/***********************************************************************
 * NAME : 			void SMtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GSE to GSM coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x SM coordinate (R_E)
 * 		double	Yin		y SM coordinate (R_E)
 * 		double	Zin		z SM coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to GSM coordinates
 * 		[3] Calls FORTRAN code to convert from GSM to GSE coordinates
 * 
 * ********************************************************************/
void SMtoGSE(	double Xin, double Yin, double Zin,
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	smgsw_08_(&Xin,&Yin,&Zin,&X,&Y,&Z,&dirp);
	gswgse_08_(&X,&Y,&Z,Xout,Yout,Zout,&dirp);

	return;
}

void SMtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
				double *Vx, double *Vy, double *Vz,
				int *Date, float *ut, 
				double *Xout, double *Yout, double *Zout) {
					
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		SMtoGSE(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	
}

/***********************************************************************
 * NAME : 			void MAGtoGSE(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to GSE coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GSE coordinate (R_E)
 * 		double	*Yout	y GSE coordinate (R_E)
 * 		double	*Zout	z GSE coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to SM coordinates
 * 		[3] Calls FORTRAN code to convert from SM to GSM coordinates
 * 		[4] Calls FORTRAN code to convert from GSM to GSE coordinates
 * 
 * ********************************************************************/
void MAGtoGSE(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	double X,Y,Z,Xt,Yt,Zt;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	magsm_08_(&Xin,&Yin,&Zin,&Xt,&Yt,&Zt,&dirp);
	smgsw_08_(&Xt,&Yt,&Zt,&X,&Y,&Z,&dirp);
	gswgse_08_(&X,&Y,&Z,Xout,Yout,Zout,&dirp);	
	
	return;
}

void MAGtoGSEUT(	double *Xin, double *Yin, double *Zin, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {
						
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MAGtoGSE(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	
}


/***********************************************************************
 * NAME : 			void MLONtoMLT(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MLON to MLT coordinates
 * 
 * INPUTS : 
 * 		double	MLon	Magnetic longitude (degrees)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*MLT	Magnetic Local Time (hours)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to SM coordinates
 * 
 * ********************************************************************/
void MLONtoMLT(	double MLon, double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, double *MLT) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	X0 = cos(MLon*M_PI/180.0);
	Y0 = sin(MLon*M_PI/180.0);
	Z0 = 0.0;
	magsm_08_(&X0,&Y0,&Z0,&X1,&Y1,&Z1,&dirp);
	//smgsw_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirp);
	//MLT[0] =  atan2f(-Y0,-X0)*180.0/(M_PI*15.0);
	MLT[0] =  fmod(atan2f(-Y1,-X1)*180.0/(M_PI*15.0) + 24.0,24.0);
	
	return;
}

/***********************************************************************
 * NAME : 			void MLTtoMLON(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MLT to MLON coordinates
 * 
 * INPUTS : 
 * 		double	MLT	Magnetic Local Time (hours)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*MLon	Magnetic longitude (degrees)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from SM to MAG coordinates
 * 
 * ********************************************************************/
void MLTtoMLON(	double MLT, double Vx, double Vy, double Vz, int recalc, 
				int Year, int DayNo, int Hr, int Mn, int Sc, double *MLon) {

	double X0,Y0,Z0,X1,Y1,Z1;
	int dirp = 1, dirn = -1;

	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	X0 = cos(MLT*M_PI/12.0);
	Y0 = sin(MLT*M_PI/12.0);
	Z0 = 0.0;
	//smgsw_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirn);
	magsm_08_(&X1,&Y1,&Z1,&X0,&Y0,&Z0,&dirn);
	//MLon[0] =  atan2f(-Y0,-X0)*180.0/(M_PI);
	MLon[0] =  atan2f(-Y1,-X1)*180.0/(M_PI);
	
	return;
}

void MLONtoMLTUT(	double *MLon, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, double *MLT) {
						
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MLONtoMLT(	MLon[i],vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&MLT[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	

	
}

void MLTtoMLONUT(	double *MLT, int n,
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, double *MLon) {
						
	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MLTtoMLON(	MLT[i],vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&MLon[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
	
}


/***********************************************************************
 * NAME : 			void GEOtoMAG(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to MAG coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x MAG coordinate (R_E)
 * 		double	*Yout	y MAG coordinate (R_E)
 * 		double	*Zout	z MAG coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEO to MAG coordinates
 * 
 * ********************************************************************/
void GEOtoMAG(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geomag_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEOtoMAG_LL(	double Lon, double Lat, 
					double Vx, double Vy, double Vz, int recalc,
					int Year, int DayNo, int Hr, int Mn, int Sc, 
					double *MLon, double *MLat) {

	double X0,Y0,Z0,X1,Y1,Z1;

	/* convert to cartesian */
	X0 = cos(Lon*M_PI/180.0)*cos(Lat*M_PI/180.0);
	Y0 = sin(Lon*M_PI/180.0)*cos(Lat*M_PI/180.0);
	Z0 = sin(Lat*M_PI/180.0);
	
	/* convert coordinates */
	GEOtoMAG(	X0,Y0,Z0,Vx,Vy,Vz,recalc,
				DayNo,Year,Hr,Mn,Sc,
				&X1,&Y1,&Z1);
	
	/* convert back to lat and lon*/			
	MLon[0] = atan2f(Y1,X1)*180.0/M_PI;
	MLat[0] = asin(Z1)*180.0/M_PI;
	
	return;
}


void GEOtoMAGUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GEOtoMAG(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}


void GEOtoMAGUT_LL(	double *Lon, double *Lat, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, 
					double *MLon, double *MLat) {

	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GEOtoMAG_LL(	Lon[i],Lat[i],
						vx,vy,vz,recalc,
						Year,DayNo,Hr,Mn,Sc,
						&MLon[i],&MLat[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}

/***********************************************************************
 * NAME : 			void MAGtoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting MAG to GEO coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x MAG coordinate (R_E)
 * 		double	Yin		y MAG coordinate (R_E)
 * 		double	Zin		z MAG coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from MAG to GEO coordinates
 * 
 * ********************************************************************/
void MAGtoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geomag_08_(Xout,Yout,Zout,&Xin,&Yin,&Zin,&dirn);
	
	return;
}

void MAGtoGEO_LL(	double MLon, double MLat, 
					double Vx, double Vy, double Vz, int recalc,
					int Year, int DayNo, int Hr, int Mn, int Sc, 
					double *Lon, double *Lat) {

	double X0,Y0,Z0,X1,Y1,Z1;

	/* convert to cartesian */
	X0 = cos(MLon*M_PI/180.0)*cos(MLat*M_PI/180.0);
	Y0 = sin(MLon*M_PI/180.0)*cos(MLat*M_PI/180.0);
	Z0 = sin(MLat*M_PI/180.0);
	
	/* convert coordinates */
	MAGtoGEO(	X0,Y0,Z0,Vx,Vy,Vz,recalc,
				DayNo,Year,Hr,Mn,Sc,
				&X1,&Y1,&Z1);
	
	/* convert back to lat and lon*/			
	Lon[0] = atan2f(Y1,X1)*180.0/M_PI;
	Lat[0] = asin(Z1)*180.0/M_PI;
	
	return;
}



void MAGtoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MAGtoGEO(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}


void MAGtoGEOUT_LL(	double *MLon, double *MLat, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date,float *ut, 
					double *Lon, double *Lat) {

	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		MAGtoGEO_LL(	MLon[i],MLat[i],
						vx,vy,vz,recalc,
						Year,DayNo,Hr,Mn,Sc,
						&Lon[i],&Lat[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}






/***********************************************************************
 * NAME : 			void GEItoGEO(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEI to GEO coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEI coordinate (R_E)
 * 		double	Yin		y GEI coordinate (R_E)
 * 		double	Zin		z GEI coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GEO coordinate (R_E)
 * 		double	*Yout	y GEO coordinate (R_E)
 * 		double	*Zout	z GEO coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEI to GEO coordinates
 * 
 * ********************************************************************/
void GEItoGEO(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEItoGEOUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GEItoGEO(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}



/***********************************************************************
 * NAME : 			void GEOtoGEI(	Xin, Yin, Zin, Vx, Vy, Vz, recalc,
 *									Year, DayNo, Hr, Mn, Sc, 
 *									*Xout, *Yout, *Zout)
 * 
 * DESCRIPTION : 	Wrapper for converting GEO to GEI coordinates
 * 
 * INPUTS : 
 * 		double	Xin		x GEO coordinate (R_E)
 * 		double	Yin		y GEO coordinate (R_E)
 * 		double	Zin		z GEO coordinate (R_E)
 * 		double 	Vx		x-component of SW velocity in km/s
 * 		double 	Vy		y-component of SW velocity in km/s
 * 		double 	Vz		z-component of SW velocity in km/s
 * 		int		recalc	Set to one ro call recalc_08_
 * 		int		Year	Year
 * 		int		DayNo	Day number for that year
 * 		int		Hr		Hours
 * 		int		Mn		Minutes
 * 		int		Sc		Seconds
 * 
 * OUTPUTS : 
 * 		double	*Xout	x GEI coordinate (R_E)
 * 		double	*Yout	y GEI coordinate (R_E)
 * 		double	*Zout	z GEI coordinate (R_E)
 * 
 * RETURNS : 
 * 		void
 * 
 * PROCESS : 
 * 		[1] Calls recalc_08_ function in FORTRAN (if recalc != 0)
 * 		[2] Calls FORTRAN code to convert from GEO to GEI coordinates
 * 
 * ********************************************************************/
void GEOtoGEI(	double Xin, double Yin, double Zin, 
				double Vx, double Vy, double Vz, int recalc,
				int Year, int DayNo, int Hr, int Mn, int Sc, 
				double *Xout, double *Yout, double *Zout) {

	int i;
	int dirp = 1, dirn = -1;
	
	/*Set up geopack arrays using recalc*/
	if (recalc) {
		recalc_08_(&Year,&DayNo,&Hr,&Mn,&Sc, &Vx, &Vy, &Vz);
	}
	
	/* do the coordinate conversion */
	geigeo_08_(&Xin,&Yin,&Zin,Xout,Yout,Zout,&dirp);
	
	return;
}

void GEOtoGEIUT(	double *Xin, double *Yin, double *Zin, int n, 
					double *Vx, double *Vy, double *Vz, 
					int *Date, float *ut, 
					double *Xout, double *Yout, double *Zout) {

	int Year, DayNo, Hr, Mn, Sc;
	int pDate = -1; /* previous date */
	float put = -1.0; /* previous time */
	int i, recalc;
	double vx, vy, vz, vxp, vyp, vzp; /* current and previous velocity*/
	
	
	/* loop through each date/time/position */
	for (i=0;i<n;i++) {

		recalc = 0;
		if ((Date[i] != pDate) || (ut[i] != put)) {
			/*convert date into Year and DayNo*/
			DateToYearDayNo(Date[i],&Year,&DayNo);
			
			/*convert decimal UT to Hr, Mn, Sc*/
			DecUTToHHMMSS(ut[i],&Hr,&Mn,&Sc);
			
			/*set the flag to call recalc*/
			recalc = 1;
		}

		/*Get velocity if needed*/
		if (isnan(Vx[i])) { 
			GetSWVelocity(Date[i],ut[i],NULL,&vx,&vy,&vz);
		} else {
			vx = Vx[i];
			vy = Vy[i];
			vz = Vz[i];
		}
		if ((vx != vxp) || (vy != vyp) || (vz != vzp)) {
			recalc = 1;
		}

		/* convert this vector */
		GEOtoGEI(	Xin[i],Yin[i],Zin[i],
					vx,vy,vz,recalc,
					Year,DayNo,Hr,Mn,Sc,
					&Xout[i],&Yout[i],&Zout[i]);
					
		/*note previous variables */
		pDate = Date[i];
		put = ut[i];
		vxp = vx;
		vyp = vy;
		vzp = vz;
		
	}
}

