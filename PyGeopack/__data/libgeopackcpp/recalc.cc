#include "recalc.h"


typedef struct RecalcGlobs {
	int Date;
	float ut;
	double Vx;
	double Vy;
	double Vz;
} RecalcGlobs;

static RecalcGlobs recalcglobs = {0,0.0,0.0,0.0,0.0};


bool Recalc(int Date, float ut, double Vx, double Vy, double Vz) {
	
	return Recalc(Date,ut,Vx,Vy,Vz,false);
}
	
bool Recalc(int Date, float ut, double Vx, double Vy, double Vz, bool force) {
	
	bool recalc;
	int Year, DyNo, Hr, Mn, Sc;
	double Ms, utd;
	
	if (force) {
		recalc = true;
	} else {
		recalc = (Date != recalcglobs.Date) || (ut != recalcglobs.ut) ||
				(Vx != recalcglobs.Vx) || (Vy != recalcglobs.Vy) || 
				(Vz != recalcglobs.Vz);
	}
	
	if (recalc) {
		/* update globals */
		recalcglobs.Date = Date;
		recalcglobs.ut = ut;
		recalcglobs.Vx = Vx;
		recalcglobs.Vy = Vy;
		recalcglobs.Vz = Vz;
		
		/*convert date into Year and DayNo*/
		DayNo(1,&Date,&Year,&DyNo);
			
		/*convert decimal UT to Hr, Mn, Sc*/
		utd = (double) ut;
		DectoHHMM(1,&utd,&Hr,&Mn,&Sc,&Ms);
		
		
		/* now call the fortran function */
		recalc_08_(&Year,&DyNo,&Hr,&Mn,&Sc,&Vx,&Vy,&Vz);
	}
	return recalc;
}
