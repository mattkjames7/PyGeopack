#include "Sun.h"


void Sun(int iyear, int iday, int ihour, int min, int isec, double *gst, double *slong, double *srasn, double *sdec, double *obliq) {
	/*****************************************************************************
	Uses the time and date to calculate the following quantities:

	gst: greenwich sidereal time
	slong: solar longitude
	srasn: right ascension of the Sun
	sdec: solar declination
	oblique: obliquity of the ecliptic

	*****************************************************************************/

	/* Convert between degrees and radians*/
	double  Rad = 180.0/M_PI;

	/* Time since start of day in days */
	double FDay = ((double) (ihour*3600 + min*60 + isec))/86400.0d;

	/* Days since noon on Jan 1st 1900 */
	double DJ = 365.0d * (iyear-1900) + (iyear-1901)/4 + iday -0.5d + FDay;

	/* Days to centuries*/
	double T = DJ/36525.0;

	/* Mean longitude of the Sun */
	double VL = fmod(279.696678+0.9856473354*DJ,360.0);

	/* Mean anomaly of the Sun*/
	double G = fmod(358.475845+0.985600267*DJ,360.0)/Rad;

	/* Calculate the obliquity of the ecliptic */
	*obliq =(23.45229f-0.0130125f*T)/Rad;

	/*Calculate the greenwich siderial time */
	*gst = fmod(279.690983+0.9856473354*DJ+360.0*FDay+180.0,360.0)/Rad;

	/* Calculate the solar longitude */
	*slong = fmod((VL+(1.91946f-0.004789f*T)*sin(G)+0.020094f*sin(2.0*G)),360.0)/Rad;

	double   Sob, Slp, SinD, CosD, SC;
	Sob = sin(Obliq);
	Slp = *slong - 9.924E-5;
	SinD = Sob*sin(Slp);
	CosD = sqrt(1.0f - SinD*SinD);
	SC = SinD/CosD;

	/* Solar declination */
	*sdec = atan(SC);

	/* Solar right ascension */
	*srasn = M_PI - atan2(cos(obliq)/Sob*SC,-cos(Slp)/CosD);

	return;

}
