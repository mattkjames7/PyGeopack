#include "Sun.h"


void Sun(int iyear, int iday, int ihour, int min, int isec, double *gst, double *slong, double *srasn, double *sdec) {
	if (iyear < 1901 || iyear > 2099) {
		return;
	}
	
	double  Rad = 57.295779513, T, VL, G, Obliq, Sob, Slp, SinD, CosD, SC;
	double DJ, FDay;
	FDay = ((double) (ihour*3600 + min*60 + isec))/86400.0d;
	DJ = 365.0d * (iyear-1900) + (iyear-1901)/4 + iday -0.5d + FDay;
	T = DJ/36525.0;
	VL = fmod(279.696678f+0.9856473354f*DJ,360.0d);
	
	*gst = fmod(279.690983f+0.9856473354f*DJ+360.0f*FDay+180.d,360.0f)/Rad;
	G = fmod(358.475845f+0.985600267f*DJ,360.0f)/Rad;
	*slong = (VL+(1.91946f-0.004789f*T)*sinf(G)+0.020094f*sinf(2.*G))/Rad;
	if (*slong > 6.2831853f) {
		*slong = *slong-6.2831853f;
	}
	if (*slong < 0.0f) {
		*slong = *slong+6.2831853f;
	}
	Obliq =(23.45229f-0.0130125f*T)/Rad;
	Sob = sinf(Obliq);
	Slp = *slong - ((double) 9.924E-5);
	SinD = Sob*sinf(Slp);
	CosD = sqrtf(1.0f - SinD*SinD);
	SC = SinD/CosD;
	*sdec = atanf(SC);
	*srasn = 3.141592654f - atan2f(cosf(Obliq)/Sob*SC,-cosf(Slp)/CosD);
	//printf("SUN: %11.9f %10.9e\n",cosf(Obliq),Sob*SC);
	//printf("SUN: %10.9e %11.9f\n",cosf(Obliq)/Sob*SC,-cosf(Slp)/CosD);
	//printf("SUN: %11.9f %11.9f %11.9f %11.9f\n",*gst,*slong,*srasn,*sdec);
	return;
	
} 
