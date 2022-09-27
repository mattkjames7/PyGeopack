#include "latlonlt.h"


void MagLatLonLT(double x, double y, double z, double *lat, double *lon, double *lt) {
	int dirp = 1;
	int dirn = -1;
	double X1, Y1, Z1, X2, Y2, Z2;
	double r, theta, phi;
	
	/*convert GSW to SM*/
	smgsw_08_(&X1,&Y1,&Z1,&x,&y,&z,&dirn);
	
	/* convert SM to MAG */
	magsm_08_(&X2,&Y2,&Z2,&X1,&Y1,&Z1,&dirn);
	
	/* Calculate the spherical coordinate */
	CartToSpherical(X2,Y2,Z2,&r,&theta,&phi);	
	
	/* latitude */
	lat[0] = 90.0 - (theta*180.0/M_PI);
	
	/* longitude */
	lon[0] = phi*180.0/M_PI;
	
	/* local time */	
	lt[0] = fmod(atan2(-Y1,-X1)*12.0/M_PI + 24.0,24.0);
}

void GeoLatLonLT(float ut, double x, double y, double z, double *lat, double *lon, double *lt) {
	int dirp = 1;
	int dirn = -1;
	double X1, Y1, Z1;
	double r, theta, phi;
	
	/*convert GSW to SM*/
	geogsw_08_(&X1,&Y1,&Z1,&x,&y,&z,&dirn);

	/* Calculate the spherical coordinate */
	CartToSpherical(X1,Y1,Z1,&r,&theta,&phi);	
	
	/* latitude */
	lat[0] = 90.0 - (theta*180.0/M_PI);
	
	/* longitude */
	lon[0] = phi*180.0/M_PI;
	
	/* local time */	
	//lt[0] = fmod(atan2(-Y1,-X1)*12.0/M_PI + 24.0,24.0);
	lt[0] = fmod(ut + lon[0]/15.0 + 24.0,24.0);
}

