#ifndef __LATLONLT_H__
#define __LATLONLT_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../fortran/geopack.h"
#include "../tools/carttospherical.h"
#endif
using namespace std;


void MagLatLonLT(	double x, double y, double z, 
					double *lat, double *lon, double *lt);
void GeoLatLonLT(	float ut, double x, double y, double z, 
					double *lat, double *lon, double *lt);

