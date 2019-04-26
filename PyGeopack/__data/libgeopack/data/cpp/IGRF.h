#ifndef __IGRF_h__
#define __IGRF_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "libgeopack.h"
#include "ConvCoords.h"
using namespace std;

extern "C" {
	void IGRF_GEO(double R, double Theta, double Phi, double *Br, double *Btheta, double *Bphi);
	void IGRF_GSW(double XGSW, double YGSW, double ZGSW, double *HxGSW, double *HyGSW, double *HzGSW);
	void Dipole(double XGSW, double YGSW, double ZGSW, double *BxGSW, double *ByGSW, double *BzGSW);
}
#endif
