#ifndef __ModelField_h__
#define __ModelField_h__
#include <stdio.h>
#include <math.h>
#include "libgeopack.h"
#include "T96.h"
#include "ConvCoords.h"
#include "IGRF.h"
#endif


//CtypeStart
//PyFunc iiiiiiiiiooo
void ModelField(double *Xin, double *Yin, double *Zin, int n, int Date, double ut, const char *Model, int CoordIn, int CoordOut, double *Bx, double *By, double *Bz);
//CtypeStop
