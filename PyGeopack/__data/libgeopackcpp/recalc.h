#ifndef __RECALC_H__
#define __RECALC_H__
#include <stdio.h>
#include <stdlib.h>
#include "fortran/geopack.h"
#include "libdatetime/DayNo.h"
#include "libdatetime/hhmm.h"
#endif
using namespace std;



bool Recalc(int Date, float ut, double Vx, double Vy, double Vz, bool force);
bool Recalc(int Date, float ut, double Vx, double Vy, double Vz);
