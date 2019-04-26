#ifndef __Recalc_h__
#define __Recalc_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "IGRFParams.h"
#include "Sun.h"
using namespace std;



extern GPPar1 GP1;

extern "C" {
	void Recalc(int Year, int DayNo, int Hour, int Min, int Sec, double Vx, double Vy, double Vz);
}
	
#endif
