#ifndef __IGRFParams_h__
#define __IGRFParams_h__
#include <stdio.h>
#include <stdlib.h>
#include "libgeopack.h"
using namespace std;



/* at the time of writing there are 25 sets of parameters, plus one set
 * of secular variation parameters (I believe these are a yearly change*/
extern IGRFP IGRFParams[25];
extern IGRFP IGRFCurr;

void ReadIGRFParameters(const char *FileName);
void SetIGRFParams(int Year, int DayNo);
#endif
