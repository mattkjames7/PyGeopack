#ifndef __TESTSWITCHMODEL_H__
#define __TESTSWITCHMODEL_H__
#include <stdio.h>
#include <stdlib.h>
#include "../tracing/trace.h"
#include "../modelparams/modelparams.h"

#endif

Trace GetT96Trace(int n, double x0, double y0, double z0, int Date, float ut);
Trace GetT01Trace(int n, double x0, double y0, double z0, int Date, float ut);
void Fill(int n, double *x, double f);
