#include "testswitchmodel.h"



Trace GetT96Trace(int n, double x0, double y0, double z0, int Date, float ut) {
	
	Trace T;
	T.InputPos(n,&x0,&y0,&z0,&Date,&ut,"GSM");
	T.SetModel("T96");
	T.SetModelParams();
	T.TraceGSM();
	
	return T;
}

Trace GetT01Trace(int n, double x0, double y0, double z0, int Date, float ut) {
	
	Trace T;
	T.InputPos(n,&x0,&y0,&z0,&Date,&ut,"GSM");
	T.SetModel("T01");
	T.SetModelParams();
	T.TraceGSM();
	return T;
}

void Fill(int n, double *x, double f) {
	
	int i;
	for (i=0;i<n;i++) {
		x[i] = f;
	}
}

int main() {
	
	InitParams("/media/data1/Data/Testing/Geopack/TSdata.bin");
	
	/* create starting positions */
	int n = 1;
	double x0 = -4.0;
	double y0 = 0.0;
	double z0 = 0.0;
	int Date = 20140827;
	float ut = 10.25;
	int i;
	
	/* create output arrays */
	int nstep1;
	int nstep3;
	double **x1 = new double*[1];
	double **y1 = new double*[1];
	double **z1 = new double*[1];	
	double **x3 = new double*[1];
	double **y3 = new double*[1];
	double **z3 = new double*[1];
	x1[0] = new double[1000];
	y1[0] = new double[1000];
	z1[0] = new double[1000];	
	x3[0] = new double[1000];
	y3[0] = new double[1000];
	z3[0] = new double[1000];

	/* fill with nans */
	Fill(1000,x1[0],NAN);
	Fill(1000,y1[0],NAN);
	Fill(1000,z1[0],NAN);
	Fill(1000,x3[0],NAN);
	Fill(1000,y3[0],NAN);
	Fill(1000,z3[0],NAN);	

	/* get the three traces in order */
	printf("Trace 1: T01\n");
	Trace T1 = GetT01Trace(n,x0,y0,z0,Date,ut);
	printf("Trace 2: T96\n");
	Trace T2 = GetT96Trace(n,x0,y0,z0,Date,ut);
	printf("Trace 3: T01\n");
	Trace T3 = GetT01Trace(n,x0,y0,z0,Date,ut);

	/* get the x-coordinate along the field line */
	T1.GetTraceGSM(x1,y1,z1);
	T3.GetTraceGSM(x3,y3,z3);
	T1.GetTraceNstep(&nstep1);
	T3.GetTraceNstep(&nstep3);

	/* check if they are the same */
	bool same = true;
	if (nstep1 != nstep3) {
		same = false;
	} else {
		for (i=0;i<nstep1;i++) {
			if (x1[i] != x3[i]) {
				same = false;
				break;
			}
		}
	}

	if (same) {
		printf("Arrays match\n");
	} else {
		printf("Arrays differ\n");
	}
}
