#include "test.h"

int main(int argc, char *argv[]) {
	

	
	/* intialize the parameters for the model */
	//InitParams(argv[argc-1]);
	
	/* set up the initial tracing position etc. */
	int Date[] = {20120101};
	float ut[] = {12.0};
	double xin[] = {0.0};
	double yin[] = {10.0};
	double zin[] = {0.0};
	const char *Model = "T96";
	const char *CoordIn = "GSM";
	int n = 1;
	double Vx = -359.0;
	double Vy = 11.0;
	double Vz = -17.4;
	int iopt = 1;
	double *parmod = new double[10];
	int i;
	parmod[0] = 1.34;
	parmod[1] = -9.0;
	parmod[2] = 0.2;
	parmod[3] = -1.96;
	for (i=4;i<10;i++) {
		parmod[i] = 0.0;
	}
	
	/* create the trace object */
	printf("Creating trace object \n");
	Trace T;
	
	/* input stuff */
	printf("Input position and model \n");
	T.InputPos(n,xin,yin,zin,Date,ut,CoordIn,&Vx,&Vy,&Vz);
	T.SetModel(Model);
	
	printf("Setting parameters\n");
	T.SetModelParams(&iopt,&parmod);
	
	/* do the field traces */
	printf("Trace\n");
	T.TraceGSM();
	T.TraceGSE();
	T.TraceSM();
	
	/* calculate some stuff */
	printf("Calculating footprints etc. \n");
	T.CalculateTraceDist();
	T.CalculateTraceR();
	T.CalculateTraceFP();
	T.CalculateTraceRnorm();
	
	/* now for h alpha */
	printf("Setting alpha\n");
	int nalpha = 2;
	double alpha[] = {0.0,90.0} ;
	T.SetAlpha(nalpha,alpha,0.05);
	
	printf("Attempting to calculate h alpha\n");
	T.CalculateHalpha();
	
	/* free params */
	//FreeParams();
}
