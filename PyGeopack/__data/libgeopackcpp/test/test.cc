#include "test.h"

int main(int argc, char *argv[]) {
	

	
	/* intialize the parameters for the model */
	InitParams(argv[argc-1]);
	
	/* set up the initial tracing position etc. */
	int Date[] = {20120101};
	float ut[] = {12.0};
	double xin[] = {5.0};
	double yin[] = {0.0};
	double zin[] = {0.0};
	const char *Model = "T96";
	const char *CoordIn = "GSM";
	int n = 1;
	
	/* create the trace object */
	printf("Creating trace object \n");
	Trace T;
	
	/* input stuff */
	printf("Input position and model \n");
	T.InputPos(n,xin,yin,zin,Date,ut,CoordIn);
	T.SetModel(Model);
	
	printf("Setting parameters\n");
	T.SetModelParams();
	
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
	int nalpha = 1;
	double alpha = 90.0 ;
	T.SetAlpha(nalpha,&alpha,0.1);
	
	printf("Attempting to calculate h alpha\n");
	T.CalculateHalpha();
	
	/* free params */
	FreeParams();
}
