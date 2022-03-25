#ifndef __TRACE_H__
#define __TRACE_H__
#include <stdio.h>
#include <stdlib.h>
#include "../modelparams/modelparams.h"
#include "../ConvCoords.h"
#include "../fortran/geopack.h"
#include "../recalc.h"
#include "tracefieldline.h"
#include "../modelfield.h"
#include "fieldlinedist.h"
#include "fieldliner.h"
#include "fieldlinernorm.h"
#include "tracefootprints.h"
#include "converttracecoords.h"
#include "../withinmp.h"
//#include "../matrix/matrixarray.h"
#include "interptraceclosestpos.h"
//#include "tracerotationmatrices.h"



/***********************************************************************
 * This object will store a bunch of field traces within it.
 * 
 * It will have the ability to either allocate and store field vectors
 * and positions, or to accept pointers which can be created externally
 * (e.g. inside Python)
 * 
 * There will be optional member functions which obtain things like 
 * footprints and h_alphas.
 * 
 * The basic trace will be in GSM/GSW.
 * 
 * Other coordinate systems will be calculated as needed.
 * 
 * ********************************************************************/
class Trace {
	
	public:
		/* initialize the object */
		Trace();
		
		/* delete the object */
		~Trace();
		
		/* copy constructor */
	//	Trace(const Trace &);
		
		/* this will take in the input positions where the traces start*/
		void InputPos(int,double*,double*,double*,int*,float*,const char*, double*, double*, double*);
		void InputPos(int,double*,double*,double*,int*,float*,const char*);
		
		/* set model parameters */
		void SetModelParams(int*, double**);
		void SetModelParams();
		void SetModel(const char *);
		
		/* set the trace configuration */
		void SetTraceCFG(double,int,double,bool,int);
		void SetTraceCFG();
		
		/* polarization stuff */
		void SetAlpha(int,double*,double);

			
		/* trace function to do basic trace in GSW coords */
		void TraceGSM(int*,double**,double**,double**,double**,double**,double**);
		void TraceGSM(int*);
		void TraceGSM();
		
		
		/* these will convert to other coords */
		void TraceGSE(double**,double**,double**,double**,double**,double**);
		void TraceGSE();
		void TraceSM(double**,double**,double**,double**,double**,double**);
		void TraceSM();
	
		/* calculate trace distance,R,Rnorm */
		void CalculateTraceDist(double**);
		void CalculateTraceDist();
		void _CalculateTraceDist();
		void CalculateTraceR(double**);
		void CalculateTraceR();
		void _CalculateTraceR();
		void CalculateTraceRnorm(double**);
		void CalculateTraceRnorm();
		void _CalculateTraceRnorm();
	
		/* Calculate footprints */
		void CalculateTraceFP(double**);
		void CalculateTraceFP();
		void _CalculateTraceFP();
		
		/* calculate halpha */
		void CalculateHalpha();
		void CalculateHalpha(double*);
		void CalculateHalpha(double***);
		void CalculateHalpha(double*,double***);
	
		/* return things*/
		void GetTraceNstep(int*);
		void GetTraceGSM(double**,double**,double**);
		void GetTraceGSM(double**,double**,double**,double**,double**,double**);
		void GetTraceGSE(double**,double**,double**);
		void GetTraceGSE(double**,double**,double**,double**,double**,double**);
		void GetTraceSM(double**,double**,double**);
		void GetTraceSM(double**,double**,double**,double**,double**,double**);
		void GetTraceDist(double**);
		void GetTraceR(double**);
		void GetTraceRnorm(double**);
		void GetTraceFootprints(double**);
		void GetTraceHalpha(double*);	/* python will use this */
		void GetTraceHalpha(double***); /* no idea how to link this to python*/
		
		Trace TracePosition(int,double,double,double);
	

		/* input coords */
		int n_;
		double *x0_, *y0_, *z0_;  
		int *Date_;
		float *ut_;

		/* SW velocity */
		double *Vx_, *Vy_, *Vz_;

		/* trace params */
		int MaxLen_;
		double DSMax_;
		bool Verbose_;
		double alt_;
		int TraceDir_;
		
		/* model params */
		int *iopt_;
		double **parmod_;

		/* trace coords */
		int *nstep_;
		bool *inMP_;
		double **xgsm_, **ygsm_, **zgsm_;
		double **xgse_, **ygse_, **zgse_;
		double **xsm_, **ysm_, **zsm_;
	
		/* trace fields */
		double **bxgsm_, **bygsm_, **bzgsm_;
		double **bxgse_, **bygse_, **bzgse_;
		double **bxsm_, **bysm_, **bzsm_;

		/* trace end points */
		double *xfn_, *yfn_, *zfn_;
		double *xfs_, *yfs_, *zfs_;
		double *xfe_, *yfe_, *zfe_;

	private:
		/* booleans to tell the object what has been done */
		bool inputPos_;
		bool inputModelParams_,allocModelParams_;
		bool traceConfigured_;
		bool allocV_;
		bool tracedGSM_,allocGSM_;
		bool tracedGSE_,allocGSE_;
		bool tracedSM_,allocSM_;
		bool allocEndpoints_;
		bool hasFootprints_,allocFootprints_;
		bool hasDist_,allocDist_;
		bool hasR_,allocR_;
		bool hasRnorm_,allocRnorm_;
		bool hasHalpha_,allocHalpha_, allocHalpha3D_;
		bool setModel_;
		bool allocNstep_;
		bool allocAlpha_;
		bool allocEqFP_;
		bool allocMP_;

		

	
		/* field length, R, Rnorm, Halpha, Footprints */
		int nalpha_;
		double *alpha0_, *alpha1_;
		double Delta_;
		double **S_;
		double **R_;
		double **Rnorm_;
		double *Halpha_;
		double ***Halpha3D_;
		double **FP_;
		
		/* model */
		const char *Model_;
		ModelFuncPtr ModelFunc_;
	
		/* hidden trace functions */
		void _TraceGSM();
		void _TraceGSE();
		void _TraceSM();

		/* halpha functions */
		bool _CheckHalpha();
		void _CalculateHalpha();
		void _CalculateTraceHalpha(int,int,double*);
		void _CalculateHalphaStartPoints(int i, int j,
							double *xe0, double *ye0, double *ze0,
							double *xe1, double *ye1, double *ze1);
};



#endif

