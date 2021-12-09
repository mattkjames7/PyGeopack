#ifndef __TSYGDATA_H__
#define __TSYGDATA_H__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../libdatetime/ContUT.h"
#include "../tools/BubbleArgSort.h"
#endif


class TsygData {
	public:
		TsygData(const char*);
		~TsygData();
		
		/* Parameters stored in the file */
		bool loaded_;
		int n_, *Date_, *Year_, *DayNo_, *Hr_, *Mn_, *IMFFlag_, *ISWFlag_;
		float *ut_;
		double *utc_, *Bx_, *By_, *Bz_, *Vx_, *Vy_, *Vz_, *Den_, *Temp_, *SymH_;
		double *Tilt_, *Pdyn_, *W1_, *W2_, *W3_, *W4_, *W5_, *W6_, *G1_, *G2_, *Kp_;
		
		/* use these to get parameters from the data */
		double InterpParam(double *x, int Date, float ut);
		void GetParameter(int n, int *Date, float *ut, 
							double *x, double *xin, double *xout);
		void GetSWVelocity(int n, int *Date, float *ut,
				double *Vxin, double *Vyin, double *Vzin,
				double *Vx, double *Vy, double *Vz);
		void GetModelParams(int n, const char *Model,
							double *Kp, double *Pdyn, double *SymH,
							double *By, double *Bz, 
							double *G1, double *G2,
							double *W1, double *W2, double *W3,
							double *W4, double *W5, double *W6,
							int *iopt, double **parmod);		
		void GetModelParams(int n, int *Date, float *ut, 
							const char *Model, int *iopt, double **parmod);
		void GetVx(int n, int *Date, float *ut, double *Vx);
		void GetVy(int n, int *Date, float *ut, double *Vy);
		void GetVz(int n, int *Date, float *ut, double *Vz);
		void InterpParam(int n, double *utc, double fillval, bool fill, double *xi, double *xo);
	
	private:
		/* these indices should be used to speed up interpolation,but
		 * may be changed */
		int nMonth_, *MonthInds_, minYr_, minMn_, maxYr_, maxMn_;
		double  *Monthutc_;
		
		/* These functions will read in data from the data file */
		bool _LoadFile(const char*);
		void _ReadInt(FILE*,int,int**);
		void _ReadFloat(FILE*,int,float**);
		void _ReadDouble(FILE*,int,double**);
	
		/* for interpolation of parameters */
		void PopulateMonthInds();
		int _MonthStartInd(int Date);
		int _GetIndex(double utc, int prevI);
		double _Interp(	double t,
						double t0, double x0,
						double t1, double x1,
						double fillval);
};


