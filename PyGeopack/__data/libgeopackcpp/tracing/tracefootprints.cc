#include "tracefootprints.h"



void TraceFootprints(	int nstep, float ut, 
						double *x, double *y, double *z, 
						double *s, double *R, 
						double xfn, double yfn, double zfn, 
						double xfs, double yfs, double zfs, 
						double *xfe, double *yfe, double *zfe, 
						double alt, double *FP, int MaxLen, int TraceDir) {

    const double Re = 6371.2;
	double MaxR = (Re + alt)/Re + 0.01;
	double RFN, RFS;
	RFN = sqrt(pow(xfn,2.0) + pow(yfn,2.0) + pow(zfn,2.0));
	RFS = sqrt(pow(xfs,2.0) + pow(yfs,2.0) + pow(zfs,2.0));
	
	double MlatN,MlatS,GlatN,GlatS;
	double MlonN,MlonS,GlonN,GlonS;
	double MltN,MltS,MltE,GltN,GltS;
	double FlLen,Lshell;

	/* Calculate the lat, long and lt of the northern footprint*/
	if ((RFN <= MaxR) && (TraceDir >= 0)) {
		GeoLatLonLT(ut,xfn,yfn,zfn,&GlatN,&GlonN,&GltN);
		MagLatLonLT(xfn,yfn,zfn,&MlatN,&MlonN,&MltN);
	} else {
		MltN = NAN;
		MlatN = NAN;
		MlonN = NAN;
		GltN = NAN;
		GlatN = NAN;
		GlonN = NAN;			
	}

	/* Calculate the lat, long and lt of the southern footprint*/ 
	if ((RFS <= MaxR) && (TraceDir <= 0)) {
		GeoLatLonLT(ut,xfs,yfs,zfs,&GlatS,&GlonS,&GltS);
		MagLatLonLT(xfs,yfs,zfs,&MlatS,&MlonS,&MltS);
	} else {
		MltS = NAN;
		MlatS = NAN;
		MlonS = NAN;
		GltS = NAN;
		GlatS = NAN;
		GlonS = NAN;		
	}

	if ((RFN <= MaxR) && (RFS <= MaxR)) {
		/* Calculate the position of the equatorial footprint */
		GetMagEquatorFP(x,y,z,s,R,nstep,xfe,yfe,zfe,&Lshell,&MltE);
		FlLen = s[nstep-1];
	} else {
		/* Don't provide FlLen and Lshell and MltE if the field line is open*/
		Lshell = NAN;
		MltE = NAN;
		FlLen = NAN;
		xfe[0] = NAN;
		yfe[0] = NAN;
		zfe[0] = NAN;
		
	}
		
	/* place all the footprints in the output array*/
	FP[0] = GlatN;
	FP[1] = GlatS;
	FP[2] = MlatN;
	FP[3] = MlatS;
	FP[4] = GlonN;
	FP[5] = GlonS;
	FP[6] = MlonN;
	FP[7] = MlonS;
	FP[8] = GltN;
	FP[9] = GltS;
	FP[10] = MltN;
	FP[11] = MltS;
	FP[12] = Lshell;
	FP[13] = MltE;
	FP[14] = FlLen;
	
}

void TraceFootprintsSM(	int nstep, float ut, 
						double *xsm, double *ysm, double *zsm, 
						double *s, double *R, 
						double xfn, double yfn, double zfn, 
						double xfs, double yfs, double zfs, 
						double *xfe, double *yfe, double *zfe, 
						double alt, double *FP, int MaxLen, int TraceDir) {

    const double Re = 6371.2;
	double MaxR = (Re + alt)/Re + 0.01;
	double RFN, RFS;
	RFN = sqrt(pow(xfn,2.0) + pow(yfn,2.0) + pow(zfn,2.0));
	RFS = sqrt(pow(xfs,2.0) + pow(yfs,2.0) + pow(zfs,2.0));

	double MlatN,MlatS,GlatN,GlatS;
	double MlonN,MlonS,GlonN,GlonS;
	double MltN,MltS,MltE,GltN,GltS;
	double FlLen,Lshell;

	/* Calculate the lat, long and lt of the northern footprint*/
	if ((RFN <= MaxR) && (TraceDir >= 0)) {
		GeoLatLonLT(ut,xfn,yfn,zfn,&GlatN,&GlonN,&GltN);
		MagLatLonLT(xfn,yfn,zfn,&MlatN,&MlonN,&MltN);
	} else {
		MltN = NAN;
		MlatN = NAN;
		MlonN = NAN;
		GltN = NAN;
		GlatN = NAN;
		GlonN = NAN;			
	}

	/* Calculate the lat, long and lt of the southern footprint*/ 
	if ((RFS <= MaxR) && (TraceDir <= 0)) {
		GeoLatLonLT(ut,xfs,yfs,zfs,&GlatS,&GlonS,&GltS);
		MagLatLonLT(xfs,yfs,zfs,&MlatS,&MlonS,&MltS);
	} else {
		MltS = NAN;
		MlatS = NAN;
		MlonS = NAN;
		GltS = NAN;
		GlatS = NAN;
		GlonS = NAN;		
	}

	if ((RFN <= MaxR) && (RFS <= MaxR)) {
		/* Calculate the position of the equatorial footprint */
		GetMagEquatorFPSM(xsm,ysm,zsm,s,R,nstep,xfe,yfe,zfe,&Lshell,&MltE);
		FlLen = s[nstep-1];
	} else {
		/* Don't provide FlLen and Lshell and MltE if the field line is open*/
		Lshell = NAN;
		MltE = NAN;
		FlLen = NAN;
		xfe[0] = NAN;
		yfe[0] = NAN;
		zfe[0] = NAN;
		
	}

	/* place all the footprints in the output array*/
	FP[0] = GlatN;
	FP[1] = GlatS;
	FP[2] = MlatN;
	FP[3] = MlatS;
	FP[4] = GlonN;
	FP[5] = GlonS;
	FP[6] = MlonN;
	FP[7] = MlonS;
	FP[8] = GltN;
	FP[9] = GltS;
	FP[10] = MltN;
	FP[11] = MltS;
	FP[12] = Lshell;
	FP[13] = MltE;
	FP[14] = FlLen;

}
