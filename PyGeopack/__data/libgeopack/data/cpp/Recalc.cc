#include "Recalc.h"

GPPar1 GP1;

void Recalc(int Year, int DayNo, int Hour, int Min, int Sec, double Vx, double Vy, double Vz) {
	/* Recalc initializes the geopack parameters stored in GP1 
	 * All code here is based on that of Geopack2008.f
	 * Names have changed to try and make it a little more obvious what is
	 * happening, the function shoud still produce identical results (in theory)*/


	/*Start by calculating the IGRF parameters*/
	SetIGRFParams(Year,DayNo);

	/*now calculate the components of the unit vector EzMAG parallel to
	 * the geodipole axis*/
	double g10,g11,h11,sq,sqq,sqr;
	g10 = -IGRFCurr.g[1];
	g11 = IGRFCurr.g[2];
	h11 = IGRFCurr.h[2];

	sq = g11*g11 + h11*h11;
	sqq = sqrt(sq);
	sqr = sqrt(g10*g10 + sq);
	GP1.SL0 = -h11/sqq;
	GP1.CL0 = -g11/sqq;
	GP1.ST0 = sqq/sqr;
	GP1.CT0 = g10/sqr;
	GP1.STCL = GP1.ST0*GP1.CL0;
	GP1.STSL = GP1.ST0*GP1.SL0;
	GP1.CTSL = GP1.CT0*GP1.SL0;
	GP1.CTCL = GP1.CT0*GP1.CL0;

	/* Here, we calculate the GEI components of the unit vector S = ExGSE
	 * which is the x GSE vector expressed in GEI coords
	 * used to be called S1,S2,S3*/
	double xGSEx,xGSEy,xGSEz,gst,slong,srasn,sdec,obliq;
	Sun(Year,DayNo,Hour,Min,Sec,&gst,&slong,&srasn,&sdec,&obliq);
	xGSEx = cos(srasn) * cos(sdec);
	xGSEy = sin(srasn) * cos(sdec);
	xGSEz = sin(sdec);


	/*calculate GEI components of the unit vector pointing northward, 
	 * orthogonal to the ecliptic plane - in other words the z GSE
	 * vector expressed in GEI coords - used to be caled DZ1,DZ2,DZ3*/
	double zGSEx,zGSEy,zGSEz;
	zGSEx = 0.0d;
	zGSEy = -sin(obliq);
	zGSEz = cos(obliq);

	/*now for EYGSE (EZGSE x EXGSE) - the y GSE vector expressed in
	 * GEI coords - used to be DY1, DY2, DY3*/
	double yGSEx,yGSEy,yGSEz;
	yGSEx = zGSEy*xGSEz - zGSEz*xGSEy;
	yGSEy = zGSEz*xGSEx - zGSEx*xGSEz;
	yGSEz = zGSEx*xGSEy - zGSEy*xGSEx;

	/*calculating the GEI components of X = EXGSW - directed into the SW flow */
	double V;
	V = sqrt(pow(Vx,2.0) + pow(Vy,2.0) + pow(Vz,2.0));

	/* Calculate the x GSW unit vector in GEI coords - used to used DX1,DX2,DX3,X1,X2,X3*/
	double xGSWx,xGSWy,xGSWz;
	xGSWx = -(Vx*xGSEx + Vy*yGSEx + Vz*zGSEx)/V;
	xGSWy = -(Vx*xGSEy + Vy*yGSEy + Vz*zGSEy)/V;
	xGSWz = -(Vx*xGSEz + Vy*yGSEz + Vz*zGSEz)/V;

	/*calculate GEI components of unit vector DIP = EZ_SM = EZ_MAG
	 * aligned with the geodipole (pointing northwards out of the ecliptic)
	 * previously Dip1,Dip2,Dip3 - this is the z SM unit vector in GEI*/
	GP1.CGST = cos(gst);
	GP1.SGST = sin(gst);

	double zSMx,zSMy,zSMz;
	zSMx = GP1.STCL*GP1.CGST - GP1.STSL*GP1.SGST;
	zSMy = GP1.STCL*GP1.SGST + GP1.STSL*GP1.CGST;
	zSMz = GP1.CT0;


	/* find unit vector Y = EYGSW*/
	double yGSWx, yGSWy, yGSWz, y;
	yGSWx = zSMy*xGSWz - zSMz*xGSWy;
	yGSWy = zSMz*xGSWx - zSMx*xGSWz;
	yGSWz = zSM1*xGSWy - zSMy*xGSWx;
	y = sqrt(yGSWx*yGSWx + yGSWy*yGSWy + yGSWz*yGSWz);
	yGSWx = yGSWx/y;
	yGSWy = yGSWy/y;
	yGSWz = yGSWz/y;

	/* GEI components of Z = EZGSW = EXGSW x EYGSW
	 * the z GSW unit vector in GEI, previously z1,z2,z3*/
	double zGSWx,zGSWy,zGSWz;
	zGSWx = xGSWy*yGSWz - xGSWz*yGSWy;
	zGSWy = xGSWz*yGSWx - xGSWx*yGSWz;
	zGSWz = xGSWx*yGSWy - xGSWy*yGSWx;

	/* matrix elements from GSE to GSW are:
	 E11=(EXGSE,EXGSW)  E12=(EXGSE,EYGSW)  E13=(EXGSE,EZGSW)
	 E21=(EYGSE,EXGSW)  E22=(EYGSE,EYGSW)  E23=(EYGSE,EZGSW)
	 E31=(EZGSE,EXGSW)  E32=(EZGSE,EYGSW)  E33=(EZGSE,EZGSW)*/

	GP1.E11 = xGSEx*xGSWx + xGSEy*xGSWy + xGSEz*xGSWz;
	GP1.E12 = xGSEx*yGSWx + xGSEy*yGSWy + xGSEz*yGSWz;
	GP1.E13 = xGSEx*zGSWx + xGSEy*zGSWy + xGSEz*zGSWz;
	GP1.E21 = yGSEx*xGSWx + yGSEy*xGSWy + yGSEz*xGSWz;
	GP1.E22 = yGSEx*yGSWx + yGSEy*yGSWy + yGSEz*yGSWz;
	GP1.E23 = yGSEx*zGSWx + yGSEy*zGSWy + yGSEz*zGSWz;
	GP1.E31 = zGSEx*xGSWx + zGSEy*xGSWy + zGSEz*xGSWz;
	GP1.E32 = zGSEx*yGSWx + zGSEy*yGSWy + zGSEz*yGSWz;
	GP1.E33 = zGSEx*zGSWx + zGSEy*zGSWy + zGSEz*zGSWz;

	/*Dipole tilt in GSW system PSI = arcsin(DIP.EXGSW)*/
	GP1.SPS = zSMx*xGSWx + zSMy*xGSWy + zSMz*xGSWz;
	GP1.CPS = sqrt(1.0-pow(GP1.SPS,2.0));
	GP1.PSI = asin(GP1.SPS);

	/*GEO to GSW matrix
	A11=(EXGEO,EXGSW), A12=(EYGEO,EXGSW), A13=(EZGEO,EXGSW),
	A21=(EXGEO,EYGSW), A22=(EYGEO,EYGSW), A23=(EZGEO,EYGSW),
	A31=(EXGEO,EZGSW), A32=(EYGEO,EZGSW), A33=(EZGEO,EZGSW),*/

	GP1.A11 =  xGSWx*GP1.CGST + xGSWy*GP1.SGST;
	GP1.A12 = -xGSWx*GP1.SGST + xGSWy*GP1.CGST;
	GP1.A13 =  xGSWz;
	GP1.A21 =  yGSWx*GP1.CGST + yGSWy*GP1.SGST;
	GP1.A22 = -yGSWx*GP1.SGST + yGSWy*GP1.CGST;
	GP1.A23 =  yGSWz;
	GP1.A31 =  zGSWx*GP1.CGST + zGSWy*GP1.SGST;
	GP1.A32 = -zGSWx*GP1.SGST + zGSWy*GP1.CGST;
	GP1.A33 =  zGSWz;

	/*
	NOW CALCULATE ELEMENTS OF THE MATRIX MAG TO SM (ONE ROTATION ABOUT THE GEODIPOLE AXIS);
	   THEY ARE FOUND AS THE SCALAR PRODUCTS: CFI=GM22=(EYSM,EYMAG)=(EYGSW,EYMAG),
	                                          SFI=GM23=(EYSM,EXMAG)=(EYGSW,EXMAG),
	    DERIVED AS FOLLOWS:

	 IN GEO, THE VECTORS EXMAG AND EYMAG HAVE THE COMPONENTS (CT0*CL0,CT0*SL0,-ST0)
	  AND (-SL0,CL0,0), RESPECTIVELY.    HENCE, IN GEI THEIR COMPONENTS ARE:
	  EXMAG:    CT0*CL0*COS(GST)-CT0*SL0*SIN(GST)
	            CT0*CL0*SIN(GST)+CT0*SL0*COS(GST)
	            -ST0
	  EYMAG:    -SL0*COS(GST)-CL0*SIN(GST)
	            -SL0*SIN(GST)+CL0*COS(GST)
	             0
	  NOW, NOTE THAT GEI COMPONENTS OF EYSM=EYGSW WERE FOUND ABOVE AS Y1, Y2, AND Y3,
	  AND WE ONLY HAVE TO COMBINE THESE QUANTITIES INTO SCALAR PRODUCTS:*/

	double xMAGx,xMAGy,xMAGz,yMAGx,yMAGy;
	xMAGx = GP1.CT0*(GP1.CL0*GP1.CGST - GP1.SL0*GP1.SGST);
	xMAGy = GP1.CT0*(GP1.CL0*GP1.SGST + GP1.SL0*GP1.CGST);
	xMAGz =-GP1.ST0;
	yMAGx =-(GP1.SL0*GP1.CGST + GP1.CL0*GP1.SGST);
	yMAGy =-(GP1.SL0*GP1.SGST - GP1.CL0*GP1.CGST);
	GP1.CFI = yGSWx*yMAGx + yGSWy*yMAGy;
	GP1.SFI = yGSWx*xMAGx + yGSWy*xMAGy + yGSWz*xMAGz;


	return;

}
