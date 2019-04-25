#include "Recalc.h"

GPPar1 GP1;

void Recalc(int Year, int DayNo, int Hour, int Min, int Sec, double Vx, double Vy, double Vz) {
	/* Recalc initializes the geopack parameters stored in GP1 */
	
	
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
	
	/* Here, we calculate the GEI components of the unit vector S = ExGSE*/
	double S1,S2,S3,gst,slong,srasn,sdec;
	Sun(Year,DayNo,Hour,Min,Sec,&gst,&slong,&srasn,&sdec);
	S1 = cos(srasn) * cos(sdec);
	S2 = sin(srasn) * cos(sdec);
	S3 = sin(sdec);	


	/*calculate GEI components of the unit vector pointing northward, orthogonal to the ecliptic plane (EXGSE)*/
	double DJ, Obliq,Dz1,Dz2,Dz3,T;
	DJ = ((double) ((365*(Year - 1900) + (Year-1901)/4+DayNo)))-0.5f+((double) (Hour*3600 + Min*60 + Sec))/86400.0d;
	T = DJ/36525.0d;
	Obliq=(23.45229d-0.0130125d*T)/57.2957795d;
	Dz1 = 0.0d;
	Dz2 = -sin(Obliq);
	Dz3 = cos(Obliq);

	/*now for EYGSE (EZGSE x EXGSE)*/
	double Dy1, Dy2, Dy3;
	Dy1 = Dz2*S3 - Dz3*S2;
	Dy2 = Dz3*S1 - Dz1*S3;
	Dy3 = Dz1*S2 - Dz2*S1;
	
	/*calculating the GEI components of X = EXGSW - directed into the SW flow */
	double V, Dx1, Dx2, Dx3;
	V = sqrt(pow(Vx,2.0) + pow(Vy,2.0) + pow(Vz,2.0));
	Dx1 = -Vx/V;
	Dx2 = -Vy/V;
	Dx3 = -Vz/V;
	
	/*above is in GSE, now to convert to GEI*/
	double x1, x2, x3;
	x1 = Dx1*S1 + Dx2*Dy1 + Dx3*Dz1;
	x2 = Dx1*S2 + Dx2*Dy2 + Dx3*Dz2;
	x3 = Dx1*S3 + Dx2*Dy3 + Dx3*Dz3;
	
	/*calculate GEI components of unit vecotr DIP = EZ_SM = EZ_MAG*/
	GP1.CGST = cos(gst);
	GP1.SGST = sin(gst);
	
	double Dip1, Dip2, Dip3;
	Dip1 = GP1.STCL*GP1.CGST - GP1.STSL*GP1.SGST;
	Dip2 = GP1.STCL*GP1.SGST + GP1.STSL*GP1.CGST;
	Dip3 = GP1.CT0;
	
	
	/* find unit vector Y = EYGSW*/
	double y1, y2, y3, y;
	y1 = Dip2*x3 - Dip3*x2;
	y2 = Dip3*x1 - Dip1*x3;
	y3 = Dip1*x2 - Dip2*x1;
	y = sqrt(y1*y1 + y2*y2 + y3*y3);
	y1 = y1/y;
	y2 = y2/y;
	y3 = y3/y;	
	
	/* GEI components of Z = EZGSW = EXGSW x EYGSW*/
	double z1,z2,z3;
	z1 = x2*y3 - x3*y2;
	z2 = x3*y1 - x1*y3;
	z3 = x1*y2 - x2*y1;
	
	/* matrix elements fro GSE to GSW are:
	 E11=(EXGSE,EXGSW)  E12=(EXGSE,EYGSW)  E13=(EXGSE,EZGSW)
	 E21=(EYGSE,EXGSW)  E22=(EYGSE,EYGSW)  E23=(EYGSE,EZGSW)
	 E31=(EZGSE,EXGSW)  E32=(EZGSE,EYGSW)  E33=(EZGSE,EZGSW)*/
	
	GP1.E11 =  S1*x1 +  S2*x2 +  S3*x3;
	GP1.E12 =  S1*y1 +  S2*y2 +  S3*y3;
	GP1.E13 =  S1*z1 +  S2*z2 +  S3*z3;
	GP1.E21 = Dy1*x1 + Dy2*x2 + Dy3*x3;
	GP1.E22 = Dy1*y1 + Dy2*y2 + Dy3*y3;
	GP1.E23 = Dy1*z1 + Dy2*z2 + Dy3*z3;
	GP1.E31 = Dz1*x1 + Dz2*x2 + Dz3*x3;
	GP1.E32 = Dz1*y1 + Dz2*y2 + Dz3*y3;
	GP1.E33 = Dz1*z1 + Dz2*z2 + Dz3*z3;
	
	/*Dipole tilt in GSW system PSI = arcsin(DIP.EXGSW)*/
	GP1.SPS = Dip1*x1 + Dip2*x2 + Dip3*x3;
	GP1.CPS = sqrt(1.0-pow(GP1.SPS,2.0));
	GP1.PSI = asin(GP1.SPS);
	
	/*GEO to GSW matrix
	A11=(EXGEO,EXGSW), A12=(EYGEO,EXGSW), A13=(EZGEO,EXGSW),
	A21=(EXGEO,EYGSW), A22=(EYGEO,EYGSW), A23=(EZGEO,EYGSW),
	A31=(EXGEO,EZGSW), A32=(EYGEO,EZGSW), A33=(EZGEO,EZGSW),*/

	GP1.A11 =  x1*GP1.CGST + x2*GP1.SGST;
	GP1.A12 = -x1*GP1.SGST + x2*GP1.CGST;
	GP1.A13 =  x3; 
	GP1.A21 =  y1*GP1.CGST + y2*GP1.SGST;
	GP1.A22 = -y1*GP1.SGST + y2*GP1.CGST;
	GP1.A23 =  y3;
	GP1.A31 =  z1*GP1.CGST + z2*GP1.SGST;
	GP1.A32 = -z1*GP1.SGST + z2*GP1.CGST;
	GP1.A33 =  z3;	
	
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
	
	double EXMAGX,EXMAGY,EXMAGZ,EYMAGX,EYMAGY;
	EXMAGX = GP1.CT0*(GP1.CL0*GP1.CGST - GP1.SL0*GP1.SGST);
	EXMAGY = GP1.CT0*(GP1.CL0*GP1.SGST + GP1.SL0*GP1.CGST);
	EXMAGZ =-GP1.ST0;
	EYMAGX =-(GP1.SL0*GP1.CGST + GP1.CL0*GP1.SGST);
	EYMAGY =-(GP1.SL0*GP1.SGST - GP1.CL0*GP1.CGST);
	GP1.CFI = y1*EYMAGX + y2*EYMAGY;
	GP1.SFI = y1*EXMAGX + y2*EXMAGY + y3*EXMAGZ;
	

	return;

}
