#ifndef __T96_h__
#define __T96_h__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

using namespace std;

typedef struct{
	double Rh;
	double Dr;
} RhDr;

typedef struct{
	double dx;
	double ScaleIn;
	double ScaleOut;
} Dx1;

typedef struct{
	double Tilt;
	double XCentre[2];
	double Radius[2];
	double DipX;
	double DipY;
} LoopDip1;

typedef struct{
	double xx[12];
	double yy[12];
} Coord11;

typedef struct{
	double xx[14];
	double yy[14];
	double zz[14];
} Coord21;


typedef struct{
	double cpss;
	double spss;
	double dpsrr;
	double rps;
	double warp;
	double d;
	double xs;
	double zs;
	double dxsx;
	double dxsy;
	double dxsz;
	double dzsx;
	double dzsy;
	double dzsz;
	double dzetas;
	double ddzetadx;
	double ddzetady;
	double ddzetadz;
	double zsww;
} Warp;

extern "C" {
	void T96(int Iopt, double *ParMod, double Ps, double x, double y, double z, double *Bx, double *By, double *Bz);
}
void DipShld(double Ps, double x, double y, double z, double *Bx, double *By, double *Bz);
void CylHarm(double *A, double x, double y, double z, double *Bx, double *By, double *Bz);
void CylHar1(double *A, double x, double y, double z, double *Bx, double *By, double *Bz);
double Bes(double x, int k);
double Bes0(double x);
double Bes1(double x);
void Intercon(double x, double y, double z, double *Bx, double *By, double *Bz);
void TailRC96(double SPS, double x, double y, double z, double *BxRC, double *ByRC, double *BzRC, double *BxT2, double *ByT2, double *BzT2, double *BxT3, double *ByT3, double *BzT3);
void RingCurr96(double x, double y, double z, double *Bx, double *By, double *Bz);
void TailDisk(double x, double y, double z, double *Bx, double *By, double *Bz);
void Tail87(double x, double z, double *Bx, double *Bz);
void ShlCar3x3(double *A, double x, double y, double z, double SPS, double *Bx, double *By, double *Bz);
void Birk1Tot(double Ps, double x, double y, double z, double *Bx, double *By, double *Bz);
void DipLoop1(double *Xi, double D[3][26]);
void Circle(double x, double y, double z, double Rl, double *Bx, double *By, double *Bz);
void CrossLP(double x, double y, double z, double *Bx, double *By, double *Bz, double Xc, double Rl, double Al);
void DipXYZ(double x, double y, double z, double *Bxx, double *Byx, double *Bzx, double *Bxy, double *Byy, double *Bzy, double *Bxz, double *Byz, double *Bzz);
void ConDip1(double *Xi, double D[3][79]);
void Birk1Shld(double ps, double x, double y, double z, double *Bx, double *By, double *Bz);
void Birk2Tot(double ps, double x, double y, double z, double *Bx, double *By, double *Bz);
void Birk2Shl(double x, double y, double z, double ps, double *Bx, double *By, double *Bz);
void R2_Birk(double x, double y, double z, double ps, double *Bx, double *By, double *Bz);
void R2Inner(double x, double y, double z, double *Bx, double *By, double *Bz);
void BConic(double x, double y, double z, double *CBx, double *CBy, double *CBz, int Nmax);
void DipDistR(double x, double y, double z, double *Bx, double *By, double *Bz, int Mode);
void R2Outer(double x, double y, double z, double *Bx, double *By, double *Bz) ;
void Loops4(double x, double y, double z, double *Bx, double *By, double *Bz, double Xc, double Yc, double Zc, double R, double Theta, double Phi);
void R2Sheet(double x, double y, double z, double *Bx, double *By, double *Bz);
double XKSI(double x, double y, double z);
double Fexp(double S, double A);
double Fexp1(double S, double A);
double TKSI( double xksi, double xks0, double Dxksi);
void Dipole(double ps, double x, double y, double z, double *Bx, double *By, double *Bz);


#endif
