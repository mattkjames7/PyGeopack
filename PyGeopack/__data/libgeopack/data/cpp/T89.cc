#include "T89.h"


/*
 * This is an attempt to rewrite the original Fortran code for the T89
 * model (Tsyganenko et al) in C++, hopefully making the code a little 
 * more transparent too.
 *  
 */
 
 /*list of 30 model parameters for 7 different Kp values*/
double _A01[] = {    -116.53,    -55.553,    -101.34,    -181.69,    -436.54,    -707.77,    -1190.4}; //Tail current
double _A02[] = {     -10719,     -13198,     -13480,     -12320,      -9001,    -4471.9,     2749.9}; //Tail current
double _A03[] = {     42.375,     60.647,     111.35,     173.79,     323.66,     432.81,     742.56}; //symmetric closure current
double _A04[] = {     59.753,     61.072,     12.386,    -96.664,    -410.08,    -435.51,    -1110.3}; //asymmetric closure current
double _A05[] = {     -11363,     -16064,     -24699,     -39051,     -50340,     -60400,     -77193}; //ring current
double _A06[] = {     1.7844,     2.2534,     2.6459,     3.2633,     3.9932,     4.6229,     7.6727}; //C_06 These are CF and Birkeland current field parameters
double _A07[] = {     30.268,     34.407,     38.948,     44.968,     58.524,     68.178,     102.05}; //C_07
double _A08[] = {  -0.035372,  -0.038887,   -0.03408,  -0.046377,  -0.038519,  -0.088245,  -0.096015}; //C_08
double _A09[] = {  -0.066832,  -0.094571,   -0.12404,   -0.16686,   -0.26822,   -0.21002,   -0.74507}; //C_09
double _A10[] = {   0.016456,   0.027154,   0.029702,   0.048298,   0.074528,    0.11846,    0.11214}; //C_10
double _A11[] = {    -1.3024,    -1.3901,    -1.4052,    -1.5473,    -1.4268,    -2.6711,    -1.3614}; //C_11
double _A12[] = {  0.0016529,   0.001346,  0.0012103,  0.0010277, -0.0010985,  0.0022305,  0.0015157}; //C_12
double _A13[] = {  0.0020293,  0.0013238,  0.0016381,  0.0031632,  0.0096613,    0.01091,   0.022283}; //C_13
double _A14[] = {     20.289,     23.005,      24.49,     27.341,     27.557,     27.547,     23.164}; //C_14
double _A15[] = {  -0.025203,  -0.030565,  -0.037705,  -0.050655,  -0.056522,   -0.05408,  -0.074146}; //C_15
double _A16[] = {     224.91,     55.047,    -298.32,     -514.1,    -867.03,    -424.23,    -2219.1}; //Tail current tilt angle
double _A17[] = {    -9234.8,    -3875.7,     4400.9,      12482,      20652,     1100.2,      48253}; //tail current tilt angle
double _A18[] = {     22.788,     20.178,     18.692,     16.257,     14.101,     13.954,     12.714}; // \delta x
double _A19[] = {     7.8813,     7.9693,     7.9064,     8.5834,     8.3501,     7.5337,     7.6777}; // a_RC
double _A20[] = {     1.8362,     1.4575,     1.3047,     1.0194,    0.72996,    0.89714,    0.57138}; // D_0
double _A21[] = {   -0.27228,    0.89471,     2.4541,     3.6148,     3.8149,     3.7813,     2.9633}; // gamma_RC
double _A22[] = {     8.8184,     9.4039,     9.7012,     8.6042,     9.2908,     8.2945,     9.3909}; // R_c
double _A23[] = {     2.8714,     3.5215,     7.1624,     5.5057,     6.4674,      5.174,     9.7263}; // G
double _A24[] = {     14.468,     14.474,     14.288,     13.778,     13.729,     14.213,     11.123}; // a_T - characteristic radius of the tail current
double _A25[] = {     32.177,     36.555,     33.822,     32.373,     28.353,     25.237,     21.558}; // D_y - characteristic scale distance in Y
double _A26[] = {       0.01,       0.01,       0.01,       0.01,       0.01,       0.01,       0.01}; // Delta - thickening rate of tail current sheet
double _A27[] = {          0,          0,          0,          0,          0,          0,          0}; // Q - set to 0
double _A28[] = {     7.0459,     7.0787,     6.7442,     7.3195,     7.4237,     7.0037,     4.4518}; // x_0
double _A29[] = {          4,          4,          4,          4,          4,          4,          4}; // \gamma_T
double _A30[] = {         20,         20,         20,         20,         20,         20,         20}; // D_yc (equation 19) - fixed at 20

/*Globals variables (I know, I know...) to save some calculation*/
double SPS, CPS, TPS; //sin(psi), cos(psi), tan(psi)
double C_01,C_02,C_03,C_04,C_05,C_06,C_07,C_08,C_09,C_10,C_11,C_12,C_13,
		C_14,C_15,C_16,C_17,C_18,C_19;
double Delx, Rc,G, x0, gammaT, Dyc, aRC, gammaRC, D0;

void _TrigTilt(double psi) {
	/* calculates sin(psi) etc. */
	SPS = sin(psi);
	CPS = cos(psi);
	TPS = SPS/CPS;
}

void _GetSM(double x, double y, double z, double *xsm, double *ysm, double *zsm) {
	/* using the dipole tilt angle, converts GSM to SM*/
	xsm[0] = x*CPS + z*SPS;
	ysm[0] = y;
	zsm[0] = x*SPS + z*CPS;
}

void _PopulateParams(int kp) {
	/* Selects the appropriate parameters from above based on the kp 
	 * index supplied by Iopt (must not be greater than 6) */
	C_01 = _A01[kp];
	C_02 = _A02[kp];
	C_03 = _A03[kp];
	C_04 = _A04[kp];
	C_05 = _A05[kp];
	C_06 = _A06[kp];
	C_07 = _A07[kp];
	C_08 = _A08[kp];
	C_09 = _A09[kp];
	C_10 = _A10[kp];
	C_11 = _A11[kp];
	C_12 = _A12[kp];
	C_13 = _A13[kp];
	C_14 = _A14[kp];
	C_15 = _A15[kp];
	C_16 = ;
	C_17 = ;
	C_18 = ;
	C_19 = ;
	
	
	Delx = _A18[kp];
	aRC = _A19[kp];
	D0 = _A20[kp];
	gammaRC = _A21[kp];
	Rc = _A22[kp];
	G = _A23[kp];
	aT = _A24[kp];
	Dy = _A25[kp];
	Delta = _A26[kp];
	Q = _A27[kp];
	x0 = _A28[kp];
	gammaT = _A29[kp];
	Dyc = _A30[kp];
	

}
 
	
 
void _GetZs(double xsm, double ysm, double zsm, double *zs, double *dzdx, double *dzdy) {
	/* calcualtes z_s and its spatial derivatives (shape of tail current sheet)*/
	double y4, y410, xrc, xrc2;
	y4 = pow(ysm,4.0);
	xrc = xsm + Rc;
	xrc2 = sqrt(pow(xsm,2.0) + 16.0);
	y410 = y4 + 10000.0;
	
	zs[0] = 0.5*TPS*(xrc - xrc2) - G*SPS*y4/y410;
	
	dzdx[0] = -0.5*TPS*(xrc - xrc2)/xrc2;
	
	dzdy[0] = 40000.0*G*SPS*pow(y,3.0)/pow(y410,2.0);
}

void _GetRC(double xsm, double ysm, double zsm, double zs, double dzdx, double dzdy, double *Bx, double *By, double *Bz) {
	/* Calculates the ring current contribution using equations 13,16 and 17*/
	double zr, Lrc2, hrc, Drc, xiRC, Src, rho2,;
	double Bxsm, Bzsm; 
	/* Get h_1 - equation 13 - originally it used the values 16 and 36, 
	 * but I think the most recent T89 uses 0 and 40 instead for some reason*/
	h1 = 0.5*(1.0 + xsm*sqrt(1.0/(xsm*xsm + 40.0)));
	
	/*now to get h_RC (FA0 in original code) L_RC = 5.0*/
	hRC = 0.5*(1.0 + xsm*sqrt(xsm*xsm + 25.0));
	
	/* Calculate D_RC  - note that gamma1 = 1.0*/
	DRC = D0 + gammaRC*hRC + h1;
	
	/* Get rho - used in S_RC - equation 13*/
	rho = sqrt(pow(xsm,2.0) + pow(ysm,2.0));
	
	/* Calcualte z_r */
	zr = zsm - zs;
	
	/* Now xi_RC */
	xiRC = sqrt(pow(zr,2.0) + pow(Drc,2.0));
	
	/* And S_RC */
	Src = sqrt(rho*rho + pow(aRC + xiRC,2.0));
	
	/* Q_RC */
	Qrc = 3.0*C_03*xsm*(aRC + xiRC)*zr/(xiRC*pow(Src,5.0));
	
	/*Bx and By in SM (I think)*/
	Bx[0] = Qrc*xsm*zr;
	By[0] = Qrc*ysm*zr;
	
	/* now a harder bit */
	
	
	/*A^(RC)*/
	ARC = C_03*rho/pow(Src,3.0);
	
	/*some intermediate quantities*/
	fc = pow(Src,-5.0);
	facxy = fc*3.0*(aRC + xiRC)/(xiRC);
	faq = zr*(xsm*dzdx + ysm*dzdy) - Drc*D0*xsm*Lrc2/pow(xsm*xsm + Lrc2,1.5);
	
	/*calculate the output field in GSM*/
	Bxsm = facxy*xzr;
	By[0] = facxy*ysm*zr;
	Bzsm = fc*(2.0*pow(aRC + xiRC,2.0) - rho2) + facxy*faq;

	Bx[0] = Bxsm*CPS + Bzsm*SPS;
	Bz[0] = Bzsm*CPS - Bxsm*SPS;

}

void _GetTail() {
	
	/* Get W(x,y) */
	W = 0.5*(1 - (x-x0)/sqrt(pow(x-x0,2.0) + 13.0*13.0))/(1 + (ysm*ysm)/(Dy*Dy));	
}
 
void T89(int Iopt, double *ParMod, double Ps, double x, double y, double z, double *Bx, double *By, double *Bz) {
	 
	 
} 
 
