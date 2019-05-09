import numpy as np

def _GetZs(xsm,ysm,zsm,Rc,TPS,SPS,G):
	#/* calcualtes z_s and its spatial derivatives (shape of tail current sheet)*/
	y4 = ysm**4;
	xrc = xsm + Rc;
	xrc2 = np.sqrt(xsm**2.0 + 16.0);
	y410 = y4 + 10000.0;
	
	zs = 0.5*TPS*(xrc - xrc2) - G*SPS*y4/y410;
	
	dzdx = -0.5*TPS*(xrc - xrc2)/xrc2;
	
	dzdy = 40000.0*G*SPS*ysm**3.0/y410**2.0;
	return zs,dzdx,dzdy


def T89c(kp,psi,x,y,z):
	
	#define the arrays of Kp-based parameters first
	_A01 = np.array([    -116.53,    -55.553,    -101.34,    -181.69,    -436.54,    -707.77,    -1190.4],dtype='float64')
	_A02 = np.array([     -10719,     -13198,     -13480,     -12320,      -9001,    -4471.9,     2749.9],dtype='float64')
	_A03 = np.array([     42.375,     60.647,     111.35,     173.79,     323.66,     432.81,     742.56],dtype='float64')
	_A04 = np.array([     59.753,     61.072,     12.386,    -96.664,    -410.08,    -435.51,    -1110.3],dtype='float64')
	_A05 = np.array([     -11363,     -16064,     -24699,     -39051,     -50340,     -60400,     -77193],dtype='float64')
	_A06 = np.array([     1.7844,     2.2534,     2.6459,     3.2633,     3.9932,     4.6229,     7.6727],dtype='float64') #//C_06
	_A07 = np.array([     30.268,     34.407,     38.948,     44.968,     58.524,     68.178,     102.05],dtype='float64') #//C_07
	_A08 = np.array([  -0.035372,  -0.038887,   -0.03408,  -0.046377,  -0.038519,  -0.088245,  -0.096015],dtype='float64') #//C_08
	_A09 = np.array([  -0.066832,  -0.094571,   -0.12404,   -0.16686,   -0.26822,   -0.21002,   -0.74507],dtype='float64') #//C_09
	_A10 = np.array([   0.016456,   0.027154,   0.029702,   0.048298,   0.074528,    0.11846,    0.11214],dtype='float64') #//C_10
	_A11 = np.array([    -1.3024,    -1.3901,    -1.4052,    -1.5473,    -1.4268,    -2.6711,    -1.3614],dtype='float64') #//C_11
	_A12 = np.array([  0.0016529,   0.001346,  0.0012103,  0.0010277, -0.0010985,  0.0022305,  0.0015157],dtype='float64') #//C_12
	_A13 = np.array([  0.0020293,  0.0013238,  0.0016381,  0.0031632,  0.0096613,    0.01091,   0.022283],dtype='float64') #//C_13
	_A14 = np.array([     20.289,     23.005,      24.49,     27.341,     27.557,     27.547,     23.164],dtype='float64') #//C_14
	_A15 = np.array([  -0.025203,  -0.030565,  -0.037705,  -0.050655,  -0.056522,   -0.05408,  -0.074146],dtype='float64') #//C_15
	_A16 = np.array([     224.91,     55.047,    -298.32,     -514.1,    -867.03,    -424.23,    -2219.1],dtype='float64')
	_A17 = np.array([    -9234.8,    -3875.7,     4400.9,      12482,      20652,     1100.2,      48253],dtype='float64')
	_A18 = np.array([     22.788,     20.178,     18.692,     16.257,     14.101,     13.954,     12.714],dtype='float64') #// \delta x
	_A19 = np.array([     7.8813,     7.9693,     7.9064,     8.5834,     8.3501,     7.5337,     7.6777],dtype='float64') #// a_RC
	_A20 = np.array([     1.8362,     1.4575,     1.3047,     1.0194,    0.72996,    0.89714,    0.57138],dtype='float64') #// D_0
	_A21 = np.array([   -0.27228,    0.89471,     2.4541,     3.6148,     3.8149,     3.7813,     2.9633],dtype='float64') #// gamma_RC
	_A22 = np.array([     8.8184,     9.4039,     9.7012,     8.6042,     9.2908,     8.2945,     9.3909],dtype='float64') #// R_c
	_A23 = np.array([     2.8714,     3.5215,     7.1624,     5.5057,     6.4674,      5.174,     9.7263],dtype='float64') #// G
	_A24 = np.array([     14.468,     14.474,     14.288,     13.778,     13.729,     14.213,     11.123],dtype='float64')
	_A25 = np.array([     32.177,     36.555,     33.822,     32.373,     28.353,     25.237,     21.558],dtype='float64')
	_A26 = np.array([       0.01,       0.01,       0.01,       0.01,       0.01,       0.01,       0.01],dtype='float64')
	_A27 = np.array([          0,          0,          0,          0,          0,          0,          0],dtype='float64') #// 
	_A28 = np.array([     7.0459,     7.0787,     6.7442,     7.3195,     7.4237,     7.0037,     4.4518],dtype='float64') #// x_0
	_A29 = np.array([          4,          4,          4,          4,          4,          4,          4],dtype='float64') #// \gamma_T
	_A30 = np.array([         20,         20,         20,         20,         20,         20,         20],dtype='float64') #// D_yc

	#use Iopt to select the appropriate ones
	cA01 = _A01[kp]
	cA02 = _A02[kp]
	cA03 = _A03[kp]
	cA04 = _A04[kp]
	cA05 = _A05[kp]
	cA06 = _A06[kp]
	cA07 = _A07[kp]
	cA08 = _A08[kp]
	cA09 = _A09[kp]
	cA10 = _A10[kp]
	cA11 = _A11[kp]
	cA12 = _A12[kp]
	cA13 = _A13[kp]
	cA14 = _A14[kp]
	cA15 = _A15[kp]
	cA16 = _A16[kp]
	cA17 = _A17[kp]
	cA18 = _A18[kp]
	cA19 = _A19[kp]
	cA20 = _A20[kp]
	cA21 = _A21[kp]
	cA22 = _A22[kp]
	cA23 = _A23[kp]
	cA24 = _A24[kp]
	cA25 = _A25[kp]
	cA26 = _A26[kp]
	cA27 = _A27[kp]
	cA28 = _A28[kp]
	cA29 = _A29[kp]
	cA30 = _A30[kp]
	
	#print(A01,A02,A03,A04,A05)
	#print(A06,A07,A08,A09,A10)
	
	#some parameters
	A02 = 25.0
	XLW2 = 170.0
	YN = 30.0
	RPI = 0.31830989
	RT = 30.0
	XD = 0.0 
	XLD2 = 40.0
	
	SXC = 4.0
	XLWC2 = 50.0
	DXL = 20.0
	
	DYC = cA30
	DYC2 = DYC**2
	DX = cA18
	HA02 = 0.5*A02
	RDX2M = -1.0/DX**2
	RDX2 = -RDX2M
	RDYC2 = 1.0/DYC2
	HLWC2M = -0.5*XLWC2
	DRDYC2 = -2.0*RDYC2
	DRDYC3 = 2.0*RDYC2*np.sqrt(RDYC2)
	HXLW2M = -0.5*XLW2
	ADR = cA19
	D0 = cA20
	DD = cA21
	RC = cA22
	G = cA23
	AT = cA24
	DT = D0
	DEL = cA26
	P = cA25
	Q = cA27
	SX = cA28
	GAM = cA29
	HXLD2M = -0.5*XLD2
	ADSL = 0.0
	XGHS = 0.0
	H = 0.0
	HS = 0.0
	GAMH = 0.0
	W1 = -0.5/DX
	DBLDEL = 2.0*DEL
	W2 = W1*2.0
	W4 = -1.0/3.0
	W3 = W4/DX
	W5 = -0.50
	W6 = -3.0
	AK1 = cA01
	AK2 = cA02
	AK3 = cA03
	AK4 = cA04 
	AK5 = cA05
	AK6 = cA06
	AK7 = cA07
	AK8 = cA08
	AK9 = cA09
	AK10 = cA10
	AK11 = cA11
	AK12 = cA12
	AK13 = cA13
	AK14 = cA14
	AK15 = cA15
	AK16 = cA16
	AK17 = cA17
	SXA = 0.0
	SYA = 0.0
	SZA = 0.0
	AK610 = AK6*W1 + AK10*W5
	AK711 = AK7*W2 - AK11
	AK812 = AK8*W2 + AK12*W6
	AK913 = AK9*W3 + AK13*W4
	RDXL = 1.0/DXL
	HRDXL = 0.5*RDXL
	A6H = AK6*0.5
	A9T = AK9/3.0
	YNP = RPI/YN*0.5
	YND = 2.0*YN

	TILT = psi
	TLT2 = TILT**2
	SPS = np.sin(TILT)
	CPS = np.cos(TILT)
	X = x
	Y = y
	Z = z
	
	X2 = X*X
	Y2 = Y*Y
	Z2 = Z*Z
	TPS = SPS/CPS
	HTP = TPS*0.5
	GSP = G*SPS
	XSM = X*CPS - Z*SPS
	ZSM = X*SPS + Z*CPS	
	
	#calculate ZS
	XRC = XSM + RC
	XRC16 = XRC**2 + 16.0
	SXRC = np.sqrt(XRC16)
	Y4 = Y2*Y2
	Y410 = Y4 + 10.0**4
	SY4 = SPS/Y410
	GSY4 = G*SY4
	ZS1 = HTP*(XRC - SXRC)
	DZSX = -ZS1/SXRC
	ZS = ZS1-GSY4*Y4
	D2ZSGY = -SY4/Y410*4.0e4*Y2*Y
	DZSY = G*D2ZSGY
	#print(Y410)
	print(ZS,DZSX,DZSY)
	print(_GetZs(XSM,Y,ZSM,RC,TPS,SPS,G))
	#CALCULATE THE COMPONENTS OF THE RING CURRENT CONTRIBUTION:

	XSM2 = XSM**2
	DSQT = np.sqrt(XSM2+A02)
	FA0 = 0.5*(1.0 + XSM/DSQT)
	DDR = D0 + DD*FA0
	DFA0 = HA02/DSQT**3
	ZR = ZSM - ZS
	TR = np.sqrt(ZR**2 + DDR**2)
	RTR = 1.0/TR
	RO2 = XSM2 + Y2
	ADRT = ADR + TR
	ADRT2 = ADRT**2
	FK = 1.0/(ADRT2 + RO2)
	DSFC = np.sqrt(FK)
	FC = FK**2*DSFC
	FACXY = 3.0*ADRT*FC*RTR
	XZR = XSM*ZR
	YZR = Y*ZR
	DBXDP = FACXY*XZR
	DERy05 = FACXY*YZR
	XZYZ = XSM*DZSX + Y*DZSY
	FAQ = ZR*XZYZ - DDR*DD*DFA0*XSM
	DBZDP = FC*(2.0*ADRT2 - RO2) + FACXY*FAQ
	DERx05 = DBXDP*CPS + DBZDP*SPS
	DERz05 = DBZDP*CPS - DBXDP*SPS
	
	
	#CALCULATE THE TAIL CURRENT SHEET CONTRIBUTION:

	DELY2 = DEL*Y2
	D = DT + DELY2
	if np.abs(GAM) >= 1e-6:
		XXD = XSM - XD
		RQD = 1.0/(XXD**2 + XLD2)
		RQDS = np.sqrt(RQD)
		H = 0.5*(1.0 + XXD*RQDS)
		HS = -HXLD2M*RQD*RQDS
		GAMH = GAM*H
		D = D + GAMH
		XGHS = XSM*GAM*HS
		ADSL = -D*XGHS
	D2 = D**2
	T = np.sqrt(ZR**2 + D2)
	XSMX = XSM - SX
	RDSQ2 = 1.0/(XSMX**2 + XLW2)
	RDSQ = np.sqrt(RDSQ2)
	V = 0.5*(1.0 - XSMX*RDSQ)
	DVX = HXLW2M*RDSQ*RDSQ2
	OM = np.sqrt(np.sqrt(XSM2 + 16.0) - XSM)
	OMS = -OM/(OM*OM + XSM)*0.5
	RDY = 1.0/(P + Q*OM)
	OMSV = OMS*V
	RDY2 = RDY**2
	FY = 1.0/(1.0 + Y2*RDY2)
	W = V*FY
	YFY1 = 2.0*FY*Y2*RDY2
	FYPR = YFY1*RDY
	FYDY = FYPR*FY
	DWX = DVX*FY + FYDY*Q*OMSV
	YDWY = -V*YFY1*FY
	DDY = DBLDEL*Y
	ATT = AT + T
	S1 = np.sqrt(ATT**2 + RO2)
	F5 = 1.0/S1
	F7 = 1.0/(S1 + ATT)
	F1 = F5*F7
	F3 = F5**3
	F9 = ATT*F3
	FS = ZR*XZYZ - D*Y*DDY + ADSL
	XDWX = XSM*DWX + YDWY
	RTT = 1.0/T
	WT = W*RTT
	BRRZ1 = WT*F1
	BRRZ2 = WT*F3
	DBXC1 = BRRZ1*XZR
	DBXC2 = BRRZ2*XZR
	DERy01 = BRRZ1*YZR
	DERy02 = BRRZ2*YZR
	DERy16 = DERy01*TLT2
	DERy17 = DERy02*TLT2
	WTFS = WT*FS
	DBZC1 = W*F5 + XDWX*F7 + WTFS*F1
	DBZC2 = W*F9 + XDWX*F1 + WTFS*F3
	DERx01 = DBXC1*CPS + DBZC1*SPS
	DERx02 = DBXC2*CPS + DBZC2*SPS
	DERz01 = DBZC1*CPS - DBXC1*SPS
	DERz02 = DBZC2*CPS - DBXC2*SPS
	DERx16 = DERx01*TLT2
	DERx17 = DERx02*TLT2
	DERz16 = DERz01*TLT2
	DERz17 = DERz02*TLT2

#	print(DERx01,DERy01,DERz01)
#	print(DERx02,DERy02,DERz02)
#	print(DERx16,DERy16,DERz16)
#	print(DERx17,DERy17,DERz17)


	#CALCULATE CONTRIBUTION FROM THE CLOSURE CURRENTS

	ZPL = Z + RT
	ZMN = Z - RT
	ROGSM2 = X2 + Y2
	SPL = np.sqrt(ZPL**2 + ROGSM2)
	SMN = np.sqrt(ZMN**2 + ROGSM2)
	XSXC = X - SXC
	RQC2 = 1.0/(XSXC**2 + XLWC2)
	RQC = np.sqrt(RQC2)
	FYC = 1.0/(1.0 + Y2*RDYC2)
	WC = 0.5*(1.0 - XSXC*RQC)*FYC
	DWCX = HLWC2M*RQC2*RQC*FYC
	DWCY = DRDYC2*WC*FYC*Y
	SZRP = 1.0/(SPL + ZPL)
	SZRM = 1.0/(SMN - ZMN)
	XYWC = X*DWCX + Y*DWCY
	WCSP = WC/SPL
	WCSM = WC/SMN
	FXYP = WCSP*SZRP
	FXYM = WCSM*SZRM
	FXPL = X*FXYP
	FXMN = -X*FXYM
	FYPL = Y*FXYP
	FYMN = -Y*FXYM
	FZPL = WCSP + XYWC*SZRP
	FZMN = WCSM + XYWC*SZRM
	DERx03 = FXPL + FXMN
	DERx04 = (FXPL - FXMN)*SPS
	DERy03 = FYPL + FYMN
	DERy04 = (FYPL - FYMN)*SPS
	DERz03 = FZPL + FZMN
	DERz04 = (FZPL - FZMN)*SPS

#	print(DERx03,DERy03,DERz03)
#	print(DERx04,DERy04,DERz04)

	#NOW CALCULATE CONTRIBUTION FROM CHAPMAN-FERRARO SOURCES + ALL OTHER

	EX = np.exp(X/DX)
	EC = EX*CPS
	ES = EX*SPS
	ECZ = EC*Z
	ESZ = ES*Z
	ESZY2 = ESZ*Y2
	ESZZ2 = ESZ*Z2
	ECZ2 = ECZ*Z
	ESY = ES*Y
	
	DERx06 = ECZ
	DERx07 = ES
	DERx08 = ESY*Y
	DERx09 = ESZ*Z
	DERy10 = ECZ*Y
	DERy11 = ESY
	DERy12 = ESY*Y2
	DERy13 = ESY*Z2
	DERz14 = EC
	DERz15 = EC*Y2
	DERz06 = ECZ2*W1
	DERz10 = ECZ2*W5
	DERz07 = ESZ*W2
	DERz11 = -ESZ
	DERz08 = ESZY2*W2
	DERz12 = ESZY2*W6
	DERz09 = ESZZ2*W3
	DERz13 = ESZZ2*W4


	#FINALLY, CALCULATE NET EXTERNAL MAGNETIC FIELD COMPONENTS,
	#BUT FIRST OF ALL THOSE FOR C.-F. FIELD:

	SX1 = AK6*DERx06 + AK7*DERx07 + AK8*DERx08 + AK9*DERx09
	SY1 = AK10*DERy10 + AK11*DERy11 + AK12*DERy12 + AK13*DERy13
	SZ1 = AK14*DERz14 + AK15*DERz15 + AK610*ECZ2 + AK711*ESZ + AK812*ESZY2 + AK913*ESZZ2
	BXCL = AK3*DERx03 + AK4*DERx04
	BYCL = AK3*DERy03 + AK4*DERy04
	BZCL = AK3*DERz03 + AK4*DERz04
	BXT = AK1*DERx01 + AK2*DERx02 + BXCL + AK16*DERx16 + AK17*DERx17
	BYT = AK1*DERy01 + AK2*DERy02 + BYCL + AK16*DERy16 + AK17*DERy17
	BZT = AK1*DERz01 + AK2*DERz02 + BZCL + AK16*DERz16 + AK17*DERz17
	Bx = BXT + AK5*DERx05 + SX1 + SXA
	By = BYT + AK5*DERy05 + SY1 + SYA
	Bz = BZT + AK5*DERz05 + SZ1 + SZA

	return Bx,By,Bz
