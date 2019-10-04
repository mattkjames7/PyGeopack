import numpy as np
import pyomnidata as pod
import kpindex
import time
import DateTimeTools as TT
import copy
from .GetDipoleTilt import GetDipoleTilt
import RecarrayTools as RT

def _InterpField(a,flags,maxgap=36):
	'''
	Interpolates gaps in an array, up to a maximum gap length
	'''
	#copy original array
	out = copy.deepcopy(a)
	
	#find the start and end indices of each gap
	g0 = np.where(np.isfinite(a[:-1]) & (np.isfinite(a[1:]) == False))[0]
	g1 = np.where(np.isfinite(a[1:]) & (np.isfinite(a[:-1]) == False))[0]+1
	
	if g0.size > 0 and g1.size > 0:
		#if the field starts with a valid value
		if g1[0] < g0[0]:
			g0 = np.append(0,g0)
		#if the field ends with a valid value
		if g0[-1] >= g1[-1]:
			g1 = np.append(g1,g1.size) 
		
		#loop through each gap	
		ng = g0.size
		for i in range(0,ng):
			gs = g1[i]-g0[i]
			if gs <= maxgap:
				x = np.arange(gs-1) + 1
				m = (a[g1[i]] - a[g0[i]])/gs
				c = a[g0[i]]
				out[g0[i]+1:g1[i]] = m*x + c
				flags[g0[i]+1:g1[i]] = 2
	
	return out

def _InterpKp(Date,ut,kp):
	'''
	Fills in the Kp values
	
	'''
	
	#create output array
	out = np.zeros(Date.size,dtype='float32') + np.nan
	
	#get a list of unique dates
	ud = np.unique(kp.Date)
	
	#loop through each date
	for i in range(0,ud.size):
		print('\rInterpolating Kp Date {0} of {1}'.format(i+1,ud.size),end='')
		ukp = np.where(kp.Date == ud[i])[0]
		tmpkp = kp[ukp]
		udt = np.where(Date == ud[i])[0]
		#loop through each ut range
		for j in range(0,tmpkp.size):
			uut = np.where((ut[udt] >= tmpkp.ut0[j]) & (ut[udt] < tmpkp.ut1[j]))[0]
			out[udt[uut]] = tmpkp.Kp[j]
	print()		
	return out
	
def _GetGParameters(data):
	n = data.size
	for i in range(0,n):
		print('\rCalculating G param {0} of {1}'.format(i+1,n),end='')
		i0 = np.max([0,i-11])
		i1 = i + 1
		
		tmp = data[i0:i1]
		use = np.where(np.isfinite(tmp.By) & np.isfinite(tmp.Bz) & (tmp.IMFFlag > -1) & (tmp.ISWFlag > -1))[0]
		if use.size > 0:
			CA = np.arctan2(-tmp.By,tmp.Bz)
			Bp = np.sqrt(tmp.By**2 + tmp.Bz**2)
			Bs = np.abs(tmp.Bz)
			pos = np.where(tmp.Bz > 0)[0]
			Bs[pos] = 0.0
			h = ((Bp/40.0)**2)/(1.0 + Bp/40.0)
			data.G1[i] = np.sum(400.0*h*(np.sin(CA/2))**3)/use.size
			data.G2[i] = 0.005*np.sum(400.0*Bs)/use.size
		else:
			data.G1[i] = 0.0
			data.G2[i] = 0.0		
	
	print()
	
def _ScanTS05Intervals(data):
	beg = []
	end = []
	SymHLowLim = -10.0
	DSymHLim = 5.0
	
	i = -1 #current record
	LenQuiet = 0 
	SymHMax = 1000.0
	SymHMin = -1000.0
	while True:
		i += 1
		if i == data.size:
			break

		if (data.SymH[i] > SymHMax) & np.isfinite(data.SymH[i]):
			SymHMax = data.SymH[i]
		if (data.SymH[i] < SymHMin) & np.isfinite(data.SymH[i]):
			SymHMin = data.SymH[i]
			
		if (data.IMFFlag[i] == -1) | (data.ISWFlag[i] == -1) | (data.Bz[i] < 0.0) | (data.SymH[i] < SymHLowLim):
			LenQuiet = 0 
			SymHMax = 1000.0
			SymHMin = -1000.0			
		else:
			if LenQuiet == 0:
				FirstGoodi = copy.deepcopy(i)
			
			if (SymHMax - SymHMin > DSymHLim):
				i = FirstGoodi + 1
				LenQuiet = 0
				SymHMax = 1000.0
				SymHMin = -1000.0	
			else:
				LenQuiet += 1
				
				if LenQuiet == 24:
					beg.append(copy.deepcopy(i))
					for i in range(beg[-1],n):
						if (data.IMFFlag[i] == -1) | (data.ISWFlag[i] == -1):
							end.append(i-2)
							beg[i] = beg[i]-LenQuiet
							LenQuiet = 0
							SymHMax = 1000.0
							SymHMin = -1000.0								
					
					end.append(n-1)
					beg[i] = beg[i]-LenQuiet
	return beg,end	
		
def _GetWParameters(data):
	n = data.size
	
	A = np.array([    1.00000,    5.44118,   0.891995,
    9.09684,    0.00000,   -7.18972,    12.2700,   -4.89408,    0.00000,
   0.870536,    1.36081,    0.00000,   0.688650,   0.602330,    0.00000,
   0.316346,    1.22728,  -0.363620E-01,  -0.405821,   0.452536,   0.755831,
   0.215662,   0.152759,    5.96235,    23.2036,    11.2994,    69.9596,
   0.989596,  -0.132131E-01,   0.985681,   0.344212E-01,    1.02389,   0.207867,
    1.51220,   0.682715E-01,    1.84714,    1.76977,    1.37690,   0.696350,
   0.343280,    3.28846,    111.293,    5.82287,    4.39664,   0.383403,
   0.648176,   0.318752E-01,   0.581168,    1.15070,   0.843004,   0.394732,
   0.846509,   0.916555,   0.550920,   0.180725,   0.898772,   0.387365,
    2.26596,    1.29123,   0.436819,    1.28211,    1.33199,   0.405553,
    1.62290,   0.699074,    1.26131,    2.42297,   0.537116,   0.619441])

	DT1=A[44]/60.0  
	DT2=A[45]/60.0
	DT3=A[46]/60.0
	DT4=A[47]/60.0
	DT5=A[48]/60.0
	DT6=A[49]/60.0

	
	#get the intervals valid for the TS05 model
	print('Scanning for valid TS05 intervals')
	beg,end = _ScanTS05Intervals(data)
	
	#calculate flow speed
	V = np.sqrt(data.Vx**2 + data.Vy**2 + data.Vz**2)
	
	#loop through each interval
	ni = len(beg)
	for i in range(0,ni):
		print('\rProcessing interval {0} of {1}'.format(i+1,ni),end='')
		iB = beg[i]
		iE = end[i]
		for j in range(iB,iE):
			Pdyn = 1.937e-6*data.Den[j]*V[j]**2
			By = data.By[j]
			Bz = data.Bz[j]
			SymH = data.SymH[j]
			
			
			W1 = 0.0
			W2 = 0.0
			W3 = 0.0
			W4 = 0.0
			W5 = 0.0
			W6 = 0.0

			Key1 = 1
			Key2 = 1
			Key3 = 1
			Key4 = 1
			Key5 = 1
			Key6 = 1

			for k in range(j,iB,-1):
				Vnorm = V[k]/400.0
				Dennorm = data.Den[k]*1.16/5.0
				Bsnorm = -data.Bz[k]/5.0
			
				if Bsnorm <= 0.0:
					Bs1 = 0.0
					Bs2 = 0.0
					Bs3 = 0.0
					Bs4 = 0.0
					Bs5 = 0.0
					Bs6 = 0.0
				else:
					Bs1 = Bsnorm**A[52]
					Bs2 = Bsnorm**A[55]
					Bs3 = Bsnorm**A[58]
					Bs4 = Bsnorm**A[61]
					Bs5 = Bsnorm**A[64]
					Bs6 = Bsnorm**A[67]
				
				FAC1 = Dennorm**A[50] *Vnorm**A[51] *Bs1
				FAC2 = Dennorm**A[53] *Vnorm**A[54] *Bs2
				FAC3 = Dennorm**A[56] *Vnorm**A[57] *Bs3
				FAC4 = Dennorm**A[59] *Vnorm**A[60] *Bs4
				FAC5 = Dennorm**A[62] *Vnorm**A[63] *Bs5
				FAC6 = Dennorm**A[65] *Vnorm**A[66] *Bs6				
				
				TAUMT = (i - k)*5.0

				ARG1 = -TAUMT*DT1
				ARG2 = -TAUMT*DT2
				ARG3 = -TAUMT*DT3
				ARG4 = -TAUMT*DT4
				ARG5 = -TAUMT*DT5
				ARG6 = -TAUMT*DT6


				if (ARG1 > -10.0) and (Key1 == 1):
					W1 = W1 + FAC1*np.exp(ARG1)
				if (ARG2 > -10.0) and (Key2 == 1):
					W2 = W2 + FAC2*np.exp(ARG2)
				if (ARG3 > -10.0) and (Key3 == 1):
					W3 = W3 + FAC3*np.exp(ARG3)
				if (ARG4 > -10.0) and (Key4 == 1):
					W4 = W4 + FAC4*np.exp(ARG4)
				if (ARG5 > -10.0) and (Key5 == 1):
					W5 = W5 + FAC5*np.exp(ARG5)
				if (ARG6 > -10.0) and (Key6 == 1):
					W6 = W6 + FAC6*np.exp(ARG6)
					
				if Key1 == 0 and Key2 == 0 and Key3 == 0 and Key4 == 0 and Key5 == 0 and Key6 == 0:
					break
			data.W1[j] = W1*DT1*5
			data.W2[j] = W2*DT2*5
			data.W3[j] = W3*DT3*5
			data.W4[j] = W4*DT4*5
			data.W5[j] = W5*DT5*5
			data.W6[j] = W6*DT6*5
	print()

def _ConvertParameters():
	'''
	This will take in all of the omni and kp index parameters required
	for the Tsyganenko models and do the following:
	
	1. Interpolate gaps - maximum gap of 3h (36 points of 5 minute data)
	2. Resample Kp to a 5 minute interval.
	3. Calculate G1 and G2 parameters for T01.
	4. Calculate W1-W6 parameters for TS05.
	5. Save in one massive file.
	
	Date used will be from 1994-current
	'''

	#find the current year to get the total year range
	years = [1994,time.localtime()[0]]
	
	#load data
	print('Reading OMNI Data')
	omni = pod.GetOMNI(years,Res=5)
	print('Reading Kp Indices')
	kp = kpindex.GetKp()
	
	#create the output array
	print('Creating output array')
	n = omni.size
	dtype = [('Date','int32'),('ut','float32'),('Year','int32'),('DayNo','int32'),
			('Hr','int32'),('Mn','int32'),('Bx','float32'),('By','float32'),('Bz','float32'),
			('Vx','float32'),('Vy','float32'),('Vz','float32'),('Den','float32'),
			('Temp','float32'),('SymH','float32'),('IMFFlag','int32'),('ISWFlag','int32'),
			('Tilt','float32'),('Pdyn','float32'),('W1','float32'),('W2','float32'),
			('W3','float32'),('W4','float32'),('W5','float32'),('W6','float32'),
			('G1','float32'),('G2','float32'),('Kp','float32')]
	data = np.recarray(n,dtype=dtype)
	
	#populate time-related fields
	data.Date = omni.Date
	data.ut = omni.ut
	data.Year = data.Date//10000
	data.DayNo = TT.DayNo(data.Date)
	data.Hr = np.int32(np.floor(data.ut))
	data.Mn = np.int32(np.round((data.ut - data.Hr)*60.0))
	
	#copy flags
	data.IMFFlag = np.int32(np.isfinite(omni.BxGSE) & np.isfinite(omni.ByGSM) & np.isfinite(omni.BzGSM))*2 - 1 
	data.ISWFlag = np.int32(np.isfinite(omni.Vx) & np.isfinite(omni.Vy) & np.isfinite(omni.Vz))*2 - 1
	
	
	#interpolate some fields
	print('Interpolating fields')
	data.Bx = _InterpField(omni.BxGSE,data.IMFFlag,36)
	data.By = _InterpField(omni.ByGSM,data.IMFFlag,36)
	data.Bz = _InterpField(omni.BzGSM,data.IMFFlag,36)
	data.Vx = _InterpField(omni.Vx,data.ISWFlag,36)
	data.Vy = _InterpField(omni.Vy,data.ISWFlag,36)
	data.Vz = _InterpField(omni.Vz,data.ISWFlag,36)
	data.Den = _InterpField(omni.ProtonDensity,data.ISWFlag,36)
	data.Temp = _InterpField(omni.Temp,data.ISWFlag,36)
	data.SymH = _InterpField(omni.SymH,data.ISWFlag,36)
	data.Pdyn = _InterpField(omni.FlowPressure,data.ISWFlag,36)
	
	return _ScanTS05Intervals(data)
	
	#fill in the Kp index
	data.Kp = _InterpKp(data.Date,data.ut,kp)
	
	#calculate G1 and G2
	_GetGParameters(data)
	
	#get the dipole tilt angle
	print('Calculating dipole tilt angles')
	data.Tilt = GetDipoleTilt((data.Year,data.DayNo),(data.Hr,data.Mn),(data.Vx,data.Vy,data.Vz))
	
	#calculate W parameters
	_GetWParameters(data)
		
	return data
