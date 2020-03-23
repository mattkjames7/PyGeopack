import numpy as np

import time
import DateTimeTools as TT
import copy
from .GetDipoleTilt import GetDipoleTilt
import RecarrayTools as RT
from ._CFunctions import _CCalculateW,_CFillInKp,_CCalculateG

def _InterpField(a,flags,maxgap=36):
	'''
	Interpolates gaps in an array, up to a maximum gap length
	'''
	#copy original array
	out = copy.deepcopy(a)
	
	#find the start and end indices of each gap
	fina = np.isfinite(a)
	g0 = np.where(fina[:-1] & (fina[1:] == False))[0]
	g1 = np.where((fina[:-1] == False) & fina[1:])[0]+1
	
	if g0.size > 0 and g1.size > 0:
		#if the field starts with a valid value
		if g1[0] <= g0[0]:
			g0 = np.append(0,g0)
		#if the field ends with a valid value
		if g0[-1] > g1[-1]:
			g1 = np.append(g1,a.size) 
		
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

	
def _FillInKp(Date,ut,kp):
	'''
	Fills in the Kp indices.
	'''
	_n = np.int32(Date.size)
	_Date = Date.astype('int32')
	_ut = ut.astype('float32')
	_kpout = np.zeros(_n,dtype='float32')
	_nk = np.int32(kp.size)
	_kDate = kp.Date.astype('int32')
	_kut0 = kp.ut0.astype('float32')
	_kut1 = kp.ut1.astype('float32')
	_kp = kp.Kp.astype('float32')

	_CFillInKp(_nk,_kDate,_kut0,_kut1,_kp,_n,_Date,_ut,_kpout)
	
	return _kpout
	
# def _GetGParameters(data):
	# n = data.size
	# for i in range(0,n):
		# print('\rCalculating G param {0} of {1}'.format(i+1,n),end='')
		# i0 = np.max([0,i-11])
		# i1 = i + 1
		
		# tmp = data[i0:i1]
		# use = np.where(np.isfinite(tmp.By) & np.isfinite(tmp.Bz) & (tmp.IMFFlag > -1) & (tmp.ISWFlag > -1))[0]
		# if use.size > 0:
			# CA = np.arctan2(-tmp.By,tmp.Bz)
			# Bp = np.sqrt(tmp.By**2 + tmp.Bz**2)
			# Bs = np.abs(tmp.Bz)
			# pos = np.where(tmp.Bz > 0)[0]
			# Bs[pos] = 0.0
			# h = ((Bp/40.0)**2)/(1.0 + Bp/40.0)
			# data.G1[i] = np.sum(400.0*h*(np.sin(CA/2))**3)/use.size
			# data.G2[i] = 0.005*np.sum(400.0*Bs)/use.size
		# else:
			# data.G1[i] = 0.0
			# data.G2[i] = 0.0		
	
	# print()
	
def _GetGParameters(data):
	_n = np.int32(data.size)
	_good = np.bool8(np.isfinite(data.By) & np.isfinite(data.Bz) & (data.IMFFlag > -1) & (data.ISWFlag > -1))
	_By = data.By.astype('float32')
	_Bz = data.Bz.astype('float32')
	_V = np.sqrt(data.Vx**2 + data.Vy**2 + data.Vz**2).astype('float32')
	_G1 = np.zeros(_n,dtype='float32')
	_G2 = np.zeros(_n,dtype='float32')
	
	_CCalculateG(_n,_By,_Bz,_V,_good,_G1,_G2)
	
	return _G1,_G2	
	

	
# def _ScanTS05IntervalsSortOfWorking(data):
	# n = data.size
	
	# beg = []
	# end = []
	# SymHLowLim = -10.0
	# DSymHLim = 5.0
	# i = 0
	# while i < n-24:
		# gd_Bz = data.Bz[i:i+24] >= 0.0
		# gd_SymH = data.SymH[i:i+24]  >= SymHLowLim
		# gd_B = data.IMFFlag[i:i+24] > -1
		# gd_SW = data.ISWFlag[i:i+24] > -1
		# mxS = np.max(data.SymH[i:i+24])
		# mnS = np.min(data.SymH[i:i+24])
		# gd_dSymH = mxS-mnS <= DSymHLim
		# gd = (gd_Bz & gd_SymH & gd_B & gd_SW).all() & gd_dSymH
		# if gd:
			# beg.append(i)
			# i += 24
			# bad = np.where((data.ISWFlag[i:] == -1) | (data.IMFFlag[:1] == -1))[0]
			# if bad.size == 0:
				# #must be good until the end, so break
				# end.append(n-1)
				# break
			# else:
				# end.append(bad[0]-1+i)
				# i = bad[0] + i
			# print('Interval {0} found: {1}-{2}T{3}:{4}  - {5}-{6}T{7}:{8}'.format(len(beg),data.Year[beg[-1]],data.DayNo[beg[-1]],data.Hr[beg[-1]],data.Mn[beg[-1]],data.Year[end[-1]],data.DayNo[end[-1]],data.Hr[end[-1]],data.Mn[end[-1]]))
		# i+=1
	# return beg,end	
		
# def _GetWParametersNotWorking(data):
	# n = data.size
	
	# A = np.array([    1.00000,    5.44118,   0.891995,
    # 9.09684,    0.00000,   -7.18972,    12.2700,   -4.89408,    0.00000,
   # 0.870536,    1.36081,    0.00000,   0.688650,   0.602330,    0.00000,
   # 0.316346,    1.22728,  -0.363620E-01,  -0.405821,   0.452536,   0.755831,
   # 0.215662,   0.152759,    5.96235,    23.2036,    11.2994,    69.9596,
   # 0.989596,  -0.132131E-01,   0.985681,   0.344212E-01,    1.02389,   0.207867,
    # 1.51220,   0.682715E-01,    1.84714,    1.76977,    1.37690,   0.696350,
   # 0.343280,    3.28846,    111.293,    5.82287,    4.39664,   0.383403,
   # 0.648176,   0.318752E-01,   0.581168,    1.15070,   0.843004,   0.394732,
   # 0.846509,   0.916555,   0.550920,   0.180725,   0.898772,   0.387365,
    # 2.26596,    1.29123,   0.436819,    1.28211,    1.33199,   0.405553,
    # 1.62290,   0.699074,    1.26131,    2.42297,   0.537116,   0.619441])

	# #Parameters from the paper (I think the values have changed slightly since publication)
	# r =  np.array([0.383403,0.648176,0.318752E-01,0.581168,1.15070,0.843004])/60.0
	# gamma = np.array([0.916555,0.898772,1.29123,1.33199,0.699074,0.537116])
	# beta = np.array([0.846509,0.180725,2.26596,1.28211,1.62290,2.42297])
	# lamda = np.array([0.394732,0.550920,0.387365,0.436819,0.405553,1.26131])

	# #let's calculate Sk
	# Nk = 1.16*data.Den/5.0
	# V = np.sqrt(data.Vx**2 + data.Vy**2 + data.Vz**2)
	# Vk = V/400.0
	# Bsk = np.abs(data.Bz.clip(max=0.0))/5.0
	
	# Sk1 = Nk**gamma[0] * Vk**beta[0] * Bsk**lamda[0]
	# Sk2 = Nk**gamma[1] * Vk**beta[1] * Bsk**lamda[1]
	# Sk3 = Nk**gamma[2] * Vk**beta[2] * Bsk**lamda[2]
	# Sk4 = Nk**gamma[3] * Vk**beta[3] * Bsk**lamda[3]
	# Sk5 = Nk**gamma[4] * Vk**beta[4] * Bsk**lamda[4]
	# Sk6 = Nk**gamma[5] * Vk**beta[5] * Bsk**lamda[5]


	# #get the intervals valid for the TS05 model
	# print('Scanning for valid TS05 intervals')
	# beg,end = _ScanTS05Intervals2(data)
	
	# #calculate flow speed
	# V = np.sqrt(data.Vx**2 + data.Vy**2 + data.Vz**2)
	
	# #loop through each interval
	# ni = len(beg)
	# for i in range(0,ni):
		# iB = beg[i]
		# iE = end[i]
		# print('Processing interval {0} of {1} ({2} to {3})'.format(i+1,ni,iB,iE))

		
		# for j in range(iB,iE):
			# print('\r{:5.1f}%'.format(100.0*(j+1-iB)/(iE-iB)),end='')
			# # W1 = 0.0
			# # W2 = 0.0
			# # W3 = 0.0
			# # W4 = 0.0
			# # W5 = 0.0
			# # W6 = 0.0

			# # Key1 = 1
			# # Key2 = 1
			# # Key3 = 1
			# # Key4 = 1
			# # Key5 = 1
			# # Key6 = 1
		
			# k = np.arange(j,iB-1,-1)
			# #print(k)
			# TAUMT = (j-k)*5.0
			# ARG1 = -TAUMT*r[0]
			# ARG2 = -TAUMT*r[1]
			# ARG3 = -TAUMT*r[2]
			# ARG4 = -TAUMT*r[3]
			# ARG5 = -TAUMT*r[4]
			# ARG6 = -TAUMT*r[5]
			
			# W1 = np.cumsum(Sk1[k])*np.exp(ARG1)
			# W2 = np.cumsum(Sk2[k])*np.exp(ARG2)
			# W3 = np.cumsum(Sk3[k])*np.exp(ARG3)
			# W4 = np.cumsum(Sk4[k])*np.exp(ARG4)
			# W5 = np.cumsum(Sk5[k])*np.exp(ARG5)
			# W6 = np.cumsum(Sk6[k])*np.exp(ARG6)
			
			# Key1 = ARG1 >= -10.0
			# Key2 = ARG2 >= -10.0
			# Key3 = ARG3 >= -10.0
			# Key4 = ARG4 >= -10.0
			# Key5 = ARG5 >= -10.0
			# Key6 = ARG6 >= -10.0
		
			# k1 = np.max(np.append(k[0],np.where(Key1)[0])) - k[0]
			# k2 = np.max(np.append(k[0],np.where(Key2)[0])) - k[0]
			# k3 = np.max(np.append(k[0],np.where(Key3)[0])) - k[0]
			# k4 = np.max(np.append(k[0],np.where(Key4)[0])) - k[0]
			# k5 = np.max(np.append(k[0],np.where(Key5)[0])) - k[0]
			# k6 = np.max(np.append(k[0],np.where(Key6)[0])) - k[0]

		
			# # for k in range(j,iB,-1):
				# # TAUMT = (j - k)*5.0


				# # ARG1 = -TAUMT*r[0]
				# # ARG2 = -TAUMT*r[1]
				# # ARG3 = -TAUMT*r[2]
				# # ARG4 = -TAUMT*r[3]
				# # ARG5 = -TAUMT*r[4]
				# # ARG6 = -TAUMT*r[5]


				# # if (ARG1 > -10.0) and (Key1 == 1):
					# # W1 = W1 + Sk1[k]*np.exp(ARG1)
				# # else:
					# # Key1 = 0
				# # if (ARG2 > -10.0) and (Key2 == 1):
					# # W2 = W2 + Sk2[k]*np.exp(ARG2)
				# # else:
					# # Key2 = 0
				# # if (ARG3 > -10.0) and (Key3 == 1):
					# # W3 = W3 + Sk3[k]*np.exp(ARG3)
				# # else:
					# # Key3 = 0
				# # if (ARG4 > -10.0) and (Key4 == 1):
					# # W4 = W4 + Sk4[k]*np.exp(ARG4)
				# # else:
					# # Key4 = 0
				# # if (ARG5 > -10.0) and (Key5 == 1):
					# # W5 = W5 + Sk5[k]*np.exp(ARG5)
				# # else:
					# # Key5 = 0
				# # if (ARG6 > -10.0) and (Key6 == 1):
					# # W6 = W6 + Sk6[k]*np.exp(ARG6)
				# # else:
					# # Key6 = 0	
				# # if Key1 == 0 and Key2 == 0 and Key3 == 0 and Key4 == 0 and Key5 == 0 and Key6 == 0:
					# # break
			# # data.W1[j] = W1*r[0]*5
			# # data.W2[j] = W2*r[1]*5
			# # data.W3[j] = W3*r[2]*5
			# # data.W4[j] = W4*r[3]*5
			# # data.W5[j] = W5*r[4]*5
			# # data.W6[j] = W6*r[5]*5
			# data.W1[j] = W1[k1]*r[0]*5
			# data.W2[j] = W2[k2]*r[1]*5
			# data.W3[j] = W3[k3]*r[2]*5
			# data.W4[j] = W4[k4]*r[3]*5
			# data.W5[j] = W5[k5]*r[4]*5
			# data.W6[j] = W6[k6]*r[5]*5
		# print()
		
def _GetWParameters(data):
	'''
	Tried to fill in the W parameters using Tsyganenko's code, though I
	don't think it produces the same numbers - use with caution!
	
	'''
	n = np.int32(data.size)
	S = np.array(data.SymH).astype('float32')
	Bz = np.array(data.Bz).astype('float32')
	SWF = np.array(data.ISWFlag).astype('int32')
	IMF = np.array(data.IMFFlag).astype('int32')
	V = np.sqrt(data.Vx**2 + data.Vy**2 + data.Vz**2).astype('float32')
	Den = np.array(data.Den).astype('float32')
	
	W1 = np.zeros(data.size,dtype='float32')
	W2 = np.zeros(data.size,dtype='float32')
	W3 = np.zeros(data.size,dtype='float32')
	W4 = np.zeros(data.size,dtype='float32')
	W5 = np.zeros(data.size,dtype='float32')
	W6 = np.zeros(data.size,dtype='float32')
	
	_CCalculateW(n,S,Bz,SWF,IMF,V,Den,W1,W2,W3,W4,W5,W6)
	
	data.W1 = W1
	data.W2 = W2
	data.W3 = W3
	data.W4 = W4
	data.W5 = W5
	data.W6 = W6
	
	

def _LoadData(years):

	try:
		import pyomnidata as pod
		import kpindex
	except:
		print('Please install the following packages in order to create the parameter file: "kpindex" and "pyomnidata"')
		return 

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

	return data,kp
	
def _CalculateParameters(SkipWParameters=True):
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
	data,kp = _LoadData(years)

	#get the dipole tilt angle
	print('Calculating dipole tilt angles')
	data.Tilt = GetDipoleTilt(data.Date,data.ut,(data.Vx,data.Vy,data.Vz))
	
	#fill in the Kp index
	data.Kp = _FillInKp(data.Date,data.ut,kp)
	
	#calculate G1 and G2
	data.G1,data.G2 = _GetGParameters(data)

	#calculate W parameters
	if SkipWParameters:
		data.W1 = 0.0
		data.W2 = 0.0
		data.W3 = 0.0
		data.W4 = 0.0
		data.W5 = 0.0
		data.W6 = 0.0
	else:
		_GetWParameters(data)
	
	
	return data



