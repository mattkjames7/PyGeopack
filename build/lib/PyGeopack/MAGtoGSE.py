import numpy as np
from ._CFunctions import _CMAGtoGSEUT


def MAGtoGSE(Xin, Yin, Zin, Date, ut, V=None):
	'''
	Converts from Cartesian MAG to GSE coordinates.
	
	Inputs
	======
	Xin	: Array or scalar of x coordinates in R_E.
	Yin	: Array or scalar of y coordinates in R_E.
	Zin	: Array or scalar of z coordinates in R_E.
	Date	: Integer dat in format yyyymmdd.
	ut	: Time in hours (ut = hh + mm/60 + ss/3600).	
	
	Returns
	=======
	Xout,You,Zout	: x, y and z coordinates in R_E
	
	'''
	#Convert input variables to appropriate numpy dtype:
	_Xin = np.array([Xin]).flatten().astype("float64")
	_Yin = np.array([Yin]).flatten().astype("float64")
	_Zin = np.array([Zin]).flatten().astype("float64")
	_n = np.int32(_Xin.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_Xout = np.zeros(_n,dtype="float64")
	_Yout = np.zeros(_n,dtype="float64")
	_Zout = np.zeros(_n,dtype="float64")

	#make velocity arrays
	if V is None:
		_Vx = np.nan
		_Vy = np.nan
		_Vz = np.nan
	else:
		_Vx = np.float32(V[0])
		_Vy = np.float32(V[1])
		_Vz = np.float32(V[2])
				

	_CMAGtoGSEUT(_Xin, _Yin, _Zin, _n, _Vx, _Vy, _Vz, _date, _UT, _Xout, _Yout, _Zout)

	return _Xout,_Yout,_Zout
