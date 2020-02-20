import numpy as np
from ._CFunctions import _CMLTtoMLONUT


def MLTtoMLON(MLT, date, UT, V=None):
	'''
	Converts from magnetic local time to magnetic longitude.
	
	Inputs
	======
	MLT	: Array or scalar of magnetic local time(s) in hours.
	Date	: Integer dat in format yyyymmdd.
	ut	: Time in hours (ut = hh + mm/60 + ss/3600).
	
	Returns
	=======
	MLon	: array of magnetic longitudes.
	
	'''
	#Convert input variables to appropriate numpy dtype:
	_MLT = np.array([MLT]).flatten().astype("float64")
	_n = np.int32(_MLT.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_MLon = np.zeros(_n,dtype="float64")

	#make velocity arrays
	if V is None:
		_Vx = np.nan
		_Vy = np.nan
		_Vz = np.nan
	else:
		_Vx = np.float32(V[0])
		_Vy = np.float32(V[1])
		_Vz = np.float32(V[2])
				

	_CMLTtoMLONUT(_MLT, _n, _Vx, _Vy, _Vz, _date, _UT, _MLon)

	return _MLon
