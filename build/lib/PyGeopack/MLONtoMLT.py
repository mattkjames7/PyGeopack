import numpy as np
from ._CFunctions import _CMLONtoMLTUT


def MLONtoMLT(MLon, Date, ut, V=None):
	'''
	Converts from magnetic longitude to magnetic local time.
	
	Inputs
	======
	MLon	: Array or scalar of magnetic longitude(s), in degrees.
	Date	: Integer dat in format yyyymmdd.
	ut	: Time in hours (ut = hh + mm/60 + ss/3600).
	
	Returns
	=======
	MLT	: array of magnetic local times.
	
	'''
	#Convert input variables to appropriate numpy dtype:
	_MLon = np.array([MLon]).flatten().astype("float64")
	_n = np.int32(_MLon.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_MLT = np.zeros(_n,dtype="float64")

	#make velocity arrays
	if V is None:
		_Vx = np.nan
		_Vy = np.nan
		_Vz = np.nan
	else:
		_Vx = np.float32(V[0])
		_Vy = np.float32(V[1])
		_Vz = np.float32(V[2])
				

	_CMLONtoMLTUT(_MLon, _n, _Vx, _Vy, _Vz, _date, _UT, _MLT)

	return _MLT
