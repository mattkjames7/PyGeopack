import numpy as np
from ._CFunctions import _CMLTtoMLONUT


def MLTtoMLON(MLT, date, UT):
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
	_MLT = np.array([MLT]).flatten().astype("float32")
	_n = np.int32(_MLT.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_MLon = np.zeros(_n,dtype="float32")
	_CMLTtoMLONUT(_MLT, _n, _date, _UT, _MLon)

	return _MLon
