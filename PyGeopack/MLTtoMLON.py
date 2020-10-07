import numpy as np
from ._CFunctions import _CMLTtoMLONUT
from ._CTConv import _CTConv


def MLTtoMLON(MLT, Date, ut, V=None):
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
	_MLT = _CTConv(MLT,'c_double_ptr')
	_n = _CTConv(_MLT.size,'c_int')
	if V is None:
		Vx = np.zeros(_n) + np.nan
		Vy = np.zeros(_n) + np.nan
		Vz = np.zeros(_n) + np.nan
	else:
		Vx = np.zeros(_n) + V[0]
		Vy = np.zeros(_n) + V[1]
		Vz = np.zeros(_n) + V[2]
	_Vx = _CTConv(Vx,'c_double_ptr')
	_Vy = _CTConv(Vy,'c_double_ptr')
	_Vz = _CTConv(Vz,'c_double_ptr')
	_Date = _CTConv(np.zeros(_n) + Date,'c_int_ptr')
	_ut = _CTConv(np.zeros(_n) + ut,'c_float_ptr')
	
	_MLon = np.zeros(_n,dtype="float64")
				

	_CMLTtoMLONUT(_MLT, _n, _Vx, _Vy, _Vz, _Date, _ut, _MLon)

	return _MLon
