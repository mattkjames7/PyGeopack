import numpy as np
from ._CFunctions import _CMLONtoMLTUT
from ._CTConv import _CTConv


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
	_MLon = _CTConv(MLon,'c_double_ptr')
	_n = _CTConv(_MLon.size,'c_int')
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
	
	_MLT = np.zeros(_n,dtype="float64")
				

	_CMLONtoMLTUT(_MLon, _n, _Vx, _Vy, _Vz, _Date, _ut, _MLT)

	return _MLT
