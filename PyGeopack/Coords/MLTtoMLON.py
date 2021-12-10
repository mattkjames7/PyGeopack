import numpy as np
from ._CFunctions import _CMLTtoMLONUT
from ..ct import ctInt,ctIntPtr,ctFloatPtr,ctDoublePtr



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
	_MLT = ctDoublePtr(MLT)
	_n = ctInt(_MLT.size)
	if V is None:
		Vx = np.zeros(_n) + np.nan
		Vy = np.zeros(_n) + np.nan
		Vz = np.zeros(_n) + np.nan
	else:
		Vx = np.zeros(_n) + V[0]
		Vy = np.zeros(_n) + V[1]
		Vz = np.zeros(_n) + V[2]
	_Vx = ctDoublePtr(Vx)
	_Vy = ctDoublePtr(Vy)
	_Vz = ctDoublePtr(Vz)
	_Date = ctIntPtr(np.zeros(_n) + Date)
	_ut = ctFloatPtr(np.zeros(_n) + ut)
	
	_MLon = np.zeros(_n,dtype="float64")
				

	_CMLTtoMLONUT(_MLT, _n, _Vx, _Vy, _Vz, _Date, _ut, _MLon)

	return _MLon
