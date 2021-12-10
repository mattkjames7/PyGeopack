import numpy as np
from ._CFunctions import _CMLONtoMLTUT
from ..ct import ctInt,ctIntPtr,ctFloatPtr,ctDoublePtr


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
	_MLon = ctDoublePtr(MLon)
	_n = ctInt(_MLon.size)
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
	
	_MLT = np.zeros(_n,dtype="float64")
				

	_CMLONtoMLTUT(_MLon, _n, _Vx, _Vy, _Vz, _Date, _ut, _MLT)

	return _MLT
