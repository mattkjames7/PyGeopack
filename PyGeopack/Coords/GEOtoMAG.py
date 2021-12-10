import numpy as np
from ._CFunctions import _CGEOtoMAGUT,_CGEOtoMAGUT_LL
from ..ct import ctInt,ctIntPtr,ctFloatPtr,ctDoublePtr



def GEOtoMAGLL(Lon, Lat, Date, ut, V=None):
	'''
	Converts from geographic to magnetic longitude and latitude.
	
	Inputs
	======
	Lon	: Array or scalar of longitude(s), in degrees.
	Lat	: Array or scalar of latitude(s), in degrees.
	Date	: Integer dat in format yyyymmdd.
	ut	: Time in hours (ut = hh + mm/60 + ss/3600).
	
	Returns
	=======
	Mlon	: Magnetic longitude.
	Mlat	: Magnetic latitude.
	
	'''
	
	#Convert input variables to appropriate numpy dtype:
	_Lon = ctDoublePtr(Lon)
	_Lat = ctDoublePtr(Lat)
	_n = ctInt(_Lon.size)
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
	_MLat = np.zeros(_n,dtype="float64")
				

	_CGEOtoMAGUT_LL(_Lon, _Lat, _n, _Vx, _Vy, _Vz, _Date, _ut, _MLon, _MLat)

	return _MLon,_MLat
	
def GEOtoMAG(Xin, Yin, Zin, Date, ut, V=None):
	'''
	Converts from geographic to magnetic longitude and latitude.
	
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
	_Xin = ctDoublePtr(Xin)
	_Yin = ctDoublePtr(Yin)
	_Zin = ctDoublePtr(Zin)
	_n = ctInt(_Xin.size)
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
	
	_Xout = np.zeros(_n,dtype="float64")
	_Yout = np.zeros(_n,dtype="float64")
	_Zout = np.zeros(_n,dtype="float64")


	_CGEOtoMAGUT(_Xin, _Yin, _Zin, _n, _Vx, _Vy, _Vz, _Date, _ut, _Xout, _Yout, _Zout)

	return _Xout, _Yout, _Zout
