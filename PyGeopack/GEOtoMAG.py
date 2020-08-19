import numpy as np
from ._CFunctions import _CGEOtoMAGUT,_CGEOtoMAGUT_LL
from ._CTConv import _CTConv


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
	_Lon = _CTConv(Lon,'c_double_ptr')
	_Lat = _CTConv(Lat,'c_double_ptr')
	_n = _CTConv(_Lon.size,'c_int')
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
	_Xin = _CTConv(Xin,'c_double_ptr')
	_Yin = _CTConv(Yin,'c_double_ptr')
	_Zin = _CTConv(Zin,'c_double_ptr')
	_n = _CTConv(_Xin.size,'c_int')
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
	
	_Xout = np.zeros(_n,dtype="float64")
	_Yout = np.zeros(_n,dtype="float64")
	_Zout = np.zeros(_n,dtype="float64")


	_CGEOtoMAGUT(_Xin, _Yin, _Zin, _n, _Vx, _Vy, _Vz, _Date, _ut, _Xout, _Yout, _Zout)

	return _Xout, _Yout, _Zout
