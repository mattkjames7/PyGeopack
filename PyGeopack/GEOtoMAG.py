import numpy as np
from ._CFunctions import _CGEOtoMAGUT

###### File created automatically using PopulateCtypes ######

def GEOtoMAG(Lon, Lat, Date, ut, V=None):
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
	_Lon = np.array([Lon]).flatten().astype("float64")
	_Lat = np.array([Lat]).flatten().astype("float64")
	_n = np.int32(_Lon.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_MLon = np.zeros(n,dtype="float64")
	_MLat = np.zeros(n,dtype="float64")

	#make velocity arrays
	if V is None:
		_Vx = np.nan
		_Vy = np.nan
		_Vz = np.nan
	else:
		_Vx = np.float32(V[0])
		_Vy = np.float32(V[1])
		_Vz = np.float32(V[2])
				

	_CGEOtoMAGUT(_Lon, _Lat, _n, _Vx, _Vy, _Vz, _date, _UT, _MLon, _MLat)

	return _MLon,_MLat
