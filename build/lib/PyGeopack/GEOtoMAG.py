import numpy as np
from ._CFunctions import _CGEOtoMAGUT

###### File created automatically using PopulateCtypes ######

def GEOtoMAG(Lon, Lat, Date, ut):
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
	_Lon = np.array([Lon]).flatten().astype("float32")
	_Lat = np.array([Lat]).flatten().astype("float32")
	_n = np.int32(_Lon.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_MLon = np.zeros(n,dtype="float32")
	_MLat = np.zeros(n,dtype="float32")
	_CGEOtoMAGUT(_Lon, _Lat, _n, _date, _UT, _MLon, _MLat)

	return _MLon,_MLat
