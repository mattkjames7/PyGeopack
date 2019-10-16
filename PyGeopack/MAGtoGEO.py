import numpy as np
from ._CFunctions import _CMAGtoGEOUT

###### File created automatically using PopulateCtypes ######

def MAGtoGEO(MLon, MLat, Date, ut):
	'''
	Converts from magnetic to geographic longitude and latitude.
	
	Inputs
	======
	MLon	: Array or scalar of magnetic longitude(s), in degrees.
	MLat	: Array or scalar of magnetic latitude(s), in degrees.
	Date	: Integer dat in format yyyymmdd.
	ut	: Time in hours (ut = hh + mm/60 + ss/3600).
	
	Returns
	=======
	lon	: Geographic longitude.
	lat	: Geographic latitude.
	
	'''
	#Convert input variables to appropriate numpy dtype:
	_MLon = np.array([MLon]).flatten().astype("float32")
	_MLat = np.array([MLat]).flatten().astype("float32")
	_n = np.int32(_MLon.size)
	_date = np.int32(Date)
	_UT = np.float32(ut)
	_Lon = np.zeros(_n,dtype="float32")
	_Lat = np.zeros(_n,dtype="float32")
	_CMAGtoGEOUT(_MLon, _MLat, _n, _date, _UT, _Lon, _Lat)

	return _Lon,_Lat
