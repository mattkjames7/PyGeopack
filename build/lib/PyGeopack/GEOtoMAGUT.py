import numpy as np
from ._CFunctions import _CGEOtoMAGUT

###### File created automatically using PopulateCtypes ######

def GEOtoMAGUT(Lon, Lat, date, UT):

	#Convert input variables to appropriate numpy dtype:
	_Lon = np.array(Lon).astype("float32")
	_Lat = np.array(Lat).astype("float32")
	_n = np.int32(_Lon.size)
	_date = np.int32(date)
	_UT = np.float32(UT)
	_MLon = np.zeros(n,dtype="float32")
	_MLat = np.zeros(n,dtype="float32")
	_CGEOtoMAGUT(_Lon, _Lat, _n, _date, _UT, _MLon, _MLat)

	return _MLon,_MLat
