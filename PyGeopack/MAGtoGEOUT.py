import numpy as np
from ._CFunctions import _CMAGtoGEOUT

###### File created automatically using PopulateCtypes ######

def MAGtoGEOUT(MLon, MLat, date, UT):

	#Convert input variables to appropriate numpy dtype:
	_MLon = np.array(MLon).astype("float32")
	_MLat = np.array(MLat).astype("float32")
	_n = np.int32(_MLon.size)
	_date = np.int32(date)
	_UT = np.float32(UT)
	_Lon = np.zeros(_n,dtype="float32")
	_Lat = np.zeros(_n,dtype="float32")
	_CMAGtoGEOUT(_MLon, _MLat, _n, _date, _UT, _Lon, _Lat)

	return _Lon,_Lat
