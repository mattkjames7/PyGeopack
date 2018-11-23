import numpy as np
from ._CFunctions import _CGSMtoGSEUT

###### File created automatically using PopulateCtypes ######

def GSMtoGSEUT(Xin, Yin, Zin, date, UT):

	#Convert input variables to appropriate numpy dtype:
	_Xin = np.array(Xin).astype("float32")
	_Yin = np.array(Yin).astype("float32")
	_Zin = np.array(Zin).astype("float32")
	_n = np.int32(_Xin.size)
	_date = np.int32(date)
	_UT = np.float32(UT)
	_Xout = np.zeros(_n,dtype="float32")
	_Yout = np.zeros(_n,dtype="float32")
	_Zout = np.zeros(_n,dtype="float32")
	_CGSMtoGSEUT(_Xin, _Yin, _Zin, _n, _date, _UT, _Xout, _Yout, _Zout)

	return _Xout,_Yout,_Zout
