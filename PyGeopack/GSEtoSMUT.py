import numpy as np
from ._CFunctions import _CGSEtoSMUT

###### File created automatically using PopulateCtypes ######

def GSEtoSMUT(Xin, Yin, Zin, date, UT):

	#Convert input variables to appropriate numpy dtype:
	_n = np.int32(np.size(Xin))
	_date = np.int32(date)
	_UT = np.float32(UT)
	_Xout = np.zeros(_n,dtype="float32")
	_Yout = np.zeros(_n,dtype="float32")
	_Zout = np.zeros(_n,dtype="float32")	

	if np.size(date) > 1 or np.size(UT) > 1:
		for i in range(0,_n):
			_Xin = np.array([Xin[i]]).astype('float32')
			_Yin = np.array([Yin[i]]).astype('float32')
			_Zin = np.array([Zin[i]]).astype('float32')
			tmpX = np.zeros((1,),dtype='float32')
			tmpY = np.zeros((1,),dtype='float32')
			tmpZ = np.zeros((1,),dtype='float32')
			_CGSEtoSMUT(_Xin, _Yin, _Zin, 1, _date[i], _UT[i], tmpX, tmpY, tmpZ)
			_Xout[i] = tmpX[0]
			_Yout[i] = tmpY[0]
			_Zout[i] = tmpZ[0]
	else:
		_Xin = np.array(Xin).astype("float32")
		_Yin = np.array(Yin).astype("float32")
		_Zin = np.array(Zin).astype("float32")


		_CGSEtoSMUT(_Xin, _Yin, _Zin, _n, _date, _UT, _Xout, _Yout, _Zout)

	return _Xout,_Yout,_Zout
