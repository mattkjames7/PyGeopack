import numpy as np
from ._CFunctions import _CMLONtoMLTUT

###### File created automatically using PopulateCtypes ######

def MLONtoMLTUT(MLon, date, UT):

	#Convert input variables to appropriate numpy dtype:
	_MLon = np.array(MLon).astype("float32")
	_n = np.int32(_MLon.size)
	_date = np.int32(date)
	_UT = np.float32(UT)
	_MLT = np.zeros(_n,dtype="float32")
	_CMLONtoMLTUT(_MLon, _n, _date, _UT, _MLT)

	return _MLT
