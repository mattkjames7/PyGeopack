import numpy as np
from ._CFunctions import _CMLTtoMLONUT

###### File created automatically using PopulateCtypes ######

def MLTtoMLONUT(MLT, date, UT):

	#Convert input variables to appropriate numpy dtype:
	_MLT = np.array(MLT).astype("float32")
	_n = np.int32(_MLT.size)
	_date = np.int32(date)
	_UT = np.float32(UT)
	_MLon = np.zeros(_n,dtype="float32")
	_CMLTtoMLONUT(_MLT, _n, _date, _UT, _MLon)

	return _MLon
