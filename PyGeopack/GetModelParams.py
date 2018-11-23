import numpy as np
from ._CFunctions import _CGetModelParams
import ctypes

###### File created automatically using PopulateCtypes ######

def GetModelParams(Date, ut, Model):

	#Convert input variables to appropriate numpy dtype:
	_Date = np.int32(Date)
	_ut = np.float32(ut)
	_Model = ctypes.c_char_p(Model.encode('utf-8'))
	_iopt = np.zeros(1,dtype="int32")
	_parmod = np.zeros(10,dtype="float32")
	_tilt = np.zeros(1,dtype="float32")
	_Vx = np.zeros(1,dtype="float32")
	_Vy = np.zeros(1,dtype="float32")
	_Vz = np.zeros(1,dtype="float32")
	_CGetModelParams(_Date, _ut, _Model, _iopt, _parmod, _tilt, _Vx, _Vy, _Vz)

	return _iopt[0],_parmod,_tilt[0],_Vx[0],_Vy[0],_Vz[0]
