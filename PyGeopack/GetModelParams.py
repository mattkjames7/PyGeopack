import numpy as np
from ._CFunctions import _CGetModelParams
import ctypes


def GetModelParams(Date, ut, Model):
	'''
	Returns the parameters which would be used to drive a model given a
	date and a time.
	
	Inputs
	======
	Date	: Integer date, in the format yyyymmdd.
	ut	: Floating point time in hours (i.e. ut = hh + mm/60).
	Model	: String denoting which model to return parameters for out
		of the following - 'T89'|'T96'|'T01'|'TS05'.
	
	Returns
	=======
	iopt	: Integer, related to Kp - used only for T89.
	parmod	: 10 element array used for T96, T01 and TS05.
	tilt	: The dipole tilt angle in radians.
	Vx		: The x component of the solar wind velocity.
	Vy		: The y component of the solar wind velocity.
	Vz		: The z component of the solar wind velocity.
	
	'''


	#Convert input variables to appropriate numpy dtype:
	_Date = np.int32(Date)
	_ut = np.float32(ut)
	_Model = ctypes.c_char_p(Model.encode('utf-8'))
	_iopt = np.zeros(1,dtype="int32")
	_parmod = np.zeros(10,dtype="float64")
	_tilt = np.zeros(1,dtype="float64")
	_Vx = np.zeros(1,dtype="float64")
	_Vy = np.zeros(1,dtype="float64")
	_Vz = np.zeros(1,dtype="float64")
	_CGetModelParams(_Date, _ut, _Model, _iopt, _parmod, _tilt, _Vx, _Vy, _Vz)

	return _iopt[0],_parmod,_tilt[0],_Vx[0],_Vy[0],_Vz[0]
