import numpy as np
from ._CFunctions import _CSetCustParam

###### File created automatically using PopulateCtypes ######

def SetCustParam(iopt, parmod, tilt, Vx, Vy, Vz):

	if iopt is None:
		iopt = 1.0
	if parmod is None:
		parmod = np.array([2.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],dtype='float32')
	if tilt is None:
		tilt = np.nan
	if Vx is None:
		Vx = -400.0
	if Vy is None:
		Vy = 0.0
	if Vz is None:
		Vz = 0.0
		
	#Convert input variables to appropriate numpy dtype:
	_iopt = np.int32(iopt)
	_parmod = np.array(parmod).astype("float32")
	_tilt = np.float32(tilt)
	_Vx = np.float32(Vx)
	_Vy = np.float32(Vy)
	_Vz = np.float32(Vz)
	_CSetCustParam(_iopt, _parmod, _tilt, _Vx, _Vy, _Vz)

