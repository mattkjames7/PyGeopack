import numpy as np
from ._CFunctions import _CModelField
from .SetCustParam import SetCustParam
import ctypes
###### File created automatically using PopulateCtypes ######

def ModelField(Xin, Yin, Zin, Date, ut, Model='T96', CoordIn='GSM', CoordOut='GSM',iopt=None,parmod=None,tilt=None,Vx=None,Vy=None,Vz=None):

	if ('c' in Model):
		SetCustParam(iopt,parmod,tilt,Vx,Vy,Vz)
		
	if CoordIn is 'GSE':
		CoordIn = 1
	elif CoordIn is 'GSM':
		CoordIn = 2
	elif CoordIn is 'SM':
		CoordIn = 3
	else:
		print('invalid choice for CoordIn')
		return

	if CoordOut is 'GSE':
		CoordOut = 1
	elif CoordOut is 'GSM':
		CoordOut = 2
	elif CoordOut is 'SM':
		CoordOut = 3
	else:
		print('invalid choice for CoordOut')
		return
		

	#Convert input variables to appropriate numpy dtype:
	_Xin = np.array(Xin).astype("float32")
	_Yin = np.array(Yin).astype("float32")
	_Zin = np.array(Zin).astype("float32")
	_n = np.int32(_Xin.size)
	_Date = np.int32(Date)
	_ut = np.float32(ut)
	_Model = ctypes.c_char_p(Model.encode('utf-8'))
	_CoordIn = np.int32(CoordIn)
	_CoordOut = np.int32(CoordOut)
	_Bx = np.zeros(_n,dtype="float32")
	_By = np.zeros(_n,dtype="float32")
	_Bz = np.zeros(_n,dtype="float32")
	_CModelField(_Xin, _Yin, _Zin, _n, _Date, _ut, _Model, _CoordIn, _CoordOut, _Bx, _By, _Bz)

	return _Bx,_By,_Bz
