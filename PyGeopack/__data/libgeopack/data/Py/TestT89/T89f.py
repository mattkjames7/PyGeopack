import numpy as np
import ctypes as ct
import os

lib = ct.CDLL(os.path.dirname(__file__)+"/libt89.so")

_t89 = lib.wrap89
_t89.argtypes = [np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_t89.restype = None


def T89f(kp,psi,x,y,z):
	
	Iopt = np.array([kp+1]).astype('int32')
	ParMod = np.zeros(10,dtype='float32')
	_psi = np.array([psi],dtype='float32')
	_x = np.array([x],dtype='float32')
	_y = np.array([y],dtype='float32')
	_z = np.array([z],dtype='float32')
	_Bx = np.array([0],dtype='float32')
	_By = np.array([0],dtype='float32')
	_Bz = np.array([0],dtype='float32')
	
	_t89(Iopt,ParMod,_psi,_x,_y,_z,_Bx,_By,_Bz)
	
	return _Bx[0],_By[0],_Bz[0]
