import numpy as np
import ctypes as ct
import os
import platform
from . import Globals

Arch = platform.architecture()[0]
try:
	libgeopack = ct.CDLL(os.path.dirname(__file__)+"/__data/libgeopackdp/libgeopackdp.so")
except:
	print('importing libgeopackdb.so failed, attempting to recompile')
	path = os.path.dirname(__file__)
	if '/usr/local/' in path:
		sudo = 'sudo '
	else:
		sudo = ''

	CWD = os.getcwd()
	os.chdir(os.path.dirname(__file__)+"/__data/libgeopackdp/")
	os.system(sudo+'make clean')
	os.system(sudo+'make')
	os.chdir(CWD)	
	libgeopack = ct.CDLL(os.path.dirname(__file__)+"/__data/libgeopackdp/libgeopackdp.so")

#define some dtypes
c_int = ct.c_int
c_float = ct.c_float
c_double = ct.c_double
c_float_ptr = np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")
c_double_ptr = np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")
c_int_ptr = np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS")
c_bool_ptr = np.ctypeslib.ndpointer(ct.c_bool,flags="C_CONTIGUOUS")

#Convert GSE to GSM coordinates
_CGSEtoGSMUT = libgeopack.GSEtoGSMUT
_CGSEtoGSMUT.argtypes = [	c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr, 		#z GSE
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSM (out)
							c_double_ptr, 		#y GSM (out)
							c_double_ptr	]	#z GSM (out)
_CGSEtoGSMUT.restype = None

_CGSMtoGSEUT = libgeopack.GSMtoGSEUT
_CGSMtoGSEUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CGSMtoGSEUT.restype = None

_CGSMtoSMUT = libgeopack.GSMtoSMUT
_CGSMtoSMUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CGSMtoSMUT.restype = None

_CSMtoGSMUT = libgeopack.SMtoGSMUT
_CSMtoGSMUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CSMtoGSMUT.restype = None

_CGSEtoSMUT = libgeopack.GSEtoSMUT
_CGSEtoSMUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CGSEtoSMUT.restype = None

_CGSEtoMAGUT = libgeopack.GSEtoMAGUT
_CGSEtoMAGUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CGSEtoMAGUT.restype = None

_CSMtoGSEUT = libgeopack.SMtoGSEUT
_CSMtoGSEUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CSMtoGSEUT.restype = None

_CMAGtoGSEUT = libgeopack.MAGtoGSEUT
_CMAGtoGSEUT.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr, c_double_ptr]
_CMAGtoGSEUT.restype = None

_CMLONtoMLTUT = libgeopack.MLONtoMLTUT
_CMLONtoMLTUT.argtypes = [c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr]
_CMLONtoMLTUT.restype = None

_CMLTtoMLONUT = libgeopack.MLTtoMLONUT
_CMLTtoMLONUT.argtypes = [c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr]
_CMLTtoMLONUT.restype = None

_CGEOtoMAGUT = libgeopack.GEOtoMAGUT
_CGEOtoMAGUT.argtypes = [c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr]
_CGEOtoMAGUT.restype = None

_CMAGtoGEOUT = libgeopack.MAGtoGEOUT
_CMAGtoGEOUT.argtypes = [c_double_ptr, c_double_ptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, c_double_ptr, c_double_ptr]
_CMAGtoGEOUT.restype = None

_CLoadTSData = libgeopack.LoadTSData
_CLoadTSData.argtypes = []
_CLoadTSData.restype = None

_CFreeTSData = libgeopack.FreeTSData
_CFreeTSData.argtypes = []
_CFreeTSData.restype = None

_CSetCustParam = libgeopack.SetCustParam
_CSetCustParam.argtypes = [ct.c_int, c_float_ptr, ct.c_float, ct.c_float, ct.c_float, ct.c_float]
_CSetCustParam.restype = None

_CGetModelParams = libgeopack.GetModelParams
_CGetModelParams.argtypes = [ct.c_int, ct.c_float, ct.c_char_p, c_int_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr]
_CGetModelParams.restype = None

_CModelField = libgeopack.ModelField
_CModelField.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, ct.c_int, ct.c_float, ct.c_char_p, ct.c_int, ct.c_int, c_double_ptr, c_double_ptr, c_double_ptr]
_CModelField.restype = None


_CTraceField = libgeopack.TraceField
_CTraceField.argtypes = [c_double_ptr, c_double_ptr, c_double_ptr, ct.c_int, c_int_ptr, c_float_ptr, ct.c_char_p,
						ct.c_int, ct.c_int, ct.c_double, ct.c_int, ct.c_double,
						c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, c_double_ptr, 
						c_int_ptr, c_double_ptr, ct.c_bool]
_CTraceField.restype = None

_CInit = libgeopack.Init
_CInit.argtypes = [ct.c_char_p]

_CGetDipoleTilt = libgeopack.GetDipoleTiltUT
_CGetDipoleTilt.argtypes = [ct.c_int,ct.c_float,ct.c_double,ct.c_double,ct.c_double]
_CGetDipoleTilt.restype = ct.c_double


_CFindIntervals = libgeopack.FindIntervals
_CFindIntervals.argtypes = [ct.c_int,c_float_ptr,c_float_ptr,c_int_ptr,c_int_ptr,c_int_ptr,c_int_ptr,c_int_ptr]
_CFindIntervals.restype = None


_CCalculateW = libgeopack.CalculateW
_CCalculateW.argtypes = [ct.c_int,c_float_ptr,c_float_ptr,c_int_ptr,c_int_ptr,c_float_ptr,c_float_ptr,c_float_ptr,c_float_ptr,c_float_ptr,c_float_ptr,c_float_ptr,c_float_ptr] 
_CCalculateW.restype = None

_CFillInKp = libgeopack.FillInKp
_CFillInKp.argtypes = [ct.c_int,c_int_ptr,c_float_ptr,c_float_ptr,c_float_ptr,ct.c_int,c_int_ptr,c_float_ptr,c_float_ptr]
_CFillInKp.restype = None

_CCalculateG = libgeopack.CalculateG
_CCalculateG.argtypes = [ct.c_int,c_float_ptr,c_float_ptr,c_float_ptr,c_bool_ptr,c_float_ptr,c_float_ptr] 
_CCalculateG.restype = None
