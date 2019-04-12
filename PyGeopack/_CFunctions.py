import numpy as np
import ctypes as ct
import os
import platform
from . import Globals

Arch = platform.architecture()[0]
libgeopack = ct.CDLL(os.path.dirname(__file__)+"/__data/libgeopack/libgeopack.so")


_CGSEtoGSMUT = libgeopack.GSEtoGSMUT
_CGSEtoGSMUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGSEtoGSMUT.restype = None

_CGSMtoGSEUT = libgeopack.GSMtoGSEUT
_CGSMtoGSEUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGSMtoGSEUT.restype = None

_CGSMtoSMUT = libgeopack.GSMtoSMUT
_CGSMtoSMUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGSMtoSMUT.restype = None

_CSMtoGSMUT = libgeopack.SMtoGSMUT
_CSMtoGSMUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CSMtoGSMUT.restype = None

_CGSEtoSMUT = libgeopack.GSEtoSMUT
_CGSEtoSMUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGSEtoSMUT.restype = None

_CGSEtoMAGUT = libgeopack.GSEtoMAGUT
_CGSEtoMAGUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGSEtoMAGUT.restype = None

_CSMtoGSEUT = libgeopack.SMtoGSEUT
_CSMtoGSEUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CSMtoGSEUT.restype = None

_CMAGtoGSEUT = libgeopack.MAGtoGSEUT
_CMAGtoGSEUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CMAGtoGSEUT.restype = None

_CMLONtoMLTUT = libgeopack.MLONtoMLTUT
_CMLONtoMLTUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CMLONtoMLTUT.restype = None

_CMLTtoMLONUT = libgeopack.MLTtoMLONUT
_CMLTtoMLONUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CMLTtoMLONUT.restype = None

_CGEOtoMAGUT = libgeopack.GEOtoMAGUT
_CGEOtoMAGUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGEOtoMAGUT.restype = None

_CMAGtoGEOUT = libgeopack.MAGtoGEOUT
_CMAGtoGEOUT.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CMAGtoGEOUT.restype = None

_CLoadTSData = libgeopack.LoadTSData
_CLoadTSData.argtypes = []
_CLoadTSData.restype = None

_CFreeTSData = libgeopack.FreeTSData
_CFreeTSData.argtypes = []
_CFreeTSData.restype = None

_CSetCustParam = libgeopack.SetCustParam
_CSetCustParam.argtypes = [ct.c_int, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_float, ct.c_float, ct.c_float, ct.c_float]
_CSetCustParam.restype = None

_CGetModelParams = libgeopack.GetModelParams
_CGetModelParams.argtypes = [ct.c_int, ct.c_float, ct.c_char_p, np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGetModelParams.restype = None

_CModelField = libgeopack.ModelField
_CModelField.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, ct.c_int, ct.c_float, ct.c_char_p, ct.c_int, ct.c_int, np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CModelField.restype = None


_CTraceField = libgeopack.TraceField
_CTraceField.argtypes = [np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_int, np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_char_p, ct.c_int, ct.c_int, ct.c_float, ct.c_int, ct.c_float,  np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), ct.c_bool]
_CTraceField.restype = None

_CInit = libgeopack.Init
_CInit.argtypes = [ct.c_char_p]

