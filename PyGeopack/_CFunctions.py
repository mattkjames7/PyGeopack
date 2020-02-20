import numpy as np
import ctypes as ct
import os
import platform
from . import Globals

Arch = platform.architecture()[0]
libgeopack = ct.CDLL(os.path.dirname(__file__)+"/__data/libgeopackdp/libgeopackdp.so")

fptr = np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")
dptr = np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")
iptr = np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS")
bptr = np.ctypeslib.ndpointer(ct.c_bool,flags="C_CONTIGUOUS")

_CGSEtoGSMUT = libgeopack.GSEtoGSMUT
_CGSEtoGSMUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CGSEtoGSMUT.restype = None

_CGSMtoGSEUT = libgeopack.GSMtoGSEUT
_CGSMtoGSEUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CGSMtoGSEUT.restype = None

_CGSMtoSMUT = libgeopack.GSMtoSMUT
_CGSMtoSMUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CGSMtoSMUT.restype = None

_CSMtoGSMUT = libgeopack.SMtoGSMUT
_CSMtoGSMUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CSMtoGSMUT.restype = None

_CGSEtoSMUT = libgeopack.GSEtoSMUT
_CGSEtoSMUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CGSEtoSMUT.restype = None

_CGSEtoMAGUT = libgeopack.GSEtoMAGUT
_CGSEtoMAGUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CGSEtoMAGUT.restype = None

_CSMtoGSEUT = libgeopack.SMtoGSEUT
_CSMtoGSEUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CSMtoGSEUT.restype = None

_CMAGtoGSEUT = libgeopack.MAGtoGSEUT
_CMAGtoGSEUT.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr, dptr]
_CMAGtoGSEUT.restype = None

_CMLONtoMLTUT = libgeopack.MLONtoMLTUT
_CMLONtoMLTUT.argtypes = [dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr]
_CMLONtoMLTUT.restype = None

_CMLTtoMLONUT = libgeopack.MLTtoMLONUT
_CMLTtoMLONUT.argtypes = [dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr]
_CMLTtoMLONUT.restype = None

_CGEOtoMAGUT = libgeopack.GEOtoMAGUT
_CGEOtoMAGUT.argtypes = [dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr]
_CGEOtoMAGUT.restype = None

_CMAGtoGEOUT = libgeopack.MAGtoGEOUT
_CMAGtoGEOUT.argtypes = [dptr, dptr, ct.c_int, ct.c_double, ct.c_double, ct.c_double, ct.c_int, ct.c_float, dptr, dptr]
_CMAGtoGEOUT.restype = None

_CLoadTSData = libgeopack.LoadTSData
_CLoadTSData.argtypes = []
_CLoadTSData.restype = None

_CFreeTSData = libgeopack.FreeTSData
_CFreeTSData.argtypes = []
_CFreeTSData.restype = None

_CSetCustParam = libgeopack.SetCustParam
_CSetCustParam.argtypes = [ct.c_int, fptr, ct.c_float, ct.c_float, ct.c_float, ct.c_float]
_CSetCustParam.restype = None

_CGetModelParams = libgeopack.GetModelParams
_CGetModelParams.argtypes = [ct.c_int, ct.c_float, ct.c_char_p, iptr, dptr, dptr, dptr, dptr, dptr]
_CGetModelParams.restype = None

_CModelField = libgeopack.ModelField
_CModelField.argtypes = [dptr, dptr, dptr, ct.c_int, ct.c_int, ct.c_float, ct.c_char_p, ct.c_int, ct.c_int, dptr, dptr, dptr]
_CModelField.restype = None


_CTraceField = libgeopack.TraceField
_CTraceField.argtypes = [dptr, dptr, dptr, ct.c_int, iptr, fptr, ct.c_char_p,
						ct.c_int, ct.c_int, ct.c_double, ct.c_int, ct.c_double,
						dptr, dptr, dptr, dptr, dptr, dptr, dptr, dptr, dptr, 
						iptr, dptr, ct.c_bool]
_CTraceField.restype = None

_CInit = libgeopack.Init
_CInit.argtypes = [ct.c_char_p]

_CGetDipoleTilt = libgeopack.GetDipoleTiltUT
_CGetDipoleTilt.argtypes = [ct.c_int,ct.c_float,ct.c_double,ct.c_double,ct.c_double]
_CGetDipoleTilt.restype = ct.c_double


_CFindIntervals = libgeopack.FindIntervals
_CFindIntervals.argtypes = [ct.c_int,fptr,fptr,iptr,iptr,iptr,iptr,iptr]
_CFindIntervals.restype = None


_CCalculateW = libgeopack.CalculateW
_CCalculateW.argtypes = [ct.c_int,fptr,fptr,iptr,iptr,fptr,fptr,fptr,fptr,fptr,fptr,fptr,fptr] 
_CCalculateW.restype = None

_CFillInKp = libgeopack.FillInKp
_CFillInKp.argtypes = [ct.c_int,iptr,fptr,fptr,fptr,ct.c_int,iptr,fptr,fptr]
_CFillInKp.restype = None

_CCalculateG = libgeopack.CalculateG
_CCalculateG.argtypes = [ct.c_int,fptr,fptr,fptr,bptr,fptr,fptr] 
_CCalculateG.restype = None
