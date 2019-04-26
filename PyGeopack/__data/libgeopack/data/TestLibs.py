import numpy as np
import ctypes as ct
import ctypes
from numpy.ctypeslib import ndpointer
import os

#load both libraries
libC = ct.CDLL(os.path.dirname(__file__)+"/cpp/libgeopack.so")
libF = ct.CDLL(os.path.dirname(__file__)+"/for/libgeopack.so")
DataFile = '/media/data1/Data/Testing/Geopack/TSdata.bin'
IGRFFile = os.path.dirname(__file__)+'/cpp/igrf12coeffs.txt'
DataFileCT = ct.c_char_p(DataFile.encode('utf-8'))
IGRFFileCT = ct.c_char_p(IGRFFile.encode('utf-8'))

#function prototypes
_CInit = libC.Init
_CInit.argtypes = [ct.c_char_p,ct.c_char_p]
_FInit = libF.Init
_FInit.argtypes = [ct.c_char_p]

_CGetModelParams = libC.GetModelParams
_CGetModelParams.argtypes = [ct.c_int, ct.c_float, ct.c_char_p, np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_CGetModelParams.restype = None
_FGetModelParams = libF.GetModelParams
_FGetModelParams.argtypes = [ct.c_int, ct.c_float, ct.c_char_p, np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"), np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_FGetModelParams.restype = None

_RecalcF = libF.recalc_08_
_RecalcF.argtypes = [	np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_RecalcF.restype = None

_RecalcC = libC.Recalc
_RecalcC.argtypes = [	ct.c_int,
						ct.c_int,
						ct.c_int,
						ct.c_int,
						ct.c_int,
						ct.c_double,
						ct.c_double,
						ct.c_double]
_RecalcC.restype = None


_GetGeopackParamsF = libF.GetGeopackParams
_GetGeopackParamsF.argtypes = [	np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
								np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_GetGeopackParamsF.restype = None
_GetGeopackParamsC = libC.GetGeopackParams
_GetGeopackParamsC.argtypes = [	np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
								np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")]
_GetGeopackParamsC.restype = None
_SetGeopackParamsC = libC.SetGeopackParams
_SetGeopackParamsC.argtypes = [	np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
								np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")]
_SetGeopackParamsC.restype = None


_IGRF_GSWF = libF.igrf_gsw_08_
_IGRF_GSWF.argtypes = [	np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_IGRF_GSWF.restype = None
_IGRF_GSWC = libC.IGRF_GSW
_IGRF_GSWC.argtypes = [	ct.c_double,
						ct.c_double,
						ct.c_double,
						np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
						np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")]
_IGRF_GSWC.restype = None


_T96C = libC.T96
_T96C.argtypes = [	ct.c_int,
					np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
					ct.c_double,
					ct.c_double,
					ct.c_double,
					ct.c_double,
					np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")]
_T96C.restype = None

_T96F = libF.t96_
_T96F.argtypes = [	np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS"),
					np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")]
_T96F.restype = None

#	void T96(int Iopt, double *ParMod, double Ps, double x, double y, double z, double *Bx, double *By, double *Bz);

#Init functions (load Tsyganenko data)
def InitF():
	_FInit(DataFileCT)
def InitC():
	_CInit(DataFileCT,IGRFFileCT)


#Get model params
def GetModelParamsF(Date=20120529, ut=5.5, Model='T96'):

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
	_FGetModelParams(_Date, _ut, _Model, _iopt, _parmod, _tilt, _Vx, _Vy, _Vz)

	return _iopt[0],_parmod,_tilt[0],_Vx[0],_Vy[0],_Vz[0]
def GetModelParamsC(Date=20120529, ut=5.5, Model='T96'):

	#Convert input variables to appropriate numpy dtype:
	_Date = np.int32(Date)
	_ut = np.float32(ut)
	_Model = ctypes.c_char_p(Model.encode('utf-8'))
	_iopt = np.zeros(1,dtype="int32")
	_parmod = np.zeros(10,dtype="float64")
	_tilt = np.zeros(1,dtype="float32")
	_Vx = np.zeros(1,dtype="float32")
	_Vy = np.zeros(1,dtype="float32")
	_Vz = np.zeros(1,dtype="float32")
	_CGetModelParams(_Date, _ut, _Model, _iopt, _parmod, _tilt, _Vx, _Vy, _Vz)

	return _iopt[0],_parmod,_tilt[0],_Vx[0],_Vy[0],_Vz[0]

#Recalc functions
def RecalcF(Year=2012,Doy=150,Hr=5,Mn=30,Sc=0,Vx=-400.0,Vy=0.0,Vz=0.0):

	_Yr = np.array([Year]).astype('int32')
	_Doy = np.array([Doy]).astype('int32')
	_Hr = np.array([Hr]).astype('int32')
	_Mn = np.array([Mn]).astype('int32')
	_Sc = np.array([Sc]).astype('int32')
	_Vx = np.array([Vx]).astype('float32')
	_Vy = np.array([Vy]).astype('float32')
	_Vz = np.array([Vz]).astype('float32')

	_RecalcF(_Yr,_Doy,_Hr,_Mn,_Sc,_Vx,_Vy,_Vz)
	
def RecalcC(Year=2012,Doy=150,Hr=5,Mn=30,Sc=0,Vx=-400.0,Vy=0.0,Vz=0.0):
	_Yr = np.array([Year]).astype('int32')[0]
	_Doy = np.array([Doy]).astype('int32')[0]
	_Hr = np.array([Hr]).astype('int32')[0]
	_Mn = np.array([Mn]).astype('int32')[0]
	_Sc = np.array([Sc]).astype('int32')[0]
	_Vx = np.array([Vx]).astype('float64')[0]
	_Vy = np.array([Vy]).astype('float64')[0]
	_Vz = np.array([Vz]).astype('float64')[0]

	_RecalcC(_Yr,_Doy,_Hr,_Mn,_Sc,_Vx,_Vy,_Vz)	
	
	
#now to compare the parameters produced when calling recalc
def GetGeopackParamsF():
	InitF()
	RecalcF()
	
	gp0 = np.zeros(34,dtype='float32')
	gp1 = np.zeros(315,dtype='float32')
	
	_GetGeopackParamsF(gp0,gp1)
	
	return gp0,gp1
def GetGeopackParamsC():
	InitC()
	RecalcC()
	
	gp0 = np.zeros(34,dtype='float64')
	gp1 = np.zeros(315,dtype='float64')
	
	_GetGeopackParamsC(gp0,gp1)
	
	return gp0.astype('float32'),gp1.astype('float32')


#compare IGRF output
def IGRFF(x=3.5,y=2.5,z=1.2):
	InitF()
	RecalcF()
	
	_x = np.array([x]).astype('float32')	
	_y = np.array([y]).astype('float32')	
	_z = np.array([z]).astype('float32')	
	_Bx = np.zeros(1,dtype='float32')
	_By = np.zeros(1,dtype='float32')
	_Bz = np.zeros(1,dtype='float32')

	_IGRF_GSWF(_x,_y,_z,_Bx,_By,_Bz)
	
	return _Bx[0],_By[0],_Bz[0]
def IGRFC(x=3.5,y=2.5,z=1.2):
	InitF()
	RecalcF()
	gp0,gp1 = GetGeopackParamsF()
	InitC()
	RecalcC()
	_SetGeopackParamsC(gp0.astype('float64'),gp1.astype('float64'))
	
	_x = np.array([x]).astype('float64')	
	_y = np.array([y]).astype('float64')	
	_z = np.array([z]).astype('float64')	
	_Bx = np.zeros(1,dtype='float64')
	_By = np.zeros(1,dtype='float64')
	_Bz = np.zeros(1,dtype='float64')

	_IGRF_GSWC(_x,_y,_z,_Bx,_By,_Bz)
	
	return _Bx[0].astype('float32'),_By[0].astype('float32'),_Bz[0].astype('float32')

#compare T96 outputs
def T96F(x=3.5,y=2.5,z=1.2):
	InitF()
	RecalcF()
	
	_iopt,_parmod,_tilt,_Vx,_Vy,_Vz = GetModelParamsF()
	
	_iopt = np.array([_iopt])
	_tilt = np.array([_tilt])
	_x = np.array([x]).astype('float32')	
	_y = np.array([y]).astype('float32')	
	_z = np.array([z]).astype('float32')	
	_Bx = np.zeros(1,dtype='float32')
	_By = np.zeros(1,dtype='float32')
	_Bz = np.zeros(1,dtype='float32')

	_T96F(_iopt,_parmod,_tilt,_x,_y,_z,_Bx,_By,_Bz)
	
	return _Bx[0],_By[0],_Bz[0]

def T96C(x=3.5,y=2.5,z=1.2):
	InitF()
	RecalcF()
	gp0,gp1 = GetGeopackParamsF()
	InitC()
	RecalcC()
	_SetGeopackParamsC(gp0.astype('float64'),gp1.astype('float64'))
	
	_iopt,_parmod,_tilt,_Vx,_Vy,_Vz = GetModelParamsC()
	
	_iopt = np.array([_iopt])
	_tilt = np.array([_tilt])
	_x = np.array([x]).astype('float64')	
	_y = np.array([y]).astype('float64')	
	_z = np.array([z]).astype('float64')	
	_Bx = np.zeros(1,dtype='float64')
	_By = np.zeros(1,dtype='float64')
	_Bz = np.zeros(1,dtype='float64')

	_T96C(_iopt,_parmod,_tilt,_x,_y,_z,_Bx,_By,_Bz)
	
	return _Bx[0].astype('float32'),_By[0].astype('float32'),_Bz[0].astype('float32')

