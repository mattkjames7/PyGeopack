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
c_char_p = ct.c_char_p
c_bool = ct.c_bool
c_int = ct.c_int
c_float = ct.c_float
c_double = ct.c_double
c_float_ptr = np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")
c_double_ptr = np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")
c_int_ptr = np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS")
c_bool_ptr = np.ctypeslib.ndpointer(ct.c_bool,flags="C_CONTIGUOUS")

#coordinate convertsion function
_CConvCoords = libgeopack.ConvCoords
_CConvCoords.argtypes = [	c_double_ptr, 		#x GSE
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
							c_double_ptr,		#z GSM (out)
							c_char_p,			#CoordIn
							c_char_p]			#CoordOut
							
_CConvCoords.restype = None


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

#Convert GSM to GSE coordinates
_CGSMtoGSEUT = libgeopack.GSMtoGSEUT
_CGSMtoGSEUT.argtypes = [	c_double_ptr,  		#x GSM
							c_double_ptr, 		#y GSM
							c_double_ptr, 		#z GSM
							c_int, 				#number of vectors
							c_double_ptr,		#SW Vx
							c_double_ptr,		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr,		#UT array
							c_double_ptr, 		#x GSE (out)
							c_double_ptr, 		#y GSE (out)
							c_double_ptr]		#z GSE (out)
_CGSMtoGSEUT.restype = None

#Convert GSM to SM coordinates
_CGSMtoSMUT = libgeopack.GSMtoSMUT
_CGSMtoSMUT.argtypes = [	c_double_ptr, 		#x GSM
							c_double_ptr, 		#y GSM
							c_double_ptr, 		#z GSM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr]		#z SM
_CGSMtoSMUT.restype = None

_CSMtoGSMUT = libgeopack.SMtoGSMUT
_CSMtoGSMUT.argtypes = [	c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr, 		#z SM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSM
							c_double_ptr, 		#y GSM
							c_double_ptr]		#z GSM
_CSMtoGSMUT.restype = None

_CGSEtoSMUT = libgeopack.GSEtoSMUT
_CGSEtoSMUT.argtypes = [	c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr, 		#z GSE
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr]		#z SM
_CGSEtoSMUT.restype = None

_CGSEtoMAGUT = libgeopack.GSEtoMAGUT
_CGSEtoMAGUT.argtypes = [	c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr, 		#z GSE
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr]		#z MAG
_CGSEtoMAGUT.restype = None

_CSMtoGSEUT = libgeopack.SMtoGSEUT
_CSMtoGSEUT.argtypes = [	c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr, 		#z SM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr]		#z GSE
_CSMtoGSEUT.restype = None

_CMAGtoGSEUT = libgeopack.MAGtoGSEUT
_CMAGtoGSEUT.argtypes = [	c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr, 		#z MAG
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr]		#z GSE
_CMAGtoGSEUT.restype = None

_CMLONtoMLTUT = libgeopack.MLONtoMLTUT
_CMLONtoMLTUT.argtypes = [	c_double_ptr,		#Mlon
							c_int, 				#number of longitudes
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr]		#MLT
_CMLONtoMLTUT.restype = None

_CMLTtoMLONUT = libgeopack.MLTtoMLONUT
_CMLTtoMLONUT.argtypes = [	c_double_ptr,		#MLT
							c_int, 				#number of local times
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr]		#MLon
_CMLTtoMLONUT.restype = None

_CGEOtoMAGUT = libgeopack.GEOtoMAGUT
_CGEOtoMAGUT.argtypes = [	c_double_ptr, 		#x GEO
							c_double_ptr, 		#y GEO
							c_double_ptr, 		#z GEO
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr]		#z MAG
_CGEOtoMAGUT.restype = None

_CMAGtoGEOUT = libgeopack.MAGtoGEOUT
_CMAGtoGEOUT.argtypes = [	c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr, 		#z MAG
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEO
							c_double_ptr, 		#y GEO
							c_double_ptr]		#z GEO
_CMAGtoGEOUT.restype = None


#Convert GEI to GEO coordinates
_CGEItoGEOUT = libgeopack.GEItoGEOUT
_CGEItoGEOUT.argtypes = [	c_double_ptr, 		#x GEI
							c_double_ptr, 		#y GEI
							c_double_ptr, 		#z GEI
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEO (out)
							c_double_ptr, 		#y GEO (out)
							c_double_ptr	]	#z GEO (out)
_CGEItoGEOUT.restype = None


#Convert GEO to GEI coordinates
_CGEOtoGEIUT = libgeopack.GEOtoGEIUT
_CGEOtoGEIUT.argtypes = [	c_double_ptr, 		#x GEO
							c_double_ptr, 		#y GEO
							c_double_ptr, 		#z GEO
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEI (out)
							c_double_ptr, 		#y GEI (out)
							c_double_ptr	]	#z GEI (out)
_CGEOtoGEIUT.restype = None


#Convert GSM to GEO coordinates
_CGSMtoGEOUT = libgeopack.GSMtoGEOUT
_CGSMtoGEOUT.argtypes = [	c_double_ptr, 		#x GSM
							c_double_ptr, 		#y GSM
							c_double_ptr, 		#z GSM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEO (out)
							c_double_ptr, 		#y GEO (out)
							c_double_ptr	]	#z GEO (out)
_CGSMtoGEOUT.restype = None


#Convert GEO to GSM coordinates
_CGEOtoGSMUT = libgeopack.GEOtoGSMUT
_CGEOtoGSMUT.argtypes = [	c_double_ptr, 		#x GEO
							c_double_ptr, 		#y GEO
							c_double_ptr, 		#z GEO
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSM (out)
							c_double_ptr, 		#y GSM (out)
							c_double_ptr	]	#z GSM (out)
_CGEOtoGSMUT.restype = None


#Convert GSE to GEO coordinates
_CGSEtoGEOUT = libgeopack.GSEtoGEOUT
_CGSEtoGEOUT.argtypes = [	c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr, 		#z GSE
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEO (out)
							c_double_ptr, 		#y GEO (out)
							c_double_ptr	]	#z GEO (out)
_CGSEtoGEOUT.restype = None


#Convert GEO to GSE coordinates
_CGEOtoGSEUT = libgeopack.GEOtoGSEUT
_CGEOtoGSEUT.argtypes = [	c_double_ptr, 		#x GEO
							c_double_ptr, 		#y GEO
							c_double_ptr, 		#z GEO
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSE (out)
							c_double_ptr, 		#y GSE (out)
							c_double_ptr	]	#z GSE (out)
_CGEOtoGSEUT.restype = None




#Convert SM to GEO coordinates
_CSMtoGEOUT = libgeopack.SMtoGEOUT
_CSMtoGEOUT.argtypes = [	c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr, 		#z SM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEO (out)
							c_double_ptr, 		#y GEO (out)
							c_double_ptr	]	#z GEO (out)
_CSMtoGEOUT.restype = None


#Convert GEO to SM coordinates
_CGEOtoSMUT = libgeopack.GEOtoSMUT
_CGEOtoSMUT.argtypes = [	c_double_ptr, 		#x GEO
							c_double_ptr, 		#y GEO
							c_double_ptr, 		#z GEO
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x SM (out)
							c_double_ptr, 		#y SM (out)
							c_double_ptr	]	#z SM (out)
_CGEOtoSMUT.restype = None



#Convert GSE to GEI coordinates
_CGSEtoGEIUT = libgeopack.GSEtoGEIUT
_CGSEtoGEIUT.argtypes = [	c_double_ptr, 		#x GSE
							c_double_ptr, 		#y GSE
							c_double_ptr, 		#z GSE
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEI (out)
							c_double_ptr, 		#y GEI (out)
							c_double_ptr	]	#z GEI (out)
_CGSEtoGEIUT.restype = None


#Convert GEI to GSE coordinates
_CGEItoGSEUT = libgeopack.GEItoGSEUT
_CGEItoGSEUT.argtypes = [	c_double_ptr, 		#x GEI
							c_double_ptr, 		#y GEI
							c_double_ptr, 		#z GEI
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSE (out)
							c_double_ptr, 		#y GSE (out)
							c_double_ptr	]	#z GSE (out)
_CGEItoGSEUT.restype = None




#Convert GSM to GEI coordinates
_CGSMtoGEIUT = libgeopack.GSMtoGEIUT
_CGSMtoGEIUT.argtypes = [	c_double_ptr, 		#x GSM
							c_double_ptr, 		#y GSM
							c_double_ptr, 		#z GSM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEI (out)
							c_double_ptr, 		#y GEI (out)
							c_double_ptr	]	#z GEI (out)
_CGSMtoGEIUT.restype = None


#Convert GEI to GSM coordinates
_CGEItoGSMUT = libgeopack.GEItoGSMUT
_CGEItoGSMUT.argtypes = [	c_double_ptr, 		#x GEI
							c_double_ptr, 		#y GEI
							c_double_ptr, 		#z GEI
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSM (out)
							c_double_ptr, 		#y GSM (out)
							c_double_ptr	]	#z GSM (out)
_CGEItoGSMUT.restype = None




#Convert SM to GEI coordinates
_CSMtoGEIUT = libgeopack.SMtoGEIUT
_CSMtoGEIUT.argtypes = [	c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr, 		#z SM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEI (out)
							c_double_ptr, 		#y GEI (out)
							c_double_ptr	]	#z GEI (out)
_CSMtoGEIUT.restype = None


#Convert GEI to SM coordinates
_CGEItoSMUT = libgeopack.GEItoSMUT
_CGEItoSMUT.argtypes = [	c_double_ptr, 		#x GEI
							c_double_ptr, 		#y GEI
							c_double_ptr, 		#z GEI
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x SM (out)
							c_double_ptr, 		#y SM (out)
							c_double_ptr	]	#z SM (out)
_CGEItoSMUT.restype = None



#Convert MAG to GEI coordinates
_CMAGtoGEIUT = libgeopack.MAGtoGEIUT
_CMAGtoGEIUT.argtypes = [	c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr, 		#z MAG
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GEI (out)
							c_double_ptr, 		#y GEI (out)
							c_double_ptr	]	#z GEI (out)
_CMAGtoGEIUT.restype = None


#Convert GEI to MAG coordinates
_CGEItoMAGUT = libgeopack.GEItoMAGUT
_CGEItoMAGUT.argtypes = [	c_double_ptr, 		#x GEI
							c_double_ptr, 		#y GEI
							c_double_ptr, 		#z GEI
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x MAG (out)
							c_double_ptr, 		#y MAG (out)
							c_double_ptr	]	#z MAG (out)
_CGEItoMAGUT.restype = None




#Convert MAG to GSM coordinates
_CMAGtoGSMUT = libgeopack.MAGtoGSMUT
_CMAGtoGSMUT.argtypes = [	c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr, 		#z MAG
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x GSM (out)
							c_double_ptr, 		#y GSM (out)
							c_double_ptr	]	#z GSM (out)
_CMAGtoGSMUT.restype = None


#Convert GSM to MAG coordinates
_CGSMtoMAGUT = libgeopack.GSMtoMAGUT
_CGSMtoMAGUT.argtypes = [	c_double_ptr, 		#x GSM
							c_double_ptr, 		#y GSM
							c_double_ptr, 		#z GSM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x MAG (out)
							c_double_ptr, 		#y MAG (out)
							c_double_ptr	]	#z MAG (out)
_CGSMtoMAGUT.restype = None




#Convert MAG to SM coordinates
_CMAGtoSMUT = libgeopack.MAGtoSMUT
_CMAGtoSMUT.argtypes = [	c_double_ptr, 		#x MAG
							c_double_ptr, 		#y MAG
							c_double_ptr, 		#z MAG
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x SM (out)
							c_double_ptr, 		#y SM (out)
							c_double_ptr	]	#z SM (out)
_CMAGtoSMUT.restype = None


#Convert SM to MAG coordinates
_CSMtoMAGUT = libgeopack.SMtoMAGUT
_CSMtoMAGUT.argtypes = [	c_double_ptr, 		#x SM
							c_double_ptr, 		#y SM
							c_double_ptr, 		#z SM
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#x MAG (out)
							c_double_ptr, 		#y MAG (out)
							c_double_ptr	]	#z MAG (out)
_CSMtoMAGUT.restype = None



_CGEOtoMAGUT_LL = libgeopack.GEOtoMAGUT_LL
_CGEOtoMAGUT_LL.argtypes = [c_double_ptr, 		#GEO Longitude
							c_double_ptr, 		#GEO Latitude
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#MAG Longitude
							c_double_ptr]		#MAG Latitude
_CGEOtoMAGUT_LL.restype = None

_CMAGtoGEOUT_LL = libgeopack.MAGtoGEOUT_LL
_CMAGtoGEOUT_LL.argtypes = [c_double_ptr, 		#MAG longitude
							c_double_ptr, 		#MAG latitude
							c_int, 				#number of vectors
							c_double_ptr, 		#SW Vx
							c_double_ptr, 		#SW Vy
							c_double_ptr, 		#SW Vz
							c_int_ptr, 			#Date array
							c_float_ptr, 		#UT array
							c_double_ptr, 		#GEO Longitude
							c_double_ptr]		#GEO Latitude
_CMAGtoGEOUT_LL.restype = None

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

#obtain the model magnetic field
_CModelField = libgeopack.ModelField
_CModelField.argtypes = [	c_double_ptr, 	#x coord
							c_double_ptr, 	#y coord
							c_double_ptr, 	#z coord
							c_int, 			#number of positions
							c_int_ptr, 		#Date array
							c_float_ptr, 	#ut array
							c_int,			#a flag which tells the function whether there is one or more dates
							c_char_p, 		#Model name
							c_int, 			#input coords
							c_int, 			#output coords
							c_double_ptr, 	#output Bx
							c_double_ptr, 	#output By
							c_double_ptr]	#otuput Bz
_CModelField.restype = None

#Trace field lines
_CTraceField = libgeopack.TraceField
_CTraceField.argtypes = [	c_double_ptr,	#x coord
							c_double_ptr, 	#y coord
							c_double_ptr, 	#z coord
							c_int, 			#number of positions
							c_int_ptr, 		#Date 
							c_float_ptr, 	#UT
							c_char_p,		#Model
							c_int, 			#input coord system
							c_int,			#output coord system
							c_double, 		#termination altitude (km)
							c_int,			#Maximum number of trace steps
							c_double,		#Max step size
							c_double_ptr, 	#output x positions
							c_double_ptr, 	#output y positions
							c_double_ptr, 	#output z positions
							c_double_ptr, 	#output distance along field line
							c_double_ptr, 	#output R
							c_double_ptr, 	#output Rnorm
							c_double_ptr, 	#output Bx
							c_double_ptr, 	#output By
							c_double_ptr, 	#output Bz
							c_int_ptr, 		#output number of steps
							c_double_ptr, 	#output footprint info
							c_bool,			#Verbose 
							c_int ]			#TraceDir
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
