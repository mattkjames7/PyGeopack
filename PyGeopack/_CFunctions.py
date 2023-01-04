import numpy as np
import ctypes
import os
from . import Globals
from .ct import c_char_p,c_bool,c_bool_ptr,c_int,c_int_ptr
from .ct import c_float,c_float_ptr,c_double,c_double_ptr,c_double_ptr_ptr
from ._SourceCompilation import getLibFilename, compileSource


try:
	libgeopack = ctypes.CDLL(getLibFilename())
except:
	print('Importing '+getLibFilename(isShort=True)+' failed, attempting to recompile')
	compileSource()
	libgeopack = ctypes.CDLL(getLibFilename())


#obtain the model magnetic field
_CModelField = libgeopack.ModelField
_CModelField.argtypes = [	c_int, 				#number of positions
							c_double_ptr, 		#x coord
							c_double_ptr, 		#y coord
							c_double_ptr, 		#z coord
							c_int_ptr, 			#Date array
							c_float_ptr, 		#ut array
							c_bool,				#a flag which tells the function whether there is one or more dates
							c_char_p, 			#Model name
							c_int_ptr,			#iopt for each position
							c_double_ptr_ptr,	#parmod for each position
							c_double_ptr,		#Vx
							c_double_ptr,		#Vy
							c_double_ptr,		#Vz
							c_char_p, 			#input coords
							c_char_p, 			#output coords
							c_bool,				#whether to only include vectors within MP
							c_double_ptr, 		#output Bx
							c_double_ptr, 		#output By
							c_double_ptr]		#output Bz
_CModelField.restype = None

	
							
#Trace field lines
_CTraceField = libgeopack.TraceField
_CTraceField.argtypes = [	c_int, 				#number of positions (n)
							c_double_ptr,		#x coord
							c_double_ptr, 		#y coord
							c_double_ptr, 		#z coord
							c_int_ptr, 			#Date 
							c_float_ptr, 		#UT
							c_char_p,			#Model
							c_int_ptr,			#iopt array (n,)
							c_double_ptr_ptr,	#parmod array (n,10)
							c_double_ptr,		#Vx
							c_double_ptr,		#Vy
							c_double_ptr,		#Vz
							c_double, 			#termination altitude (km)
							c_int,				#Maximum number of trace steps
							c_double,			#Max step size
							c_bool,				#Verbose 
							c_int,				#TraceDir
							c_char_p,			#input coord system
							c_int_ptr, 			#output number of steps
							c_double_ptr_ptr, 	#output x positions (GSM)
							c_double_ptr_ptr, 	#output y positions
							c_double_ptr_ptr, 	#output z positions
							c_double_ptr_ptr, 	#output Bx
							c_double_ptr_ptr, 	#output By
							c_double_ptr_ptr, 	#output Bz							
							c_double_ptr_ptr, 	#output x positions (GSE)
							c_double_ptr_ptr, 	#output y positions
							c_double_ptr_ptr, 	#output z positions
							c_double_ptr_ptr, 	#output Bx
							c_double_ptr_ptr, 	#output By
							c_double_ptr_ptr, 	#output Bz							
							c_double_ptr_ptr, 	#output x positions (SM)
							c_double_ptr_ptr, 	#output y positions
							c_double_ptr_ptr, 	#output z positions
							c_double_ptr_ptr, 	#output Bx
							c_double_ptr_ptr, 	#output By
							c_double_ptr_ptr, 	#output Bz							
							c_double_ptr_ptr, 	#output distance along field line
							c_double_ptr_ptr, 	#output R
							c_double_ptr_ptr, 	#output Rnorm
							c_double_ptr_ptr, 	#output footprint info
							c_int,				#the number of alphas
							c_double_ptr,		#alpha (n_alpha,)
							c_double_ptr]	#output h_alpha (n,MaxLen,n_alpha)
_CTraceField.restype = None
