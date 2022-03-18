import numpy as np
from .._CFunctions import libgeopack
from ..ct import c_char_p,c_bool,c_bool_ptr,c_int,c_int_ptr
from ..ct import c_float,c_float_ptr,c_double,c_double_ptr,c_double_ptr_ptr

# get model parameters
_CGetModelParams = libgeopack.GetModelParams #remove me
_CGetModelParams.argtypes = [	c_int, 			#number of elements
								c_int_ptr,			#Date
								c_float_ptr,		#ut
								c_char_p, 		#Model name
								c_double_ptr, 		#Vx input
								c_double_ptr, 		#Vy input
								c_double_ptr, 		#Vz input
								c_double_ptr, 		#Kp input
								c_double_ptr, 		#Pdyn input
								c_double_ptr, 		#SymH input
								c_double_ptr, 		#By input
								c_double_ptr, 		#Bz input
								c_double_ptr, 		#G1 input
								c_double_ptr, 		#G2 input
								c_double_ptr, 		#W1 input
								c_double_ptr, 		#W2 input
								c_double_ptr, 		#W3 input
								c_double_ptr, 		#W4 input
								c_double_ptr, 		#W5 input
								c_double_ptr, 		#W6 input
								c_double_ptr, 		#Vx output
								c_double_ptr, 		#Vy output
								c_double_ptr, 		#Vz output
								c_double_ptr, 		#Kp output
								c_double_ptr, 		#Pdyn output
								c_double_ptr, 		#SymH output
								c_double_ptr, 		#By output
								c_double_ptr, 		#Bz output
								c_double_ptr, 		#G1 output
								c_double_ptr, 		#G2 output
								c_double_ptr, 		#W1 output
								c_double_ptr, 		#W2 output
								c_double_ptr, 		#W3 output
								c_double_ptr, 		#W4 output
								c_double_ptr, 		#W5 output
								c_double_ptr, 		#W6 output
								c_double_ptr, 		#Dipole tilt
								c_int_ptr,			#iopt array
								c_double_ptr_ptr]	#parmod array
_CGetModelParams.restype = None



# Function to initialize the data object
_CInit = libgeopack.InitParams
_CInit.argtypes = [c_char_p] #full file name

# Function to delete parameters from memory
_CFreeParams = libgeopack.FreeParams

#function to return dipole tilt
_CGetDipoleTilt = libgeopack.GetDipoleTiltUT
_CGetDipoleTilt.argtypes = [	c_int,		#Date
								c_float,		#ut
								c_double,	#Vx
								c_double,	#Vy
								c_double]	#Vz
_CGetDipoleTilt.restype = c_double			#tilt

#this finds the intervals to calculate W params over
_CFindIntervals = libgeopack.FindIntervals
_CFindIntervals.argtypes = [	c_int,		#number of elements of data
								c_double_ptr,	#SymH
								c_double_ptr,	#Bz IMF
								c_int_ptr,		#SW Flag
								c_int_ptr,		#IMF flag
								c_int_ptr,		#number of intervals
								c_int_ptr,		#start indices
								c_int_ptr]		#end indices
_CFindIntervals.restype = None

#calcualtes the W parameters
_CCalculateW = libgeopack.CalculateW
_CCalculateW.argtypes = [	c_int,		#number of data elements
							c_double_ptr,	#SymH
							c_double_ptr,	#Bz
							c_int_ptr,		#SW flag
							c_int_ptr,		#IMF flag
							c_double_ptr,	#V (SW speed)
							c_double_ptr,	#Density
							c_double_ptr,	#W1
							c_double_ptr,	#W2
							c_double_ptr,	#W3
							c_double_ptr,	#W4
							c_double_ptr,	#W5
							c_double_ptr] 	#W6
_CCalculateW.restype = None

#fill in the Kp indices for all times
_CFillInKp = libgeopack.FillInKp
_CFillInKp.argtypes = [	c_int,		#number of Kp indices
						c_int_ptr,		#Kp dates
						c_float_ptr,	#Kp ut0
						c_float_ptr,	#Kp ut1
						c_float_ptr,	#Kp value
						c_int,		#number of data elements
						c_int_ptr,		#Date
						c_float_ptr,	#ut
						c_float_ptr]	#kp output
_CFillInKp.restype = None

#Calculate the G1 and G2 indices
_CCalculateG = libgeopack.CalculateG
_CCalculateG.argtypes = [	c_int,		#number of elements
							c_double_ptr,	#By IMF
							c_double_ptr,	#Bz IMF
							c_double_ptr,	#V SW speed
							c_bool_ptr,		#good (true == good)
							c_double_ptr,	#G1
							c_double_ptr] 	#G2
_CCalculateG.restype = None

#get the solar wind velocity
_CFillSWVelocity = libgeopack.FillSWVelocity
_CFillSWVelocity.argtypes = [	c_int,			#number of elements
								c_int_ptr,		#Date array
								c_float_ptr,	#UT array
								c_double_ptr,	#Vx
								c_double_ptr,	#Vy
								c_double_ptr]	#Vz
_CFillSWVelocity.restype = None

#get T89 parameters
_CFillT89Params = libgeopack.FillT89Params
_CFillT89Params.argtypes = [	c_int,				#number of elements
								c_int_ptr,			#Date array
								c_float_ptr,		#UT array
								c_double_ptr,		#Kp
								c_int_ptr,			#iopt
								c_double_ptr_ptr]	#parmod
_CFillT89Params.restype = None

#get T96 parameters
_CFillT96Params = libgeopack.FillT96Params
_CFillT96Params.argtypes = [	c_int,				#number of elements
								c_int_ptr,			#Date array
								c_float_ptr,		#UT array
								c_double_ptr,		#Pdyn
								c_double_ptr,		#SymH
								c_double_ptr,		#By
								c_double_ptr,		#Bz
								c_int_ptr,			#iopt
								c_double_ptr_ptr]	#parmod
_CFillT96Params.restype = None

#get T01 parameters
_CFillT01Params = libgeopack.FillT01Params
_CFillT01Params.argtypes = [	c_int,				#number of elements
								c_int_ptr,			#Date array
								c_float_ptr,		#UT array
								c_double_ptr,		#Pdyn
								c_double_ptr,		#SymH
								c_double_ptr,		#By
								c_double_ptr,		#Bz
								c_double_ptr,		#G1
								c_double_ptr,		#G2
								c_int_ptr,			#iopt
								c_double_ptr_ptr]	#parmod
_CFillT01Params.restype = None

#get TS05 parameters
_CFillTS05Params = libgeopack.FillTS05Params
_CFillTS05Params.argtypes = [	c_int,				#number of elements
								c_int_ptr,			#Date array
								c_float_ptr,		#UT array
								c_double_ptr,		#Pdyn
								c_double_ptr,		#SymH
								c_double_ptr,		#By
								c_double_ptr,		#Bz
								c_double_ptr,		#W1
								c_double_ptr,		#W2
								c_double_ptr,		#W3
								c_double_ptr,		#W4
								c_double_ptr,		#W5
								c_double_ptr,		#W6
								c_int_ptr,			#iopt
								c_double_ptr_ptr]	#parmod
_CFillTS05Params.restype = None
