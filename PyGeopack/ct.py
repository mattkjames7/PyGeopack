import numpy as np
import ctypes

#define some dtypes
c_char_p = ctypes.c_char_p
c_bool = ctypes.c_bool
c_int = ctypes.c_int
c_float = ctypes.c_float
c_double = ctypes.c_double
c_float_ptr = np.ctypeslib.ndpointer(ctypes.c_float,flags="C_CONTIGUOUS")

#this one is a hack found at: https://stackoverflow.com/a/32138619/15482422
#it allows us to send None instead of an array which is treated as NULL
c_double_ptr_base = np.ctypeslib.ndpointer(ctypes.c_double,flags="C_CONTIGUOUS")
def _from_param(cls, obj):
		if obj is None:
			return obj
		return c_double_ptr_base.from_param(obj)
c_double_ptr = type('c_double_ptr',(c_double_ptr_base,),{'from_param':classmethod(_from_param)})

c_double_ptr_ptr = np.ctypeslib.ndpointer(np.uintp,ndim=1,flags="C_CONTIGUOUS")
c_int_ptr = np.ctypeslib.ndpointer(ctypes.c_int,flags="C_CONTIGUOUS")
c_bool_ptr = np.ctypeslib.ndpointer(ctypes.c_bool,flags="C_CONTIGUOUS")


def ctNull():
	'''
	Crazy pointless function. It's just None.
	
	'''
	return None
	
def ctString(x):
	'''
	Convert a string to a char string.
	
	'''
	return ctypes.c_char_p(x.encode('utf-8'))
	
def ctBool(x):
	'''
	Convert python Boolean to C
	
	'''
	return np.array([x]).flatten().astype('bool8')[0]
	
	
def ctBoolPtr(x):
	'''
	Convert python Boolean pointer to C
	
	'''
	return np.array([x]).flatten().astype('bool8')
	
def ctBoolPtrPtr(x):
	'''
	Convert python Boolean 2d array pointer to C
	
	'''
	return (x.__array_interface__['data'][0] + np.arange(x.shape[0])*x.strides[0]).astype(np.uintp)
	
	
def ctInt(x):
	'''
	Convert python Integer to C
	
	'''
	return np.array([x]).flatten().astype('int32')[0]
	
	
def ctIntPtr(x):
	'''
	Convert python Integer pointer to C
	
	'''
	return np.array([x]).flatten().astype('int32')
	
def ctIntPtrPtr(x):
	'''
	Convert python Integer 2d array pointer to C
	
	'''
	return (x.__array_interface__['data'][0] + np.arange(x.shape[0])*x.strides[0]).astype(np.uintp)
	
	
def ctFloat(x):
	'''
	Convert python Float to C
	
	'''
	return np.array([x]).flatten().astype('float32')[0]
	
	
def ctFloatPtr(x):
	'''
	Convert python Float pointer to C
	
	'''
	return np.array([x]).flatten().astype('float32')
	
def ctFloatPtrPtr(x):
	'''
	Convert python Float 2d array pointer to C
	
	'''
	return (x.__array_interface__['data'][0] + np.arange(x.shape[0])*x.strides[0]).astype(np.uintp)
	
	
def ctDouble(x):
	'''
	Convert python Double to C
	
	'''
	return np.array([x]).flatten().astype('float64')[0]
	
	
def ctDoublePtr(x):
	'''
	Convert python Double pointer to C
	
	'''
	return np.array([x]).flatten().astype('float64')
	
def ctDoublePtrPtr(x):
	'''
	Convert python Double 2d array pointer to C
	
	'''
	return (x.__array_interface__['data'][0] + np.arange(x.shape[0])*x.strides[0]).astype(np.uintp)
	
	

