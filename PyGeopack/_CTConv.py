import numpy as np
import ctypes as ct

def _CTConv(x,ctype,nd=1):
	'''
	Convert Python datatype to a C datatype
	
	Inputs
	======
	x : any array/scalar
		Can be a scalar or array
	ctype : str
		'c_bool'|'c_int'|'c_float'|'c_double'|'c_bool_ptr'|'c_int_ptr'|
		'c_float_ptr'|'c_double_ptr'|'null'
	
	'''
	if ctype == 'null':
		return None
		
	if ctype == 'c_char_p':
		return ct.c_char_p(x.encode('utf-8'))

	scl = ['c_bool','c_int','c_float','c_double']
	arr = ['c_bool_ptr','c_int_ptr','c_float_ptr','c_double_ptr']
	
	st = {	'c_bool' : 'bool8',
			'c_int' : 'int32',
			'c_float' : 'float32',
			'c_double' : 'float64'}
	
	at = {	'c_bool_ptr' : 'bool8',
			'c_int_ptr' : 'int32',
			'c_float_ptr' : 'float32',
			'c_double_ptr' : 'float64'}
	
	if ctype in scl:
		dt = st[ctype]
	elif ctype in arr:
		dt = at[ctype]
	else:
		pass
	
	if ctype in scl:
		o = np.array([x]).flatten().astype(dt)[0]
	elif ctype in arr:
		if nd == 1:
			o = np.array([x]).flatten().astype(dt)
		else:
			pass
			
	return o
		
		
