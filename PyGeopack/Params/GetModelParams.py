import numpy as np
from ._CFunctions import _CFillSWVelocity
from ._CFunctions import _CFillT89Params
from ._CFunctions import _CFillT96Params
from ._CFunctions import _CFillT01Params
from ._CFunctions import _CFillTS05Params
import ctypes
from .. import ct

_fields = { 'T89' : ['Vx','Vy','Vz','Kp'],
			'T96' : ['Vx','Vy','Vz','Pdyn','SymH','By','Bz'],
			'T01' : ['Vx','Vy','Vz','Pdyn','SymH','By','Bz','G1','G2'],
			'TS05' : ['Vx','Vy','Vz','Pdyn','SymH','By','Bz','W1','W2','W3','W4','W5','W6']}


def GetModelParams(Date, ut, Model,**kwargs):
	'''
	Returns the parameters which would be used to drive a model given a
	date and a time.
	
	Inputs
	======
	Date : int
		Integer date, in the format yyyymmdd.
	ut : float
		Floating point time in hours (i.e. ut = hh + mm/60).
	Model : str
		String denoting which model to return parameters for out
		of the following - 'T89'|'T96'|'T01'|'TS05'.
	
	Returns
	=======
	iopt	: Integer, related to Kp - used only for T89.
	parmod	: 10 element array used for T96, T01 and TS05.
	tilt	: The dipole tilt angle in radians.
	Vx		: The x component of the solar wind velocity.
	Vy		: The y component of the solar wind velocity.
	Vz		: The z component of the solar wind velocity.
	
	'''

	if not Model in ['T89','T96','T01','TS05']:
		print('Model {:s} not recognised'.format(Model))
		return

	#get the number of elements
	n_ = np.int32(np.size(Date))

	#this is a list of the input keywords
	keys = list(kwargs.keys())
	fields = _fields[Model]
	
	#output dictionary
	out = {}
	for f in fields:
		if f in keys:
			x = kwargs[f]
			try:
				out[f] = np.zeros(n_,dtype='float64') + x
			except:
				print("Parameter shape {:s} must be either (1,) or (n,)".format(f))
				raise ValueError
		else:
			out[f] = np.zeros(n_,dtype='float64') + np.nan

	#Convert input variables to appropriate numpy dtype:
	out['Date'] = np.zeros(n_,dtype='int32') + np.array(Date).astype('int32')
	out['ut'] = np.zeros(n_,dtype='float32') + np.array(ut).astype('float32')
	out['Model'] = Model
	out['iopt'] = np.zeros(n_,dtype="int32")
	out['parmod'] = np.zeros((n_,10),dtype='float64')
	_parmod = ct.ctDoublePtrPtr(out['parmod'])
	out['tilt'] = np.zeros(n_,dtype="float64") #fix me

	#get SW velocity first
	_CFillSWVelocity(n_,out['Date'],out['ut'],out['Vx'],out['Vy'],out['Vz'])
	
	#get appropriate model parameters
	if Model == 'T89':
		_CFillT89Params(n_,out['Date'],out['ut'],
						out['Kp'],out['iopt'],_parmod)
	elif Model == 'T96':
		_CFillT96Params(n_,out['Date'],out['ut'],
						out['Pdyn'],out['SymH'],
						out['By'],out['Bz'],
						out['iopt'],_parmod)
	elif Model == 'T01':
		_CFillT01Params(n_,out['Date'],out['ut'],
						out['Pdyn'],out['SymH'],
						out['By'],out['Bz'],
						out['G1'],out['G2'],
						out['iopt'],_parmod)
	elif Model == 'TS05':
		_CFillTS05Params(n_,out['Date'],out['ut'],
						out['Pdyn'],out['SymH'],
						out['By'],out['Bz'],
						out['W1'],out['W2'],
						out['W3'],out['W4'],
						out['W5'],out['W6'],
						out['iopt'],_parmod)

	return out
