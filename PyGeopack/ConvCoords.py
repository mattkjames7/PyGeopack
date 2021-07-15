import numpy as np
from ._CFunctions import _CConvCoords
from ._CTConv import _CTConv


def ConvCoords(Xin, Yin, Zin, Date, ut, CoordIn, CoordOut, V=None):
	'''
	Converts between various coordinate systems
	
	Inputs
	======
	Xin	: float
		Array or scalar of x coordinates in R_E.
	Yin	: float
		Array or scalar of y coordinates in R_E.
	Zin	: float
		Array or scalar of z coordinates in R_E.
	Date : float
		Integer dat in format yyyymmdd.
	ut : float
		Time in hours (ut = hh + mm/60 + ss/3600).	
	CoordIn : str
		Input coordinates 'GSE'|'GSM'|'SM'|'GEO'|'MAG'|'GEI'
	CoordOut : str
		Output coordinates 'GSE'|'GSM'|'SM'|'GEO'|'MAG'|'GEI'
	
	Returns
	=======
	Xout,Yout,Zout	: x, y and z coordinates in R_E
	
	'''
	
	#check if the input and output coordinate systems are valid
	CS = ['GSE','GSM','SM','GEO','MAG','GEI']
	if not CoordIn.upper() in CS:
		print('Input coordinates must be one of the following:')
		print("'GSE'|'GSM'|'SM'|'GEO'|'MAG'|'GEI'")
		raise ValueError
	if not CoordOut.upper() in CS:
		print('Output coordinates must be one of the following:')
		print("'GSE'|'GSM'|'SM'|'GEO'|'MAG'|'GEI'")
		raise ValueError
	
	#Convert input variables to appropriate numpy dtype:
	_Xin = _CTConv(Xin,'c_double_ptr')
	_Yin = _CTConv(Yin,'c_double_ptr')
	_Zin = _CTConv(Zin,'c_double_ptr')
	_n = _CTConv(_Xin.size,'c_int')
	if V is None:
		Vx = np.zeros(_n) + np.nan
		Vy = np.zeros(_n) + np.nan
		Vz = np.zeros(_n) + np.nan
	else:
		Vx = np.zeros(_n) + V[0]
		Vy = np.zeros(_n) + V[1]
		Vz = np.zeros(_n) + V[2]
	_Vx = _CTConv(Vx,'c_double_ptr')
	_Vy = _CTConv(Vy,'c_double_ptr')
	_Vz = _CTConv(Vz,'c_double_ptr')
	_Date = _CTConv(np.zeros(_n) + Date,'c_int_ptr')
	_ut = _CTConv(np.zeros(_n) + ut,'c_float_ptr')
	
	_Xout = np.zeros(_n,dtype="float64")
	_Yout = np.zeros(_n,dtype="float64")
	_Zout = np.zeros(_n,dtype="float64")	
	
	_CoordIn = _CTConv(CoordIn.upper(),'c_char_p')
	_CoordOut = _CTConv(CoordOut.upper(),'c_char_p')
				
				
	_CConvCoords(_Xin, _Yin, _Zin, _n, _Vx, _Vy, _Vz, _Date, _ut, _Xout, _Yout, _Zout, _CoordIn, _CoordOut)

	return _Xout,_Yout,_Zout
