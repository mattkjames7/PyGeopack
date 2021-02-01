import numpy as np
from ._CFunctions import _CModelField
from .SetCustParam import SetCustParam
import ctypes
from ._CoordCode import _CoordCode
from ._CTConv import _CTConv

def ModelField(Xin, Yin, Zin, Date, ut, Model='T96', CoordIn='GSM', CoordOut='GSM',OutDtype='float64',**kwargs):
	'''
	Calculates the model magnetic field at a given position or array of
	positions in space.
	
	Inputs
	=======
	Xin	: scalar or array containing the x positions(s).
	Yin	: scalar or array containing the y positions(s).
	Zin	: scalar or array containing the z positions(s).
	Date	: Date - an integer in the format yyyymmdd.
	ut	: Time in hours i.e. ut = h + m/60 + s/3600.
	Model	: String to say which model to use out of the following:
		'T89'|'T96'|'T01'|'TS05' (see further below about models).
	CoordIn	: String denoting system of input coordinates out of:
		'GSE'|'GSM'|'SM'.
	CoordOut	: String denoting output coordinate system out of:
		'GSE'|'GSM'|'SM'.
	
	Keyword arguments
	=================
	iopt	: This keyword, if set, will override the iopt parameter
	parmod	: set to a 10 element floating point array to override 
		the  default model params (see below for more info)
	tilt	: Override for the actual dipole tilt angle (based on 
			the	date and time) in radians
	Vx		: X component solar wind velocity override.
	Vy		: Y component solar wind velocity override.
	Vz		: Z component solar wind velocity override.
	Kp		: Sets the Kp index - essentially an override for iopt,
			where iopt = Kp + 1
	Pdyn	: Override for parmod[0] - dynamic pressure
	SymH	: Override for parmod[1] - SymH
	By		: Override for parmod[2] - IMF By
	Bz		: Override for parmod[3] - IMF Bz
	
	Model Fields
	============
	'T89'	: The T89 model is only dependent upon the iopt parameter
			which is valid in the range 1 - 7, and is essentially
			equal to Kp + 1 (use iopt = 7 for Kp >= 6). No other 
			model uses the iopt parameter. Pdyn and Bz will be used to 
			check if we are inside the MP.
	'T96'	: This model uses the first four parameters of parmod
			which are Pdyn, SymH, By and Bz, respectively. All other 
			elements of parmod are ignored.
	'T01'	: This model uses the same 4 parameters as the T96 model,
			but also uses next two elements of the parmod array for
			the G1 and G2 parameters.
	'TS05'	: This model uses all ten parmod elements - the first 4
			are the same as T96, the next 6 are the W1-W6 parameters
			which **apparently** aren't too important (you could
			probably set these to 0). The ones calcualted by this 
			module are probably erroneous - they are calculated by
			using Tsyganenko's own Fortran code, but they still 
			don't match the ones provided on his website! So maybe
			you *should* set these to 0.
			
	NOTE 1 : Vx, Vy and Vz are used to convert from GSM to GSW coords.
	NOTE 2 : All parameters here will be automatically filled in 
		based on the OMNI data - any gaps could be replaced with 
		custom parameters.
	NOTE 3 : When using custom parameters, all other parameters will
		still be calculated automatically, unless they are also
		customized.
			
	'''


	#look for custom parameters
	keys = list(kwargs.keys())
	CustP = False
	CustFields = ['Kp','Pdyn','SymH','By','Bz','iopt','parmod','tilt','Vx','Vy','Vz']
	for f in CustFields:
		if f in keys:
			CustP = True

	#now we know if any exist, so let's add them
	if (CustP):
		Model = Model+'c' #this will let the C code know that we aren't just interpolating real values
		iopt = kwargs.get('iopt',-1)
		iopt = kwargs.get('Kp',-2)+1
		
		parmod = kwargs.get('parmod',np.zeros(10,dtype='float32')+np.nan)
		Pdyn = kwargs.get('Pdyn',parmod[0])
		SymH = kwargs.get('SymH',parmod[1])
		By = kwargs.get('By',parmod[2])
		Bz = kwargs.get('Bz',parmod[3])
		parmod[0] = Pdyn
		parmod[1] = SymH
		parmod[2] = By
		parmod[3] = Bz
		
		tilt = kwargs.get('tilt',np.nan)
		
		Vx = kwargs.get('Vx',np.nan)
		Vy = kwargs.get('Vy',np.nan)
		Vz = kwargs.get('Vz',np.nan)
		
		SetCustParam(iopt,parmod,tilt,Vx,Vy,Vz)

		
		

	#Convert input variables to appropriate numpy dtype:
	_Xin = _CTConv(Xin,'c_double_ptr')
	_Yin = _CTConv(Yin,'c_double_ptr')
	_Zin = _CTConv(Zin,'c_double_ptr')
	_n = _CTConv(_Xin.size,'c_int')
	_Date = _CTConv(np.zeros(_n) + Date,'c_int_ptr')
	_ut = _CTConv(np.zeros(_n) + ut,'c_float_ptr')
	_SameTime = _CTConv(0,'c_int')
	_Model = _CTConv(Model,'c_char_p')
	_CoordIn = _CTConv(_CoordCode(CoordIn),'c_int')
	_CoordOut = _CTConv(_CoordCode(CoordOut),'c_int')
	_Bx = np.zeros(_n,dtype="float64")
	_By = np.zeros(_n,dtype="float64")
	_Bz = np.zeros(_n,dtype="float64")
	_CModelField(_Xin, _Yin, _Zin, _n, _Date, _ut, _SameTime, _Model, _CoordIn, _CoordOut, _Bx, _By, _Bz)

	return _Bx.astype(OutDtype),_By.astype(OutDtype),_Bz.astype(OutDtype)
