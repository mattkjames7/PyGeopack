import numpy as np
from ._CFunctions import _CTraceField
from .SetCustParam import SetCustParam
import ctypes
from ._CoordCode import _CoordCode
		
		
class TraceField(object):
	'''
	Object which stores the result of a magnetic field trace or a series 
	of traces performed using the Tsygenenko model.
	
	Stored in this object are the following variables:
		x = x coordinate along the field trace
		y = y coordinate along the field trace
		z = z coordinate along the field trace
		Bx = x component of the magnetic field along the trace
		By = y component of the magnetic field along the trace
		Bz = z component of the magnetic field along the trace			
		nstep = number of steps along the trace
		GlatN = Geographic latitude of the northern footprint
		GlatS = Geographic latitude of the southern footprint
		MlatN = Magnetic latitude of the northern footprint
		MlatS = Magnetic latitude of the southern footprint
		GlonN = Geographic longitude of the northern footprint
		GlonS = Geographic longitude of the southern footprint
		MlonN = Magnetic longitude of the northern footprint
		MlonS = Magnetic longitude of the southern footprint
		GltN = Geographic local time of the northern footprint
		GltS = Geographic local time of the southern footprint
		MltN = Magnetic local time of the northern footprint
		MltS = Magnetic local time of the southern footprint
		Lshell = L-shell of the field line at the equator
		MltE = Magnetic local time of the equatorial footprint
		FlLen = Field line length in planetary radii
		R = sqrt(x**2 + y**2 + z**2)		
	
	'''
	
	def __init__(self,Xin, Yin, Zin, Date, ut, Model='T96', CoordIn = 'GSM', CoordOut = 'GSM', 
				alt=100.0, MaxLen=1000, DSMax=1.0,FlattenSingleTraces=True,Verbose=False,**kwargs):
		'''
		Traces along the magnetic field given a starting set of 
		coordinates (or for multiple traces, arrays of starting 
		coordinates).
		
		Inputs
		=======
		Xin	: scalar or array containing the x component of the starting 
			point(s).
		Yin	: scalar or array containing the y component of the starting 
			point(s).
		Zin	: scalar or array containing the z component of the starting 
			point(s).
		Date	: Date (one for all traces) or array of dates (one for 
			each trace), each an integer in the format yyyymmdd.
		ut	: Time in hours either as a scalar (one for all traces) or 
			an array (one time for each trace): ut = h + m/60 + s/3600.
		Model	: String to say which model to use out of the following:
			'T89'|'T96'|'T01'|'TS05' (see further below about models).
		CoordIn	: String denoting system of input coordinates out of:
			'GSE'|'GSM'|'SM'.
		CoordOut	: String denoting output coordinate system out of:
			'GSE'|'GSM'|'SM'.
		alt	:	Altitude in km to stop the trace, default is 100km.
		MaxLen	: Maximum number of steps in the trace, default is 1000.
		DSMax	: Maximum step length, default is 1.0 Re.
		FlattenSingleTraces	: Boolean, set to True if performing only a
			single trace to flatten all of the arrays (position, 
			magnetic field, etc.)
		Verbose	: Boolean, if True will display an indication of the 
			progress made during traces.
		
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
				model uses the iopt parameter. 
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
		_Xin = np.array(Xin).astype("float32")
		_Yin = np.array(Yin).astype("float32")
		_Zin = np.array(Zin).astype("float32")
		_n = np.int32(_Xin.size)
		if np.size(Date) == 1:
			_Date = np.zeros(_n,dtype='int32') + Date
		else:
			_Date = np.array(Date).astype('int32')
		if np.size(ut) == 1:
			_ut = np.zeros(_n,dtype='float32')
		else:
			_ut = np.array(ut).astype('float32')
		_Model = ctypes.c_char_p(Model.encode('utf-8'))
		_CoordIn = _CoordCode(CoordIn)
		_CoordOut = _CoordCode(CoordOut)
		_alt = np.float32(alt)
		_MaxLen = np.int32(MaxLen)
		_DSMax = np.float32(DSMax)
		_Xout = np.zeros(_n*_MaxLen,dtype="float32") + np.nan
		_Yout = np.zeros(_n*_MaxLen,dtype="float32") + np.nan
		_Zout = np.zeros(_n*_MaxLen,dtype="float32") + np.nan
		_Bx = np.zeros(_n*_MaxLen,dtype="float32") + np.nan
		_By = np.zeros(_n*_MaxLen,dtype="float32") + np.nan
		_Bz = np.zeros(_n*_MaxLen,dtype="float32") + np.nan
		_nstep = np.zeros(_n,dtype="int32")
		_GlatN = np.zeros(_n,dtype="float32")
		_GlatS = np.zeros(_n,dtype="float32")
		_MlatN = np.zeros(_n,dtype="float32")
		_MlatS = np.zeros(_n,dtype="float32")
		_GlonN = np.zeros(_n,dtype="float32")
		_GlonS = np.zeros(_n,dtype="float32")
		_MlonN = np.zeros(_n,dtype="float32")
		_MlonS = np.zeros(_n,dtype="float32")
		_GltN = np.zeros(_n,dtype="float32")
		_GltS = np.zeros(_n,dtype="float32")
		_MltN = np.zeros(_n,dtype="float32")
		_MltS = np.zeros(_n,dtype="float32")
		_Lshell = np.zeros(_n,dtype="float32")
		_MltE = np.zeros(_n,dtype="float32")
		_FlLen = np.zeros(_n,dtype="float32")
		_Verb = np.bool(Verbose)
		_CTraceField(_Xin, _Yin, _Zin, _n, _Date, _ut, _Model, _CoordIn, _CoordOut, _alt, _MaxLen, _DSMax, _Xout, _Yout, _Zout, _Bx, _By, _Bz, _nstep, _GlatN, _GlatS, _MlatN, _MlatS, _GlonN, _GlonS, _MlonN, _MlonS, _GltN, _GltS, _MltN, _MltS, _Lshell, _MltE, _FlLen, _Verb)

		self.n = _n
		if _n == 1 and FlattenSingleTraces:
			self.nstep = _nstep[0]
			self.x = (_Xout.reshape((_n,MaxLen)))[0,:self.nstep]
			self.y = (_Yout.reshape((_n,MaxLen)))[0,:self.nstep]
			self.z = (_Zout.reshape((_n,MaxLen)))[0,:self.nstep]
			self.Bx = (_Bx.reshape((_n,MaxLen)))[0,:self.nstep]
			self.By = (_By.reshape((_n,MaxLen)))[0,:self.nstep]
			self.Bz = (_Bz.reshape((_n,MaxLen)))[0,:self.nstep]
			self.GlatN = _GlatN[0]
			self.GlatS = _GlatS[0]
			self.MlatN = _MlatN[0]
			self.MlatS = _MlatS[0]
			self.GlonN = _GlonN[0]
			self.GlonS = _GlonS[0]
			self.MlonN = _MlonN[0]
			self.MlonS = _MlonS[0]
			self.GltN = _GltN[0]
			self.GltS = _GltS[0]
			self.MltN = _MltN[0]
			self.MltS = _MltS[0]
			self.Lshell = _Lshell[0]
			self.MltE = _MltE[0]
			self.FlLen = _FlLen[0]
			self.R = np.sqrt(self.x**2 + self.y**2 + self.z**2)
		else:
			self.x = _Xout.reshape((_n,MaxLen))
			self.y = _Yout.reshape((_n,MaxLen))
			self.z = _Zout.reshape((_n,MaxLen))
			self.Bx = _Bx.reshape((_n,MaxLen))
			self.By = _By.reshape((_n,MaxLen))
			self.Bz = _Bz.reshape((_n,MaxLen))			
			self.nstep = _nstep
			self.GlatN = _GlatN
			self.GlatS = _GlatS
			self.MlatN = _MlatN
			self.MlatS = _MlatS
			self.GlonN = _GlonN
			self.GlonS = _GlonS
			self.MlonN = _MlonN
			self.MlonS = _MlonS
			self.GltN = _GltN
			self.GltS = _GltS
			self.MltN = _MltN
			self.MltS = _MltS
			self.Lshell = _Lshell
			self.MltE = _MltE
			self.FlLen = _FlLen
			self.R = np.sqrt(self.x**2 + self.y**2 + self.z**2)
