import numpy as np
from ._CFunctions import _CTraceField
#from .SetCustParam import SetCustParam
import ctypes
from ._CoordCode import _CoordCode
from .GetModelParams import GetModelParams
from .GSMtoGSE import GSMtoGSE
from .GSMtoSM import GSMtoSM
from ._CTConv import _CTConv
		
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
	
	def __init__(self,Xin,Yin,Zin,Date,ut,Model='T96',CoordIn='GSM', 
				alt=100.0,MaxLen=1000,DSMax=1.0,FlattenSingleTraces=True,
				Verbose=False,TraceDir='both',alpha=[0.0,90.0],**kwargs):
		'''
		Traces along the magnetic field given a starting set of 
		coordinates (or for multiple traces, arrays of starting 
		coordinates).
		
		Inputs
		=======
		Xin : float
			scalar or array containing the x component of the starting 
			point(s).
		Yin : float
			scalar or array containing the y component of the starting 
			point(s).
		Zin : float
			scalar or array containing the z component of the starting 
			point(s).
		Date : int
			Date (one for all traces) or array of dates (one for 
			each trace), each an integer in the format yyyymmdd.
		ut : float
			Time in hours either as a scalar (one for all traces) or 
			an array (one time for each trace): ut = h + m/60 + s/3600.
		Model : str
			String to say which model to use out of the following:
			'T89'|'T96'|'T01'|'TS05' (see further below about models).
		CoordIn : str
			String denoting system of input coordinates out of:
			'GSE'|'GSM'|'SM'.
		CoordOut : str
			String denoting output coordinate system out of:
			'GSE'|'GSM'|'SM'.
		alt : float
			Altitude in km to stop the trace, default is 100km.
		MaxLen : int
			Maximum number of steps in the trace, default is 1000.
		DSMax : float
			Maximum step length, default is 1.0 Re.
		FlattenSingleTraces	: bool
			When set to True and if performing only a single trace to 
			flatten all of the arrays (position, magnetic field, etc.)
		Verbose	: bool
			Boolean, if True will display an indication of the progress 
			made during traces.
		TraceDir : int|str
			if set to 0 or 'both' then the trace will run in both 
			directions. Set to 1 to trace along the field direction
			(from south to north), or set to -1 to trace in the opposite
			direction to the magnetic field (north to south).
		
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


		#Convert input variables to appropriate numpy dtype:
		self.Xin = np.array(Xin).astype("float64")
		self.Yin = np.array(Yin).astype("float64")
		self.Zin = np.array(Zin).astype("float64")
		self.n = np.int32(self.Xin.size)
		if np.size(Date) == 1:
			self.Date = np.zeros(self.n,dtype='int32') + Date
		else:
			self.Date = np.array(Date).astype('int32')
		if np.size(ut) == 1:
			self.ut = np.zeros(self.n,dtype='float32') + np.float32(ut)
		else:
			self.ut = np.array(ut).astype('float32')
		self.Model = Model
		self.ModelCode = ctypes.c_char_p(Model.encode('utf-8'))
		self.CoordIn = CoordIn
		self.CoordInCode =_CTConv(CoordIn,'c_char_p')
		self.CoordOutCode =_CTConv("GSM",'c_char_p')
		self.alt = np.float64(alt)
		self.MaxLen = np.int32(MaxLen)
		self.DSMax = np.float64(DSMax)
		self.x = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.y = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.z = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.s = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.R = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Rnorm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bx = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.By = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bz = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.nstep = np.zeros(self.n,dtype="int32")
		self.FP = np.zeros((self.n,15),dtype="float64")
		self.Verb = np.bool(Verbose)
		if TraceDir == 'both':
			TraceDir = 0
		self.TraceDir = np.int32(TraceDir)
		self.nalpha = np.int32(np.size(alpha))
		self.alpha = np.array(alpha).astype('float64')
		self.halpha = np.zeros((self.n,self.MaxLen*self.nalpha),dtype="float64") + np.nan #hopefully this will be reshaped to (n,nalpha,MaxLen)
		
		_x = _CTConv(self.x,'c_double_ptr',nd=2)
		_y = _CTConv(self.y,'c_double_ptr',nd=2)
		_z = _CTConv(self.z,'c_double_ptr',nd=2)
		_s = _CTConv(self.s,'c_double_ptr',nd=2)
		_R = _CTConv(self.R,'c_double_ptr',nd=2)
		_Rnorm = _CTConv(self.Rnorm,'c_double_ptr',nd=2)
		_Bx = _CTConv(self.Bx,'c_double_ptr',nd=2)
		_By = _CTConv(self.By,'c_double_ptr',nd=2)
		_Bz = _CTConv(self.Bz,'c_double_ptr',nd=2)
		_FP = _CTConv(self.FP,'c_double_ptr',nd=2)
		_halpha = _CTConv(self.halpha,'c_double_ptr',nd=2)
		
		#get model parameters
		self.params = GetModelParams(self.Date,self.ut,Model,**kwargs)
		_Parmod = _CTConv(self.params['parmod'],'c_double_ptr',nd=2)
		
		#call the C code
		_CTraceField(self.Xin,self.Yin,self.Zin,self.n,
					self.Date,self.ut,self.ModelCode,
					self.params['iopt'],_Parmod,
					self.params['Vx'],self.params['Vy'],self.params['Vz'],
					self.CoordInCode,self.CoordOutCode,
					self.alt,self.MaxLen,self.DSMax,
					self.Verb,self.TraceDir,
					_x,_y,_z,
					_s,_R,_Rnorm,
					self.nalpha,self.alpha,_halpha,
					_Bx,_By,_Bz,
					self.nstep,_FP)

		#reshape the footprints
		fpnames = ['GlatN','GlatS','MlatN','MlatS',
					'GlonN','GlonS','MlonN','MlonS',
					'GltN','GltS','MltN','MltS',
					'Lshell','MltE','FlLen']

		if self.n == 1 and FlattenSingleTraces:
			self.nstep = self.nstep[0]
			self.x = self.x[0]
			self.y = self.y[0]
			self.z = self.z[0]
			self.Bx = self.Bx[0]
			self.By = self.By[0]
			self.Bz = self.Bz[0]
			self.s = self.s[0]
			self.R = self.R[0]
			self.Rnorm = self.Rnorm[0]
			self.halpha = (self.halpha.reshape((self.n,self.nalpha,self.MaxLen)))[0]
			for i in range(0,15):
				setattr(self,fpnames[i],self.FP[0,i])
		else:
			self.halpha = self.halpha.reshape((self.n,self.nalpha,self.MaxLen))
			for i in range(0,15):
				setattr(self,fpnames[i],self.FP[:,i])

	def CalculateHalpha(self,I,Polarization='toroidal'):
		'''
		Calculate h_alpha (see Singer et al 1982)
		
		'''
		
		#get field line in SM coords
		
		#get starting positions near the equator
		
		#get two traces in opposite polarization directions
		
		#rescale s (distance along the field line) to match this field line
		
		#create splines
		
		#interpolate field lines to match original
		
		#calculate nearest points
		
		#get halphas
		
		#take mean of two halphas
		
		pass
		
	def GetTrace(self,I,Coord='SM'):
		'''
		Return traces in a given coordinate system
		
		'''
		pass
		
	def Save(self,fname):
		'''
		Save the data in this object to file.
		'''
		pass
		
		
		
	
		
	
