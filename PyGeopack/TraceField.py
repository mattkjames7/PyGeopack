import numpy as np
from ._CFunctions import _CTraceField
import ctypes
from .Params.GetModelParams import GetModelParams
from .ct import ctString,ctBool,ctInt,ctIntPtr,ctFloatPtr,ctDoublePtr,ctDoublePtrPtr
import PyFileIO as pf
from .Coords.ConvCoords import ConvCoords
		
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
	def __init__(self,*args,**kwargs):
		
		#check if we are loading from file, or creating new traces
		if len(args) == 1:
			#read from file
			print(args)
			self._Load(*args)
		elif len(args) == 5:
			#new traces
			self._Trace(*args,**kwargs)
		else:
			#something's wrong
			print('TraceField was supplied with {:d} arguments...'.format(len(args)))
			print('Either use 1 string (file name), or')
			print('use 5 inputs (x,y,z,Date,ut)')
			return None
		
	
	
	
	def _Load(self,fname):
		self.__dict__ = pf.LoadObject(fname)
	
	
	
	def _Trace(self,Xin,Yin,Zin,Date,ut,Model='T96',CoordIn='GSM', 
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
		ModelCode = ctString(Model)
		self.CoordIn = CoordIn
		CoordInCode = ctString(CoordIn)
		self.alt = np.float64(alt)
		self.MaxLen = np.int32(MaxLen)
		self.DSMax = np.float64(DSMax)

		self.Verb = np.bool(Verbose)
		if TraceDir == 'both':
			TraceDir = 0
		self.TraceDir = np.int32(TraceDir)



		self.xgsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.ygsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.zgsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bxgsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bygsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bzgsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.xgse = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.ygse = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.zgse = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bxgse = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bygse = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bzgse = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.xsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.ysm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.zsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bxsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bysm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Bzsm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan

		self.nstep = np.zeros(self.n,dtype="int32")

		self.s = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.R = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan
		self.Rnorm = np.zeros((self.n,self.MaxLen),dtype="float64") + np.nan

		self.nalpha = np.int32(np.size(alpha))
		self.alpha = np.array(alpha).astype('float64')
		self.halpha = np.zeros((self.n*self.MaxLen*self.nalpha,),dtype="float64") + np.nan #hopefully this will be reshaped to (n,nalpha,MaxLen)
		self.FP = np.zeros((self.n,15),dtype="float64")

		#these are 2D pointers
		_xgsm = ctDoublePtrPtr(self.xgsm)
		_ygsm = ctDoublePtrPtr(self.ygsm)
		_zgsm = ctDoublePtrPtr(self.zgsm)

		_Bxgsm = ctDoublePtrPtr(self.Bxgsm)
		_Bygsm = ctDoublePtrPtr(self.Bygsm)
		_Bzgsm = ctDoublePtrPtr(self.Bzgsm)
		
		_xgse = ctDoublePtrPtr(self.xgse)
		_ygse = ctDoublePtrPtr(self.ygse)
		_zgse = ctDoublePtrPtr(self.zgse)

		_Bxgse = ctDoublePtrPtr(self.Bxgse)
		_Bygse = ctDoublePtrPtr(self.Bygse)
		_Bzgse = ctDoublePtrPtr(self.Bzgse)
		
		_xsm = ctDoublePtrPtr(self.xsm)
		_ysm = ctDoublePtrPtr(self.ysm)
		_zsm = ctDoublePtrPtr(self.zsm)

		_Bxsm = ctDoublePtrPtr(self.Bxsm)
		_Bysm = ctDoublePtrPtr(self.Bysm)
		_Bzsm = ctDoublePtrPtr(self.Bzsm)
		
		
		_s = ctDoublePtrPtr(self.s)
		_R = ctDoublePtrPtr(self.R)
		_Rnorm = ctDoublePtrPtr(self.Rnorm)		
		_FP = ctDoublePtrPtr(self.FP)


		
		#get model parameters
		self.params = GetModelParams(self.Date,self.ut,Model,**kwargs)
		_Parmod = ctDoublePtrPtr(self.params['parmod'])
		
		#call the C code
		_CTraceField(	self.n,self.Xin,self.Yin,self.Zin,
						self.Date,self.ut,ModelCode,
						self.params['iopt'],_Parmod,
						self.params['Vx'],self.params['Vy'],self.params['Vz'],
						self.alt,self.MaxLen,self.DSMax,
						self.Verb,self.TraceDir,
						CoordInCode,self.nstep,
						_xgsm,_ygsm,_zgsm,
						_Bxgsm,_Bygsm,_Bzgsm,
						_xgse,_ygse,_zgse,
						_Bxgse,_Bygse,_Bzgse,
						_xsm,_ysm,_zsm,
						_Bxsm,_Bysm,_Bzsm,
						_s,_R,_Rnorm,_FP,
						self.nalpha,self.alpha,self.halpha)

		#reshape the footprints
		fpnames = ['GlatN','GlatS','MlatN','MlatS',
					'GlonN','GlonS','MlonN','MlonS',
					'GltN','GltS','MltN','MltS',
					'Lshell','MltE','FlLen']


		
		#flatten things and unpack footprints
		if self.n == 1 and FlattenSingleTraces:
			flat = ['nstep','xgsm','ygsm','zgsm','xgse','ygse','zgse',
					'xsm','ysm','zsm','Bxgsm','Bygsm','Bzgsm','Bxgse',
					'Bygse','Bzgse','Bxsm','Bysm','Bzsm','s','R','Rnorm']
			for f in flat:
				self.__dict__[f] = self.__dict__[f][0]
			self.halpha = (self.halpha.reshape((self.n,self.nalpha,self.MaxLen)))[0]
			for i in range(0,15):
				setattr(self,fpnames[i],self.FP[0,i])
		else:
			self.halpha = self.halpha.reshape((self.n,self.nalpha,self.MaxLen))
			for i in range(0,15):
				setattr(self,fpnames[i],self.FP[:,i])

		
	
	
		
	def GetTrace(self,i,Coord='SM'):
		'''
		Return traces in a given coordinate system
		
		'''
		if Coord.upper() in ['GSM','GSE','SM']:
			if np.size(np.shape(self.xgsm)) == 1:
				x = getattr(self,'x'+Coord.lower())[:self.nstep]
				y = getattr(self,'y'+Coord.lower())[:self.nstep]
				z = getattr(self,'z'+Coord.lower())[:self.nstep]
				bx = getattr(self,'bx'+Coord.lower())[:self.nstep]
				by = getattr(self,'by'+Coord.lower())[:self.nstep]
				bz = getattr(self,'bz'+Coord.lower())[:self.nstep]
			else:
				x = getattr(self,'x'+Coord.lower())[i,:self.nstep[i]]
				y = getattr(self,'y'+Coord.lower())[i,:self.nstep[i]]
				z = getattr(self,'z'+Coord.lower())[i,:self.nstep[i]]
				bx = getattr(self,'bx'+Coord.lower())[i,:self.nstep[i]]
				by = getattr(self,'by'+Coord.lower())[i,:self.nstep[i]]
				bz = getattr(self,'bz'+Coord.lower())[i,:self.nstep[i]]
		else:
			if np.size(np.shape(self.xgsm)) == 1:
				x = self.xgsm[:self.nstep]
				y = self.ygsm[:self.nstep]
				z = self.zgsm[:self.nstep]
				bx = self.Bxgsm[:self.nstep]
				by = self.Bygsm[:self.nstep]
				bz = self.Bzgsm[:self.nstep]
				Date = self.Date
				ut = self.ut
			else:
				x = self.xgsm[i,:self.nstep[i]]
				y = self.ygsm[i,:self.nstep[i]]
				z = self.zgsm[i,:self.nstep[i]]
				bx = self.Bxgsm[i,:self.nstep[i]]
				by = self.Bygsm[i,:self.nstep[i]]
				bz = self.Bzgsm[i,:self.nstep[i]]
				Date = self.Date[i]
				ut = self.ut[i]
			if  Coord.upper() in ['GEO','MAG','GEI']:
				x,y,z = ConvCoords(x,y,z,Date,ut,'GSM',Coord)
				bx,by,bz = ConvCoords(bx,by,bz,Date,ut,'GSM',Coord)
			else:
				print('Coordinate system {:s} not recognised,returning GSM'.format(Coord.upper()))
		if np.size(np.shape(self.xgsm)) == 1:
			r = self.R[:self.nstep]
			s = self.s[:self.nstep]
			h = self.halpha[:,:self.nstep]
		else:
			r = self.R[i,:self.nstep[i]]
			s = self.s[i,:self.nstep[i]]
			h = self.halpha[i,:,:self.nstep[i]]
			
		return (x,y,z,bx,by,bz,r,s,h)
			
	def Save(self,fname,RemoveNAN=True):
		'''
		Save the data in this object to file.
		'''
		#we could save a fair bit of space by removing NANs - this will
		#mean that simple 2D arrays will become arrays of objects
		if RemoveNAN:
			ptrs = ['xgsm','ygsm','zgsm','xgse','ygse','zgse',
					'xsm','ysm','zsm','Bxgsm','Bygsm','Bzgsm','Bxgse',
					'Bygse','Bzgse','Bxsm','Bysm','Bzsm','s','R','Rnorm']
			out = {}
			keys = list(self.__dict__.keys())
			for k in keys:
				if k in ptrs:
					#fix these
					if np.size(self.__dict__[k].shape) == 1:
						#flattened
						tmp = self.__dict__[k][:self.nstep]
					else: 
						#2D
						tmp = np.zeros(self.n,dtype='object')
						for i in range(0,self.n):
							tmp[i] = self.__dict__[k][i,:self.nstep[i]]
					out[k] = tmp
				elif k == 'halpha':
					if np.size(self.halpha.shape) == 2:
						#flattened
						tmp = np.zeros(self.halpha.shape[0],dtype='object')
						for i in range(0,self.nalpha):
							tmp[i] = self.halpha[i,:self.nstep]
					else:
						#3D
						tmp = np.zeros(self.halpha.shape[:2],dtype='object')
						for i in range(0,self.n):
							for j in range(0,self.nalpha):
								tmp[i,j] = self.halpha[i,j,:self.nstep[i]]
					out[k] = tmp
				else:
					out[k] = self.__dict__[k]
		else:
			out = self.__dict__
		
		print('Saving file: {:s}'.format(fname))
		
		pf.SaveObject(out,fname)

		
	
		
	
