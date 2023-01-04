import numpy as np
from ._CFunctions import _CTraceField
import ctypes
from .Params.GetModelParams import GetModelParams
from .ct import ctString,ctBool,ctInt,ctIntPtr,ctFloatPtr,ctDoublePtr,ctDoublePtrPtr
import PyFileIO as pf
from .Coords.ConvCoords import ConvCoords
import matplotlib.pyplot as plt
from scipy.interpolate import InterpolatedUnivariateSpline,interp1d
from .Tools.PlotPlanet import PlotPlanetXY,PlotPlanetXZ,PlotPlanetYZ
from .Tools.GetLegendHandLab import GetLegendHandLab

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
			#read from file or dict
			if isinstance(args[0],dict):
				#assume that the dictionary provided is a TraceField dict
				self.__dict__ = args[0]
			else:
				#load from file
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
				alt=100.0,MaxLen=1000,DSMax=1.0,
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
		self.halpha = self.halpha.reshape((self.n,self.nalpha,self.MaxLen))
		for i in range(0,15):
			setattr(self,fpnames[i],self.FP[:,i])
	
	
	
		
	def GetTrace(self,i,Coord='SM'):
		'''
		Return traces in a given coordinate system.
		
		Inputs
		======
		i : int
			Index of the trace to be returned.
		Coord : str
			Coordinate system of the returned arrays: 
			'GSE'|'GSM'|'SM'|'MAG'|'GEI'|'GEO'
		
		Returns
		=======
		x : float
			x-coordinate (R_E)
		y : float
			y-coordinate (R_E)
		z : float
			z-coordinate (R_E)
		bx : float
			x-component of the magnetic field (nT)
		by : float
			y-component of the magnetic field (nT)
		bz : float
			z-component of the magnetic field (nT)
		r : float
			radial distance (R_E)
		rnorm : float
			Normalised radial distance (Rnorm = 1.0 at Rmax)
		s : float
			Distance along the field line trace (R_E)
		h : float
			H_alpha array.
		
		'''
		if Coord.upper() in ['GSM','GSE','SM']:
			x = getattr(self,'x'+Coord.lower())[i][:self.nstep[i]]
			y = getattr(self,'y'+Coord.lower())[i][:self.nstep[i]]
			z = getattr(self,'z'+Coord.lower())[i][:self.nstep[i]]
			bx = getattr(self,'Bx'+Coord.lower())[i][:self.nstep[i]]
			by = getattr(self,'By'+Coord.lower())[i][:self.nstep[i]]
			bz = getattr(self,'Bz'+Coord.lower())[i][:self.nstep[i]]
		else:
			x = self.xgsm[i][:self.nstep[i]]
			y = self.ygsm[i][:self.nstep[i]]
			z = self.zgsm[i][:self.nstep[i]]
			bx = self.Bxgsm[i][:self.nstep[i]]
			by = self.Bygsm[i][:self.nstep[i]]
			bz = self.Bzgsm[i][:self.nstep[i]]
			Date = self.Date[i]
			ut = self.ut[i]
			if  Coord.upper() in ['GEO','MAG','GEI']:
				x,y,z = ConvCoords(x,y,z,Date,ut,'GSM',Coord)
				bx,by,bz = ConvCoords(bx,by,bz,Date,ut,'GSM',Coord)
			else:
				print('Coordinate system {:s} not recognised,returning GSM'.format(Coord.upper()))
		r = self.R[i][:self.nstep[i]]
		rnorm = self.Rnorm[i][:self.nstep[i]]
		s = self.s[i][:self.nstep[i]]
		h = self.halpha[i,:][:self.nstep[i]]
			
		return (x,y,z,bx,by,bz,r,rnorm,s,h)
			
	def Save(self,fname,RemoveNAN=True):
		'''
		Save the data in this object to file.
		
		Inputs
		======
		fname : str
			Path to the file where this trace will be save on disk.
		RemoveNAN : bool
			If True then arrays will be shortened by removing nans.
			
		'''
		out = TraceDict(RemoveNAN)
		
		print('Saving file: {:s}'.format(fname))
		
		pf.SaveObject(out,fname)

	def TraceDict(self,RemoveNAN=True):
		'''
		Return a dictionary with all of the outputs of the field trace.
		
		Inputs
		======
		RemoveNAN : bool
			If True then arrays will be shortened by removing nans.
			
		Returns
		=======
		out : dict
			Contains the field traces coordinates, field components etc.
		
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
					if self.flat:
						#flattened
						tmp = self.__dict__[k][:self.nstep]
					else: 
						#2D
						tmp = np.zeros(self.n,dtype='object')
						for i in range(0,self.n):
							tmp[i] = self.__dict__[k][i,:self.nstep[i]]
					out[k] = tmp
				elif k == 'halpha':
					if self.flat:
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
		return out
	
		
	def PlotHarmonics(self,I,A,fig=None,maps=[1,1,0,0],Harmonics=[1,2,3],
						x0=None,df=0.1,Method='Simple',**kwargs):
		'''
		Plot the field displacement as in Singer et al 1981.
		
		This module requires the MHDWaveHarmonics module to be present.
		
		Inputs
		======
		I : int
			Index of the trace to plot.
		A : int
			Index for the alpha value to plot.
		fig : None|matplotlib.pyplot 
			If None, then a new figure will be created, if an instance 
			of matplotlib.pyplot is provided, an existin figure will be 
			used with a new axes, if an existing axes instance is 
			provided then the plotting will occur ont hat existing
			subplot.
		maps : list
			4-element list: [xmaps,ymaps,xmap,ymap] denoting the number
			of vertical (ymaps) and horizontal (xmaps) subplots on the 
			current figure and the position horizontally (xmap) and 
			vertically (ymap) of the subplot.
		Harmonics : list
			Harmonic numbers to calculate.
		x0 : float|None
			starting frequency in mHz
		df : float
			Size of frequency step in mHz
		Method : str
			'Simple'|'Complex'
		
		
		'''
		#see if we can import the harmonic package
		try:
			import MHDWaveHarmonics as wh
		except:
			print('Install MHDWaveHarmonics package to use this..')
			return
		
		
		
		#get the s,B and halpha
		x,y,z,Bx,By,Bz,R,Rnorm,s,h = self.GetTrace(I)
		B = np.sqrt(Bx**2 + By**2 + Bz**2)
		s = s*6380.0
		h = h[A]
		if np.size(self.s.shape) == 2:
			L = self.Lshell[I]
			M = self.MltE[I]
		else:
			L = self.Lshell
			M = self.MltE
			
		
		#get PMD
		
		pmd = kwargs.get('pmd',None)
		if pmd is None:
			print('provide plasma mass densities or power law')
			return
		elif np.size(pmd) == Rnorm.size:
			PMD = pmd
		else:
			pmd0 = pmd[0]
			pwr = pmd[1]
			PMD = pmd0*Rnorm**-pwr

		#alfven speed
		Va = wh.CalcFieldLineVaPMD(B,PMD)
		VaMid = wh.CalcFieldLineVaMidPMD(B,PMD,s)

		#calculate harmonic frequencies
		freq,success,_ = wh.FindHarmonicsPMD(B,PMD,s,halpha=h,RhoBG=None,
						Harmonics=Harmonics,x0=x0,df=df,Method=Method)
		
		#check for fails
		if (success == False).any():
			print("WARNING: potentially bad fits")
			
		#find crossing over z = 0
		North = np.where(z >= 0.0)[0]
		South = np.where(z < 0.0)[0]
		use = np.append(South[:-2],North[:2])
		if use.size == 4:
			f = InterpolatedUnivariateSpline(z[use],s[use])
		else:
			f = interp1d(z[use],s[use])
		Smid = f(0.0)		
			
		#create figure/axes
		if fig is None:
			fig = plt
			fig.figure()
		if hasattr(fig,'Axes'):	
			ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
		else:
			ax = fig
			
		#solve the wave for each harmonic
		ywave = []
		
		for f in freq:
			ywave = wh.SolveWave(f,s,B,Va=Va,halpha=h)

			#then plot
			ax.plot(s,ywave,label='f={:5.2f} mHz'.format(f))

		#add some stuff
		ax.set_xlim(0.0,s.max())
		ylim = ax.get_ylim()
		ax.set_ylim(ylim)
		ax.vlines(Smid,ylim[0],ylim[1],color=[0.0,0.0,0.0],linestyle=':')
		ax.text(0.2,0.75,'South',ha='center',va='center',color=[0.7,0.7,0.7],zorder=-2.0,fontsize='x-large',transform=ax.transAxes)
		ax.text(0.8,0.75,'North',ha='center',va='center',color=[0.7,0.7,0.7],zorder=-2.0,fontsize='x-large',transform=ax.transAxes)
		ax.text(Smid,0.5*(ylim[1]-ylim[0])+ylim[0],'Magnetic Equator',va='center',color=[0.0,0.0,0.0],zorder=2.0,fontsize='medium',rotation=-90.0)
		ax.plot([0.0,np.max(s)],[0.0,0.0],color=[0.0,0.0,0.0],linestyle='--')
		
		ax.legend()

		return ax



	def PlotXZ(self,ind='all',Coord='GSM',fig=None,maps=[1,1,0,0],
				label=None,color='black'):
		'''
		Plot field lines in the X-Z plane
		
		Inputs
		======
		ind : int|str
			Index of trace to plot. Can be scalar or an array. If set 
			ind='all' then all traces will be plotted.
		Coord : str
			Coordinate system of the trace to be plotted.
		fig : None|pyplot|pyplot.Axes instance
			None - new figure will be created
			pyplot - new subplot will be created on existing figure
			pyplot.Axes - existing subplot will be used
		maps : list
			4-element array-like to determine the subplot position,
			ignored when fig=pyplot.Axes.
			maps = [xmaps,ymaps,xmap,ymap]
			xmaps - number of subplots in x-direction
			ymaps - number of subplots in y-direction
			xmap - x position of this subplot
			ymap - y position of this subplot
		label : None|str
			Add label to traces.
		color : str|array-like
			Colour to plot the field lines
		'''
		
		if ind == 'all':
			ind = np.arange(self.n)
		elif np.size(ind) == 1:
			ind = np.array([ind]).flatten()
		else:
			ind = np.array(ind)
			
		
		if fig is None:
			fig = plt
			fig.figure()
		if hasattr(fig,'Axes'):	
			ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
		else:
			ax = fig
		
		mx = 0.0
		mz = 0.0
		for i in ind:
			x,_,z,_,_,_,_,_,_,_ = self.GetTrace(i,Coord=Coord)
			mx = np.nanmax([mx,np.nanmax(np.abs(x))])
			mz = np.nanmax([mz,np.nanmax(np.abs(z))])
			ln = ax.plot(x,z,color=color)
		if not label is None:
			hs,ls = GetLegendHandLab(ax)
			hs.append(ln[0])
			ls.append(label)
			ax.legend(hs,ls)
		
		ax.set_ylabel('$z_{'+'{:s}'.format(Coord)+'}$ (R$_E$)')
		ax.set_xlabel('$x_{'+'{:s}'.format(Coord)+'}$ (R$_E$)')

		mxx = mx
		mxz = mz
		mx = 1.1*np.nanmax([mxx,mxz])		
		ax.set_xlim(-mx,mx)
		ax.set_ylim(-mx,mx)
		
		PlotPlanetXZ(ax,NoonTop=False)
		ax.set_aspect(1.0)

		return ax
	
	def PlotXY(self,ind='all',Coord='GSM',fig=None,maps=[1,1,0,0],
				label=None,color='black'):
		'''
		Plot field lines in the X-Y plane
		
		Inputs
		======
		ind : int|str
			Index of trace to plot. Can be scalar or an array. If set 
			ind='all' then all traces will be plotted.
		fig : None|pyplot|pyplot.Axes instance
			None - new figure will be created
			pyplot - new subplot will be created on existing figure
			pyplot.Axes - existing subplot will be used
		maps : list
			4-element array-like to determine the subplot position,
			ignored when fig=pyplot.Axes.
			maps = [xmaps,ymaps,xmap,ymap]
			xmaps - number of subplots in x-direction
			ymaps - number of subplots in y-direction
			xmap - x position of this subplot
			ymap - y position of this subplot
		label : None|str
			Add label to traces.
		color : str|array-like
			Colour to plot the field lines		
		'''
		
		if ind == 'all':
			ind = np.arange(self.n)
		elif np.size(ind) == 1:
			ind = np.array([ind]).flatten()
		else:
			ind = np.array(ind)
			
		
		if fig is None:
			fig = plt
			fig.figure()
		if hasattr(fig,'Axes'):	
			ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
		else:
			ax = fig
		
		mx = 0.0
		my = 0.0
		for i in ind:
			x,y,_,_,_,_,_,_,_,_ = self.GetTrace(i,Coord=Coord)
			mx = np.nanmax([mx,np.nanmax(np.abs(x))])
			my = np.nanmax([my,np.nanmax(np.abs(y))])
			ln = ax.plot(y,x,color=color)
		if not label is None:
			hs,ls = GetLegendHandLab(ax)
			hs.append(ln[0])
			ls.append(label)
			ax.legend(hs,ls)
		yl = ax.get_xlim()
		ax.set_xlim(yl[::-1])
		
		ax.set_xlabel('$y_{'+'{:s}'.format(Coord)+'}$ (R$_E$)')
		ax.set_ylabel('$x_{'+'{:s}'.format(Coord)+'}$ (R$_E$)')

		mxx = mx
		mxy = my
		mx = 1.1*np.nanmax([mxx,mxy])		
		ax.set_xlim(mx,-mx)
		ax.set_ylim(-mx,mx)
		
		PlotPlanetXY(ax)
		ax.set_aspect(1.0)
		return ax
	
	def PlotRhoZ(self,ind='all',Coord='GSM',fig=None,maps=[1,1,0,0],
					label=None,color='black'):
		'''
		Plot field lines in the rho-Z plane

		
		Inputs
		======
		ind : int|str
			Index of trace to plot. Can be scalar or an array. If set 
			ind='all' then all traces will be plotted.
		fig : None|pyplot|pyplot.Axes instance
			None - new figure will be created
			pyplot - new subplot will be created on existing figure
			pyplot.Axes - existing subplot will be used
		maps : list
			4-element array-like to determine the subplot position,
			ignored when fig=pyplot.Axes.
			maps = [xmaps,ymaps,xmap,ymap]
			xmaps - number of subplots in x-direction
			ymaps - number of subplots in y-direction
			xmap - x position of this subplot
			ymap - y position of this subplot
		label : None|str
			Add label to traces.
		color : str|array-like
			Colour to plot the field lines		
		'''
		
		if ind == 'all':
			ind = np.arange(self.n)
		elif np.size(ind) == 1:
			ind = np.array([ind]).flatten()
		else:
			ind = np.array(ind)
			
		
		if fig is None:
			fig = plt
			fig.figure()
		if hasattr(fig,'Axes'):	
			ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
		else:
			ax = fig
		
		for i in ind:
			x,y,z,_,_,_,_,_,_,_ = self.GetTrace(i,Coord=Coord)
			
			r = np.sqrt(x**2 + y**2)
			ln = ax.plot(r,z,color=color)
		if not label is None:
			hs,ls = GetLegendHandLab(ax)
			hs.append(ln[0])
			ls.append(label)
			ax.legend(hs,ls)
		
		ax.set_ylabel('$z_{'+'{:s}'.format(Coord)+'}$ (R$_E$)')
		ax.set_xlabel(r'$\rho_{'+'{:s}'.format(Coord)+'}$ (R$_E$)')

		mxr = np.nanmax(r)
		mxz = np.nanmax(z)
		mx = 1.1*np.nanmax([mxr,mxz])		
		ax.set_xlim(-mx,mx)
		ax.set_ylim(-mx,mx)
		
		PlotPlanetXZ(ax,NoonTop=False,NoShadow=True)
		ax.set_aspect(1.0)
		return ax
	
	
	def PlotHalpha(self,TI='all',AI='all',fig=None,maps=[1,1,0,0]):
		'''
		Plot h_alpha (see Singer et al 1982) for a field line.
		
		Inputs
		======
		TI : int|str
			Index of trace to plot. TI='all' will plot for all traces.
		AI : int|str
			Index of alpha angle to plot for. AI will plot all alphas.
		fig : None|matplotlib.pyplot|matplotlib.pyplot.Axes
			None - a new figure will be created with new axes
			matplotlib.pyplot - existing figure, new axes
			matplotlib.pyplot.Axes - existing axes instance to be used
				(maps ignored in the case).
		maps : list|tuple|numpy.ndarray
			Four element array-like, denoting subplot position,
			e.g. [xmaps,ymaps,xmap,ymap]
				xmaps : number of subplots in x-direction
				ymaps : number of subplots in y-direction
				xmap : position index (0 is left)
				ymap : position index (0 is top)
		
		
		'''
		if AI == 'all':
			AI = np.arange(self.nalpha)
		
		if np.size(AI) == 1:
			AI = np.array([AI]).flatten()
			
		if TI == 'all':
			TI = np.arange(self.n)
		
		if np.size(TI) == 1:
			TI = np.array([TI]).flatten()
			
		if fig is None:
			fig = plt
			fig.figure()
		if hasattr(fig,'Axes'):	
			ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
		else:
			ax = fig		
			
		for t in TI:
			if np.size(np.shape(self.s)) == 1:
				#trace arrays have been flattened 
				for a in AI:
					ax.plot(self.s,self.halpha[a],label=r'$\alpha=${:5.1f}'.format(self.alpha[a]))
			else:
				for a in AI:
					ax.plot(self.s[t],self.halpha[t,a],label=r'Trace {:d} $\alpha=${:5.1f}'.format(t,self.alpha[a]))

		ax.legend()
		ax.set_xlabel(r'$s$ (R$_E$)')
		ax.set_ylabel(r'$h_{\alpha}$')

		return ax
		
