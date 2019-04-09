import numpy as np
from ._CFunctions import _CTraceField
from .SetCustParam import SetCustParam
import ctypes

###### File created automatically using PopulateCtypes ######

		
class TraceField(object):
	def __init__(self,Xin, Yin, Zin, Date, ut, Model='T96', CoordIn = 'GSM', CoordOut = 'GSM', alt=100.0, MaxLen=1000, DSMax=1.0,
				iopt=None,parmod=None,tilt=None,Vx=None,Vy=None,Vz=None,FlattenSingleTraces=True,Verbose=False):

		if ('c' in Model):
			SetCustParam(iopt,parmod,tilt,Vx,Vy,Vz)
		

		if CoordIn is 'GSE':
			CoordIn = 1
		elif CoordIn is 'GSM':
			CoordIn = 2
		elif CoordIn is 'SM':
			CoordIn = 3
		else:
			print('invalid choice for CoordIn')
			return

		if CoordOut is 'GSE':
			CoordOut = 1
		elif CoordOut is 'GSM':
			CoordOut = 2
		elif CoordOut is 'SM':
			CoordOut = 3
		else:
			print('invalid choice for CoordOut')
			return

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
		_CoordIn = np.int32(CoordIn)
		_CoordOut = np.int32(CoordOut)
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
