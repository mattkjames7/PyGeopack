import numpy as np
from ..Params.GetDipoleTilt import GetDipoleTilt
from ..Coords.SMtoGSE import SMtoGSE

from ..ShueMP import ShueMP

def _T89CurrentSheet(x,y,psi):

	Rc = 8.8184
	G = 2.8714
	Ly = 10.0

	zs = 0.5*np.tan(psi)*(x + Rc - np.sqrt((x + Rc)**2 + 16)) - (G*np.sin(psi)*y**4)/(y**4 + Ly**4)

	return zs


def T89CurrentSheetGSE(x,y,Date,ut):
	'''

	Inputs are in SM

	'''

	#get dipole tilt
	psi = GetDipoleTilt(Date,ut)

	#current sheet SM coords
	z = _T89CurrentSheet(x,y,psi)


	#convert to GSE
	xgse,ygse,zgse = SMtoGSE(x,y,z,Date,ut)

	return xgse,ygse,zgse

def T89CurrentSheetSM(x,y,Date,ut):
	'''

	Inputs are in SM

	'''

	#get dipole tilt
	psi = GetDipoleTilt(Date,ut)

	#current sheet SM coords
	z = _T89CurrentSheet(x,y,psi)

	return x,y,z

def LMtoCSxyz(Date,ut,L,M):
	'''
	calculate the x,y,z (SM) coordinates of a
	local time and L-shell on the current sheet

	
	'''

	#get a range of L-shells covering more than the range input
	L0 = np.nanmax([0.75*np.nanmin(L),1.0])
	L1 = 1.5*np.nanmax(L)
	Ls = np.linspace(L0,L1,200)

	#calculate MLT in rads
	mrad = M*np.pi/12

	#now x and y coords
	x0 = -Ls*np.cos(mrad)
	y0 = -Ls*np.sin(mrad)

	#get dipole tilt
	psi = GetDipoleTilt(Date,ut)

	#and z
	z0 = _T89CurrentSheet(x0,y0,psi)

	#interpolate for original L values
	r = np.sqrt(x0**2 + y0**2 + z0**2)

	xfp = np.interp(L,r,x0)
	yfp = np.interp(L,r,y0)
	zfp = np.interp(L,r,z0) 

	return xfp,yfp,zfp

def CSTrace(Date,ut,L,M):
	'''
	get traces which originate in the current sheet
	
	'''

	#get footprints
	x,y,z = LMtoCSxyz(Date,ut,L,M)

	#trace
	from ..TraceField import TraceField
	T = TraceField(x,y,z,Date,ut,Model='T96',CoordIn='SM',alpha=[])

	return T

def GetLastClosedFieldLine(Date,ut,M,nL=250):

	#define the start and stop L-shells
	L0 = 7.0
	sL = ShueMP(M*np.pi/12.0 + np.pi,2.0,0.0)

	L1 = np.nanmin([300.0,sL])

	#make an array of L-shells
	L = np.linspace(L0,L1,nL)

	#get the traces
	T = CSTrace(Date,ut,L,M)

	#dtermine which ones are good (i.e. closed and within MP)
	good = np.isfinite(T.Lshell) & (T.nstep > 0) & np.isfinite(T.MlatN) & np.isfinite(T.MlatS)

	if not good.any():
		return None

	#loop to find the first time there is a "not good" footprint
	#for i in range(0,good.size):
	#	if good[i] == False:
	#		break
	i = np.where(good)[0].max() +1

	#get the trace for just that L-shell
	return CSTrace(Date,ut,L[i-1],M)