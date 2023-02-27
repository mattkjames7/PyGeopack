import numpy as np
import matplotlib.pyplot as plt
from ..Tools.PlotPlanet import PlotPlanetXZ
from ..TraceField import TraceField
from ..ShueMP import PlotMPXZ,WithinMP,ShueMP
from ..Tools.T89CurrentSheet import _T89CurrentSheet
from ..Params.GetDipoleTilt import GetDipoleTilt
from ..Coords.SMtoGSM import SMtoGSM

def PlotTraces(Date=20230320,ut=22.983,fig=None,maps=[1,1,0,0],MaxDT=1.0,Pdyn=2.0,Bz=0.0):
	

	#colatitudes to trace from
	clat = (90 - np.linspace(45,135,19))*np.pi/180

	#SM coords (North)
	xn = 1.02*np.sin(clat)
	yn = np.zeros(19)
	zn = 1.02*np.cos(clat)

	#SM coords (south)
	xs = -xn[::-1]
	ys = yn
	zs = -zn[::-1]

	#get northern traces
	Tn = TraceField(xn,yn,zn,Date,ut,Model='T96',CoordIn='SM',Pdyn=Pdyn,Bz=Bz)

	#southern traces for where north has open field lines
	sth = np.where(np.isnan(Tn.Lshell))
	Ts = TraceField(xs[sth],ys[sth],zs[sth],Date,ut,Model='T96',CoordIn='SM',Pdyn=Pdyn,Bz=Bz)

	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig

	
	nuse = np.where(((Tn.MltE >= (12 - MaxDT)) & (Tn.MltE <= (12 + MaxDT))) | (Tn.MltE <= MaxDT) | (Tn.MltE >= (24 - MaxDT)) | np.isnan(Tn.MltE))[0]
	suse = np.where(((Ts.MltE >= (12 - MaxDT)) & (Ts.MltE <= (12 + MaxDT))) | (Ts.MltE <= MaxDT) | (Ts.MltE >= (24 - MaxDT)) | np.isnan(Ts.MltE))[0]
	Tn.PlotXZ(nuse,Coord='GSE',fig=ax)
	Ts.PlotXZ(suse,Coord='GSE',fig=ax)
	
		
	ax.set_xlim(12,-12)
	ax.set_ylim(-12,12)
	ax.set_aspect(1.0)

	PlotMPXZ(ax,Date,ut,color='blue',linewidth=3.0)

	return ax


def GetDaysideTraces(Date,ut,Pdyn=2.0,Bz=0.0):

	#find the magnetopause first
	psi = GetDipoleTilt(Date,ut)
	x = np.linspace(2,20,100)
	y = np.zeros(100)
	z = _T89CurrentSheet(x,y,psi)

	wmp = WithinMP(x,y,z)

	use = np.where(wmp)[0]
	xmx = x[use].max()

	#create trace footprint srtarting points in SM
	nfp = 6
	xfp = np.linspace(2.0,xmx,nfp)
	yfp = np.zeros(6,dtype='float64')
	zfp = _T89CurrentSheet(x,y,psi)

	#trace
	T = TraceField(xfp,yfp,zfp,Date,ut,CoordIn='SM',alpha=[],Model='T96',Pdyn=Pdyn,Bz=Bz)


	return T

def GetNightsideTraces(Date,ut,Pdyn=2.0,Bz=0.0):

	psi = GetDipoleTilt(Date,ut)
	x0 = np.linspace(-2,-30,100)
	y0 = np.zeros(100)
	z0 = _T89CurrentSheet(x0,y0,psi)	
	d0 = np.sqrt((x0[1:] - x0[:-1])**2 + (y0[1:] - y0[:-1])**2 + (z0[1:] - z0[:-1])**2)
	s0 = np.append(0.0,np.cumsum(d0))

	nfp = 6
	s = np.linspace(2.0,20.0,nfp)
	xfp = np.interp(s,s0,x0)
	yfp = np.interp(s,s0,y0)
	zfp = np.interp(s,s0,z0)


	#trace
	T = TraceField(xfp,yfp,zfp,Date,ut,CoordIn='SM',alpha=[],Model='T96',Pdyn=Pdyn,Bz=Bz)

	return T


def GetMPatX(x):

	theta = np.linspace(0,179.0,1000)*np.pi/180
	rmp = ShueMP(theta,2.0,0.0)
	xmp = rmp*np.cos(theta)
	zmpn = np.interp(x,xmp[::-1],rmp[::-1])

	return zmpn

def GetLobeTracesCrap(Date,ut,Tn,Pdyn=2.0,Bz=0.0):

	#start by finding the current sheet at x = -10
	psi = GetDipoleTilt(Date,ut)
	zs = _T89CurrentSheet(-10.0,0.0,psi)

	#find the furthest trace
	ind = np.where(Tn.Lshell == np.nanmax(Tn.Lshell))[0]

	#find roughly where the trace crosses x = -10
	nth = np.where(Tn.zsm[ind] > zs)[0]
	sth = np.where(Tn.zsm[ind] < zs)[0]

	In = np.abs(Tn.xsm[ind,nth] + 10).argmin()
	Is = np.abs(Tn.xsm[ind,sth] + 10).argmin()

	zn = Tn.zsm[ind,nth[In]]
	zs = Tn.zsm[ind,sth[Is]]

	#get magnetopause at x = -10
	xgn,_,zgn = SMtoGSM(-10.0,0.0,zn,Date,ut)
	xgs,_,zgs = SMtoGSM(-10.0,0.0,zs,Date,ut)
	rmpn = GetMPatX(xgn)
	rmps = GetMPatX(xgs)
	print(rmpn,rmps)

	#get the footprints for starting traces
	nfp = 6
	zfpn = np.linspace(zgn,rmpn,nfp+1)[1:]
	zfps = np.linspace(zgs,-rmps,nfp+1)[1:]
	xfpn = np.zeros(nfp,dtype='float64') + xgn
	xfps = np.zeros(nfp,dtype='float64') + xgs
	yfp = np.zeros(nfp,dtype='float64')

	#trace
	Tln = TraceField(xfpn,yfp,zfpn,Date,ut,CoordIn='SM',alpha=[],Model='T96',Pdyn=Pdyn,Bz=Bz)
	Tls = TraceField(xfps,yfp,zfps,Date,ut,CoordIn='SM',alpha=[],Model='T96',Pdyn=Pdyn,Bz=Bz)
	print(Tls.nstep)
	return Tln,Tls

def GetLobeTraces(Date,ut,Td,Tn,Pdyn=2.0,Bz=0.0):

	#find the furthest trace
	indn = np.where(Tn.Lshell == np.nanmax(Tn.Lshell))[0][0]
	indd = np.where(Td.Lshell == np.nanmax(Td.Lshell))[0][0]

	#find roughly where the trace crosses r = 10
	psi = GetDipoleTilt(Date,ut)
	zsd = _T89CurrentSheet(Td.Lshell[indd],0.0,psi)
	zsn = _T89CurrentSheet(Tn.Lshell[indn],0.0,psi)

	nthd = np.where(Td.zsm[indd] > zsd)[0]
	sthd = np.where(Td.zsm[indd] < zsd)[0]

	nthn = np.where(Tn.zsm[indn] > zsn)[0]
	sthn = np.where(Tn.zsm[indn] < zsn)[0]

	Rc = 7.0
	Ind = np.abs(Td.R[indd,nthd] - Rc).argmin()
	Isd = np.abs(Td.R[indd,sthd] - Rc).argmin()

	Inn = np.abs(Tn.R[indn,nthn] - Rc).argmin()
	Isn = np.abs(Tn.R[indn,sthn] - Rc).argmin()


	xnd = Td.xgsm[indd,nthd[Ind]]
	xsd = Td.xgsm[indd,sthd[Isd]]

	xnn = Tn.xgsm[indn,nthn[Inn]]
	xsn = Tn.xgsm[indn,sthn[Isn]]

	znd = Td.zgsm[indd,nthd[Ind]]
	zsd = Td.zgsm[indd,sthd[Isd]]

	znn = Tn.zgsm[indn,nthn[Inn]]
	zsn = Tn.zgsm[indn,sthn[Isn]]


	thetand = np.arctan2(znd,xnd)
	thetasd = np.arctan2(zsd,xsd)

	thetann = (np.arctan2(znn,xnn) + 2*np.pi) % (2*np.pi)
	thetasn = np.arctan2(zsn,xsn)
	if thetasn > 0:
		thetasn -= (2*np.pi)

	nfp = 6
	thetan = np.linspace(thetand,thetann,nfp + 2)[1:-1]
	thetas = np.linspace(thetasd,thetasn,nfp + 2)[1:-1]


	xfpn = Rc*np.cos(thetan)
	zfpn = Rc*np.sin(thetan)

	xfps = Rc*np.cos(thetas)
	zfps = Rc*np.sin(thetas)

	yfp = np.zeros(nfp,dtype='float64')



	Tln = TraceField(xfpn,yfp,zfpn,Date,ut,CoordIn='GSM',alpha=[],Model='T96',Pdyn=Pdyn,Bz=Bz)
	Tls = TraceField(xfps,yfp,zfps,Date,ut,CoordIn='GSM',alpha=[],Model='T96',Pdyn=Pdyn,Bz=Bz)

	return Tln,Tls	

def PlotTracesXZ(Date=20230320,ut=22.983,fig=None,maps=[1,1,0,0],Pdyn=2.0,Bz=0.0):
	

	#dayside traces with starting points along CS
	Td = GetDaysideTraces(Date,ut,Pdyn=Pdyn,Bz=Bz)

	#nightside traces
	Tn = GetNightsideTraces(Date,ut,Pdyn=Pdyn,Bz=Bz)
	usen = np.where(np.isfinite(Tn.Lshell))[0]

	#lobe traces
	Tln,Tls = GetLobeTraces(Date,ut,Td,Tn,Pdyn=Pdyn,Bz=Bz)
	useln = np.where(Tln.nstep > 0)[0]
	usels = np.where(Tls.nstep > 0)[0]

	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig


	
	Tn.PlotXZ(usen,Coord='GSE',fig=ax)
	Td.PlotXZ(Coord='GSE',fig=ax)
	Tln.PlotXZ(useln,Coord='GSE',fig=ax,color='purple')
	Tls.PlotXZ(usels,Coord='GSE',fig=ax,color='purple')
	

		
	ax.set_xlim(12,-12)
	ax.set_ylim(-12,12)
	ax.set_aspect(1.0)

	PlotMPXZ(ax,Date,ut,color='blue',linewidth=3.0,Pdyn=Pdyn,Bz=Bz)

	return ax
