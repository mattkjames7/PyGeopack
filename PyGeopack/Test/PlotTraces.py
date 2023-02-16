import numpy as np
import matplotlib.pyplot as plt
from ..Tools.PlotPlanet import PlotPlanetXZ
from ..TraceField import TraceField
from ..ShueMP import PlotMPXZ

def PlotTraces(Date=20230320,ut=22.983,fig=None,maps=[1,1,0,0],MaxDT=1.0):
	

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
	Tn = TraceField(xn,yn,zn,Date,ut,Model='T96',CoordIn='SM')

	#southern traces for where north has open field lines
	sth = np.where(np.isnan(Tn.Lshell))
	Ts = TraceField(xs[sth],ys[sth],zs[sth],Date,ut,Model='T96',CoordIn='SM')

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
