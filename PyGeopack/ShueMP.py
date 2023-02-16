import numpy as np


def ShueMP(Theta,Pdyn,Bz):
	
	
	R0 = (10.22 + 1.29*np.tanh(0.184*(Bz+8.14)))*Pdyn**(-0.15151515)
	
	alpha = (0.58 - 0.007*Bz)*(1.0 + 0.024*np.log(Pdyn))

	Rmp = R0*(2/(1 + np.cos(Theta)))**alpha

	return Rmp


def PlotMPXZ(ax,Date,ut,**kwargs):

	from .Params.GetModelParams import GetModelParams

	params = GetModelParams(Date,ut,'T96')

	Pdyn = params['Pdyn'][0]
	Bz = params['Bz'][0]

	theta = np.linspace(-179,179,1000)*np.pi/180.0
	Rmp = ShueMP(theta,Pdyn,Bz)

	x = Rmp*np.cos(theta)
	z = Rmp*np.sin(theta)

	ax.plot(x,z,**kwargs)
