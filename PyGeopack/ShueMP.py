import numpy as np


def ShueMP(Theta,Pdyn=2.0,Bz=0.0):
	'''
	Calculate the radial distance of the Shue et al magnetopause
	given a dynamic pressure and IMF Bz

	Inputs
	======
	Theta : float
		Angle from the x-GSM axis (rads)
	Pdyn : float
		Dynamic pressure, nPa
	Bz : float
		IMF Bz (nT)

	Returns
	=======
	Rmp : float
		Radial distance fromt he centre of the Earth to the MP (R_E)
	
	'''
	
	R0 = (10.22 + 1.29*np.tanh(0.184*(Bz+8.14)))*Pdyn**(-0.15151515)
	
	alpha = (0.58 - 0.007*Bz)*(1.0 + 0.024*np.log(Pdyn))

	Rmp = R0*(2/(1 + np.cos(Theta)))**alpha

	return Rmp


def PlotMPXZ(ax,Date,ut,Pdyn=2.0,Bz=0.0,noonTop=False,**kwargs):
	'''
	Plot the magnetopause in the X-Z GSM plane

	Inputs
	======
	ax : pyplot.Axes
		Instance of a pyplot Axes
	Date : int
		Date, format yyyymmdd
	ut : float
		Hours since the start of the day

	
	'''

	from .Params.GetModelParams import GetModelParams

	params = GetModelParams(Date,ut,'T96')


	theta = np.linspace(-179,179,1000)*np.pi/180.0
	Rmp = ShueMP(theta,Pdyn,Bz)

	x = Rmp*np.cos(theta)
	z = Rmp*np.sin(theta)

	if noonTop:
		ax.plot(z,x,**kwargs)
	else:
		ax.plot(x,z,**kwargs)



def WithinMP(x,y,z,Pdyn=2.0,Bz=0.0):
	'''
	Determine whether a position in
	either GSE or GSM is within the MP or not.

	Inputs
	======
	x : float
		x-coordinate, (R_E)
	y : float
		y-coordinate, (R_E)
	z : float
		z-coordinate, (R_E)
	Pdyn : float
		Dynamic pressure, nPa
	Bz : float
		IMF Bz (nT)

	Returns
	=======
	out : bool
		True if the position is within the magnetopause
	
	'''
	#calcualte rho and theta
	rho = np.sqrt(y**2 + z**2)
	theta = np.arctan2(rho,x)
	r = np.sqrt(rho**2 + x**2)

	rmp = ShueMP(theta,Pdyn,Bz)

	return r < rmp
