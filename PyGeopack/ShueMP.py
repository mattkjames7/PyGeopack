import numpy as np


def ShueMP(Theta,Pdyn,V,Bz):
	
	
	R0 = (10.22 + 1.29*np.tanh(0.184*(Bz+8.14)))*Pdyn**(-0.15151515)
	
	alpha = (0.58 - 0.007*Bz)*(1.0 + 0.024*np.log(Pdyn))
	
	Rm = R0
