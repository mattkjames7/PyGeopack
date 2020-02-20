import numpy as np
from ._CFunctions import _CGetDipoleTilt
import DateTimeTools as TT

def GetDipoleTilt(Date,ut,V=None):
	'''
	Calculate the dipole tilt angle.
	
	Inputs
	======
	Date: Either an array of date integers in the format yyymmdd, or a
		tuple containing arrays of (Years,DayNo).
	ut	: Either an array of time in hours (ut = hh + mm/60), or a tuple
		containing integer arrays of (Hours,Minutes).
	V	: Default = None, otherwise a tuple containing solar wind 
		velocity components (Vx,Vy,Vz).
		
	Returns
	=======
	psi : An array of dipole tilt angles in radians.
	
	'''
	_Date = np.array([Date]).flatten().astype('int32')
	_ut = np.array([ut]).flatten().astype('float32')
		

	
	#V should be a tuple of (Vx,Vy,Vz)
	if V is None:
		Vx = np.zeros(_Date.size,dtype='float64') + np.nan
		Vy = np.zeros(_Date.size,dtype='float64') + np.nan
		Vz = np.zeros(_Date.size,dtype='float64') + np.nan
	else:
		Vx,Vy,Vz = V
		Vx = Vx.astype('float64')
		Vy = Vy.astype('float64')
		Vz = Vz.astype('float64')

	#loop through each one
	n = _Date.size
	psi = np.zeros(n,dtype='float64')
	for i in range(0,n):
		psi[i] = _CGetDipoleTilt(Date[i],ut[i],Vx[i],Vy[i],Vz[i])

	return psi
