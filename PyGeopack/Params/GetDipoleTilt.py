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
		

	n = _Date.size
	#V should be a tuple of (Vx,Vy,Vz)
	if V is None:
		Vx = np.zeros(n,dtype='float64') + np.nan
		Vy = np.zeros(n,dtype='float64') + np.nan
		Vz = np.zeros(n,dtype='float64') + np.nan
	else:
		Vx,Vy,Vz = V
		Vx = (np.zeros(n,dtype='float64') + np.float64(Vx))
		Vy = (np.zeros(n,dtype='float64') + np.float64(Vy))
		Vz = (np.zeros(n,dtype='float64') + np.float64(Vz))


	#loop through each one
	
	psi = np.zeros(n,dtype='float64')
	for i in range(0,n):
		psi[i] = _CGetDipoleTilt(_Date[i],_ut[i],Vx[i],Vy[i],Vz[i])

	return psi
