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
	#if date is a tuple, assume it contains (year,doy)
	if isinstance(Date,tuple):
		Year,Doy = Date
		Year = np.array([Year]).flatten().astype('int32')
		Doy = np.array([Doy]).flatten().astype('int32')
	else:
		Doy = TT.DayNo(Date).flatten().astype('int32')
		Year = np.array([Date//10000]).flatten().astype('int32')
		
	#if ut is a tuple, assume it contains (Hour, Minute)
	if isinstance(ut,tuple):
		Hr,Mn = ut
		Hr = np.array([Hr]).flatten().astype('int32')
		Mn = np.array([Mn]).flatten().astype('int32')		
	else:
		Hr = np.array([ut]).flatten().astype('int32')
		Mn = np.array([(ut - Hr)*60]).flatten().astype('int32')
	
	#V should be a tuple of (Vx,Vy,Vz)
	if V is None:
		Vx = np.zeros(Year.size,dtype='float64') + np.nan
		Vy = np.zeros(Year.size,dtype='float64') + np.nan
		Vz = np.zeros(Year.size,dtype='float64') + np.nan
	else:
		Vx,Vy,Vz = V
		Vx = Vx.astype('float64')
		Vy = Vy.astype('float64')
		Vz = Vz.astype('float64')

	#loop through each one
	n = Year.size
	psi = np.zeros(n,dtype='float64')
	for i in range(0,n):
		psi[i] = _CGetDipoleTilt(Year[i],Doy[i],Hr[i],Mn[i],Vx[i],Vy[i],Vz[i])

	return psi
