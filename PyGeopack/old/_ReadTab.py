import PyFileIO as pf
from . import Globals
import os
import DateTimeTools as TT
import numpy as np

def _ReadTab(Year):
	'''
	Reads OMNI data tab with Tsyganenko parameters.
	
	Input:
		Year: Integer year to read
	'''
	
	dtype_in = [('Year','int32'),('DayNo','int32'),('Hr','int32'),('Mn','int32'),
			('Bx','float32'),('By','float32'),('Bz','float32'),
			('Vx','float32'),('Vy','float32'),('Vz','float32'),
			('Den','float32'),('Temp','float32'),('SymH','float32'),('IMFFlag','int32'),('ISWFlag','int32'),
			('Tilt','float32'),('Pdyn','float32'),('W1','float32'),('W2','float32'),('W3','float32'),
			('W4','float32'),('W5','float32'),('W6','float32')]
	
	fname = Globals.DataPath+'tab/{:04d}.tab'.format(Year)		
	data = pf.ReadASCIIData(fname,Header=False,dtype=dtype_in)
	
			
	dtype_out = [('Date','int32'),('ut','float32'),('Year','int32'),('DayNo','int32'),('Hr','int32'),('Mn','int32'),
			('Bx','float32'),('By','float32'),('Bz','float32'),('Vx','float32'),('Vy','float32'),('Vz','float32'),
			('Den','float32'),('Temp','float32'),('SymH','float32'),('IMFFlag','int32'),('ISWFlag','int32'),
			('Tilt','float32'),('Pdyn','float32'),('W1','float32'),('W2','float32'),('W3','float32'),
			('W4','float32'),('W5','float32'),('W6','float32'),('G1','float32'),('G2','float32'),('Kp','float32')]

	out = np.recarray(data.size,dtype=dtype_out)
	
	names = data.dtype.names
	
	for n in names:
		if n in out.dtype.names:
			out[n] = data[n]
			
	out.G1 = 0.0
	out.G2 = 0.0
	out.Kp = 0.0
	out.ut = out.Hr + out.Mn/60.0
	
	for i in range(0,out.size):
		out.Date[i] = TT.DayNotoDate(out.Year[i],out.DayNo[i])
		
		
	return out
			
	
