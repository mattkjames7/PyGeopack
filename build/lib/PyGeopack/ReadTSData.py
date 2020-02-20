import numpy as np
from . import Globals
import RecarrayTools as RT


def ReadTSData():
	'''
	Reads the model parameter dataset stored in GEOPACK_PATH
	
	'''
	
	#get the filename
	fname = Globals.DataFile
	
	#define the dtype
	dtype = [('Date','int32'),('ut','float32'),('Year','int32'),('DayNo','int32'),
			('Hr','int32'),('Mn','int32'),('Bx','float32'),('By','float32'),
			('Bz','float32'),('Vx','float32'),('Vy','float32'),('Vz','float32'),
			('Den','float32'),('Temp','float32'),('SymH','float32'),('IMFFlag','int32'),
			('ISWFlag','int32'),('Tilt','float32'),('Pdyn','float32'),
			('W1','float32'),('W2','float32'),('W3','float32'),
			('W4','float32'),('W5','float32'),('W6','float32'),
			('G1','float32'),('G2','float32'),('Kp','float32')]
			
	#read it in!
	return RT.ReadRecarray(fname,dtype)
