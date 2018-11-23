import numpy as np

def ReadOmniData():
	
	dtype = [('Date','int32'),('ut','float32'),('Year','int32'),('DayNo','int32'),('Hr','int32'),('Mn','int32'),
			('Bx','float32'),('By','float32'),('Bz','float32'),('Vx','float32'),('Vy','float32'),('Vz','float32'),
			('Den','float32'),('Temp','float32'),('SymH','float32'),('IMFFlag','int32'),('ISWFlag','int32'),
			('Tilt','float32'),('Pdyn','float32'),('W1','float32'),('W2','float32'),('W3','float32'),
			('W4','float32'),('W5','float32'),('W6','float32'),('G1','float32'),('G2','float32'),('Kp','float32')]
			
			
	f = open('TSdata.bin','rb')
	n = np.fromfile(f,dtype='int32',count=1)[0]
	
	data = np.recarray(n,dtype=dtype)
	names = data.dtype.names
	
	for name in names:
		data[name] = np.fromfile(f,dtype=data[name].dtype,count=n)
		
	f.close()
	return data
