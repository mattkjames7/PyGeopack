import numpy as np
import os
import TimeTools as TT
from kp import GetKp

def ConvertOmniData():
	infiles = np.array(os.listdir('omnidata/'))
	infiles.sort()
	
	outfile = 'TSdata.bin'

	for i in range(0,infiles.size):
		print('\rReading File {0} of {1}'.format(i+1,infiles.size),end='')
		f = open('omnidata/'+infiles[i],'r')
		tmp  = f.readlines()
		f.close()
		if i == 0:
			lines = np.array(tmp)
		else:
			lines = np.append(lines,np.array(tmp))
	print()
	n = np.size(lines)
	
	dtype = [('Date','int32'),('ut','float32'),('Year','int32'),('DayNo','int32'),('Hr','int32'),('Mn','int32'),
			('Bx','float32'),('By','float32'),('Bz','float32'),('Vx','float32'),('Vy','float32'),('Vz','float32'),
			('Den','float32'),('Temp','float32'),('SymH','float32'),('IMFFlag','int32'),('ISWFlag','int32'),
			('Tilt','float32'),('Pdyn','float32'),('W1','float32'),('W2','float32'),('W3','float32'),
			('W4','float32'),('W5','float32'),('W6','float32'),('G1','float32'),('G2','float32'),('Kp','float32')]
	
	data = np.recarray(n,dtype=dtype)
	
	
	for i in range(0,n):
		print('\rCopying line {0} of {1}'.format(i+1,n),end='')
		s = lines[i].split()
		
		data[i].Year = np.int32(s[0])
		data[i].DayNo = np.int32(s[1])
		data[i].Hr = np.int32(s[2])
		data[i].Mn = np.int32(s[3])
		data[i].Bx = np.float32(s[4])
		data[i].By = np.float32(s[5])
		data[i].Bz = np.float32(s[6])
		data[i].Vx = np.float32(s[7])
		data[i].Vy = np.float32(s[8])
		data[i].Vz = np.float32(s[9])
		data[i].Den = np.float32(s[10])
		data[i].Temp = np.float32(s[11])
		data[i].SymH = np.float32(s[12])
		data[i].IMFFlag = np.int32(s[13])
		data[i].ISWFlag = np.int32(s[14])
		data[i].Tilt = np.float32(s[15])
		data[i].Pdyn = np.float32(s[16])
		data[i].W1 = np.float32(s[17])
		data[i].W2 = np.float32(s[18])
		data[i].W3 = np.float32(s[19])
		data[i].W4 = np.float32(s[20])
		data[i].W5 = np.float32(s[21])
		data[i].W6 = np.float32(s[22])

		data[i].Date = TT.DayNotoDate(data[i].Year,data[i].DayNo)
		data[i].ut = data[i].Hr + data[i].Mn/60.0
		
		data[i].Kp = GetKp(data[i].Date,data[i].ut)
	
	print()	
	#now to calculate G1 and G2 params
		
	for i in range(0,n):
		print('\rCalculating G param {0} of {1}'.format(i+1,n),end='')
		i0 = np.max([0,i-11])
		i1 = i + 1
		
		tmp = data[i0:i1]
		use = np.where(np.isfinite(tmp.By) & np.isfinite(tmp.Bz) & (tmp.IMFFlag > -1) & (tmp.ISWFlag > -1))[0]
		if use.size > 0:
			CA = np.arctan2(-tmp.By,tmp.Bz)
			Bp = np.sqrt(tmp.By**2 + tmp.Bz**2)
			Bs = np.abs(tmp.Bz)
			pos = np.where(tmp.Bz > 0)[0]
			Bs[pos] = 0.0
			h = ((Bp/40.0)**2)/(1.0 + Bp/40.0)
			data.G1[i] = np.sum(400.0*h*(np.sin(CA/2))**3)/use.size
			data.G2[i] = 0.005*np.sum(400.0*Bs)/use.size
		else:
			data.G1[i] = 0.0
			data.G2[i] = 0.0			

	print()
	print('Saving')
	
	f = open(outfile,'wb')
	np.int32(n).tofile(f)
	for name in data.dtype.names:
		data[name].tofile(f)
		
	f.close()
	print('Done')
