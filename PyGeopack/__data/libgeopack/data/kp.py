import numpy as np
import os






def CombineKP():
	
	files = np.array(os.listdir('kp/'))
	yymm = np.array([np.int32(x[2:6]) for x in files])
	
	bm = np.where(yymm > 9000)[0]
	am = np.where(yymm < 2000)[0]
	yymm[bm] += 190000
	yymm[am] += 200000
	
	ind = np.argsort(yymm)
	
	files = files[ind]
	

	n = files.size
	
	fo = open('kp.dat','w')
	
	for i in range(0,n):
		f = open('kp/'+files[i],'r')
		lines = f.readlines()
		f.close()
		
		nl = np.size(lines)
		for j in range(0,nl):
			if lines[j][4] == ' ':
				break;
			fo.write(lines[j])
		
	f.close()
		
def ConverttoBinary():
	f = open('kp.dat','r')
	lines = f.readlines()
	f.close()
	
	n = np.size(lines)
	
	dtype = [('Date','int32'),('kp','int32',(8,))]
	data = np.recarray(n,dtype=dtype)
	
	for i in range(0,n):
		s = lines[i].replace('+','').replace('-','').replace('o','').split()
		if s[0][0] == '9':
			data.Date[i] = 19000000 + np.int32(s[0])
		else:
			data.Date[i] = 20000000 + np.int32(s[0])
		data.kp[i,:] = np.int32(s[1:9])
		
	f = open('kp.bin','wb')
	np.int32(n).tofile(f)
	data.Date.tofile(f)
	data.kp.tofile(f)
	f.close()
	
	
def ReadKpBin():
	f = open('kp.bin','rb')
	n = np.fromfile(f,dtype='int32',count=1)[0]
	dtype = [('Date','int32'),('kp','int32',(8,))]
	data = np.recarray(n,dtype=dtype)
	
	data.Date = np.fromfile(f,dtype='int32',count=n)
	data.kp = np.fromfile(f,dtype='int32',count=n*8).reshape((n,8))
	f.close()
	return data
	
kpdata = ReadKpBin()

def GetKp(Date,ut):
	global kpdata
	use = np.where(kpdata.Date == Date)[0]
	return kpdata[use[0]].kp[np.int32(ut/3)]
