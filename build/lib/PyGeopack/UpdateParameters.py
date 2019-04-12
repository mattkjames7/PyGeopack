from . import Globals
import time
import kpindex as kp
from ._DownloadTS05Data import _DownloadTS05Data
import PyFileIO as pf
from ._ReadTab import _ReadTab
import RecarrayTools as RT
import os
import numpy as np

def UpdateParameters(Overwrite=False):
	'''
	This program will download and convert all of the model parameters
	required from Tsyganenko's own website along with the Kp index data.
	
	This may take a while.
	
	'''
	

	print('This process may take a while, please be patient')
	time.sleep(1.0)
	
	
	#Download Tsyganenko data
	_DownloadTS05Data(Overwrite)
	
	#Download Kp Index data
	kp.UpdateLocalData()
	
	#list the files
	tabdir = Globals.DataPath+'tab/'
	if not os.path.isdir(tabdir):
		os.system('mkdir -pv '+tabdir)
	files = os.listdir(tabdir)
	nf = np.size(files)
	Years = 1995 + np.arange(nf)
	
	#read in Tsyganenko tab files
	tabs = []
	for i in range(0,nf):
		print('\rReading Year {:04d}'.format(Years[i]),end='')
		tabs.append(_ReadTab(Years[i]))
	print()
	
	#combine to one recarray
	print('Combining Yearly Data...') 
	n = 0
	for i in range(0,nf):
		n += tabs[i].size
	data = np.recarray(n,dtype=tabs[0].dtype)
	
	p = 0
	for i in range(0,nf):
		data[p:p+tabs[i].size] = tabs[i]
		p += tabs[i].size

	#get Kp indices
	udate = np.unique(data.Date)
	nu = np.size(udate)
	for i in range(0,nu):
		print('\rFinding Kp Index {:6.2f}%'.format(100.0*(i+1)/nu),end='')
		use = np.where(data.Date == udate[i])[0]
		k = kp.GetKp(udate[i])
		if k.size > 0:
			kind = np.int32(data.ut[use]/3.0)
			data.Kp[use] = k.Kp[kind]
		else:
			data.Kp[use] = 1
	print()
	
	#get G1 and G2
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
	
	#save the data
	fname = Globals.DataPath+'TSdata.bin'
	RT.SaveRecarray(data,fname)
