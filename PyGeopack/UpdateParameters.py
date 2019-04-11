from . import Globals
import time
import kpindex as kp
from ._DownloadTS05Data import _DownloadTS05Data
import PyFileIO as pf
from ._ReadTab import _ReadTab

def UpdateParameters():
	'''
	This program will download and convert all of the model parameters
	required from Tsyganenko's own website along with the Kp index data.
	
	This may take a while.
	
	'''
	

	print('This process may take a while, please be patient')
	time.sleep(1.0)
	
	
	#Download Tsyganenko data
	_DownloadTS05Data()
	
	#Download Kp Index data
	kp.UpdateLocalData()
	
	#list the files
	files = os.listdir(Globals.DataPath+'tab/')
	nf = np.size(files)
	Years = 1995 + np.arange(nf)
	
	#read in Tsyganenko tab files
	tabs = []
	for i in range(0,nf):
		tabs.append(_ReadTab(Years[i]))
		
	#combine to one recarray
	n = 0
	for i in range(0,nf):
		n += tabs[i].size
	data = np.recarray(n,dtype=tabs[0].dtype)
	
	
	
	
