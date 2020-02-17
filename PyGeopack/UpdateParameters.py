from . import Globals
import time
from ._DownloadTS05Data import _DownloadTS05Data
import PyFileIO as pf
from ._ReadTab import _ReadTab
import RecarrayTools as RT
import os
import numpy as np
from ._CalculateParameters import _CalculateParameters

	
	
def UpdateParameters(SkipWParameters=True):
	'''
	This program will calculate all of the parameters required to drive 
	the Tsyganenko models using OMNI data and Kp.
	
	This may take a while.
	
	Inputs
	======
	SkipWParameters: Boolean (default = True) if True, then the lengthy
		(and likely incorrect) process of calculating the W parameters
		will be skipped and they will be set to 0.
	
	'''
	
	try:
		import kpindex as kp
		import pyomnidata as omni
	except:
		print('Please install the following packages in order to create the parameter file: "kpindex" and "pyomnidata"')
		return
	
	print('This process may take a while, please be patient')
	time.sleep(1.0)
	
	#Download Kp Index data	
	print('Updating Kp Indices')
	kp.UpdateLocalData()
	
	#Download OMNI data
	print('Updating OMNI Data')
	omni.UpdateLocalData()
	
	#create the output directory
	if not os.path.isdir(Globals.DataPath):
		os.system('mkdir -pv '+Globals.DataPath)

	#now combining all of the data and calculating the W and G parameters
	data = _CalculateParameters(SkipWParameters)
	
	#save the data
	print('Saving')
	fname = Globals.DataPath+'TSdata.bin'
	RT.SaveRecarray(data,fname)
