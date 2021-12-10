import os
import numpy as np
from . import Globals
from . import ct

def _CheckFirstImport():
	#check if we need root or not!
	path = os.path.dirname(__file__)
	if '/usr/local/' in path:
		sudo = 'sudo '
	else:
		sudo = ''
	
	
	#first of all - check if the shared object exists
	SOFile = os.path.dirname(__file__)+"/__data/libgeopackcpp/libgeopack.so"
	if not os.path.isfile(SOFile):
		print("libgeopack.so not found - attempting compilation!")
		CWD = os.getcwd()
		os.chdir(os.path.dirname(__file__)+"/__data/libgeopackcpp/")
		os.system(sudo+'make')
		os.chdir(CWD)

	#Check if the GEOPACK_PATH variable has been set
	Globals.DataPath = os.getenv('GEOPACK_PATH',default='')+'/'
	if Globals.DataPath == '/':
		print('The $GEOPACK_PATH variable has not been set, this module will not function correctly without it')
		

	#check for the data file
	Globals.DataFile = Globals.DataPath + 'TSdata.bin'
	if not os.path.isfile(Globals.DataFile):
		print('Data file does not exist - to create data file run "PyGeopack.Params.UpdateParameters()"')

	
	#load the data
	from .Params._CFunctions import _CInit

	if os.path.isfile(Globals.DataFile):
		DataFileCT = ct.ctString(Globals.DataFile)
		_CInit(DataFileCT)
	else:
		_CInit(ct.ctString(''))
