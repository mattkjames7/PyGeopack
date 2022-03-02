import os
import subprocess
import numpy as np
from . import Globals
from . import ct
from ._SourceCompilation import checkLibExists

def _CheckFirstImport():
	#first of all - check if the shared object exists
	checkLibExists()

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
