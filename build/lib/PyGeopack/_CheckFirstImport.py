import os
import numpy as np
from . import Globals
import ctypes as ct

def _CheckFirstImport():
	#check if we need root or not!
	path = os.path.dirname(__file__)
	if '/usr/local/' in path:
		sudo = 'sudo '
	else:
		sudo = ''
	
	
	#first of all - check if the shared object exists
	SOFile = os.path.dirname(__file__)+"/__data/libgeopack/libgeopack.so"
	if not os.path.isfile(SOFile):
		print("libgeopack.so not found - attempting compilation!")
		CWD = os.getcwd()
		os.chdir(os.path.dirname(__file__)+"/__data/libgeopack/")
		os.system(sudo+'make')
		os.chdir(CWD)

	#Check if the GEOPACK_PATH variable has been set
	Globals.DataPath = os.getenv('GEOPACK_PATH',default='')+'/'
	if Globals.DataPath == '/':
		print('The $GEOPACK_PATH variable has not been set, this module will not function correctly without it')
		

	#check for the data file
	Globals.DataFile = Globals.DataPath + 'TSdata.bin'
	if not os.path.isfile(Globals.DataFile):
		print('Data file does not exist - to create data file run "PyGeopack.UpdateParameters()"')

	
	#load the data
	from ._CFunctions import _CInit

	if os.path.isfile(Globals.DataFile):
		DataFileCT = ct.c_char_p(Globals.DataFile.encode('utf-8'))
		_CInit(DataFileCT)
	
#	DataFile = os.path.dirname(__file__)+"/__data/libgeopack/data/TSdata.bin"
#	if not os.path.isfile(DataFile):
#		print("data file has not been extracted yet - extracting!")
#		CWD = os.getcwd()
#		os.chdir(os.path.dirname(__file__)+"/__data/libgeopack/data")
#		os.system(sudo+'7z x -y TSdata.bin.tar.7z')
#		os.system(sudo+'tar -xf TSdata.bin.tar')
#		os.system(sudo+'rm -v TSdata.bin.tar.7z')
#		os.system(sudo+'rm -v TSdata.bin.tar')

#		os.chdir(CWD)		
