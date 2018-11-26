import os
import numpy as np

def _CheckFirstImport():
	#check if we need root or not!
	path = os.path.dirname(__file__)
	if '/usr/local/' in path:
		sudo = 'sudo '
	else:
		sudo = ''
	
	
	#first of all - check if the shared object exists
	SOFile64 = os.path.dirname(__file__)+"/__data/libgeopack/libgeopack64.so"
	SOFile32 = os.path.dirname(__file__)+"/__data/libgeopack/libgeopack32.so"
	if not os.path.isfile(SOFile32) or not os.path.isfile(SOFile64):
		print("libgeopack.so not found - attempting compilation!")
		CWD = os.getcwd()
		os.chdir(os.path.dirname(__file__)+"/__data/libgeopack/")
		os.system(sudo+'make')
		os.chdir(CWD)
		
	DataFile = os.path.dirname(__file__)+"/__data/libgeopack/data/TSdata.bin"
	if not os.path.isfile(DataFile):
		print("data file has not been extracted yet - extracting!")
		CWD = os.getcwd()
		os.chdir(os.path.dirname(__file__)+"/__data/libgeopack/data")
		os.system(sudo+'7z x -y TSdata.bin.tar.7z')
		os.system(sudo+'tar -xf TSdata.bin.tar')
		os.system(sudo+'rm -v TSdata.bin.tar.7z')
		os.system(sudo+'rm -v TSdata.bin.tar')

		os.chdir(CWD)		
