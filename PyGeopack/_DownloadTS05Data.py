import os
from . import Globals

def _DownloadTS05Data():
	'''
	This function will try to download all existing TS05 archives and 
	extract them in $GEOPACK_PATH/tab.
	
	
	'''
	Year = 1995
	Cont = True
	OutPath = Globals.DataPath+'tab/'
	cmd0 = 'wget -nv --show-progress '
	cmd0 += 'http://geo.phys.spbu.ru/~tsyganenko/TS05_data_and_stuff/{:4d}_OMNI_5m_with_TS05_variables.zip'
	cmd0 += ' -O ' + OutPath + '{:04d}.zip' 
	cmd1 = 'unzip ' + OutPath + '{:04d}.zip' 
	cmd2 = 'rm -v '+ OutPath + '{:04d}.zip' 
	cmd3 = 'mv -v ' + OutPath + '{:04d}_OMNI_5m_with_TS05_variables.dat '
	cmd3 += '{:04d}.tab'
	files = []
	while Cont:
		ret = os.system(cmd0.format(Year,Year))
		if ret == 0:
			#extract file
			os.system(cmd1.format(Year))
			#delete archive
			os.system(cmd2.format(Year))	
			#rename tab
			os.system(cmd3.format(Year,Year))
			files.append(OutPath+'{:04}.tab')
		else:
			#stop loop
			Cont = False
		Year += 1
