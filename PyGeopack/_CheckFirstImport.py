import os
import subprocess
import numpy as np
from . import Globals
from . import ct
import platform


def getLibFilename(isShort=False):
    """
    Return library filename string
    
    Inputs
    ======
    isShort : bool 
        If False return filename with full path, if True return only filename
        default - False
    
    Returns
    =======
    libFilename    : str
        Filename of the source library

    """
    if(isShort):
        libFilename = "libgeopack."
    else:
        libFilename = os.path.dirname(__file__) + "/__data/geopack/lib/libgeopack."

    systype = platform.system()
    if systype == 'Linux':
        extension = "so"
    elif systype == 'Windows':
        extension = "dll"
    elif systype == 'Darwin':
        extension = 'dylib'
    else:
        raise Exception("The Operating System is not supported")
    
    return libFilename + extension


def checkLibExists():
    """Check if library file exist, and start compilation script if not."""
    if not os.path.isfile(getLibFilename()):
        print(getLibFilename(isShort=True)+" not found, try reinstalling")
        raise SystemError



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
