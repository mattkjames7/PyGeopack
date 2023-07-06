import os
import subprocess

def compileSource():
    """Runs a script to compile the source files."""
    if (os.name == 'posix'):
        #check if we need root or not!
        path = os.path.dirname(__file__)
        if '/usr/local/' in path:
            sudo = 'sudo '
        else:
            sudo = ''

        CWD = os.getcwd()
        os.chdir(os.path.dirname(__file__)+"/__data/geopack/")
        os.system(sudo+'make')
        os.chdir(CWD)
    elif (os.name == 'nt'):
        CWD = os.getcwd()
        os.chdir(os.path.dirname(__file__)+"/__data/geopack/")
        compile = subprocess.Popen("compile.bat")
        os.chdir(CWD)
    else:
        raise Exception("The Operating System is not supported")


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
	libFilename	: str
        Filename of the source library

    """
    if(isShort):
        libFilename = "libgeopack."
    else:
        libFilename = os.path.dirname(__file__) + "/__data/geopack/lib/libgeopack."

    if(os.name=='posix'):
        extension = "so"
    elif(os.name=='nt'):
        extension = "dll"
    else:
        raise Exception("The Operating System is not supported")
    
    return libFilename + extension


def checkLibExists():
    """Check if library file exist, and start compilation script if not."""
    if not os.path.isfile(getLibFilename()):
        print(getLibFilename(isShort=True)+" not found - attempting compilation!")
        compileSource()
