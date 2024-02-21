import os
ModulePath = os.path.dirname(__file__)+'/'

def checkWarning():
    warnVar = os.getenv("GEOPACK_NOWARN","")
    if warnVar.lower() in ["true","1"]:
        return False
    else:
        return True
showWarnings = checkWarning()

DataPath = None
DataFile = None
