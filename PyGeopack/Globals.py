import os
ModulePath = os.path.dirname(__file__)+'/'

def checkWarning():
    warnVar = os.getenv("PYGEOPACK_NOWARN","")
    if warnVar.lower() in ["true","1"]:
        return True
    else:
        return False
showWarnings = checkWarning()

DataPath = None
DataFile = None
