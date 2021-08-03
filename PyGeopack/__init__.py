__version__ = '0.2.9'


from . import Globals
from ._CheckFirstImport import _CheckFirstImport
_CheckFirstImport()

from .FreeParams import FreeParams
from .GSEtoSM import GSEtoSM
from .GSEtoGSM import GSEtoGSM
from .MLTtoMLON import MLTtoMLON
from .SMtoGSE import SMtoGSE
from .GSEtoMAG import GSEtoMAG
from .MLONtoMLT import MLONtoMLT
#from .LoadTSData import LoadTSData
from .ModelField import ModelField
from .MAGtoGSE import MAGtoGSE
#from .SetCustParam import SetCustParam
from .GSMtoSM import GSMtoSM
from .GetModelParams import GetModelParams
from .MAGtoGEO import MAGtoGEO,MAGtoGEOLL
from .TraceField import TraceField
from .SMtoGSM import SMtoGSM
from .GSMtoGSE import GSMtoGSE
from .GEOtoMAG import GEOtoMAG,GEOtoMAGLL
from .GEOtoGEI import GEOtoGEI
from .GEItoGEO import GEItoGEO
from .GEOtoGSM import GEOtoGSM
from .GSMtoGEO import GSMtoGEO
from .__del__ import __del__
from .UpdateParameters import UpdateParameters
from .GetDipoleTilt import GetDipoleTilt

from .ReadTSData import ReadTSData

from .GEOtoGSE import GEOtoGSE
from .GEOtoSM import GEOtoSM
from .GSEtoGEO import GSEtoGEO
from .SMtoGEO import SMtoGEO
from .GEItoGSE import GEItoGSE
from .GSEtoGEI import GSEtoGEI
from .GEItoGSM import GEItoGSM
from .GSMtoGEI import GSMtoGEI
from .GEItoSM import GEItoSM
from .SMtoGEI import SMtoGEI
from .GEItoMAG import GEItoMAG
from .MAGtoGEI import MAGtoGEI
from .GSMtoMAG import GSMtoMAG
from .MAGtoGSM import MAGtoGSM
from .SMtoMAG import SMtoMAG
from .MAGtoSM import MAGtoSM

from .ConvCoords import ConvCoords


#temporary test function
from .TestHalpha import TestHalpha
