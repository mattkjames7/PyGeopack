__version__ = '0.2.9'


from . import Globals
from ._CheckFirstImport import _CheckFirstImport
_CheckFirstImport()

from .FreeTSData import FreeTSData
from .GSEtoSM import GSEtoSM
from .GSEtoGSM import GSEtoGSM
from .MLTtoMLON import MLTtoMLON
from .SMtoGSE import SMtoGSE
from .GSEtoMAG import GSEtoMAG
from .MLONtoMLT import MLONtoMLT
from .LoadTSData import LoadTSData
from .ModelField import ModelField
from .MAGtoGSE import MAGtoGSE
from .SetCustParam import SetCustParam
from .GSMtoSM import GSMtoSM
from .GetModelParams import GetModelParams
from .MAGtoGEO import MAGtoGEO,MAGtoGEOLL
from .TraceField import TraceField
from .SMtoGSM import SMtoGSM
from .GSMtoGSE import GSMtoGSE
from .GEOtoMAG import GEOtoMAG,GEOtoMAGLL
from .__del__ import __del__
from .UpdateParameters import UpdateParameters
from .GetDipoleTilt import GetDipoleTilt

from .ReadTSData import ReadTSData
