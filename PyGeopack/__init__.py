__version__ = '1.2.5'


from . import Globals
from ._CheckFirstImport import _CheckFirstImport

from . import Params
from . import Coords

_CheckFirstImport()

from .ModelField import ModelField
from .TraceField import TraceField

from .__del__ import __del__

from . import Test
from . import Tools

from .ShueMP import ShueMP,PlotMPXZ,WithinMP