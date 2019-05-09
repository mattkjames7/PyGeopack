from .T89c import T89c
from .T89f import T89f

def Test(kp,psi,x,y,z):
	print('Python:  ',T89c(kp,psi,x,y,z))
	print('Fortran: ',T89f(kp,psi,x,y,z))
