import numpy as np
import ctypes as ct

libmat = ct.CDLL("./libmatrix.so")

#define some dtypes
c_char_p = ct.c_char_p
c_bool = ct.c_bool
c_int = ct.c_int
c_float = ct.c_float
c_double = ct.c_double
c_float_ptr = np.ctypeslib.ndpointer(ct.c_float,flags="C_CONTIGUOUS")
c_double_ptr = np.ctypeslib.ndpointer(ct.c_double,flags="C_CONTIGUOUS")
c_int_ptr = np.ctypeslib.ndpointer(ct.c_int,flags="C_CONTIGUOUS")
c_bool_ptr = np.ctypeslib.ndpointer(ct.c_bool,flags="C_CONTIGUOUS")

RotMatrix = libmat.RotMatrix
RotMatrix.argtypes = [	c_double_ptr, 		
						c_double_ptr, 		
						c_double_ptr]
RotMatrix.restype = None


def _RotationMatrix(A,B):
	'''
	get rotation matrix R which maps vector A to B
	
	'''

	K = np.dot(B,A.T) - np.dot(A,B.T)
	K2 = np.dot(K,K)

	
	IP = np.sum(A*B)
	
	I = np.array([[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]])
	R = K + I + (1.0/(1.0 + IP))*K2
	
	return R
	
def RUN():
	
	A = np.array([0.5,1.0,0.4],dtype='float64')
	B = np.array([0.25,1.5,1.4],dtype='float64')
	
	A = A/np.linalg.norm(A)
	B = B/np.linalg.norm(B)

	print('A: ',A)
	print('B: ',B)
	
	Rp = _RotationMatrix(np.array([A]).T,np.array([B]).T)
	
	Rc = np.zeros(9,dtype='float64')
	
	RotMatrix(A.flatten(),B.flatten(),Rc)
	
	print('Python: ')
	print(Rp)
	print('C++: ')
	print(Rc.reshape((3,3)))
	
	
if __name__ == '__main__':
	RUN()
