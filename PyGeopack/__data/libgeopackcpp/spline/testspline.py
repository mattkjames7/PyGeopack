import numpy as np
import matplotlib.pyplot as plt
import ctypes as ct
#from Spline import Spline
from  scipy.interpolate import InterpolatedUnivariateSpline

libspline = ct.CDLL("./libspline.so")

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

spline = libspline.spline
spline.argtypes = [	c_int, 		
					c_double_ptr, 		
					c_double_ptr, 		
					c_int, 				
					c_double_ptr, 		
					c_double_ptr]
spline.restype = None


def pyspline(x0,y0,x1):
	n = x0.size - 1
	a = np.zeros(n+1,dtype='float64') + y0
	b = np.zeros(n,dtype='float64')
	c = np.zeros(n+1,dtype='float64')
	d = np.zeros(n,dtype='float64')
	h = np.zeros(n,dtype='float64')
	l = np.zeros(n+1,dtype='float64')
	mu = np.zeros(n+1,dtype='float64')
	z = np.zeros(n+1,dtype='float64')
	alpha = np.zeros(n,dtype='float64')
	
	for i in range(0,n):
		h[i] = x0[i+1] - x0[i]
	
	for i in range(1,n):
		alpha[i] = (3.0/h[i])*(a[i+1] - a[i]) - (3.0/h[i-1])*(a[i] - a[i-1])
		
	for i in range(1,n):
		l[i] = 2*(x0[i+1] - x0[i-1]) - h[i-1]*mu[i-1]
		mu[i] = h[i]/l[i]
		z[i] = (alpha[i] - h[i-1]*z[i-1])/l[i]
		
	l[n] = 1.0
	for j in range(n-1,-1,-1):
		c[j] = z[j] - mu[j]*c[j+1]
		b[j] = (a[j+1] - a[j])/h[j] - h[j]*(c[j+1] + 2*c[j])/3.0
		d[j] = (c[j+1] - c[j])/(3*h[j])
		
	#interpolate
	y1 = np.zeros(x1.size,dtype='float64')
	
	for i in range(0,x1.size):
		if x1[i] < x0[0]:
			I = 0
		elif x1[i] >= x0[-1]:
			I = n-1
		else:
			for j in range(0,n):
				if (x1[i] >= x0[j]) and (x1[i] <= x0[j+1]):
					I = j
					break
		dx = x1[i] - x0[I]
		#dx = (x1[i] - x0[I])/(x0[I+1] - x0[I])
		y1[i] = a[I] + b[I]*dx + c[I]*dx**2 + d[I]*dx**3
		
	return y1
	

def testspline(fig=None,maps=[1,1,0,0]):
	
	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig
	
	n0 = np.int32(4)
	x0 = (np.linspace(0.0,2*np.pi,n0) + 0.1*np.random.randn(n0)).astype('float64') + 5
	y0 = np.sin(x0).astype('float64')
	
	n1 = np.int32(100)
	x1 = np.linspace(-0.5,7.0,n1).astype('float64') + 5
	y1 = np.zeros(n1,dtype='float64')
	
	spline(n0,x0,y0,n1,x1,y1)
	y1p = pyspline(x0,y0,x1)
	#spl = Spline(x0,y0)
	#y1p2 = spl.Interpolate(x1)
	spspl = InterpolatedUnivariateSpline(x0,y0)
	y1sp = spspl(x1)
	
	
	ax.scatter(x0,y0)
	ax.plot(x1,y1,label='C')
	ax.plot(x1,y1p+0.1,label='Py')
	#ax.plot(x1,y1p2+0.2,label='Py 2')
	ax.plot(x1,y1sp+0.2,label='scipy')
	ax.set_ylim(-2.0,2.0)
	ax.legend()
	plt.show()
	return ax


if __name__ == '__main__':
	testspline()
