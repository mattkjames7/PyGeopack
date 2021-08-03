import numpy as np
import matplotlib.pyplot as plt
from .TraceField import TraceField


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
	
def _RotMatrices(T):
	
	B = np.array([[0.0,0.0,1.0]]).T
	
	R = []
	for i in range(0,T.nstep):
		A = np.array([[T.Bxsm[i],T.Bysm[i],T.Bzsm[i]]]).T
		A = A/np.linalg.norm(A)
		
		R.append(_RotationMatrix(B,A))

	return R
	
def _RotateTrace(T,R):
	
	x = T.xsm[:T.nstep]
	y = T.ysm[:T.nstep]
	z = T.zsm[:T.nstep]
	v = np.array([x,y,z])

	r = np.zeros(v.shape,dtype='float64')
	for i in range(0,T.nstep):
		r[:,i] = np.dot(np.array([v[:,i]]),R)[0]
	
	return r[0],r[1],r[2]
	

def _MidPoint(T):
	
	ind = np.argmax(T.R[:T.nstep])
	
	return ind,T.xsm[ind],T.ysm[ind],T.zsm[ind]

def TestHalpha(fig=None,maps=[1,1,0,0]):
	
	
	#firstly - get a trace
	T = TraceField(5.0,0.0,0.0,20120101,12.0,CoordIn='SM')
	ife,xfe,yfe,zfe = _MidPoint(T)
	
	#get the new trace position
	alpha0 = 90.0*np.pi/180.0
	alpha1 = 90.0*np.pi/180.0 + np.pi
	Delta = 0.1
	
	dt = Delta*np.cos(alpha0)
	dp = Delta*np.sin(alpha0)
	
	
	beta = np.arctan2(-yfe,-xfe) - np.pi
	dx = dp*np.cos(beta) - dt*np.sin(beta)
	dy = dp*np.sin(beta) + dt*np.cos(beta)
	xt0 = xfe + dx
	yt0 = yfe + dy
	zt0 = zfe

	xt1 = xfe - dx
	yt1 = yfe - dy
	zt1 = zfe
	
	# get two traces at a slightly different positions
	T0 = TraceField(xt0,yt0,zt0,20120101,12.0,CoordIn='SM')
	T1 = TraceField(xt1,yt1,zt1,20120101,12.0,CoordIn='SM')
	
	#list the rotation matrices for each vector in the original trace
	R = _RotMatrices(T)
	
	#pick one to rotate by
	Ir0 = 20
	rx0,ry0,rz0 = _RotateTrace(T,R[Ir0])

	

	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig

	ax.plot(T.xsm,T.zsm,label='Original')
	ax.plot(T0.xsm,T0.zsm,label='Outer')
	ax.plot(T1.xsm,T1.zsm,label='Inner')
	
	ax.plot(rx0,rz0,linestyle='--',label='Original Rotated',color='magenta')
	ax.scatter(T.xsm[Ir0],T.zsm[Ir0],color='magenta')
	ax.scatter(rx0[Ir0],rz0[Ir0],color='magenta')
	
	ax.set_aspect(1.0)
	ax.legend()

	return ax
