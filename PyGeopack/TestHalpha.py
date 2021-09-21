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
	
def _RotateTrace(T,R,P0):
	
	x = T.xsm[:T.nstep] - P0[0]
	y = T.ysm[:T.nstep] - P0[1]
	z = T.zsm[:T.nstep] - P0[2]
	v = np.array([x,y,z])

	r = np.zeros(v.shape,dtype='float64')
	for i in range(0,T.nstep):
		r[:,i] = np.dot(np.array([v[:,i]]),R)[0]
	
	return r[0],r[1],r[2]
	

def _MidPoint(T):
	
	ind = np.argmax(T.R[:T.nstep])
	
	return ind,T.xsm[ind],T.ysm[ind],T.zsm[ind]
	
def _ClosestPos(P,Tx,Ty,Tz,nstep):
	if nstep == 0 :
		return None
	elif nstep < 4:
		return np.arange([nstep]),Tx[:nstep],Ty[:nstep],Tz[:nstep]
	
	
	
	#P is perhaps uneccessary as it should be [0,0,0]
	dx = (P[0] - Tx)**2
	dy = (P[1] - Ty)**2
	dz = (P[2] - Tz)**2
	
	
	dp = np.sqrt(dx**2 + dy**2 + dz**2)[:nstep]
	
	lt0 = np.where(Tz < 0)[0]
	gt0 = np.where(Tz >= 0)[0]
	
	i1 = lt0[dp[lt0].argmin()]
	i2 = gt0[dp[gt0].argmin()]
	
	if i1 < i2:
		i0 = i1 - 1
		i3 = i2 + 1
	else:
		i0 = i1 + 1
		i3 = i2 - 1
		
	I = np.array([i0,i1,i2,i3])
	
	if I.min() < 0:
		I += 1
	if I.max() >= nstep:
		I -= 1
		
	x = Tx[I]
	y = Ty[I]
	z = Tz[I]
	
	return I,x,y,z
	

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
	Ir = 20
	P = np.array([T.xsm[Ir],T.ysm[Ir],T.zsm[Ir]])
	
	#rotate all of them
	rx,ry,rz = _RotateTrace(T,R[Ir],P)
	rx0,ry0,rz0 = _RotateTrace(T0,R[Ir],P)
	rx1,ry1,rz1 = _RotateTrace(T1,R[Ir],P)
	Pr = np.array([rx[Ir],ry[Ir],rz[Ir]])

	#find 4 values closest
	Ic0,xc0,yc0,zc0 = _ClosestPos(Pr,rx0,ry0,rz0,T0.nstep)
	

	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig

	ax.plot(T.xsm,T.zsm,label='Original',color='blue')
	ax.plot(T0.xsm,T0.zsm,label='Outer',color='orange')
	ax.plot(T1.xsm,T1.zsm,label='Inner',color='green')
	
	ax.plot(rx,rz,linestyle='--',label='Original Rotated',color='blue')
	ax.plot(rx0,rz0,linestyle='--',label='Outer Rotated',color='orange')
	ax.plot(rx1,rz1,linestyle='--',label='Inner Rotated',color='green')
	ax.scatter(T.xsm[Ir],T.zsm[Ir],color='magenta')
	ax.scatter(rx[Ir],rz[Ir],color='magenta')
	
	ax.scatter(xc0,zc0)
	
	ax.set_aspect(1.0)
	ax.legend()

	return ax
