import numpy as np
import matplotlib.pyplot as plt
from .TraceField import TraceField
from scipy.interpolate import InterpolatedUnivariateSpline
from scipy.optimize import minimize
import time

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
	x1 = np.array([x1]).flatten()
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
	
def _Closest4Pos(P,Tx,Ty,Tz,nstep):
	if nstep == 0 :
		return None
	elif nstep < 4:
		return np.arange([nstep]),Tx[:nstep],Ty[:nstep],Tz[:nstep]
	
	
	
	#P is perhaps uneccessary as it should be [0,0,0]
	dx = (P[0] - Tx)**2
	dy = (P[1] - Ty)**2
	dz = (P[2] - Tz)**2
	
	dp = np.sqrt(dx**2 + dy**2 + dz**2)[:nstep]
	
	c0 = (Tz[:-1] >= 0) & (Tz[1:] < 0)
	if not c0.any():
		i1 = dp.argmin()
	else:
		i1 = np.where(c0)[0][0]
	i2 = i1 + 1

	if i1 < i2:
		i0 = i1 - 1
		i3 = i2 + 1
	else:
		i0 = i1 + 1
		i3 = i2 - 1
		
	I = np.array([i0,i1,i2,i3])

	while I.min() < 0:
		I += 1
	while I.max() >= nstep:
		I -= 1
	
	if Tz[I[1]] > Tz[I[2]]:
		I = I[::-1]
	
	x = Tx[I]
	y = Ty[I]
	z = Tz[I]
	
	
	
	return I,x,y,z
	
def _ClosestPosSpline(x,y,z):
	
	#splines, newz = 0
	zout = 0.0
	xout = pyspline(z,x,zout)
	yout = pyspline(z,y,zout)
	
	return xout,yout,zout
	
	
def _ClosestPos(Ir,R,T,T0,T1):
	
	#pick one to rotate by
	P = np.array([T.xsm[Ir],T.ysm[Ir],T.zsm[Ir]])
	
	#rotate all of them
	rx,ry,rz = _RotateTrace(T,R[Ir],P)
	rx0,ry0,rz0 = _RotateTrace(T0,R[Ir],P)
	rx1,ry1,rz1 = _RotateTrace(T1,R[Ir],P)
	Pr = np.array([rx[Ir],ry[Ir],rz[Ir]])
	
	

	#find 4 values closest
	Ic0,xc0,yc0,zc0 = _Closest4Pos(Pr,rx0,ry0,rz0,T0.nstep)
	Ic1,xc1,yc1,zc1 = _Closest4Pos(Pr,rx1,ry1,rz1,T1.nstep)
	

		
	
	#get the closest point, where z' = 0
	x0,y0,z0 = _ClosestPosSpline(xc0,yc0,zc0)
	x1,y1,z1 = _ClosestPosSpline(xc1,yc1,zc1)	

	#rotate back
	p0 = np.array([x0,y0,z0])
	p1 = np.array([x1,y1,z1])
	p0 = np.dot(R[Ir],p0)
	p1 = np.dot(R[Ir],p1)

	x0,y0,z0 = p0 + P
	x1,y1,z1 = p1 + P
	
	return x0,y0,z0,x1,y1,z1
	
def AllClosestPos(T):
	
	#list the rotation matrices for each vector in the original trace
	R = _RotMatrices(T)
	
	#store positions
	x0 = np.zeros(T.nstep,dtype='float64')	
	y0 = np.zeros(T.nstep,dtype='float64')	
	z0 = np.zeros(T.nstep,dtype='float64')	
	x1 = np.zeros(T.nstep,dtype='float64')	
	y1 = np.zeros(T.nstep,dtype='float64')	
	z1 = np.zeros(T.nstep,dtype='float64')	
	
	#get trace midpoint
	ife,xfe,yfe,zfe = _MidPoint(T)
	
	#get the new trace position
	alpha0 = 90.0*np.pi/180.0
	alpha1 = 90.0*np.pi/180.0 + np.pi
	Delta = 0.05
	
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
	T0 = FullTrace(xt0,yt0,zt0,20120101,12.0,CoordIn='SM')
	T1 = FullTrace(xt1,yt1,zt1,20120101,12.0,CoordIn='SM')	
	
	#get each closest position
	for i in range(0,T.nstep):
		x0[i],y0[i],z0[i],x1[i],y1[i],z1[i] = _ClosestPos(i,R,T,T0,T1)
	
	#get distances
	d0 = np.zeros(T.nstep,dtype='float64')	 
	d1 = np.zeros(T.nstep,dtype='float64')	 
	h0 = np.zeros(T.nstep,dtype='float64')	 
	h1 = np.zeros(T.nstep,dtype='float64')	 
	
	d0[:] = np.sqrt((T.xsm[:T.nstep]-x0)**2 + (T.ysm[:T.nstep]-y0)**2 + (T.zsm[:T.nstep]-z0)**2)
	d1[:] = np.sqrt((T.xsm[:T.nstep]-x1)**2 + (T.ysm[:T.nstep]-y1)**2 + (T.zsm[:T.nstep]-z1)**2)
	
	h0 = d0/Delta
	h1 = d1/Delta
	
	
	return (T0,h0,x0,y0,z0),(T1,h1,x1,y1,z1),R
	
def GetHalpha(T,ReturnAll=False):
	
	
	#list the rotation matrices for each vector in the original trace
	R = _RotMatrices(T)
	
	#store positions
	x0 = np.zeros(T.nstep,dtype='float64')	
	y0 = np.zeros(T.nstep,dtype='float64')	
	z0 = np.zeros(T.nstep,dtype='float64')	
	x1 = np.zeros(T.nstep,dtype='float64')	
	y1 = np.zeros(T.nstep,dtype='float64')	
	z1 = np.zeros(T.nstep,dtype='float64')	
	
	#get trace midpoint
	ife,xfe,yfe,zfe = _MidPoint(T)
	
	#get the new trace position
	alpha0 = 90.0*np.pi/180.0
	alpha1 = 90.0*np.pi/180.0 + np.pi
	Delta = 0.05
	
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
	T0 = FullTrace(xt0,yt0,zt0,20120101,12.0,CoordIn='SM')
	T1 = FullTrace(xt1,yt1,zt1,20120101,12.0,CoordIn='SM')	
	print(T.nstep,T0.nstep,T1.nstep,xt0,yt0,zt0,xt1,yt1,zt1)
	#get each closest position
	for i in range(0,T.nstep):
		x0[i],y0[i],z0[i],x1[i],y1[i],z1[i] = _ClosestPos(i,R,T,T0,T1)
	
	#get distances
	d0 = np.zeros(T.nstep,dtype='float64')	 
	d1 = np.zeros(T.nstep,dtype='float64')	 
	h0 = np.zeros(T.nstep,dtype='float64')	 
	h1 = np.zeros(T.nstep,dtype='float64')	 
	
	d0[:] = np.sqrt((T.xsm[:T.nstep]-x0)**2 + (T.ysm[:T.nstep]-y0)**2 + (T.zsm[:T.nstep]-z0)**2)
	d1[:] = np.sqrt((T.xsm[:T.nstep]-x1)**2 + (T.ysm[:T.nstep]-y1)**2 + (T.zsm[:T.nstep]-z1)**2)
	
	h0 = d0/Delta
	h1 = d1/Delta
	
	if ReturnAll:
		return 0.5*(h0 + h1),h0,h1,0.5*(d0+d1),d0,d1
	
	return 0.5*(h0 + h1)	

def GetHalphaOld(T,ReturnAll=False):
	

	
	#get trace midpoint
	ife,xfe,yfe,zfe = _MidPoint(T)
	
	#get the new trace position
	alpha0 = 90.0*np.pi/180.0
	alpha1 = 90.0*np.pi/180.0 + np.pi
	Delta = 0.05
	
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
	T0 = FullTrace(xt0,yt0,zt0,20120101,12.0,CoordIn='SM')
	T1 = FullTrace(xt1,yt1,zt1,20120101,12.0,CoordIn='SM')	

	s0 = T0.s[:T0.nstep]
	s1 = T1.s[:T1.nstep]
	_,uinds0 = np.unique(s0,return_index=True)
	_,uinds1 = np.unique(s1,return_index=True)



	fx0 = InterpolatedUnivariateSpline(s0[uinds0],T0.xsm[uinds0])
	fy0 = InterpolatedUnivariateSpline(s0[uinds0],T0.ysm[uinds0])
	fz0 = InterpolatedUnivariateSpline(s0[uinds0],T0.zsm[uinds0])
	

	fx1 = InterpolatedUnivariateSpline(s1[uinds1],T1.xsm[uinds1])
	fy1 = InterpolatedUnivariateSpline(s1[uinds1],T1.ysm[uinds1])
	fz1 = InterpolatedUnivariateSpline(s1[uinds1],T1.zsm[uinds1])
	
	def DistFn0(s,j):
		return np.sqrt((fx0(s)-T.xsm[j])**2.0 + (fy0(s)-T.ysm[j])**2.0 + (fz0(s)-T.zsm[j])**2.0)
	def DistFn1(s,j):
		return np.sqrt((fx1(s)-T.xsm[j])**2.0 + (fy1(s)-T.ysm[j])**2.0 + (fz1(s)-T.zsm[j])**2.0)

	#get distances
	d0 = np.zeros(T.nstep,dtype='float64')	 
	d1 = np.zeros(T.nstep,dtype='float64')	 
	h0 = np.zeros(T.nstep,dtype='float64')	 
	h1 = np.zeros(T.nstep,dtype='float64')	 
			
	for i in range(0,T.nstep):
		res = minimize(DistFn0,T.s[i],args=(i),method='Nelder-Mead')
		d0[i] = DistFn0(res.x,i)
		res = minimize(DistFn1,T.s[i],args=(i),method='Nelder-Mead')
		d1[i] = DistFn1(res.x,i)

	h0 = d0/Delta
	h1 = d1/Delta
	
	if ReturnAll:
		return 0.5*(h0 + h1),h0,h1,0.5*(d0+d1),d0,d1


	
	return 0.5*(h0 + h1)	

def TestHalpha(Ir,fig=None,maps=[1,1,0,0]):
	
	
	#firstly - get a trace
	T = FullTrace(5.0,0.0,0.0,20120101,12.0,CoordIn='SM')
	ife,xfe,yfe,zfe = _MidPoint(T)
	
	#get the new trace position
	alpha0 = 90.0*np.pi/180.0
	alpha1 = 90.0*np.pi/180.0 + np.pi
	Delta = 0.05
	
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
	T0 = FullTrace(xt0,yt0,zt0,20120101,12.0,CoordIn='SM')
	T1 = FullTrace(xt1,yt1,zt1,20120101,12.0,CoordIn='SM')
	
	#list the rotation matrices for each vector in the original trace
	R = _RotMatrices(T)
	
	#pick one to rotate by
	P = np.array([T.xsm[Ir],T.ysm[Ir],T.zsm[Ir]])
	
	#rotate all of them
	rx,ry,rz = _RotateTrace(T,R[Ir],P)
	rx0,ry0,rz0 = _RotateTrace(T0,R[Ir],P)
	rx1,ry1,rz1 = _RotateTrace(T1,R[Ir],P)
	Pr = np.array([rx[Ir],ry[Ir],rz[Ir]])

	#find 4 values closest
	Ic0,xc0,yc0,zc0 = _Closest4Pos(Pr,rx0,ry0,rz0,T0.nstep)
	Ic1,xc1,yc1,zc1 = _Closest4Pos(Pr,rx1,ry1,rz1,T1.nstep)
	
	#get the closest point, where z' = 0
	x0,y0,z0 = _ClosestPosSpline(xc0,yc0,zc0)
	x1,y1,z1 = _ClosestPosSpline(xc1,yc1,zc1)


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
	ax.scatter(x0,z0,color='cyan')
	ax.scatter(x1,z1,color='cyan')
	
	zs1 = np.linspace(zc1[0],zc1[-1],100)
	print(zs1,zc1,xc1)
	xs1 = pyspline(zc1,xc1,zs1)
	
	ax.scatter(xc1,zc1,color='green')
	ax.scatter(xc0,zc0,color='orange')
	
	ax.plot(xs1,zs1,color='green')
	
	ax.set_aspect(1.0)
	ax.legend()

	return ax


def TestHalpha2(I,fig=None,maps=[1,1,0,0]):
	#firstly - get a trace
	T = FullTrace(5.0,0.0,0.0,20120101,12.0,CoordIn='SM')	
	
	_t0,_t1,R = AllClosestPos(T)
	
	T0,h0,x0,y0,z0 = _t0
	T1,h1,x1,y1,z1 = _t1
	

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

	ax.scatter(T.xsm[I],T.zsm[I],color='magenta')
	ax.scatter(x0[I],z0[I],color='cyan')
	ax.scatter(x1[I],z1[I],color='lime')
	

	ax.set_aspect(1.0)
	ax.legend()

	return ax


def TestHalpha3(fig=None,maps=[1,1,0,0]):
	#firstly - get a trace
	T = FullTrace(5.0,0.0,0.0,20120101,12.0,CoordIn='SM')	
	
	_t0,_t1,R = AllClosestPos(T)
	
	T0,h0,x0,y0,z0 = _t0
	T1,h1,x1,y1,z1 = _t1
	

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

	ax.scatter(T.xsm,T.zsm,color='cyan')
	ax.scatter(x0,z0,color='goldenrod')
	ax.scatter(x1,z1,color='lime')
	

	ax.set_aspect(1.0)
	ax.legend()

	return ax


def PlotHalpha(fig=None,maps=[1,1,0,0]):
	#firstly - get a trace
	T = FullTrace(5.0,0.0,0.0,20120101,12.0,CoordIn='SM')	
	
	t0 = time.time()
	h = GetHalpha(T)
	t1 = time.time()
	ho = GetHalphaOld(T)
	t2 = time.time()
	
	print('New: {:f}s'.format(t1-t0))
	print('Old: {:f}s'.format(t2-t1))

	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig
		
	plt.plot(T.s[:T.nstep],h,color='red',label='new')
	plt.plot(T.s[:T.nstep],ho,color='green',label='old')

def PlotHalpha2():
	#firstly - get a trace
	T = FullTrace(5.0,0.0,0.0,20120101,12.0,CoordIn='SM')	
	
	t0 = time.time()
	new = GetHalpha(T,True)
	t1 = time.time()
	old = GetHalphaOld(T,True)
	t2 = time.time()
	
	hn,h0n,h1n,dn,d0n,d1n = new
	ho,h0o,h1o,do,d0o,d1o = old
	
	print('New: {:f}s'.format(t1-t0))
	print('Old: {:f}s'.format(t2-t1))

	fig = plt
	fig.figure(figsize=(8,11))
	ax0 = fig.subplot2grid((2,1),(0,0))
	ax1 = fig.subplot2grid((2,1),(1,0))

	ax0.plot(T.s[:T.nstep],hn,color='red',label='new')
	ax0.plot(T.s[:T.nstep],h0n,color='orange',linestyle='--',label='new')
	ax0.plot(T.s[:T.nstep],h1n,color='orange',linestyle=':',label='new')
	ax0.plot(T.s[:T.nstep],ho,color='green',label='old')
	ax0.plot(T.s[:T.nstep],h0o,color='lime',linestyle='--',label='old')
	ax0.plot(T.s[:T.nstep],h1o,color='lime',linestyle=':',label='old')
	ax0.plot(T.s[:T.nstep],T.halpha[0][:T.nstep],color='blue',linestyle='--',label='C++')
	ax0.plot(T.s[:T.nstep],T.halpha[1][:T.nstep],color='blue',linestyle='--',label='C++')
	
	ax1.plot(T.s[:T.nstep],dn,color='red',label='new')
	ax1.plot(T.s[:T.nstep],d0n,color='orange',linestyle='--',label='new')
	ax1.plot(T.s[:T.nstep],d1n,color='orange',linestyle=':',label='new')
	ax1.plot(T.s[:T.nstep],do,color='green',label='old')
	ax1.plot(T.s[:T.nstep],d0o,color='lime',linestyle='--',label='old')
	ax1.plot(T.s[:T.nstep],d1o,color='lime',linestyle=':',label='old')
	ax0.legend()
	ax1.legend()

	return new,old
