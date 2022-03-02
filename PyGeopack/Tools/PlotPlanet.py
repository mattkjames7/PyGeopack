import numpy as np

def PlotPlanetXZ(fig,R=1.0,Center=[0.0,0.0,0.0],zorder=10,NoShadow=False,
				NoonTop=True):
	

	a = 2*np.pi*np.arange(361,dtype='float32')/360
	x = R*np.sin(a) + Center[0]
	z = R*np.cos(a) + Center[2]
	
	if NoonTop:	
		fig.fill(z,x,color=[1.0,1.0,1.0],zorder=zorder)
		fig.plot(z,x,color=[0,0,0],zorder=zorder+1)
		if NoShadow == False:
			fig.fill(z[180:360],x[180:360],color=[0.0,0.0,0.0],zorder=zorder+1)
	else:
		fig.fill(x,z,color=[1.0,1.0,1.0],zorder=zorder)
		fig.plot(x,z,color=[0,0,0],zorder=zorder+1)
		if NoShadow == False:
			fig.fill(x[180:360],z[180:360],color=[0.0,0.0,0.0],zorder=zorder+1)


def PlotPlanetXY(fig,R=1.0,Center=[0.0,0.0,0.0],zorder=10,NoShadow=False,
				NoonTop=True):
	a = 2*np.pi*np.arange(361,dtype='float32')/360
	x = R*np.sin(a) + Center[0]
	y = R*np.cos(a) + Center[1]
	
	if NoonTop:	
		fig.fill(y,x,color=[1.0,1.0,1.0],zorder=zorder)
		fig.plot(y,x,color=[0,0,0],zorder=zorder+1)
		if NoShadow == False:
			fig.fill(y[180:360],x[180:360],color=[0.0,0.0,0.0],zorder=zorder+1)
	else:
		fig.fill(x,y,color=[1.0,1.0,1.0],zorder=zorder)
		fig.plot(x,y,color=[0,0,0],zorder=zorder+1)
		if NoShadow == False:
			fig.fill(x[180:360],y[180:360],color=[0.0,0.0,0.0],zorder=zorder+1)

def PlotPlanetYZ(fig,R=1.0,Center=[0.0,0.0,0.0],Side='day',zorder=10,
				NoFill=False,Color=[0.0,0.0,0.0],linestyle='-'):
	
	a = 2*np.pi*np.arange(361,dtype='float32')/360
	y = R*np.sin(a) + Center[1]
	z = R*np.cos(a) + Center[2]
	
	if NoFill == False:	
		if Side == 'day':
			fig.fill(y,z,color=[1.0,1.0,1.0],zorder=zorder)
		else:
			fig.fill(y,z,color=[0.0,0.0,0.0],zorder=zorder)
	fig.plot(y,z,color=Color,zorder=zorder+1,linestyle='-')

def PlotPlanetCoreXZ(ax,R=1.0,Center=[0.0,0.0,0.0],
				Colors=([1.0,0.7,0.0,0.5],[1.0,0.2,0.0,0.5],[0.5,0.5,0.5,0.5]),
				Layers=(0.0,0.832,1.0),zorder=1.0,NoFill=False,
				linestyle='-',linewidth=2.0):
	'''
	Plots the different layers of hte planet
	'''
	
	a = 2*np.pi*np.arange(361,dtype='float32')/360
	
	nl = len(Layers)
	if NoFill:
		for i in range(0,nl):
			x = Layers[i]*R*np.sin(a) + Center[0]
			z = Layers[i]*R*np.cos(a) + Center[2]
			ax.plot(x,z,color=Colors[i],zorder=zorder,linestyle=linestyle,linewidth=linewidth)
	else:
		for i in range(0,nl-1):
			l0 = Layers[i]
			l1 = Layers[i+1]
			x0 = l0*R*np.sin(a) + Center[0]
			z0 = l0*R*np.cos(a) + Center[2]			
			x1 = l1*R*np.sin(a) + Center[0]
			z1 = l1*R*np.cos(a) + Center[2]			
			
			if l0 == 0.0:
				ax.fill(x1,z1,color=Colors[i],zorder=zorder,linewidth=0.0)
			else:
				x = np.append(x0,x1[::-1])
				z = np.append(z0,z1[::-1])
				ax.fill(x,z,color=Colors[i],zorder=zorder,linewidth=0.0)

			x = R*np.sin(a) + Center[0]
			z = R*np.cos(a) + Center[2]			
			ax.plot(x,z,color=Colors[-1],linestyle=linestyle,linewidth=linewidth,zorder=zorder)
