import numpy as np
import matplotlib.pyplot as plt




def TestHarmonic(x,y,z,Date=20120101,ut=0.0,Params=[30.0,1.0],df=0.1):
	
	
	
	#get the old version
	old = _OldHarmonics(x,y,z,Date,ut,Params,df=df)	
	
	#get the new version
	new = _NewHarmonics(x,y,z,Date,ut,Params,df=df)

	
	ymxp = np.nanmax(np.abs(old['yp'][0]))*1.1
	ymxt = np.nanmax(np.abs(old['yt'][0]))*1.1
	
	#create the figure
	fig = plt
	fig.figure(figsize=(8,11))
	ax0 = fig.subplot2grid((2,1),(0,0))
	ax1 = fig.subplot2grid((2,1),(1,0))


	#plot poloidal mode
	for i in range(0,old['nh']):
		ax0.plot(old['s'],old['yp'][i],label='old $f$={:5.2f} mHz'.format(old['fp'][i]))
	
	for i in range(0,new['nh']):
		ax0.plot(new['s'],new['yp'][i],label='new $f$={:5.2f} mHz'.format(new['fp'][i]))
	
	#plot toroidal mode
	for i in range(0,old['nh']):
		ax1.plot(old['s'],old['yt'][i],label='old $f$={:5.2f} mHz'.format(old['ft'][i]))
	
	for i in range(0,new['nh']):
		ax1.plot(new['s'],new['yt'][i],label='new $f$={:5.2f} mHz'.format(new['ft'][i]))

	
	#axis limits
	ax0.set_xlim([0.0,np.max([old['s'].max(),new['s'].max()])])
	ax1.set_xlim([0.0,np.max([old['s'].max(),new['s'].max()])])
	ax0.set_ylim(-ymxp,ymxp)
	ax1.set_ylim(-ymxt,ymxt)
	
	#titles/labels
	ax0.set_title('Poloidal Resonance')
	ax0.set_ylabel(r'$\xi$')
	ax0.set_xlabel('$s$ (km)')
	ax1.set_title('Toroidal Resonance')
	ax1.set_ylabel(r'$\xi$')
	ax1.set_xlabel('$s$ (km)')

	#legends
	ax0.legend()
	ax1.legend()


	return ax0,ax1
	
	
def _GetTrace(x,y,z,Date,ut,Pol):
	from ..TraceField import TraceField
	
	T = TraceField(x,y,z,Date,ut,alpha=[Pol])
	T.x = T.xsm
	T.y = T.ysm
	T.z = T.zsm
	T.Bx = T.Bxsm
	T.By = T.Bysm
	T.Bz = T.Bzsm
	return T,T.s[:T.nstep]*6380.0,T.halpha[0][:T.nstep]

def _NewHarmonics(x,y,z,Date,ut,Params,df=0.1):
	#import the WH module
	import MHDWaveHarmonics as wh

	#get the traces
	Tp,Sp,hp = _GetTrace(x,y,z,Date,ut,0.0)
	Tt,St,ht = _GetTrace(x,y,z,Date,ut,90.0)
	
	#field magnitude
	B = np.sqrt(Tp.Bxsm**2 + Tp.Bysm**2 + Tp.Bzsm**2)[:Tp.nstep]
	
	#output 
	out = {}
		
	#pmd
	Rnorm = Tp.Rnorm
	pmd0 = Params[0]
	pwr = Params[1]
	PMD = pmd0*Rnorm**-pwr	
	
	#alfven speed
	Va = wh.CalcFieldLineVaPMD(B,PMD)
	VaMid = wh.CalcFieldLineVaMidPMD(B,PMD,Sp)

	
	#calculate harmonic frequencies
	nh = 3
	Method = 'Simple'	

	fp,_,_ = wh.FindHarmonicsPMD(B,PMD,Sp,halpha=hp,RhoBG=None,
						Harmonics=np.arange(nh)+1,df=df,Method=Method)
	ft,_,_ = wh.FindHarmonicsPMD(B,PMD,St,halpha=ht,RhoBG=None,
						Harmonics=np.arange(nh)+1,df=df,Method=Method)
	RhoBG = None

	out['nh'] = 3
	out['fp'] = fp
	out['ft'] = ft						

	#solve the waves
	yp = []
	yt = []
	R = Tp.R
	for (i,fff) in enumerate(zip(fp,ft)):
		fpi,fti = fff

		tmpt = wh.SolveWave(fti,St,B,Va=Va,halpha=ht)
		tmpp = wh.SolveWave(fpi,Sp,B,Va=Va,halpha=hp)
		yt.append(tmpt/ht)
		yp.append(tmpp/hp)
	out['yp'] = yp
	out['yt'] = yt
	
	out['s'] = Sp
	
	return out
						
def _OldHarmonics(x,y,z,Date,ut,Params,df=0.1):
	
	#import the WH module
	import MHDWaveHarmonics as wh
	
	#get the poloidal and toroidal traces/halphas
	#Tp,Sp,hp = wh.GetFieldLine([x,y,z],Date=Date,ut=ut,CoordIn='GSM',Model='T96',Polarization='poloidal',Delta=0.1)
	#Tt,St,ht = wh.GetFieldLine([x,y,z],Date=Date,ut=ut,CoordIn='GSM',Model='T96',Polarization='toroidal',Delta=0.1)
	
	#swapped above for new h alpha code
	Tp,Sp,hp = _GetTrace(x,y,z,Date,ut,0.0)
	Tt,St,ht = _GetTrace(x,y,z,Date,ut,90.0)
	
	#field magnitude
	B = np.sqrt(Tp.Bx**2 + Tp.By**2 + Tp.Bz**2)
	B = B[np.isfinite(B)]
	
	#Radial distance
	R = np.sqrt(Tp.x**2.0 + Tp.y**2.0 + Tp.z**2.0)[:Tp.nstep]
	
	#plasma mass density
	p = Params[0]*(R.max()/R)**Params[1]
	
	#Alfven speed
	mu0 = 4*np.pi*1e-7
	p = p*1.67377e-27*1e6
	Va = (B*1e-9/np.sqrt(mu0*p))/1000.0	
	
	#output 
	out = {}
	
	#harmonic frequencies
	nh = 3
	RhoBG = None
	Method = 'Simple'
	fp,_,_ = wh.FindHarmonics(Tp,Sp,Params,hp,RhoBG,np.arange(nh)+1,None,df,Method)
	ft,_,_ = wh.FindHarmonics(Tt,St,Params,ht,RhoBG,np.arange(nh)+1,None,df,Method)
	out['nh'] = 3
	out['fp'] = fp
	out['ft'] = ft
	
	#solve the waves
	yp = []
	yt = []
	for (fpi,fti) in zip(fp,ft):
		tmpt,_,_= wh.SolveWave(fti,St,B,R,None,ht,RhoBG,Params)
		tmpp,_,_= wh.SolveWave(fpi,Sp,B,R,None,hp,RhoBG,Params)
		yt.append(tmpt/ht)
		yp.append(tmpp/hp)
	out['yp'] = yp
	out['yt'] = yt
	
	out['s'] = Sp
	
	return out
