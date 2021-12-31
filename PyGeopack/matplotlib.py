import numpy as np
import matplotlib.pyplot as plt

def Func(fig=None,maps=[1,1,0,0]):
	
	if fig is None:
		fig = plt
		fig.figure()
	if hasattr(fig,'Axes'):	
		ax = fig.subplot2grid((maps[1],maps[0]),(maps[3],maps[2]))
	else:
		ax = fig


	return ax
