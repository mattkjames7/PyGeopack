import numpy as np


def GetLegendHandLab(ax):
	'''
	Return the existing legend handles and labels.
	
	'''
	try:
		#get the handles
		handles = ax.legend_.legendHandles
		
		#and the labels
		labels = []
		for t in ax.legend_.texts:
			labels.append(t.get_text())
			
		return handles,labels
		
	except:
		return [],[]
