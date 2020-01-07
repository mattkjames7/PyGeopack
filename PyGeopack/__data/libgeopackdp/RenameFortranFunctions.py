import numpy as np

def ReadFile(fname):
	f = open(fname,'r')
	lines = f.readlines()
	f.close()
	return lines
	
def GetFunctionList(lines):
	n = np.size(lines)
	Fns = []
	for i in range(0,n):
		if not lines[i][0] in ['C','c']:
			s = lines[i].split('!')
			if 'subroutine' in s[0].lower():
				tmp = s[0].split()
				tmp = tmp[1].split('(')
				tmp = tmp[0].strip()
				Fns.append(tmp)
	return Fns			
	
def ReduceFunctionList(Fns,Avoid):
	nf = np.size(Fns)
	keep = np.zeros(nf,dtype='bool')
	for i in range(0,nf):
		if Fns[i] in Avoid:
			keep[i] = False
		else:
			keep[i] = True
	use = np.where(keep)[0]
	return np.array(Fns)[use]

def GetNewFunctionNames(Fns,Prefix,Suffix):
	nf = np.size(Fns)
	newFns = []
	for i in range(0,nf):
		newFns.append(Prefix + Fns[i] + Suffix)
	return newFns


def ReplaceFunctionNames(lines,Fns,newFns):
	nf = np.size(Fns)
	n = np.size(lines)
	lines = lines.copy()
	for i in range(0,nf):
		for j in range(0,n):
			if Fns[i] in lines[j]:
				lines[j] = lines[j].replace(Fns[i],newFns[i])
	return lines
			
def SaveFile(fname,lines):
	f = open(fname,'w')
	f.writelines(lines)
	f.close()

def RenameFortranFunctions(fname,Prefix='',Suffix='',Avoid=[]):

	lines = ReadFile(fname)
	Fns = GetFunctionList(lines)
	Fns = ReduceFunctionList(Fns,Avoid)
	newFns = GetNewFunctionNames(Fns,Prefix,Suffix)
	newlines = ReplaceFunctionNames(lines,Fns,newFns)
	SaveFile(fname+'-replaced',newlines)
