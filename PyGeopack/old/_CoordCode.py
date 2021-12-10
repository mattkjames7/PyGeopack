import numpy as np

Coords = {	'GSE':1,
			'GSM':2,
			'SM':3}
		
def _CoordCode(code):
	if not code in list(Coords.keys()):
		print('Invalid coordinate code: "',code,'" - assuming GSE')
	return np.int32(Coords.get(code,1)) 
