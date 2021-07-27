#include "fieldlinernorm.h"


void FieldLineRnorm(int n, double *R, double Lshell, double *Rnorm) {
	/*******************************************************************
	 * Calculate R norm along the field line.
	 * 
	 * ****************************************************************/
	int i;
	
	for (i=0;i<n;i++) {
		Rnorm[i] = R[i]/Lshell;
	}
	
}
