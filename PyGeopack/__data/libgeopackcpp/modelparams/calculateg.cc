#include "calculateg.h"


void CalculateG(int n, double *By, double *Bz, double *V, bool *good, double *G1, double *G2) {
	/*******************************************************************
	 * In this function we are trying to calculate the G1 and G2 
	 * coefficients for the TS01 model (I think).
	 * 
	 * ****************************************************************/
	int i,j,u,i0,i1;
	/*calculate the clock angle, and Bs (B southward I think)*/
	double *CA = new double[n];
	double *Bs = new double[n];
	double *h = new double[n];
	double Bp;
	for (i=0;i<n;i++) {
		 CA[i] = atan2(-By[i],Bz[i]);
		 Bp = sqrt(By[i]*By[i] + Bz[i]*Bz[i]);
		 Bs[i] = fabs(min(0.0,Bz[i]));
		 h[i] = pow(Bp/40.0,2.0)/(1.0 + Bp/40.0);
	}
		 
	/*now to get G1 and G2*/
	for (i=0;i<n;i++) {
		printf("\rCalculating G parameter %d of %d",i+1,n);
		i0 = max(0,i-11);
		i1 = i + 1;
		u = 0;
		G1[i] = 0.0;
		G2[i] = 0.0;
		for (j=i0;j<i1;j++) {
			if (good[j]) {
				G1[i] += V[j]*h[j]*pow(sin(CA[j]/2.0),3.0);
				G2[i] += 0.005*V[j]*Bs[j];
				u++;
			}
		}
		if (u > 0) {
			G1[i]/=u;
			G2[i]/=u;
		} else { 
			G1[i] = 0.0;
			G2[i] = 0.0;
		}
	} 
	printf("\n");
	/*free the temporary arrays from memory*/
	delete[] CA;
	delete[] Bs;
	delete[] h;
	 
}
