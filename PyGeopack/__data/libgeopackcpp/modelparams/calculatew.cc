#include "calculatew.h"

void FindIntervals(	int n, double *SymH, double *Bz, int *SWflag, 
					int *IMFflag, int *ni, int *ibeg, int *iend) {
	/*
	 *  This procedure will scan through the OMNI data looking for 2h
	 * quiet periods (to get the baseline) followed by continuous good 
	 * data using Tsyganenko's Fortran code.
	 * 
	 */ 
	double SymHmax = -1000.0;
	double SymHmin =  1000.0;
	int LenQuiet = 0;
	double SymHLowLim = -10.0;
	double dSymHLim = 5.0;
	int FirstGood = 0;
	ni[0] = 0;
	
	int i, I0;
	while (i < n) {
		if (SymH[i] > SymHmax) {
			SymHmax = SymH[i];
		}
		if (SymH[i] < SymHmin) {
			SymHmin = SymH[i];
		}
		
		if ((IMFflag[i] == -1) || (SWflag[i] == -1) || (Bz[i] < 0) || (SymH[i] < SymHLowLim)) {
			/* not quiet enough */
			LenQuiet = 0;
			SymHmax = -1000.0;
			SymHmin =  1000.0;		
		} else {
			if (LenQuiet == 0) {
				FirstGood = i;
			}
			
			if ((SymHmax - SymHmin) > dSymHLim) {
				i = FirstGood + 1;
				LenQuiet = 0;
				SymHmax = -1000.0;
				SymHmin =  1000.0;			
			} else {	
				LenQuiet++;
				if (LenQuiet == 24) {
					I0 = i;
					for (i=I0;i<n;i++) {
						if ((IMFflag[i] == -1) || (SWflag[i] == -1)) {
							ibeg[ni[0]] = I0 - LenQuiet + 1;
							iend[ni[0]] = i - 1;
							ni[0]++;
							LenQuiet = 0;
							SymHmax = -1000.0;
							SymHmin =  1000.0;
							break;
						}
					}
					if (LenQuiet > 0) {
						ibeg[ni[0]] = I0 - LenQuiet + 1;
						iend[ni[0]] = n - 1;						
					}
				}
			}
		}
		i++;
		printf("\rFinding Intervals for W %d of %d",i,n);
	}
	
	printf("\n");
}


void CalculateW(int n, double *SymH, double *Bz, int *SWflag, int *IMFflag, 
				double *V, double *Den, double *W1, double *W2, double *W3, 
				double *W4, double *W5, double *W6) {
	/***
	 *	This code links to Tsyganenko's Fortran code which scans for the
	 * useable intervals in the OMNI data, then calculates the 6 W 
	 * parameters used for the T04/05 (I'm not sure what it's called) 
	 * model. They don't seem to come out the same as the ones listed on 
	 * Tsyganenko's website, so use with caution!
	 * 
	 * 
	 */
	
	/* Firstly we need to calculate the number of intervals, use an arbitrary number*/
	int ni = 0;
	int *ibeg = new int[10000];
	int *iend = new int[10000];
	
	/*use the Fortran code to find the intervals*/
	FindIntervals(n,SymH,Bz,SWflag,IMFflag,&ni,ibeg,iend);

	/*Calculate the W parameters*/
	double r[] = {0.383403,0.648176,0.318752E-01,0.581168,1.15070,0.843004};
	double gamm[] = {0.916555,0.898772,1.29123,1.33199,0.699074,0.537116};
	double lamda[] = {0.394732,0.550920,0.387365,0.436819,0.405553,1.26131};
	double beta[] = {0.846509,0.180725,2.26596,1.28211,1.62290,2.42297};
	
	double DT[6];
	DT[0] = r[0]/60.0;
	DT[1] = r[1]/60.0;
	DT[2] = r[2]/60.0;
	DT[3] = r[3]/60.0;
	DT[4] = r[4]/60.0;
	DT[5] = r[5]/60.0;
	
	double *FAC[6];
	bool Key[6];
	double *Bs[6];
	double Arg[6];
	double Taumt;
	double *Vnorm = new double[n];
	double *Dennorm = new double [n];
	double *Bsnorm = new double [n];
	double tmpW[6];
	
	int I, I0, I1, i, j, k;
	
	/* calculate a few parameters once, rather than many times! */
	for (i=0;i<6;i++) {
		Bs[i] = new double[n];
		FAC[i] = new double[n];
	}
	
	for (i=0;i<n;i++) {
		printf("\rCalculating some relevant parameters %d of %d",i+1,n);
		W1[i] = 0.0;
		W2[i] = 0.0;
		W3[i] = 0.0;
		W4[i] = 0.0;
		W5[i] = 0.0;
		W6[i] = 0.0;
		Vnorm[i] = V[i]/400.0;
		Dennorm[i] = Den[i]*1.16/5.0;
		Bsnorm[i] = -Bz[i]/5.0;
				
		if (Bsnorm[i] <= 0.0) {
			for (j=0;j<6;j++) {
				Bs[j][i] = 0.0;
			}
		} else {
			for (j=0;j<6;j++) {
				Bs[j][i] = pow(Bsnorm[i],lamda[j]);
			}					
		}
				
		for (j=0;j<6;j++) {
			FAC[j][i] = pow(Dennorm[i],gamm[j])*pow(Vnorm[i],beta[j])*Bs[j][i];
		} 
	}
	printf("\n");
	
	for (I=0;I<ni;I++) {
		printf("Calculating W for Interval %d of %d\n",I+1,ni);
		/* loop through each interval */
		I0 = ibeg[I];
		I1 = iend[I];
		
		for (i=I0;i<=I1;i++) {
			printf("\r%6.2f\%",(100.0*(i-I0))/(I1 - I0));
			/* loop through each element */
			for (j=0;j<6;j++) {
				tmpW[j] = 0.0;
				Key[j] = true;
			}
			
			for (k=i;k>=I0;k--) {
				/* loop backwards? */
				Taumt = (i - k)*5.0;
				
				for (j=0;j<6;j++) {
					Arg[j] = -Taumt*DT[j];
					if ((Arg[j] > -10.0) && (Key[j])) {
						tmpW[j] = tmpW[j] + FAC[j][i]*exp(Arg[j]);
					} else {
						Key[j] = false;
					}
				}
				
				
			}
			W1[i] = tmpW[0]*DT[0]*5.0;
			W2[i] = tmpW[1]*DT[1]*5.0;
			W3[i] = tmpW[2]*DT[2]*5.0;
			W4[i] = tmpW[3]*DT[3]*5.0;
			W5[i] = tmpW[4]*DT[4]*5.0;
			W6[i] = tmpW[5]*DT[5]*5.0;
		}
		printf("\n");
	}
	
	
	/*free the allocated variables*/
	delete[] ibeg;
	delete[] iend;
	for (i=0;i<6;i++) {
		delete [] Bs[i];
		delete [] FAC[i];
	}

	delete [] Vnorm;
	delete [] Bsnorm;
	delete [] Dennorm;
}


void CalculateWPrev(int n, double *SymH, double *Bz, int *SWflag, int *IMFflag, 
				double *V, double *Den, double *W1, double *W2, double *W3, 
				double *W4, double *W5, double *W6) {
	/***
	 *	This code links to Tsyganenko's Fortran code which scans for the
	 * useable intervals in the OMNI data, then calculates the 6 W 
	 * parameters used for the T04/05 (I'm not sure what it's called) 
	 * model. They don't seem to come out the same as the ones listed on 
	 * Tsyganenko's website, so use with caution!
	 * 
	 * 
	 */
	
	/* Firstly we need to calculate the number of intervals, use an arbitrary number*/
	int ni = 0;
	int *ibeg = new int[10000];
	int *iend = new int[10000];
	
	/*use the Fortran code to find the intervals*/
	FindIntervals(n,SymH,Bz,SWflag,IMFflag,&ni,ibeg,iend);

	/*Calculate the W parameters*/
	double r[] = {0.383403,0.648176,0.318752E-01,0.581168,1.15070,0.843004};
	double gamm[] = {0.916555,0.898772,1.29123,1.33199,0.699074,0.537116};
	double lamda[] = {0.394732,0.550920,0.387365,0.436819,0.405553,1.26131};
	double beta[] = {0.846509,0.180725,2.26596,1.28211,1.62290,2.42297};
	
	double DT[6];
	DT[0] = r[0]/60.0;
	DT[1] = r[1]/60.0;
	DT[2] = r[2]/60.0;
	DT[3] = r[3]/60.0;
	DT[4] = r[4]/60.0;
	DT[5] = r[5]/60.0;
	
	double FAC[6];
	bool Key[6];
	double Bs[6];
	double Arg[6];
	double Taumt;
	double Vnorm, Dennorm, Bsnorm;
	double tmpW[6];
	
	int I, I0, I1, i, j, k;
	
	/* fill with zeros */
	for (i=0;i<n;i++) {
		W1[i] = 0.0;
		W2[i] = 0.0;
		W3[i] = 0.0;
		W4[i] = 0.0;
		W5[i] = 0.0;
		W6[i] = 0.0;
	}

	for (I=0;I<ni;I++) {
		printf("Calculating W for Interval %d of %d\n",I+1,ni);
		/* loop through each interval */
		I0 = ibeg[I];
		I1 = iend[I];
		
		for (i=I0;i<=I1;i++) {
			printf("\r%6.2f\%",(100.0*(i-I0))/(I1 - I0));
			/* loop through each element */
			for (j=0;j<6;j++) {
				tmpW[j] = 0.0;
				Key[j] = true;
			}
			
			for (k=i;k>=I0;k--) {
				/* loop backwards? */
				Vnorm = V[k]/400.0;
				Dennorm = Den[k]*1.16/5.0;
				Bsnorm = -Bz[k]/5.0;
				
				if (Bsnorm <= 0.0) {
					for (j=0;j<6;j++) {
						Bs[j] = 0.0;
					}
				} else {
					for (j=0;j<6;j++) {
						Bs[j] = pow(Bsnorm,lamda[j]);
					}					
				}
				
				for (j=0;j<6;j++) {
					FAC[j] = pow(Dennorm,gamm[j])*pow(Vnorm,beta[j])*Bs[j];
				}
				Taumt = (i - k)*5.0;
				
				for (j=0;j<6;j++) {
					Arg[j] = -Taumt*DT[j];
					if ((Arg[j] > -10.0) && (Key[j])) {
						tmpW[j] = tmpW[j] + FAC[j]*exp(Arg[j]);
					} else {
						Key[j] = false;
					}
				}
				
				
			}
			W1[i] = tmpW[0]*DT[0]*5.0;
			W2[i] = tmpW[1]*DT[1]*5.0;
			W3[i] = tmpW[2]*DT[2]*5.0;
			W4[i] = tmpW[3]*DT[3]*5.0;
			W5[i] = tmpW[4]*DT[4]*5.0;
			W6[i] = tmpW[5]*DT[5]*5.0;
		}
		printf("\n");
	}
	
	
	/*free the allocated variables*/
	delete[] ibeg;
	delete[] iend;

}
