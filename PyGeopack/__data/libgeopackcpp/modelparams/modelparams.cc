#include "modelparams.h"

void InitParams(const char *fname) {
	/* init the parameter object by loading saved file */
	TData = new TsygData(fname);
}

void GetModelParams(int n, int *Date, float *ut, const char *Model,
							double *Vxin, double *Vyin, double *Vzin,
							double *Kpin, double *Pdynin, double *SymHin,
							double *Byin, double *Bzin, 
							double *G1in, double *G2in,
							double *W1in, double *W2in, double *W3in,
							double *W4in, double *W5in, double *W6in,			
							double *Vx, double *Vy, double *Vz,
							double *Kp, double *Pdyn, double *SymH,
							double *By, double *Bz, 
							double *G1, double *G2,
							double *W1, double *W2, double *W3,
							double *W4, double *W5, double *W6,
							int *iopt, double **parmod) {

	
	TData->GetSWVelocity(n,Date,ut,Vxin,Vyin,Vzin,Vx,Vy,Vz);

	TData->GetParameter(n,Date,ut,TData->Kp_,Kpin,Kp);
	TData->GetParameter(n,Date,ut,TData->Pdyn_,Pdynin,Pdyn);
	TData->GetParameter(n,Date,ut,TData->SymH_,SymHin,SymH);
	TData->GetParameter(n,Date,ut,TData->By_,Byin,By);
	TData->GetParameter(n,Date,ut,TData->Bz_,Bzin,Bz);
	TData->GetParameter(n,Date,ut,TData->G1_,G1in,G1);
	TData->GetParameter(n,Date,ut,TData->G2_,G2in,G2);
	TData->GetParameter(n,Date,ut,TData->W1_,W1in,W1);
	TData->GetParameter(n,Date,ut,TData->W2_,W2in,W2);
	TData->GetParameter(n,Date,ut,TData->W3_,W3in,W3);
	TData->GetParameter(n,Date,ut,TData->W4_,W4in,W4);
	TData->GetParameter(n,Date,ut,TData->W5_,W5in,W5);
	TData->GetParameter(n,Date,ut,TData->W6_,W6in,W6);

	TData->GetModelParams(n,Model,Kp,Pdyn,SymH,By,Bz,G1,G2,
						W1,W2,W3,W4,W5,W6,iopt,parmod);

			
								
}
