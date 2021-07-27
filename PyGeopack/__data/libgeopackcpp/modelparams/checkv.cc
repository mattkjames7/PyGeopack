#include "checkv.h"

void CheckV(int Date, float ut, double Vxin, double Vyin, double Vzin,
				double *Vx, double *Vy, double *Vz) {
	
	if (isnan(Vxin)) { 
		TData->GetVx(1,&Date,&ut,Vx);
	} else {
		Vx[0] = Vxin;
	}
	if (isnan(Vyin)) { 
		TData->GetVy(1,&Date,&ut,Vy);
	} else {
		Vy[0] = Vyin;
	}
	if (isnan(Vzin)) { 
		TData->GetVz(1,&Date,&ut,Vz);
	} else {
		Vz[0] = Vzin;
	}	
}
