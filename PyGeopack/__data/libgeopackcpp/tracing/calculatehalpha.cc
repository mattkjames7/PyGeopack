#include "calculatehalpha.h"


void CalculateHalphas(	int nalpha, double *alpha, int nstep,
						double *x, double *y, double *z,
						double *Bx, double *By, double *Bz,
						ModelFuncPtr ModelFunc, int iopt, double *parmod,
						double alt, int MaxLen, double DSMax, 
						double xfe, double yfe, double zfe,
						double *halpha) {
	
	int i;
	for (i=0;i<nalpha;i++) {
		CalculateHalpha(alpha[i],nstep,x,y,z,Bx,By,Bz,ModelFunc,iopt,parmod,
						alt,MaxLen,DSMax,xfe,yfe,zfe,&halpha[i*MaxLen]);
	}
	
	
}


void CalculateHalpha(	double alpha, int nstep,
						double *x, double *y, double *z,
						double *Bx, double *By, double *Bz,
						ModelFuncPtr ModelFunc, int iopt, double *parmod,
						double alt, int MaxLen, double DSMax, 
						double xfe, double yfe, double zfe,
						double *halpha) {
	/* calculate halpha for a singlealpha direction */
	/* actually it will be the average of two opposite directions */
	
	/* work out two alphas (radians) */
	double alpha0 = alpha*M_PI/180.0;
	double alpha1 = alpha0 + M_PI;
	
	/* Delta is the small distance to move away from the initial footprint */
	double Delta = 0.01;
	
	/* dt and dp are changes in the position in toroidal and poloidal directions*/
	double dt = Delta*cos(alpha0);
	double dp = Delta*sin(alpha0);

	/* rotate dt/dp to get dx/dy  */
	double mlt = atan2(-yfe,-xfe);
	double beta = mlt - M_PI;
	double dx = dp*cos(beta) - dt*sin(beta);
	double dy = dp*sin(beta) + dt*cos(beta);

	/* calculate the trace starting positions */
	double xt0, yt0, zt0, xt1, yt1, zt1;	
	xt0 = xfe + dx;
	yt0 = yfe + dy;
	zt0 = zfe;
	xt1 = xfe - dx;
	yt1 = yfe - dy;
	zt1 = zfe;
	
	//printf("0: %f %f %f\n",xfe,yfe,zfe);
	//printf("1: %f %f %f\n",xt0,yt0,zt0);
	//printf("2: %f %f %f\n",xt1,yt1,zt1);
	
	//printf("mlt %f, alpha0 %f, dx %f, dy %f\n",mlt*12/M_PI,alpha0*180/M_PI,dx,dy);
	//printf("mlt %f, alpha1 %f, dx %f, dy %f\n",mlt*12/M_PI,alpha1*180/M_PI,-dx,-dy);
	//printf("beta %f\n",beta);
	
	/* do the pair of traces */
	double x0[MaxLen],y0[MaxLen],z0[MaxLen];
	double x1[MaxLen],y1[MaxLen],z1[MaxLen];
	double Bx0[MaxLen],By0[MaxLen],Bz0[MaxLen];
	double Bx1[MaxLen],By1[MaxLen],Bz1[MaxLen];
	double xfn0,yfn0,zfn0;
	double xfs0,yfs0,zfs0;
	double xfn1,yfn1,zfn1;
	double xfs1,yfs1,zfs1;
	int ns0, ns1;
	double s0[MaxLen],s1[MaxLen];
	
	/* need to convert footprints from SM to GSW here */
	
	TraceFieldLine(xt0,yt0,zt0,iopt,parmod,ModelFunc,alt,MaxLen,DSMax,
				0,&xfn0,&yfn0,&zfn0,&xfs0,&yfs0,&zfs0,x0,y0,z0,&ns0);
	TraceFieldLine(xt1,yt1,zt1,iopt,parmod,ModelFunc,alt,MaxLen,DSMax,
				0,&xfn1,&yfn1,&zfn1,&xfs1,&yfs1,&zfs1,x1,y1,z1,&ns1);
				
				
	
	/* distance between field lines along normal of original */
	double d0[nstep], d1[nstep];
	
	//printf("%d %d %d\n",nstep,ns0,ns1);
}
