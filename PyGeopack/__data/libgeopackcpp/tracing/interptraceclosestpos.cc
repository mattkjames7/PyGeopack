#include "interptraceclosestpos.h"

void interptraceClosestPos(	int n, double *x, double *y, double *z,
						double *bx, double *by, double *bz,
						int n0, double *x0, double *y0, double *z0, double *s0,
						int n1, double *x1, double *y1, double *z1, double *s1,
						double *xc0, double *yc0, double *zc0,
						double *xc1, double *yc1, double *zc1 ) {
							
	/* return an array of posisions along two nearby field lines which
	 * are the closest points to each element of the original field line */
	
	/* get a couple of splines (one for each field line) */
	Spline Sx0(n0,s0,x0);
	Spline Sy0(n0,s0,y0);
	Spline Sz0(n0,s0,z0);
	Spline Sx1(n1,s1,x1);
	Spline Sy1(n1,s1,y1);
	Spline Sz1(n1,s1,z1);
	bool success;

	/* find the closest position along the splines for each position */
	int i;
	double s0_0, s1_0;
	int i0_0, i1_0;
	for (i=0;i<n;i++) {
		/* find the closest trace position to start the optimization at */
		i0_0 = ClosestS(x[i],y[i],z[i],n0,x0,y0,z0,s0);
		i1_0 = ClosestS(x[i],y[i],z[i],n1,x1,y1,z1,s1);
		s0_0 = s0[i0_0];
		s1_0 = s1[i1_0];
		
		/* Trace 0 */
		success = OptimizePos(x[i],y[i],z[i],bx[i],by[i],bz[i],s0_0,Sx0,Sy0,Sz0,&xc0[i],&yc0[i],&zc0[i]);
		if (!success) {
			interpOptimum(x[i],y[i],z[i],bx[i],by[i],bz[i],i0_0,Sx0,Sy0,Sz0,&xc0[i],&yc0[i],&zc0[i]);
		}
		/* Trace 1 */
		success = OptimizePos(x[i],y[i],z[i],bx[i],by[i],bz[i],s1_0,Sx1,Sy1,Sz1,&xc1[i],&yc1[i],&zc1[i]);
		if (!success) {
			interpOptimum(x[i],y[i],z[i],bx[i],by[i],bz[i],i1_0,Sx1,Sy1,Sz1,&xc1[i],&yc1[i],&zc1[i]);
		}
	}
							
}

void interpOptimum(	double x, double y, double z,
					double bx, double by, double bz,
					int is0, 
					Spline Sx, Spline Sy, Spline Sz,
					double *xc, double *yc, double *zc) {

	/* B unit vector */
	double B = sqrt(bx*bx + by*by + bz*bz);
	double bxu = bx/B;
	double byu = by/B;
	double bzu = bz/B;
							
	/* get the surrounding indices */
	int i0, i1;
	double s0, s1;
	if (is0 == 0) {
		i0 = 0;
		i1 = 2;
	} else if (is0 == Sx.n_-1) {
		i0 = Sx.n_ - 3;
		i1 = Sx.n_ - 1;
	} else {
		i0 = is0 - 1;
		i1 = is0 + 1;
	}
	s0 = Sx.x_[i0];
	s1 = Sx.x_[i1];
	
	/* create an array */
	double s[1000];
	double f[1000];	
	int i;
	double ds = (s1 - s0)/999;
	
	/* fill it */
	for (i=0;i<1000;i++) {
		s[i] = s0 + ds*i;
		f[i] = AngleDiff(s[i],Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
	}
	
	/* find the minimum and hope it's sensible */
	double smin = Sx.x_[is0], fmn = INFINITY;
	for (i=0;i<1000;i++) {
		if (f[i] < fmn) {
			fmn = f[i];
			smin = s[i];
		}
	}
	
	/* get the new position */
	Sx.Interpolate(1,&smin,xc);					
	Sy.Interpolate(1,&smin,yc);					
	Sz.Interpolate(1,&smin,zc);
	double d = sqrt(pow(x-xc[0],2) + pow(y-yc[0],2) + pow(z-zc[0],2)); 
	//printf("smin: %f, fmin %f, d: %f\n",smin,fmn,d);	
	
						
}


int ClosestS(double x, double y, double z,
				int nt, double *xt, double *yt, double *zt,
				double *st) {
	int i, imin;
	double dx, dy, dz, d, dmin = INFINITY;
	for (i=0;i<nt;i++) {
		dx = x - xt[i];
		dy = y - yt[i];
		dz = z - zt[i];
		d = sqrt(dx*dx + dy*dy + dz*dz);
		if (d < dmin) {
			imin = i;
			dmin = d;
		}
	}
	return imin;
				
}

double AngleDiff( 	double s,								/* current position along the field line */
					Spline Sx, Spline Sy, Spline Sz,	/* Splines converting s to a  vector */
					double x, double y, double z,		/* this is the position along the original field line */
					double bx, double by, double bz) {	/* B field unit vector */

	/* get the current position vector */
	double xc, yc, zc;
	Sx.Interpolate(1,&s,&xc);					
	Sy.Interpolate(1,&s,&yc);					
	Sz.Interpolate(1,&s,&zc);	
	
	/* get unit vector */
	double dx, dy, dz, d;
	dx = xc - x;
	dy = yc - y;
	dz = zc - z;
	d = sqrt(dx*dx + dy*dy + dz*dz);
	dx = dx/d;
	dy = dy/d;
	dz = dz/d;
	
	/* get the angle */
	double dot, angle;
	dot = dx*bx + dy*by + dz*bz;
	return fabs(M_PI/2 - acos(dot))*180.0/M_PI;
	
					
						
}

bool OptimizePos(	double x, double y, double z,
					double bx, double by, double bz,
					double s0, 
					Spline Sx, Spline Sy, Spline Sz,
					double *xc, double *yc, double *zc) {

	/* Nelder-Mead settings */
	int MaxIter = 1000;
	double tola = 0.01;
	double tolf = 0.01;
	double alpha = 1.0;
	double gamma = 2.0;
	double rho = 0.5;
	double sigma = 0.5;
	int i;
	
	/* initial/current positions */
	double s[] = {s0+0.01,s0-0.01};
	double smin = 0.0;
	double smax = Sx.x_[Sx.n_-1];
	
	/* B unit vector */
	double B = sqrt(bx*bx + by*by + bz*bz);
	double bxu = bx/B;
	double byu = by/B;
	double bzu = bz/B;
	
	/* current difference between current angle and 90 degrees */
	double f[2];
	f[0] = AngleDiff(s[0],Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
	f[1] = AngleDiff(s[1],Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
	//double f0 = AngleDiff(s0,Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
	//printf("%f %f %f\n",f[0],f0,f[1]);
	//printf("S: %f %f %f %f %f\n",s[0],s[1],fabs(s[1]-s[0]),f[0],f[1]);
	int best, wrst;
	bool cont = true, succ = false;
	bool shrink;
	double scnt, fcnt;
	double sr, se, sc;
	double fr, fe, fc;
	int n = 0;
	
	while (cont) {
		
		/* get best/worst indices */
		if (f[0] < f[1]) {
			best = 0;
			wrst = 1;
		} else {
			best = 1;
			wrst = 0;
		}
		shrink = false;
		
		/* centroid */
		fcnt = f[best];
		scnt = s[best];
		
		/* test reflection */
		sr = scnt + alpha*(scnt - s[wrst]);
		fr = AngleDiff(sr,Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
		
		if (fr < fcnt) {
			/* better than the best - try expanding */
			se = scnt + gamma*(sr - scnt);
			fe = AngleDiff(se,Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
			if (fe < fr) {
				/* accept expanded */
				s[wrst] = se;
				f[wrst] = fe;
			} else {
				/* accept reflected */
				s[wrst] = sr;
				f[wrst] = fr;
			}
		} else if (fr == fcnt) {
			/* accept the reflection */
			s[wrst] = sr;
			f[wrst] = fr;
		} else {
			/* worse than the worst or second worst - contract */
			if ((fr > fcnt) && (fr < f[wrst])) {
				
				/* outside contraction */
				sc = scnt + rho*(sr - scnt);
				fc = AngleDiff(sc,Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
				if (fc <= fr) {
					/* accept contraction */
					s[wrst] = sc;
					f[wrst] = fc;
				} else {
					shrink = true;
				}
			} else {
				/* inside contraction */
				sc = scnt + rho*(s[wrst] - scnt);
				fc = AngleDiff(sc,Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
				if (fc < f[wrst]) {
					/* accept contraction */
					s[wrst] = sc;
					f[wrst] = fc;
				} else {
					shrink = true;
				}				
			}
			
			/* shrink */
			if (shrink) {
				s[wrst] = s[wrst] + sigma*(scnt - s[wrst]);
				f[wrst] = AngleDiff(s[wrst],Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
			}
		}
		
		/* check if we have more or less converged */
		if ((fabs(0.5*(f[1]+f[0])) <= tola) && (fabs(f[1]-f[0]) <= tolf)) {
			cont = false;
			succ = true;
		}
		
		/* or if we have ran out of iteration */
		if (n >= MaxIter) {
			cont = false;
		}
		
		/* check if we have gone out bounds */
		if ((s[0] < smin) || (s[1] < smin) || (s[0] > smax) || (s[1] > smax)) {
			//if (failonce) {
				cont = false;
			//} else {
				//failonce = true;
				//if ((s[0] < smin) || (s[1] < smin)) {
					///* try shifting further along */
					//s[0] = s0 + 0.02*(smax) + 0.01;
					//s[1] = s0 + 0.02*(smax) - 0.01;
				//} else {
					//s[0] = s0 - 0.02*(smax) + 0.01;
					//s[1] = s0 - 0.02*(smax) - 0.01;					
				//}
				//f[0] = AngleDiff(s[0],Sx,Sy,Sz,x,y,z,bxu,byu,bzu);
				//f[1] = AngleDiff(s[1],Sx,Sy,Sz,x,y,z,bxu,byu,bzu);			
			//}
		}
		n++;
	//	printf("S: %f %f %f %f %f\n",s[0],s[1],fabs(s[1]-s[0]),f[0],f[1]);
	}

	if (succ) {
		/* use the average position */
		scnt = 0.5*(s[0] + s[1]);
	} else {
		/* use another method*/
		scnt = s0;
	}
	Sx.Interpolate(1,&s0,xc);					
	Sy.Interpolate(1,&s0,yc);					
	Sz.Interpolate(1,&s0,zc);
	double d0 = sqrt(pow(x-xc[0],2) + pow(y-yc[0],2) + pow(z-zc[0],2)); 
	Sx.Interpolate(1,&scnt,xc);					
	Sy.Interpolate(1,&scnt,yc);					
	Sz.Interpolate(1,&scnt,zc);
	//double a = bxu*(x-xc[0]) + byu*(y-yc[0]) + bzu*(z-zc[0]);
	//double b = acos(a)*180.0/M_PI;
	double d = sqrt(pow(x-xc[0],2) + pow(y-yc[0],2) + pow(z-zc[0],2)); 
	if (d0*1.1 < d) {
		/* it should never get much worse, right? */
		succ = false;
	}
	//printf("scnt: %f, n: %d, f: [%f,%f], s0: %f, d: %f\n",scnt,n,f[0],f[1],s0,d);	
	//printf("s0: %f, scnt: %f, niter %d, d90: %f, da: %f, dot: %f, angle: %f\n",s0,scnt,n,fabs(0.5*(f[1]+f[0])),fabs(f[1]-f[0]),a,b);
	return succ;
}
