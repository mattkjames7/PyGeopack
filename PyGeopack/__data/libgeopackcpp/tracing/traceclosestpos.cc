#include "traceclosestpos.h"

void TraceClosestPos(Trace T, Trace T0, Trace T1, int I,
					MatrixArray &R,
					double *xc0, double *yc0, double *zc0,
					double *xc1, double *yc1, double *zc1) {
	
	/* get the number of elements */
	int n = T.nstep_[I];
	int n0 = T0.nstep_[0];
	int n1 = T1.nstep_[0];
	
				
	/* calculate the closest position for each step */
	int i;
	for (i=0;i<n;i++) {
		_ClosestPos(i,I,*R.matrix[i],T,T0,T1,&xc0[i],&yc0[i],&zc0[i],&xc1[i],&yc1[i],&zc1[i]);
	}
						
}

void _ClosestPos(int i, int I, Matrix &R, Trace T, Trace T0, Trace T1,
				double *xc0, double *yc0, double *zc0,
				double *xc1, double *yc1, double *zc1) {
	
	/* get the position to rotate */
	double Px = T.xsm_[I][i];
	double Py = T.ysm_[I][i];
	double Pz = T.zsm_[I][i];
	
	/* get the number of elements */
	int n = T.nstep_[I];
	int n0 = T0.nstep_[0];
	int n1 = T1.nstep_[0];

	/* allocate some arrays to store the rotated traces in */
	double *rx = new double[n];
	double *ry = new double[n];
	double *rz = new double[n];
	double *rx0 = new double[n];
	double *ry0 = new double[n];
	double *rz0 = new double[n];
	double *rx1 = new double[n];
	double *ry1 = new double[n];
	double *rz1 = new double[n];
	
	/* rotate the traces */
	_RotateTrace(n,T,I,Px,Py,Pz,R,rx,ry,rz);
	_RotateTrace(n0,T0,0,Px,Py,Pz,R,rx0,ry0,rz0);
	_RotateTrace(n1,T1,0,Px,Py,Pz,R,rx1,ry1,rz1);
	double Prx = rx[i];
	double Pry = ry[i];
	double Prz = rz[i];
	
	/* find the four(hopefully) closest values */
	int nc0, nc1;
	double cx0[4], cy0[4], cz0[4];
	double cx1[4], cy1[4], cz1[4];
	_Closest4Pos(Prx,Pry,Prz,rx0,ry0,rz0,n0,&nc0,cx0,cy0,cz0);
	_Closest4Pos(Prx,Pry,Prz,rx1,ry1,rz1,n1,&nc1,cx1,cy1,cz1);
	
	/* get the closest point where z' = 0 */
	_ClosestPosSpline(nc0,cx0,cy0,cz0,xc0,yc0,zc0);
	_ClosestPosSpline(nc1,cx1,cy1,cz1,xc1,yc1,zc1);
	
	/* clean up */
	delete[] rx;
	delete[] ry;
	delete[] rz;
	delete[] rx0;
	delete[] ry0;
	delete[] rz0;
	delete[] rx1;
	delete[] ry1;
	delete[] rz1;
}

void _RotateTrace(	int n, Trace T, int I, 
					double Px, double Py, double Pz,
					Matrix &R,
					double *rx, double *ry, double *rz) {
	
	int i;
	Matrix v(1,3);
	Matrix r(1,3);
	for (i=0;i<n;i++) {
		/* set vector to be rotated */
		v.data[0][0] = T.xsm_[I][i] - Px;
		v.data[0][1] = T.ysm_[I][i] - Py;
		v.data[0][2] = T.zsm_[I][i] - Pz;
		
		/* rotate */
		MatrixDot(v,R,false,false,r);
		
		/* fill output arrays */
		rx[i] = r.data[0][0];
		ry[i] = r.data[0][1];
		rz[i] = r.data[0][2];
	
	}
	
}

void _Closest4Pos(	double Prx, double Pry, double Prz,
					double *rx, double *ry, double *rz, int n,
					int *nc, double *cx, double *cy, double *cz) {
	int i;
	if (n == 0) {
		nc[0] = 0;
		return;
	}
	if (n <= 4) {
		nc[0] = n;
		for (i=0;i<nc[0];i++) {
			cx[i] = rx[i];
			cy[i] = ry[i];
			cz[i] = rz[i];
		}
		return;
	}
	
	double dx, dy, dz;
	double d;
	double dmin = INFINITY;
	int Imind = -1;
	
	/* checkf or z crossing first */
	for (i=0;i<n-1;i++) {
		if ((rz[i] >= 0) & (rz[i+1] < 0)) {
			Imind = i;
			break;
		}
	}
	
	if (Imind < 0) {
		/*failing that, try the closest point */
		for (i=0;i<n;i++) {
			dx = Prx - rx[i];
			dy = Pry - ry[i];
			dz = Prz - rz[i];
			d = dx*dx + dy*dy + dz*dz;
			if (d < dmin) {
				dmin = d;
				Imind = i;
			}
		}
	}
	
	int I4[4], imn;
	
	if (rz[Imind] < rz[Imind+1]) {
		I4[0] = Imind - 1;
		I4[1] = Imind;
		I4[2] = Imind + 1;
		I4[3] = Imind + 2;
		imn = 0;
	} else {
		I4[3] = Imind - 1;
		I4[2] = Imind;
		I4[1] = Imind + 1;
		I4[0] = Imind + 2;
		imn = 3;
	}		
	
	while (I4[imn] < 0) {
		for (i=0;i<4;i++) {
			I4[i]++;
		}
	}
	
	while (I4[imn] >= n) {
		for (i=0;i<4;i++) {
			I4[i]--;
		}
	}
	
	nc[0] = 4;
	for (i=0;i<4;i++) {
		cx[i] = rx[I4[i]];
		cy[i] = ry[I4[i]];
		cz[i] = rz[I4[i]];
	}
}

void _ClosestPosSpline(int nc, double *cx, double *cy, double *cz,
						double *xc, double *yc, double *zc) {
	
	/* create some splines*/
	Spline Sx(nc,cz,cx);
	Spline Sy(nc,cz,cy);
	
	/* interpolate */
	double z0 = 0.0;
	Sx.Interpolate(1,&z0,xc);
	Sy.Interpolate(1,&z0,yc);
	zc[0] = 0.0;
}
