#include "trace.h"

Trace::Trace() {
	/* initialize all of the boolean parameters */
	inputPos_ = false;
	allocV_ = false;
	inputModelParams_ = false;
	allocModelParams_ = false;
	traceConfigured_ = false;
	tracedGSM_ = false;
	allocGSM_ = false;
	tracedGSE_ = false;
	allocGSE_ = false;
	tracedSM_ = false;
	allocSM_ = false;
	hasFootprints_ = false;
	allocFootprints_ = false;
	hasDist_ = false;
	allocDist_ = false;
	hasR_ = false;
	allocR_ = false;
	hasRnorm_= false;
	allocRnorm_ = false;
	hasHalpha_= false;
	allocHalpha_ = false;
	allocHalpha3D_ = false;
	setModel_ = false;
	allocNstep_ = false;
	allocEqFP_ = false;
	allocEndpoints_ = false;
	allocMP_ = false;
	allocAlpha_ = false;
	/* default trace parameters */
	SetTraceCFG();
	
}

Trace::~Trace() {

	/* check for each allocated variable and delete it*/
	int i, j, k = 0;
	
	/* starting positions */
	if (inputPos_) {
		delete[] x0_;
		delete[] y0_;
		delete[] z0_;
		delete[] Date_;
		delete[] ut_;
	}

	/* SW velocity */
	if (allocV_) {
		delete[] Vx_;
		delete[] Vy_;
		delete[] Vz_;
	}

	/* parameters */
	if (allocModelParams_) {
		for (i=0;i<n_;i++) {
			delete[] parmod_[i];
		}
		delete[] parmod_;
		delete[] iopt_;
	}

	/* traces */
	if (allocMP_) {
		delete[] inMP_;
	}
	if (allocGSM_) {
		for (i=0;i<n_;i++) {
			delete[] xgsm_[i];
			delete[] ygsm_[i];
			delete[] zgsm_[i];
			delete[] bxgsm_[i];
			delete[] bygsm_[i];
			delete[] bzgsm_[i];
		}
		delete[] xgsm_;
		delete[] ygsm_;
		delete[] zgsm_;
		delete[] bxgsm_;
		delete[] bygsm_;
		delete[] bzgsm_;
	}
	if (allocGSE_) {
		for (i=0;i<n_;i++) {
			delete[] xgse_[i];
			delete[] ygse_[i];
			delete[] zgse_[i];
			delete[] bxgse_[i];
			delete[] bygse_[i];
			delete[] bzgse_[i];
		}
		delete[] xgse_;
		delete[] ygse_;
		delete[] zgse_;
		delete[] bxgse_;
		delete[] bygse_;
		delete[] bzgse_;
	}
	if (allocSM_) {
		for (i=0;i<n_;i++) {
			delete[] xsm_[i];
			delete[] ysm_[i];
			delete[] zsm_[i];
			delete[] bxsm_[i];
			delete[] bysm_[i];
			delete[] bzsm_[i];
		}
		delete[] xsm_;
		delete[] ysm_;
		delete[] zsm_;
		delete[] bxsm_;
		delete[] bysm_;
		delete[] bzsm_;
	}

	/* nstep */
	if (allocNstep_) {
		delete[] nstep_;
	}

	/* field line footprints */
	if (allocFootprints_) {
		for (i=0;i<n_;i++) {
			delete[] FP_[i];
		}
		delete[] FP_;
	}

	/* field line distance */
	if (allocDist_) {
		for (i=0;i<n_;i++) {
			delete[] S_[i];
		}
		delete[] S_;
	}

	/* radial distance */
	if (allocR_) {
		for (i=0;i<n_;i++) {
			delete[] R_[i];
		}
		delete[] R_;
	}

	/* r norm distance */
	if (allocRnorm_) {
		for (i=0;i<n_;i++) {
			delete[] Rnorm_[i];
		}
		delete[] Rnorm_;
	}

	/* h alpha*/
	if (allocAlpha_) {
		delete[] alpha0_;
		delete[] alpha1_;
	}
	if (allocHalpha_) {
		delete[] Halpha_;
	}
	if (allocHalpha3D_) {
		for (i=0;i<n_;i++) {
			for (j=0;j<nalpha_;j++) {
				delete[] Halpha3D_[i][j];
			}
			delete[] Halpha3D_[i];
		}
		delete[] Halpha3D_;
	}

	/* footprint/endpoints */
	if (allocEndpoints_) {
		delete[] xfn_;
		delete[] yfn_;
		delete[] zfn_;
		delete[] xfs_;
		delete[] yfs_;
		delete[] zfs_;
	}	
	if (allocEqFP_) {
		delete[] xfe_;
		delete[] yfe_;
		delete[] zfe_;
	}
}

void Trace::InputPos(	int n, double *x, double *y, double *z,
						int *Date, float *ut, const char *CoordIn,
						double *Vx, double *Vy, double *Vz) {
	


	/* set the velocity vectors */
	int i;
	Vx_ = new double[n];
	Vy_ = new double[n];
	Vz_ = new double[n];
	for (i=0;i<n;i++) {
		Vx_[i] = Vx[i];
		Vy_[i] = Vy[i];
		Vz_[i] = Vz[i];
	}
	allocV_ = true;
	
	/* call the other overloaded function */
	InputPos(n,x,y,z,Date,ut,CoordIn);
	
}

void Trace::InputPos(	int n, double *x, double *y, double *z,
						int *Date, float *ut, const char *CoordIn) {

	/* check that we have not already done this */
	if (inputPos_) {
		printf("Input positions already set, ignoring...\n");
		return;
	}

	/* set the velocity vectors */
	if (!allocV_) {
		Vx_ = new double[n];
		Vy_ = new double[n];
		Vz_ = new double[n];

		TData->GetVx(n,Date,ut,Vx_);
		TData->GetVy(n,Date,ut,Vy_);
		TData->GetVz(n,Date,ut,Vz_);
		allocV_ = true;

	}

	/* allocate the memory to store the input coords */
	n_ = n;
	x0_ = new double[n];
	y0_ = new double[n];
	z0_ = new double[n];
	Date_ = new int[n];
	ut_ = new float[n];

	/* copy times across */
	int i;
	for (i=0;i<n_;i++) {
		Date_[i] = Date[i];
		ut_[i] = ut[i];
	}

	/* convert the coordinates to GSM */
	if (strcmp(CoordIn,"GSE") == 0) {
		GSEtoGSMUT(x,y,z,n,Vx_,Vy_,Vz_,Date,ut,x0_,y0_,z0_);
	} else if (strcmp(CoordIn,"SM") == 0) {
		SMtoGSMUT(x,y,z,n,Vx_,Vy_,Vz_,Date,ut,x0_,y0_,z0_);
	} else {
		if (strcmp(CoordIn,"GSM") != 0) {
			printf("WARNING: unrecognised input coordinate system provided (%s), using GSM instead\n",CoordIn);
		}
		/* no need to do anything, just copy */
		for (i=0;i<n_;i++) {
			x0_[i] = x[i];
			y0_[i] = y[i];
			z0_[i] = z[i];
		}
	}	

	/* set the flag so we know to delete  again once the object is deleted */
	inputPos_ = true;
						
}						

void Trace::SetModel(const char *Model) {
	
	/* set the external field model to use */
	if (strcmp(Model,"T89") == 0){
		ModelFunc_ = &t89c_;
	} else if (strcmp(Model,"T96") == 0) {
		ModelFunc_ = &t96_;
	} else if (strcmp(Model,"T01") == 0) {
		ModelFunc_ = &t01_01_;
	} else if (strcmp(Model,"TS05") == 0) {
		ModelFunc_ = &t04_s_;
	} else if (strcmp(Model,"IGRF") == 0) {
		ModelFunc_ = &DummyFunc;
	} else { 
		printf("WARNING: Model %s not found - using T96\n",Model);
		ModelFunc_ = &t96_;
		return;
	}	
	Model_ = Model;
	setModel_ = true;
}

void Trace::SetModelParams(int *iopt, double **parmod) {
	
	/* check that the input parameters have been provided */
	if (!inputPos_) {
		printf("Run 'InputPos()' function before SetModelParams\n");
		return;
	}
	
	/* check that the model has been set too */
	if (!setModel_) {
		printf("Run 'SetModel()' function before running SetModelParams()\n");
		return;
	}
	
	/* set the pointers to parameters provided externally */
	iopt_ = iopt;
	parmod_ = parmod;
	
	inputModelParams_ = true;
}

void Trace::SetModelParams() {
	
	/* check that the input parameters have been provided */
	if (!inputPos_) {
		printf("Run 'InputPos()' function before SetModelParams()\n");
		return;
	}	
	
	/* check that the model has been set too */
	if (!setModel_) {
		printf("Run 'SetModel()' function before running SetModelParams()\n");
		return;
	}
	
	/* allocate the parameters */
	int i;
	iopt_ = new int[n_];
	parmod_ = new double*[n_];
	for (i=0;i<n_;i++) {
		parmod_[i] = new double[10];
	}
	
	/* calculate them */
	TData->GetModelParams(n_,Date_,ut_,Model_,iopt_,parmod_);
	
	allocModelParams_ = true;
	inputModelParams_ = true;
}

void Trace::SetTraceCFG(double alt, int MaxLen, double DSMax, 
						bool Verbose, int TraceDir) {
	
	/* set all of the params */					
	alt_ = alt;
	MaxLen_ = MaxLen;
	DSMax_ = DSMax;
	Verbose_ = Verbose;
	TraceDir_ = TraceDir;
	
	
}

void Trace::SetTraceCFG() {
	
	/* set default params */					
	alt_ = 100.0;
	MaxLen_ = 1000;
	DSMax_ = 1.0;
	Verbose_ = false;
	TraceDir_ = 0;
	
	
}

void Trace::SetAlpha(int nalpha, double *alpha, double Delta) {
	
	/*NOTE: for each alpha, there will be two traces - one for the 
	 * supplied value and one for alpha + 180 */
	/* set the alpha pointer */
	nalpha_ = nalpha;
	//alpha_ = alpha;
	alpha0_ = new double[nalpha_];
	alpha1_ = new double[nalpha_];
	allocAlpha_ = true;
	double dtor = M_PI/180.0;
	int i;
	for (i=0;i<nalpha;i++) {
		alpha0_[i] = alpha[i]*dtor;
		alpha1_[i] = fmod(alpha[i]*dtor + M_PI,2*M_PI);
	}
	Delta_ = Delta;
}

Trace Trace::TracePosition(int i, double x, double y, double z) {
	/* return a new trace object at the supplied position using the
	 * parameters at time i */
	Trace T;
	
	/* input position and time - I am pretty certain that the midpoints
	 * of the field lines are stored in SM coords */
	T.InputPos(1,&x,&y,&z,&Date_[i],&ut_[i],"SM",&Vx_[i],&Vy_[i],&Vz_[i]);
	
	/* set the model up */
	T.SetModel(Model_);
	T.SetModelParams(&iopt_[i],&parmod_[i]);
	T.SetTraceCFG(alt_-1000.0,MaxLen_,DSMax_,false,0);
	
	/* run the GSM trace */
	T.TraceGSM();
	
	/* now convert to SM */
	T.TraceSM();
	
	/* calculate S*/
	T.CalculateTraceDist();
	
	return T;
	
}


void Trace::_CalculateTraceHalpha(	int i, int j, double *halpha) {

	/* some variables needed */
	double xe0,ye0,ze0,xe1,ye1,ze1;
	int k;
	
	/* get the trace starting points first */
	_CalculateHalphaStartPoints(i,j,&xe0,&ye0,&ze0,&xe1,&ye1,&ze1);

	/* calculate rotation matrices */
	//MatrixArray R = TraceRotationMatrices(nstep_[i],bxsm_[i],bysm_[i],bzsm_[i]);
	

	/* do two traces */
	Trace T0 = TracePosition(i,xe0,ye0,ze0);
	Trace T1 = TracePosition(i,xe1,ye1,ze1);
	
	/* the traces above may only have 0 steps - in which case we can 
	 * just fill halpha with nans and leave the function */
	if ((T0.nstep_[0] == 0) | (T1.nstep_[0] == 0)) {
		for (k=0;k<nstep_[i];k++) {
			halpha[k] = NAN;
		}
		return;
	}
	
	/* get the closest points to each step of the original trace*/
	double *xc0 = new double[nstep_[i]];
	double *yc0 = new double[nstep_[i]];
	double *zc0 = new double[nstep_[i]];
	double *xc1 = new double[nstep_[i]];
	double *yc1 = new double[nstep_[i]];
	double *zc1 = new double[nstep_[i]];

	interptraceClosestPos(	nstep_[i],xsm_[i],ysm_[i],zsm_[i],
							bxsm_[i],bysm_[i],bzsm_[i],
							T0.nstep_[0],T0.xsm_[0],T0.ysm_[0],T0.zsm_[0],T0.S_[0],
							T1.nstep_[0],T1.xsm_[0],T1.ysm_[0],T1.zsm_[0],T1.S_[0],
							xc0,yc0,zc0,xc1,yc1,zc1);

	/* calculate distances and then halpha */
	double d, dx, dy, dz, h0, h1;
	
	for (k=0;k<nstep_[i];k++) {
		dx = xsm_[i][k] - xc0[k];
		dy = ysm_[i][k] - yc0[k];
		dz = zsm_[i][k] - zc0[k];
		d = sqrt(dx*dx + dy*dy + dz*dz);
		h0 = d/Delta_;
		
		dx = xsm_[i][k] - xc1[k];
		dy = ysm_[i][k] - yc1[k];
		dz = zsm_[i][k] - zc1[k];
		d = sqrt(dx*dx + dy*dy + dz*dz);
		h1 = d/Delta_;
		
		halpha[k] = 0.5*(h0 + h1);
		//printf("%d %5.3f %5.3f %5.3f\n",k,h0,h1,halpha[k]);
	}
	/* free up memory */
	delete[] xc0;
	delete[] yc0;
	delete[] zc0;
	delete[] xc1;
	delete[] yc1;
	delete[] zc1;
}

void Trace::_CalculateHalpha() {

	/* loop through each trace and alpha combination */
	int i, j, k, I, J;
	for (i=0;i<n_;i++) {
		I = i*(nalpha_*MaxLen_);
		if (isfinite(FP_[i][12])) {
			for (j=0;j<nalpha_;j++) {
				J = j*MaxLen_;
				_CalculateTraceHalpha(i,j,Halpha3D_[i][j]);
				for (k=0;k<MaxLen_;k++) {
					Halpha_[I + J + k] = Halpha3D_[i][j][k];
				}
			}
		}
	}
}
		
bool Trace::_CheckHalpha() {
	
	if (!allocAlpha_) {
		printf("Run the 'SetAlpha()' function prior to calculating h_alpha\n");
		return false;
	}
	
	if (nalpha_ <= 0) {
		printf("1 or more values of alpha must be provided to calculate h_alpha\n");
		return false;
	}
	
	return true;
	
}
		
void Trace::CalculateHalpha() {

	if (!_CheckHalpha()) {
		return;
	}
	
	/* allocate both 1D and 3D arrays */
	Halpha_ = new double[n_*nalpha_*MaxLen_];
	Halpha3D_ = new double**[n_];
	int i, j;
	for (i=0;i<n_;i++) {
		Halpha3D_[i] = new double*[nalpha_];
		for (j=0;j<nalpha_;j++) {
			Halpha3D_[i][j] = new double[MaxLen_];
		}
	}
	allocHalpha_ = true;
	allocHalpha3D_ = true;

	_CalculateHalpha();
}

void Trace::CalculateHalpha(double *halpha) {

	if (!_CheckHalpha()) {
		return;
	}
	
	/* allocate 3D array and use pointer for 1D */
	Halpha_ = halpha;
	Halpha3D_ = new double**[n_];
	int i, j;
	for (i=0;i<n_;i++) {
		Halpha3D_[i] = new double*[nalpha_];
		for (j=0;j<nalpha_;j++) {
			Halpha3D_[i][j] = new double[MaxLen_];
		}
	}
	allocHalpha3D_ = true;
	_CalculateHalpha();
}

void Trace::CalculateHalpha(double ***halpha3d) {

	if (!_CheckHalpha()) {
		return;
	}
	
	/* allocate 1D and use pointer for 3D array */
	Halpha_ = new double[n_*nalpha_*MaxLen_];
	Halpha3D_ = halpha3d;

	allocHalpha_ = true;
	
	_CalculateHalpha();
}

void Trace::CalculateHalpha(double *halpha, double ***halpha3d) {

	if (!_CheckHalpha()) {
		return;
	}
	
	/* use pointer for both 1D and 3D arrays */
	Halpha_ = halpha;
	Halpha3D_ = halpha3d;
	
	_CalculateHalpha();
}

void Trace::_CalculateHalphaStartPoints(int i, int j,
							double *xe0, double *ye0, double *ze0,
							double *xe1, double *ye1, double *ze1) {
	
	/* calculate the tracing start points for each alpha */
	double dt, dp, beta, dx, dy;
	
	/* dt and dp are the toroidal and poloidal components of Delta */
	dt = Delta_*cos(alpha0_[j]); // alpha = 0.0 is toroidal
	dp = Delta_*sin(alpha0_[j]);
	
	/* rotate based on the local time */
	beta = atan2(-xfe_[i],-yfe_[i]);
	dy = dp*cos(beta) - dt*sin(beta);
	dx = dp*sin(beta) + dt*cos(beta);
	
	/* set the start points of the new field lines */
	xe0[0] = xfe_[i] + dx;
	ye0[0] = yfe_[i] + dy;
	ze0[0] = zfe_[i];
	xe1[0] = xfe_[i] - dx;
	ye1[0] = yfe_[i] - dy;
	ze1[0] = zfe_[i];

}


void Trace::TraceGSM(	int *nstep,
						double **xgsm, double **ygsm, double **zgsm,
						double **bxgsm, double **bygsm, double **bzgsm) {
	
	/* link the pointers within the object to those supplied by this 
	 * function					*/
	nstep_ = nstep;
	xgsm_ = xgsm;					
	ygsm_ = ygsm;					
	zgsm_ = zgsm;					
	bxgsm_ = bxgsm;					
	bygsm_ = bygsm;					
	bzgsm_ = bzgsm;		
	
	/* call the tracing code */
	_TraceGSM();
}
void Trace::TraceGSM(	int *nstep) {
	
	/* link the pointers within the object to those supplied by this 
	 * function					*/
	nstep_ = nstep;
	xgsm_ = new double*[n_];					
	ygsm_ = new double*[n_];					
	zgsm_ = new double*[n_];					
	bxgsm_ = new double*[n_];					
	bygsm_ = new double*[n_];					
	bzgsm_ = new double*[n_];
	int i;
	for (i=0;i<n_;i++) {
		xgsm_[i] = new double[MaxLen_];					
		ygsm_[i] = new double[MaxLen_];					
		zgsm_[i] = new double[MaxLen_];					
		bxgsm_[i] = new double[MaxLen_];					
		bygsm_[i] = new double[MaxLen_];					
		bzgsm_[i] = new double[MaxLen_];		
	}		
	allocGSM_ = true;
	
	
	/* call the tracing code */
	_TraceGSM();	

}

void Trace::TraceGSM() {
	
	/* no pointers provided: allocate them*/
	if (!allocNstep_) {
		nstep_ = new int[n_];
		allocNstep_ = true;
	}
	xgsm_ = new double*[n_];					
	ygsm_ = new double*[n_];					
	zgsm_ = new double*[n_];					
	bxgsm_ = new double*[n_];					
	bygsm_ = new double*[n_];					
	bzgsm_ = new double*[n_];
	int i;
	for (i=0;i<n_;i++) {
		xgsm_[i] = new double[MaxLen_];					
		ygsm_[i] = new double[MaxLen_];					
		zgsm_[i] = new double[MaxLen_];					
		bxgsm_[i] = new double[MaxLen_];					
		bygsm_[i] = new double[MaxLen_];					
		bzgsm_[i] = new double[MaxLen_];		
	}		
	allocGSM_ = true;
	
	
	/* call the tracing code */
	_TraceGSM();
	
}

void Trace::_TraceGSM() {
	
	/* this function actually calls the tracing routines */
	
	/* check this hasn't already been done */
	if (tracedGSM_) {
		printf("Attempted to trace twice? not happening mate...\n");
		return;
	}
	
	/* check we have input positions */
	if (!inputPos_) {
		printf("Need InputPos() before trace\n");
		return;
	}
	
	/* check we have a model */
	if (!setModel_) {
		printf("Set model function with SetModel() first\n");
		return;
	}
	
	/* check that we have model parameters */
	if (!inputModelParams_) {
		printf("Run SetModelParams() before tracing\n");
		return;
	}
	
	/* allocate the endpoints */
	xfn_ = new double[n_];
	yfn_ = new double[n_];
	zfn_ = new double[n_];
	xfs_ = new double[n_];
	yfs_ = new double[n_];
	zfs_ = new double[n_];
	allocEndpoints_ = true;
	
	/* check if all of the starting points are within the MP */
	inMP_ = new bool[n_];
	allocMP_ = true;
	int i;
	for (i=0;i<n_;i++) {
		inMP_[i] = WithinMP(x0_[i],y0_[i],z0_[i],parmod_[i][3],parmod_[i][0]);
	}
	
	
	for (i=0;i<n_;i++) {
		if (Verbose_) {
			printf("\rTracing field line %d of %d (%6.2f)%%",i+1,n_,((float) (i+1)*100.0)/n_);
		}

		/* call recalc */
		Recalc(Date_[i],ut_[i],Vx_[i],Vy_[i],Vz_[i]);
		
		
		if (inMP_[i]) {

			/* perform trace */
			TraceFieldLine(x0_[i],y0_[i],z0_[i],iopt_[i],parmod_[i],
							ModelFunc_,alt_,MaxLen_,DSMax_,TraceDir_,
							&xfn_[i],&yfn_[i],&zfn_[i],&xfs_[i],&yfs_[i],&zfs_[i],
							xgsm_[i],ygsm_[i],zgsm_[i],&nstep_[i]);

			/*get B vectors along trace*/
			ModelField(nstep_[i],xgsm_[i],ygsm_[i],zgsm_[i],
						&Date_[i],&ut_[i],true,Model_,&iopt_[i],&parmod_[i],
						&Vx_[i],&Vy_[i],&Vz_[i],"GSM","GSM",
						bxgsm_[i],bygsm_[i],bzgsm_[i]);

		} else {
			/*fill with NaN*/
			nstep_[i] = 0;
		}
							
	}	
	if (Verbose_) { 
		printf("\n");
	}
}


void Trace::TraceGSE(	double **xgse, double **ygse, double **zgse,
						double **bxgse, double **bygse, double **bzgse) {
	
	/* link the pointers within the object to those supplied by this 
	 * function					*/
	xgse_ = xgse;					
	ygse_ = ygse;					
	zgse_ = zgse;					
	bxgse_ = bxgse;					
	bygse_ = bygse;					
	bzgse_ = bzgse;		
	
	/* call the tracing code */
	_TraceGSE();
}

void Trace::TraceGSE() {
	
	/* no pointers provided: allocate them*/
	xgse_ = new double*[n_];					
	ygse_ = new double*[n_];					
	zgse_ = new double*[n_];					
	bxgse_ = new double*[n_];					
	bygse_ = new double*[n_];					
	bzgse_ = new double*[n_];
	int i;
	for (i=0;i<n_;i++) {
		xgse_[i] = new double[MaxLen_];					
		ygse_[i] = new double[MaxLen_];					
		zgse_[i] = new double[MaxLen_];					
		bxgse_[i] = new double[MaxLen_];					
		bygse_[i] = new double[MaxLen_];					
		bzgse_[i] = new double[MaxLen_];		
	}		
	allocGSE_ = true;
	
	/* call the tracing code */
	_TraceGSE();
	
}

void Trace::_TraceGSE() {
	int i, j;
	for (i=0;i<n_;i++) {
		ConvertTraceCoords(nstep_[i],"GSE",
							xgsm_[i],ygsm_[i],zgsm_[i],
							xgse_[i],ygse_[i],zgse_[i],
							bxgsm_[i],bygsm_[i],bzgsm_[i],
							bxgse_[i],bygse_[i],bzgse_[i]);
	}
}

void Trace::TraceSM(	double **xsm, double **ysm, double **zsm,
						double **bxsm, double **bysm, double **bzsm) {
	
	/* link the pointers within the object to those supplied by this 
	 * function					*/
	xsm_ = xsm;					
	ysm_ = ysm;					
	zsm_ = zsm;					
	bxsm_ = bxsm;					
	bysm_ = bysm;					
	bzsm_ = bzsm;		
	
	/* call the tracing code */
	_TraceSM();
}

void Trace::TraceSM() {
	
	/* no pointers provided: allocate them*/
	xsm_ = new double*[n_];					
	ysm_ = new double*[n_];					
	zsm_ = new double*[n_];					
	bxsm_ = new double*[n_];					
	bysm_ = new double*[n_];					
	bzsm_ = new double*[n_];
	int i;
	for (i=0;i<n_;i++) {
		xsm_[i] = new double[MaxLen_];					
		ysm_[i] = new double[MaxLen_];					
		zsm_[i] = new double[MaxLen_];					
		bxsm_[i] = new double[MaxLen_];					
		bysm_[i] = new double[MaxLen_];					
		bzsm_[i] = new double[MaxLen_];		
	}		
	allocSM_ = true;
	
	/* call the tracing code */
	_TraceSM();
	
}

void Trace::_TraceSM() {
	int i, j;
	for (i=0;i<n_;i++) {
		ConvertTraceCoords(nstep_[i],"SM",
							xgsm_[i],ygsm_[i],zgsm_[i],
							xsm_[i],ysm_[i],zsm_[i],
							bxgsm_[i],bygsm_[i],bzgsm_[i],
							bxsm_[i],bysm_[i],bzsm_[i]);
	}
}

void Trace::CalculateTraceDist() {
	int i;
	S_ = new double*[n_];
	for (i=0;i<n_;i++) {
		S_[i] = new double[MaxLen_];
	}
	allocDist_ = true;
	
	_CalculateTraceDist();
}

void Trace::CalculateTraceDist(double **S) {

	S_ = S;
	_CalculateTraceDist();
}


void Trace::_CalculateTraceDist() {
	int i, j;
	double dx, dy, dz;
	for (i=0;i<n_;i++) {
		S_[i][0] = 0.0;
		for (j=1;j<nstep_[i];j++) {
			dx = xgsm_[i][j] - xgsm_[i][j-1];
			dy = ygsm_[i][j] - ygsm_[i][j-1];
			dz = zgsm_[i][j] - zgsm_[i][j-1];
			S_[i][j] = S_[i][j-1] + sqrt(dx*dx + dy*dy + dz*dz);
		}
	}
	hasDist_ = true;
}




void Trace::CalculateTraceR() {
	int i;
	R_ = new double*[n_];
	for (i=0;i<n_;i++) {
		R_[i] = new double[MaxLen_];
	}
	allocR_ = true;
	
	_CalculateTraceR();
}

void Trace::CalculateTraceR(double **R) {
	
	R_ = R;
	
	_CalculateTraceR();
}

void Trace::_CalculateTraceR() {
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			FieldLineR(nstep_[i],xgsm_[i],ygsm_[i],zgsm_[i],R_[i]);
		}
	}
	hasR_ = true;
}


void Trace::CalculateTraceRnorm() {
	int i;
	Rnorm_ = new double*[n_];
	for (i=0;i<n_;i++) {
		Rnorm_[i] = new double[MaxLen_];
	}
	allocRnorm_ = true;
	
	_CalculateTraceRnorm();
}

void Trace::CalculateTraceRnorm(double **Rnorm) {
	
	Rnorm_ = Rnorm;
	
	_CalculateTraceRnorm();
}

void Trace::_CalculateTraceRnorm() {
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			/* need footprints and R done first */
			FieldLineRnorm(nstep_[i],R_[i],FP_[i][12],Rnorm_[i]);
		}
	}
	hasRnorm_ = true;
}


void Trace::CalculateTraceFP() {
	int i;
	FP_ = new double*[n_];
	for (i=0;i<n_;i++) {
		FP_[i] = new double[15];
	}
	allocFootprints_ = true;
	
	_CalculateTraceFP();
}

void Trace::CalculateTraceFP(double **FP) {
	
	FP_ = FP;
	
	_CalculateTraceFP();
}

void Trace::_CalculateTraceFP() {
	
	xfe_ = new double[n_];
	yfe_ = new double[n_];
	zfe_ = new double[n_];
	allocEqFP_ = true;
	
	int i, j;
	for (i=0;i<n_;i++) {
		TraceFootprintsSM(nstep_[i],ut_[i],xsm_[i],ysm_[i],zsm_[i],
							S_[i],R_[i],xfn_[i],yfn_[i],zfn_[i],
							xfs_[i],yfs_[i],zfs_[i],
							&xfe_[i],&yfe_[i],&zfe_[i],
							alt_,FP_[i],MaxLen_,TraceDir_);
							
	}
	hasFootprints_ = true;
}

void Trace::GetTraceGSM(double **x,double **y, double **z) {
	/* copy GSM position into output arrays*/
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			x[i][j] = xgsm_[i][j];
			y[i][j] = ygsm_[i][j];
			z[i][j] = zgsm_[i][j];
		}
	}
}

void Trace::GetTraceGSM(	double **x,double **y, double **z,
					double **Bx,double **By, double **Bz) {
	/* copy GSM field into output arrays*/
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			Bx[i][j] = bxgsm_[i][j];
			By[i][j] = bygsm_[i][j];
			Bz[i][j] = bzgsm_[i][j];
		}
	}
	
	/* get the position */
	GetTraceGSM(x,y,z);
}

void Trace::GetTraceGSE(double **x,double **y, double **z) {
	/* copy GSE position into output arrays*/
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			x[i][j] = xgse_[i][j];
			y[i][j] = ygse_[i][j];
			z[i][j] = zgse_[i][j];
		}
	}
}

void Trace::GetTraceGSE(	double **x,double **y, double **z,
					double **Bx,double **By, double **Bz) {
	/* copy GSE field into output arrays*/
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			Bx[i][j] = bxgse_[i][j];
			By[i][j] = bygse_[i][j];
			Bz[i][j] = bzgse_[i][j];
		}
	}
	
	/* get the position */
	GetTraceGSE(x,y,z);
}

void Trace::GetTraceSM(double **x,double **y, double **z) {
	/* copy SM position into output arrays*/
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			x[i][j] = xsm_[i][j];
			y[i][j] = ysm_[i][j];
			z[i][j] = zsm_[i][j];
		}
	}
}

void Trace::GetTraceSM(	double **x,double **y, double **z,
					double **Bx,double **By, double **Bz) {
	/* copy SM field into output arrays*/
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			Bx[i][j] = bxsm_[i][j];
			By[i][j] = bysm_[i][j];
			Bz[i][j] = bzsm_[i][j];
		}
	}
	
	/* get the position */
	GetTraceSM(x,y,z);
}

void Trace::GetTraceDist(double **S) {
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			S[i][j] = S_[i][j];
		}
	}	
}

void Trace::GetTraceR(double **R) {
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			R[i][j] = R_[i][j];
		}
	}	
}

void Trace::GetTraceRnorm(double **Rnorm) {
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<nstep_[i];j++) {
			Rnorm[i][j] = Rnorm_[i][j];
		}
	}	
}

void Trace::GetTraceFootprints(double **FP) {
	int i, j;
	for (i=0;i<n_;i++) {
		for (j=0;j<15;j++) {
			FP[i][j] = FP_[i][j];
		}
	}	
}

void Trace::GetTraceNstep(int *nstep) {
	int i;
	for (i=0;i<n_;i++) {
		nstep[i] = nstep_[i];
	}
}
