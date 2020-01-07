c
      SUBROUTINE RBF_MODEL_2016 (IOPT,PARMOD,PS,X,Y,Z,BX,BY,BZ)     !  A DOUBLE-PRECISION SUBROUTINE
C
C  RETURNS COMPONENTS OF THE EXTERNAL MAGNETIC FIELD VECTOR (I.E., DUE TO ONLY MAGNETOSPHERIC CURRENTS, 
C  WITHOUT CONTRIBUTION FROM THE EARTH'S SOURCES), ACCORDING TO THE DATA-BASED RBF-MODEL DRIVEN BY 
C  INTERPLANETARY AND GROUND-BASED OBSERVABLES.
C
C  VERSION OF 10/19/2016, BASED ON FITTING THE MODEL TO A DATA SET WITH 732 746 RECORDS
C
C  REFERENCES: (1) Tsyganenko, N. A., and V. A. Andreeva (2016), "An empirical RBF model of the magnetosphere
C                  parameterized by interplanetary and ground-based drivers", v.121, doi:10.1002/2016JA023217,
c                  accepted by JGRA, 10/17/2016.
C
C              (2) Andreeva, V. A., and N. A. Tsyganenko (2016), "Reconstructing the magnetosphere from data 
c                  using radial basis functions, JGRA Space Physics, v.121, 2249-2263, doi:10.1002/2015JA022242.
C
C  INPUT PARAMETERS:
C
C  IOPT       A DUMMY PARAMETER, INCLUDED TO MAKE THE SUBROUTINE COMPATIBLE WITH THE TRACING SOFTWARE 
C             PACKAGE (GEOPACK-08). IOPT DOES NOT AFFECT THE OUTPUT FIELD AND MUST BE SET AT ANY INTEGER VALUE
C
C  PARMOD     A 10-element array, with its first 4 elements to be specified as follows:
c
C  PARMOD(1) =  PDYN    - solar wind dynamic pressure in nPa
c
C  PARMOD(2) = <SymHc>  - corrected SymH index, to be calculated as <SymHc> = <0.8*SymH-13*sqrt(PDYN)>
C                         where the angular brackets denote the sliding 30-min average, centered on the
C                         current time moment
c
C  PARMOD(3) = <XIND>   - solar-wind-magnetosphere coupling index, based on Newell et al. [2007] function,
C                         averaged over the previous 30-min trailing interval, see also the documentation 
C                         file TA15_Model_description.pdf for further details. Typical values of <XIND> 
C                         lie between 0 (quiet) and 2 (strongly disturbed)
c
C  PARMOD(4) = <IMF BY> - azimuthal IMF By, in GSW (or GSM) coordinate system, in nanoTeslas, averaged over 
c                         the previous 30-min trailing interval
c
C  PS                   - geodipole tilt angle (in radians)
c
C  X, Y, Z              - Cartesian GSW (or GSM) position (in Earth's radii, Re=6371.2km)
C
C------------------------------------------------------------------------------------------------------
C  ATTENTION: THE MODEL IS VALID ONLY UP TO Xgsw=-15 Re and should NOT be used tailward of that distance
C------------------------------------------------------------------------------------------------------
c
C  OUTPUT:  BX, BY, BZ   - GSW (or GSM) components of the model field (nanoTesla)
C
C  CODED BY: N. A. TSYGANENKO AND V. A. ANDREEVA, version of Oct. 19, 2016
C
C------------------------------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION PARMOD(10),A(23328)                   !SW PARAMETERS AND LINEAR COEFFICIENTS OF RBF-EXPANSIONS
      DIMENSION XX(1296),YY(1296),ZZ(1296),ST(1296),  ! FORWARDS RBF CENTER COORDINATES AND AUXILIARY PARAMS
     *          RHO(1296),ZSP(1296),ZCP(1296),RHBR(1296)
C
      DATA IOP /100001/
      DATA D/4.D0/
      DATA PI/3.14159265359D0/
      DATA A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,
     * A17,A18,A19,A20,A21/12.544D0,-0.194D0,0.305D0,0.0573D0,2.178D0,
     * 0.0571D0,-0.999D0,16.473D0,0.00152D0,0.382D0,0.0431D0,-0.00763D0,
     * -0.210D0,0.0405D0,-4.430D0,-0.636D0,-2.600D0,0.832D0,-5.328D0,
     * 1.103D0,-0.907D0,1.450D0/
C
      SAVE
C
       IF (IOP.NE.IOPT) THEN
C
       IOP=IOPT
       OPEN (UNIT=777,FILE='TA16_RBF.par')   !  MODEL LINEAR COEFFICIENTS A_i,B_i,.. IN THE POLO- AND TORO-EXPANSIONS
       READ (777,100) A
 100   FORMAT (G15.6)
       CLOSE(777)
C----------------------------------------------------------------------------------------
C  CREATE A 3D KURIHARA'S GRID OF RBF CENTERS (ONE-TIME-ONLY PROCEDURE)
C----------------------------------------------------------------------------------------
      KLAT  =8      ! KLAT IS THE NUMBER OF LATITUDE CIRCLES IN THE NORTHERN HEMISPHERE (EXCLUDING THE POLE)
      RLOW_GRID  = 3.3D0  !  THE INNERMOST SPHERE RADIUS
      RHIGH_GRID =16.D0  !  UPPER LIMIT ON THE RADIUS OF OUTERMOST RBF SPHERE
      XGSW_LIM_DATA =-20.D0  !  UPPER LIMIT ON THE DATA TAILWARD LIMIT

      RH=8.D0      !  HINGING PARAMETERS (SEE TAG-2015)
      ALPHA=3.D0   !  ALPHA PARAMETER (SEE TAG-2015)
      NLAT=KLAT+1  ! NLAT IS THE NUMBER OF LATITUDE CIRCLES IN THE NORTHERN HEMISPHERE
C                     (INCLUDING THE POLE)
C
      DLAT_DEG=90.D0/(NLAT-0.5D0)  ! ANGULAR INTERVAL BETWEEN THE LATITUDE CIRCLES
C
      L=0
      R=RLOW_GRID  !  THE INNERMOST SPHERE RADIUS
C
      PD_TR=0.5D0    !  this is just to filter out RBF centers outside the Lin et al model magnetopause
      PM=0.D0
      BzIMF_TR=0.D0
      PSI=0.D0
C
      DO 911 J=1,100   ! J COUNTS THE NUMBER OF SPHERES WITH RBF CENTERS
C                              (WHICH IS ACTUALLY MUCH LESS THAN 100)
C
      DO 912 I=1,NLAT     ! I COUNTS THE LATITUDE CIRCLES (FROM NORTH POLE DOWN TO EQUATOR)
C
         XCOLATD=DLAT_DEG*(DFLOAT(I)-1.D0)  !  COLATITUDE OF THE Ith CIRCLE (in degs)
           NLON=4*(I-1)                     !  NUMBER OF RBF CENTERS ON THE ItH CIRCLE
           IF (I.NE.1) THEN
            DLON_DEG=360.D0/NLON          !  LONGITUDE INTERVAL BETWEEN CENTERS (degs)
           ELSE
            NLON=1                          !  NUMBER OF RBF CENTERS ON THE NORTH POLE
            DLON_DEG=0.D0
           ENDIF
            DO 912 K=1,NLON
             XLOND=(K-1)*DLON_DEG
             XCOLAT=XCOLATD*0.01745329252D0  ! COLATITUDE and
             XLON=XLOND*0.01745329252D0      ! LONGITUDE OF THE Lth RBF CENTER
C
             XXXX=R*DSIN(XCOLAT)*DCOS(XLON)
             YYYY=R*DSIN(XCOLAT)*DSIN(XLON)
             ZZZZ=R*DCOS(XCOLAT)
C----------------------------------------------------------------------------------------------
C   HERE WE CALCULATE THE DISTANCE FROM THE RBF NODE TO THE LIN ET AL. [2010] MODEL MAGNETOPAUSE.
C   THE RBF NODES LYING OUTSIDE THE BOUNDARY ARE NOT INCLUDED IN THE GRID
C----------------------------------------------------------------------------------------------
      A10=0.D0     !   IN THIS CODE WE ASSUME A SIMPLIFIED VERSION OF THE LIN ET AL MODEL (WITHOUT INDENTATIONS
      A11=0.D0     !   AND MINOR TILT-INDEPENDENT ASYMMETRIES
      A13=0.D0
      A14=0.D0
C
      EN=A21
      ES=A21
      THETAN=A19+A20*PSI
      THETAS=A19-A20*PSI
      CTN=DCOS(THETAN)
      CTS=DCOS(THETAS)
      STN=DSIN(THETAN)
      STS=DSIN(THETAS)

      RHO2=YYYY**2+ZZZZ**2
      R1=DSQRT(XXXX**2+RHO2)
      RHO1=DSQRT(RHO2)

      CTT=XXXX/R1
      STT=DSQRT(YYYY**2+ZZZZ**2)/R1
      T=DATAN2(STT,CTT)
      IF (RHO1.GT.1.D-5) THEN  ! WE ARE NOT ON THE X-AXIS - NO SINGULARITIES TO WORRY ABOUT
        SP=ZZZZ/RHO1
        CP=YYYY/RHO1
       ELSE                   ! WE ARE ON THE X-AXIS
        IF (XXXX.GT.0.D0) THEN     !   IF ON THE DAYSIDE, NO PROBLEM EITHER
         SP=0.D0
         CP=1.D0
        ELSE                  ! WE ARE ON THE TAIL AXIS; TO AVOID SINGULARITY:
         RM=1000.D0             !  ASSIGN RM=1000 (A CONVENTIONAL SUBSTITUTE VALUE)
         RETURN               !  AND EXIT
        ENDIF
      ENDIF
C
      PSIN=DACOS(CTT*CTN+STT*STN*SP)
      PSIS=DACOS(CTT*CTS-STT*STS*SP)
C
      DN=A16+(A17+A18*PSI)*PSI
      DS=A16-(A17-A18*PSI)*PSI
C
      CN=A14*(PD_TR+PM)**A15
      CS=CN
C
      B0=A6+A7*(DEXP(A8*BZIMF_TR)-1.D0)/(DEXP(A9*BZIMF_TR)+1.D0)
      B1=A10
      B2=A11+A12*PSI
      B3=A13
C
      F1=(DSQRT(0.5D0*(1.+CTT))+A5*2.D0*STT*CTT*(1.D0-
     * DEXP(-T)))**(B0+B1*CP+B2*SP+B3*SP**2)
      R0=A0*(PD_TR+PM)**A1*(1.D0+A2*(DEXP(A3*BZIMF_TR)-1.D0)/
     * (DEXP(A4*BZIMF_TR)+1.D0))
      RM=R0*F1+CN*DEXP(DN*PSIN**EN)+CS*DEXP(DS*PSIS**ES)    ! POSITION OF THE MODEL MAGNETOPAUSE
C
             IF (R.GT.RM) GOTO 912
             L=L+1                          !  COUNTER OF THE RBF CENTERS

             XX(L)=XXXX
             YY(L)=YYYY
             ZZ(L)=ZZZZ
             ST(L)=DSIN(XCOLAT)
             RHO(L)=R*ST(L)
             ZSP(L)=ZZ(L)*DSIN(XLON)
             ZCP(L)=ZZ(L)*DCOS(XLON)
             RHBR(L)=RH/R*(1.D0-(1.D0+(R/RH)**ALPHA)**(1.D0/ALPHA))

 912         CONTINUE

             RLAST=R
             R=R*(NLAT-0.5D0+PI/4.0D0)/(NLAT-0.5D0-PI/4.0D0)  ! INCREMENT R BY A FIXED FACTOR
             IF (R.GT.RHIGH_GRID) GOTO 913                    ! CENTERS CREATED ONLY INSIDE R=RHIGH
 911  CONTINUE
 913  CONTINUE

       ENDIF    !      END OF GENERATING THE RBF GRID (ONE-TIME-ONLY PROCEDURE)
C
C--------------------  START CALCULATING THE MODEL B-FIELD  ---------------------------------------
C
      XSM=X*COS(PS)-Z*SIN(PS)         !  RBF EXPANSIONS ARE IN SM COORDINATES
      YSM=Y                           !  ->  CONVERT X,Y,Z FROM GSW TO SM 
      ZSM=Z*COS(PS)+X*SIN(PS)
C
      PDYN =PARMOD(1)
      SYMH =PARMOD(2)
      XIND =PARMOD(3)
      BYIMF=PARMOD(4)
C
      FPD=DSQRT(PDYN/2.D0)-1.D0
      SYMH=SYMH/50.D0
C
      CPS=DCOS(PS)
      SPS=DSIN(PS)
      TPS=SPS/CPS
C
      BXSM=0.D0
      BYSM=0.D0
      BZSM=0.D0
C
      DO 1 I=1,1296
C
         XP = XX(I)
         YP = YY(I)
         ZP = ZZ(I)
         XM = XP
         YM = YP
         ZM =-ZP
C
         DELTA_ZR=RHBR(I)*TPS
         DTHETA =-DASIN(DELTA_ZR)*ST(I)
         SDT=DSIN(DTHETA)
         CDTM1=DCOS(DTHETA)-1.D0
         DXP=XP*CDTM1+SDT*ZCP(I)
         DYP=YP*CDTM1+SDT*ZSP(I)
         DZP=ZP*CDTM1-RHO(I)*SDT
         DXM=XM*CDTM1-SDT*ZCP(I)
         DYM=YM*CDTM1-SDT*ZSP(I)
         DZM=ZM*CDTM1-RHO(I)*SDT
C
         CP=DSQRT((XSM-XP-DXP)**2+(YSM-YP-DYP)**2+(ZSM-ZP-DZP)**2+D**2)    ! RBF Ch_i+
         CM=DSQRT((XSM-XM-DXM)**2+(YSM-YM-DYM)**2+(ZSM-ZM-DZM)**2+D**2)    ! RBF Ch_i-
         DCPX=(XSM-XP-DXP)/CP
         DCMX=(XSM-XM-DXM)/CM
         DCPY=(YSM-YP-DYP)/CP
         DCMY=(YSM-YM-DYM)/CM
         DCPZ=(ZSM-ZP-DZP)/CP
         DCMZ=(ZSM-ZM-DZM)/CM
C
         DCPX2=1.D0/CP-DCPX**2/CP
         DCMX2=1.D0/CM-DCMX**2/CM
         DCPY2=1.D0/CP-DCPY**2/CP
         DCMY2=1.D0/CM-DCMY**2/CM
         DCPZ2=1.D0/CP-DCPZ**2/CP
         DCMZ2=1.D0/CM-DCMZ**2/CM
         DCPXY=-DCPX*DCPY/CP
         DCMXY=-DCMX*DCMY/CM
         DCPXZ=-DCPX*DCPZ/CP
         DCMXZ=-DCMX*DCMZ/CM
         DCPYZ=-DCPY*DCPZ/CP
         DCMYZ=-DCMY*DCMZ/CM
C
         TXCP=ZSM*DCPY-YSM*DCPZ
         TYCP=XSM*DCPZ-ZSM*DCPX
         TZCP=YSM*DCPX-XSM*DCPY
         TXCM=ZSM*DCMY-YSM*DCMZ
         TYCM=XSM*DCMZ-ZSM*DCMX
         TZCM=YSM*DCMX-XSM*DCMY
C
         PXCP=2.D0*DCPX-XSM*(DCPY2+DCPZ2)+YSM*DCPXY+Z*DCPXZ
         PYCP=2.D0*DCPY-YSM*(DCPX2+DCPZ2)+ZSM*DCPYZ+XSM*DCPXY
         PZCP=2.D0*DCPZ-ZSM*(DCPX2+DCPY2)+XSM*DCPXZ+YSM*DCPYZ
         PXCM=2.D0*DCMX-XSM*(DCMY2+DCMZ2)+YSM*DCMXY+ZSM*DCMXZ
         PYCM=2.D0*DCMY-YSM*(DCMX2+DCMZ2)+ZSM*DCMYZ+XSM*DCMXY
         PZCM=2.D0*DCMZ-ZSM*(DCMX2+DCMY2)+XSM*DCMXZ+YSM*DCMYZ
C
         CTX = CPS*(TXCP+TXCM)
         CTY = CPS*(TYCP+TYCM)
         CTZ = CPS*(TZCP+TZCM)
C
         STX = SPS*(TXCP-TXCM)
         STY = SPS*(TYCP-TYCM)
         STZ = SPS*(TZCP-TZCM)
C
         CPX = CPS*(PXCP-PXCM)
         CPY = CPS*(PYCP-PYCM)
         CPZ = CPS*(PZCP-PZCM)
C
         SPX = SPS*(PXCP+PXCM)
         SPY = SPS*(PYCP+PYCM)
         SPZ = SPS*(PZCP+PZCM)
C
C-----------------   TOTAL FIELD:    -----------------------------------

        ACT=A(I)+A(I+5184)*FPD+A(I+10368)*SYMH+A(I+15552)*XIND
        AST=A(I+1296)+A(I+6480)*FPD+A(I+11664)*SYMH+A(I+16848)*XIND
        AT =A(I+20736)*BYIMF
        ACP=A(I+2592)+A(I+7776)*FPD+A(I+12960)*SYMH+A(I+18144)*XIND
        ASP=A(I+3888)+A(I+9072)*FPD+A(I+14256)*SYMH+A(I+19440)*XIND
        AP =A(I+22032)*BYIMF

        BXSM=BXSM+CTX*ACT+STX*AST+(TXCP-TXCM)*AT+CPX*ACP+SPX*ASP
     *   +(PXCP+PXCM)*AP
        BYSM=BYSM+CTY*ACT+STY*AST+(TYCP-TYCM)*AT+CPY*ACP+SPY*ASP
     *   +(PYCP+PYCM)*AP
        BZSM=BZSM+CTZ*ACT+STZ*AST+(TZCP-TZCM)*AT+CPZ*ACP+SPZ*ASP
     *   +(PZCP+PZCM)*AP
C
  1     CONTINUE
C-----------------------------------------------------------------------------------------
C   NOW CONVERT THE OBTAINED MAGNETIC FIELD VECTOR BACK FROM SM TO GSM (GSW) SYSTEM:
C-----------------------------------------------------------------------------------------
      BX=BXSM*CPS+BZSM*SPS
      BY=BYSM
      BZ=BZSM*CPS-BXSM*SPS
C
      RETURN
      END
C
