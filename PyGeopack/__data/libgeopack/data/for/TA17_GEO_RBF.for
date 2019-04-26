
<!-- saved from url=(0057)http://geo.phys.spbu.ru/~tsyganenko/TA17_GEO_RBF_for.html -->
<html><head><meta http-equiv="Content-Type" content="text/html; charset=windows-1252"></head><body><pre>c
      SUBROUTINE GEO_RBF_MODEL_2017 (IOPT,PARMOD,PS,X,Y,Z,BX,BY,BZ)     !  A DOUBLE-PRECISION SUBROUTINE
C
C  RETURNS COMPONENTS OF THE EXTERNAL MAGNETIC FIELD VECTOR (I.E., DUE TO ONLY MAGNETOSPHERIC CURRENTS,
C  WITHOUT CONTRIBUTION FROM THE EARTH'S SOURCES) IN THE VICINITY OF GEOSYNCHORONOUS ORBIT
C  (McIlwain PARAMETER L IN THE RANGE OF 5.0 &lt; L &lt; 8.4), ACCORDING TO THE DATA-BASED RBF-MODEL
C  DRIVEN BY INTERPLANETARY OBSERVABLES AND DYNAMICAL VARIABLES, REPRESENTING THE RESPONSE OF
C  THE MAGNETOSPHERE TO THE EXTERNAL DRIVING/LOADING (SEE THE REFERENCE BELOW).
C
C  VERSION OF 09/27/2017, BASED ON FITTING THE MODEL TO A DATA SET WITH 71 092 RECORDS, COVERING
C  133 MAGNETIC STORMS WITH -150 &lt; SYM-H_min &lt; -35 nT.
C
C  REFERENCES: (1) Andreeva, V. A., and N. A. Tsyganenko (2017), "Empirical modeling of the quiet
C                  and storm-time geosynchronous magnetic field", Space Weather, v.15, doi:10.1002/2017SW001684.
C
C              (2) Boynton, R. J., M. A. Balikhin, S. A. Billings, H. L. Wei, and N. Ganushkina (2011), "Using
C                  the NARMAX OLS-ERR algorithm to obtain the most influential coupling functions that
C                  affect the evolution of the magnetosphere", JGRA, 116, A05218, doi:10.1029/2010JA015505.
C
C  INPUT PARAMETERS:
C
C  IOPT       A DUMMY PARAMETER, INCLUDED TO MAKE THE SUBROUTINE COMPATIBLE WITH THE TRACING SOFTWARE
C             PACKAGE (GEOPACK-08). IOPT DOES NOT AFFECT THE OUTPUT FIELD AND CAN BE SET AT ANY INTEGER VALUE
C
C  PARMOD     A 10-element array, with its first 6 elements to be specified as follows:
c
C  PARMOD(1) = PDYN     - solar wind dynamic pressure in nPa
c
C  PARMOD(2) = W1       - dynamical variable, calculated by numerical integration of Eq.(7) (see Ref.(1))
C                          with the timescale T1=4 hours and the B-index (see Ref.(2)) as a driving function
C
C  PARMOD(3) = W2       - dynamical variable, calculated in the same way as W1, but with the timescale T2=20 hours
C
C  PARMOD(4) = dW1/dt   - time derivative of W1
c
C  PARMOD(5) = dW2/dt   - time derivative of W2
C
c
C  PARMOD(6) = <imf by=""> - azimuthal IMF By, in GSW (or GSM) coordinate system, in nanoTeslas, averaged over
c                         the previous 30-min trailing interval
c
C  PS                   - geodipole tilt angle (in radians)
c
C  X, Y, Z              - Cartesian GSW (or GSM) position (in Earth's radii, Re=6371.2km)
C
C---------------------------------------------------------------------------------------------------------------
C  ATTENTION: THE MODEL IS VALID ONLY FOR GEOCENTRIC DISTANCES R &gt; 4.0 Re AND INSIDE THE INTERVAL  5.0 &lt; L &lt; 8.4
C             OF THE McILWAIN's L-PARAMETER. DO NOT USE IT OUTSIDE THAT REGION !
C---------------------------------------------------------------------------------------------------------------
c
C  OUTPUT:  BX, BY, BZ   - GSW (or GSM) components of the model field (nanoTesla)
C
C  CODED BY: V. A. ANDREEVA and N. A. TSYGANENKO, version of Dec. 05, 2017
C
C------------------------------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION PARMOD(10),A(10296)       ! INTERPLANETARY PARAMETERS AND LINEAR COEFFICIENTS OF RBF EXPANSIONS
      DIMENSION XX(396),YY(396),ZZ(396)   ! FORWARDS THE RBF NODE COORDINATES
      DIMENSION XX1(33),YY1(33),ZZ1(33)   ! AUXILIARY PARAMETERS USED IN THE RBF GRID CONSTRUCTION (FIRST 33 NODES)
C
      DATA IOP /100001/
      DATA PI/3.14159265359D0/
      DATA XX1/4.925915D0,4.410719D0,3.746298D0,3.163644D0,
     +5.938032D0,5.489692D0,4.844993D0,4.221004D0,3.682127D0,
     +3.229951D0,6.946764D0,6.551808D0,5.940275D0,5.301127D0,
     +4.717121D0,4.207072D0,3.767451D0,3.389047D0,3.062279D0,
     +2.778718D0,7.953350D0,7.601375D0,7.027151D0,6.389825D0,
     +5.778696D0,5.225509D0,4.735646D0,4.304931D0,3.926461D0,
     +3.593175D0,3.298678D0,3.037445D0,2.804794D0/
      DATA ZZ1/0.492630D0,1.302426D0,1.725786D0,1.889779D0,
     +0.494860D0,1.356400D0,1.896380D0,2.169707D0,2.283929D0,
     +2.309214D0,0.496215D0,1.391395D0,2.020123D0,2.392049D0,
     +2.588020D0,2.674538D0,2.694074D0,2.672540D0,2.625852D0,
     +2.563974D0,0.497099D0,1.415206D0,2.111493D0,2.568906D0,
     +2.843613D0,2.994244D0,3.063299D0,3.078934D0,3.059517D0,
     +3.017124D0,2.959808D0,2.893017D0,2.820484D0/

      SAVE
C
       IF (IOP.NE.IOPT) THEN
C
       IOP=IOPT
       OPEN (UNIT=777,FILE='TA17_GEO_RBF.par')   !  MODEL LINEAR COEFFICIENTS A_i,B_i,.. IN THE POLO- AND TORO-EXPANSIONS
       READ (777,100) A
 100   FORMAT (G15.6)
       CLOSE(777)
C----------------------------------------------------------------------------------------
C  CREATE A 3D GRID OF RBF CENTERS, PLACED ON DIPOLE FIELD LINES (ONE-TIME-ONLY PROCEDURE)
C----------------------------------------------------------------------------------------
C
      DO 320 I=1,33
          XX(I)=XX1(I)
          YY(I)=0.D0
          ZZ(I)=ZZ1(I)
 320  CONTINUE

      I1=33
      DO 321 J=1,11
      DO 322 K=1,33
        XF=XX(K)
        YF=YY(K)
        ZF=ZZ(K)
        RHOF=DSQRT(XF**2+YF**2)
        PHI=DATAN2(YF,XF)
        PHI1=PHI+2.D0*PI/12.D0*J
        I1=I1+1
        XX(I1)=RHOF*DCOS(PHI1)
        YY(I1)=RHOF*DSIN(PHI1)
        ZZ(I1)=ZF
  322   CONTINUE
  321 CONTINUE
C
       ENDIF    !      END OF GENERATING THE RBF GRID (ONE-TIME-ONLY PROCEDURE)
C
C--------------------  START CALCULATING THE MODEL B-FIELD  ---------------------------------------
C
      XSM=X*COS(PS)-Z*SIN(PS)         !  RBF EXPANSIONS ARE IN SM COORDINATES
      YSM=Y                           !  -&gt;  CONVERT X,Y,Z FROM GSW (GSM) TO SM
      ZSM=Z*COS(PS)+X*SIN(PS)
C
      PDYN =PARMOD(1)
      W1   =PARMOD(2)*3.58D0/DSQRT(PARMOD(2)**2+3.58D0**2)     ! INCLUDE SATURATION OF W1 AND W2
      W2   =PARMOD(3)*1.08D0/DSQRT(PARMOD(3)**2+1.08D0**2)     ! SEE Ref. (1) Eq.(9)
      DW1  =PARMOD(4)
      DW2  =PARMOD(5)
      BYIMF=PARMOD(6)
C
      FPD=DSQRT(PDYN/2.D0)-1.D0
      FBY=ByIMF/5.D0
C
      CPS=DCOS(PS)
      SPS=DSIN(PS)
      TPS=SPS/CPS
C
      BXSM=0.D0
      BYSM=0.D0
      BZSM=0.D0
C
      DO 1 I=1,396
C
         XP = XX(I)
         YP = YY(I)
         ZP = ZZ(I)
         XM = XP
         YM = YP
         ZM =-ZP
C
         S_M=(XSM-XM)**2+(YSM-YM)**2+(ZSM-ZM)**2   !  square of distance between the obs.pt and node
         S_P=(XSM-XP)**2+(YSM-YP)**2+(ZSM-ZP)**2   !  square of distance between the obs.pt and node
         STP=DSQRT(DSQRT(S_P))
         STM=DSQRT(DSQRT(S_M))

         TXCP=STP*(YSM*ZP-ZSM*YP)                     ! toroidal terms
         TXCM=STM*(YSM*ZM-ZSM*YM)                     ! toroidal terms
         TYCP=STP*(ZSM*XP-XSM*ZP)
         TYCM=STM*(ZSM*XM-XSM*ZM)
         TZCP=STP*(XSM*YP-YSM*XP)
         TZCM=STM*(XSM*YM-YSM*XM)

         IF (S_P.LT.1.D-10) THEN   !  avoid division by 0 if S=0 (i.e., if the observation point coincides with the node)
          PXCP=0.D0
          PYCP=0.D0
          PZCP=0.D0
         ELSE
          SP=1.D0/DSQRT(S_P)
          PXCP=SP*(XP*S_P+0.5D0*(XSM*(YP**2+ZP**2-YSM*YP-ZSM*ZP)   ! poloidal terms
     *     +XP*(YSM**2+ZSM**2-YSM*YP-ZSM*ZP)))                     ! poloidal terms
          PYCP=SP*(YP*S_P+0.5D0*(YSM*(ZP**2+XP**2-ZSM*ZP-XSM*XP)
     *     +YP*(ZSM**2+XSM**2-ZSM*ZP-XSM*XP)))
          PZCP=SP*(ZP*S_P+0.5D0*(ZSM*(XP**2+YP**2-XSM*XP-YSM*YP)
     *     +ZP*(XSM**2+YSM**2-XSM*XP-YSM*YP)))
         ENDIF

         IF (S_M.LT.1.D-10) THEN   !  avoid division by 0 if S=0 (i.e., if the observation point coincides with the node)
          PXCM=0.D0
          PYCM=0.D0
          PZCM=0.D0
         ELSE
          SM=1.D0/DSQRT(S_M)
          PXCM=SM*(XM*S_M+0.5D0*(XSM*(YM**2+ZM**2-YSM*YM-ZSM*ZM)
     *     +XM*(YSM**2+ZSM**2-YSM*YM-ZSM*ZM)))
          PYCM=SM*(YM*S_M+0.5D0*(YSM*(ZM**2+XM**2-ZSM*ZM-XSM*XM)
     *     +YM*(ZSM**2+XSM**2-ZSM*ZM-XSM*XM)))
          PZCM=SM*(ZM*S_M+0.5D0*(ZSM*(XM**2+YM**2-XSM*XM-YSM*YM)
     *     +ZM*(XSM**2+YSM**2-XSM*XM-YSM*YM)))
         ENDIF
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

        ACT=A(I)+A(I+1584)*FPD+A(I+3168)*W1+A(I+4752)*W2+A(I+6336)*DW1+
     *  A(I+7920)*DW2
        AST=A(I+396)+A(I+1980)*FPD+A(I+3564)*W1+A(I+5148)*W2+
     *  A(I+6732)*DW1+A(I+8316)*DW2
        AT =A(I+9504)*BYIMF
        ACP=A(I+792)+A(I+2376)*FPD+A(I+3960)*W1+A(I+5544)*W2+
     *  A(I+7128)*DW1+A(I+8712)*DW2
        ASP=A(I+1188)+A(I+2772)*FPD+A(I+4356)*W1+A(I+5940)*W2+
     *  A(I+7524)*DW1+A(I+9108)*DW2
        AP =A(I+9900)*BYIMF

        BXSM=BXSM+CTX*ACT+STX*AST+(TXCP-TXCM)*AT+CPX*ACP+SPX*ASP
     *   +(PXCP+PXCM)*AP
        BYSM=BYSM+CTY*ACT+STY*AST+(TYCP-TYCM)*AT+CPY*ACP+SPY*ASP
     *   +(PYCP+PYCM)*AP
        BZSM=BZSM+CTZ*ACT+STZ*AST+(TZCP-TZCM)*AT+CPZ*ACP+SPZ*ASP
     *   +(PZCP+PZCM)*AP
C
  1     CONTINUE
C-----------------------------------------------------------------------------------------
C   NOW CONVERT THE OBTAINED MAGNETIC FIELD VECTOR BACK FROM SM TO GSW (GSM) SYSTEM:
C-----------------------------------------------------------------------------------------
      BX=BXSM*CPS+BZSM*SPS
      BY=BYSM
      BZ=BZSM*CPS-BXSM*SPS
C
      RETURN
      END
C
</imf></pre>
</body></html>