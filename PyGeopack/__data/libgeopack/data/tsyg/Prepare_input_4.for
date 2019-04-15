c  This is the latest version, specially compiled for processing the
c  yearly OMNI files with 5-min resolution, named
c       YYYY_IMF_and_SW_gaps_le_3hrs_filled_SYMH_added.txt, in which:
c
c (i) <=3hr gaps were filled by linear interpolation (codes Fill_IMF_gaps.for
c      and Fill_SW_gaps_add_Dst.for)
c
c (ii)  5-min SYM-H index values were added to each record
c
c (iii) >=2hr-long quiet-time periods were identified (based on a set of
c         criterions - see readme for details), followed by continuous data
c         intervals. Lists of those intervals were compiled, using the code
c         Prepare_intervals_1.for; their names are YYYY_Interval_list.txt
c
c---------------older comments------------------------------------------------------
c
c  This version also calculates and includes in the output the dipole tilt
c  angle (in radians).
c
          P r o g r a m   Prepare_input_4
C
c   This program prepares a file with a sequence of geoeffective parameters to be used
c     as input for the new TS05 model.
c
c   Author:  N. A. Tsyganenko  USRA/NASA GSFC,    SPBGU
c   Dates:                       01/29/2004,    09/09/2008
c
      implicit real*8 (a-h,o-z)
      REAL AAA,TILT,BBB,VXX,VYY,VZZ

      CHARACTER*80 SWNAME,NAMEOUT,LISTNAME
      DIMENSION IDAY(105408),IHOUR(105408),MIN(105408),BXGSM(105408),
     * BYGSM(105408),BZGSM(105408),VXGSE(105408),VYGSE(105408),
     * VZGSE(105408),V_SW(105408),TEMP(105408),DEN(105408),SYMH(105408),
     * IMFFLAG(105408),ISWFLAG(105408)

      DIMENSION A(69)
      DIMENSION INDBEG(500),INDEND(500)

      COMMON /GEOPACK1/ AAA(15),TILT,BBB(18)

      DO 777 IYEAR=1995,2008

      IF (IYEAR.EQ.1995) THEN
      SWNAME='1995_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='1995_Interval_list.txt'
      NAMEOUT='1995_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.1996) THEN
      SWNAME='1996_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='1996_Interval_list.txt'
      NAMEOUT='1996_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.1997) THEN
      SWNAME='1997_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='1997_Interval_list.txt'
      NAMEOUT='1997_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.1998) THEN
      SWNAME='1998_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='1998_Interval_list.txt'
      NAMEOUT='1998_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.1999) THEN
      SWNAME='1999_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='1999_Interval_list.txt'
      NAMEOUT='1999_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2000) THEN
      SWNAME='2000_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2000_Interval_list.txt'
      NAMEOUT='2000_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2001) THEN
      SWNAME='2001_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2001_Interval_list.txt'
      NAMEOUT='2001_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2002) THEN
      SWNAME='2002_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2002_Interval_list.txt'
      NAMEOUT='2002_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2003) THEN
      SWNAME='2003_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2003_Interval_list.txt'
      NAMEOUT='2003_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2004) THEN
      SWNAME='2004_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2004_Interval_list.txt'
      NAMEOUT='2004_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2005) THEN
      SWNAME='2005_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2005_Interval_list.txt'
      NAMEOUT='2005_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2006) THEN
      SWNAME='2006_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2006_Interval_list.txt'
      NAMEOUT='2006_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2007) THEN
      SWNAME='2007_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2007_Interval_list.txt'
      NAMEOUT='2007_OMNI_5m_with_TS05_variables.dat'
      ENDIF

      IF (IYEAR.EQ.2008) THEN
      SWNAME='2008_IMF_and_SW_gaps_le_2hrs_filled_SYMH_added.txt'
      LISTNAME='2008_Interval_list.txt'
      NAMEOUT='2008_OMNI_5m_with_TS05_variables.dat'
      ENDIF
C
      OPEN (UNIT=1,FILE='Parameters.par')
      READ (1,200) (A(I),I=1,69)
200   FORMAT(G15.6)
      CLOSE(1)

      DT1=A(45)/60.   !  A(45) ... A(50) are in 1/hours, while DT1 ... DT6 are in 1/minutes
      DT2=A(46)/60.
      DT3=A(47)/60.
      DT4=A(48)/60.
      DT5=A(49)/60.
      DT6=A(50)/60.
C
      VXX=-400.  ! THIS IS FOR RECALC_08
      VYY=0.
      VZZ=0.
c
C     READ THE INTERPLANETARY/DST DATA SET:
C
      OPEN (UNIT=1,FILE=SWNAME,STATUS='OLD')     !  filename for the solar wind/IMF/Sym-H data
 505  FORMAT(2I4,2I3,F8.2,2F8.2,3F8.1,F7.2,F9.0,F7.1,3X,I2,3X,I2)

 1    READ (1,505,END=2) IYEA,IDA,IHOU,MI,BX,BY,BZ,VX,VY,VZ,DE,T,DST5M,
     * IMFLAG,ISFLAG
      MINYEAR=(IDA-1)*1440+IHOU*60+MI
      IND=MINYEAR/5+1
      IDAY (IND)=IDA
      IHOUR(IND)=IHOU
      MIN  (IND)=MI
      BXGSM(IND)=BX
      BYGSM(IND)=BY
      BZGSM(IND)=BZ
      VXGSE(IND)=VX
      VYGSE(IND)=VY
      VZGSE(IND)=VZ
      V_SW(IND)=DSQRT(VX**2+VY**2+VZ**2)
C      VTH  (IND)=0.15745*SQRT(T)  !  SQRT(3kT/M_p) in km/s
      TEMP (IND)=T
      DEN  (IND)=DE
      SYMH (IND)=DST5M
      IMFFLAG(IND)=IMFLAG
      ISWFLAG(IND)=ISFLAG
      GOTO 1

 2    CLOSE(1)

      PRINT *, '  READING OF OMNI DATA FINISHED'
C
C  NOW READ THE CORRESPONDING FILE YYYY_Interval_list.txt:
C
      OPEN (UNIT=1,FILE=LISTNAME)
      NREC1=0
 111  READ (1,100,END=112) INDB,INDE
 100  FORMAT((2I10))
      NREC1=NREC1+1
      INDBEG(NREC1)=INDB
      INDEND(NREC1)=INDE
      GOTO 111
 112  CONTINUE
      CLOSE(1)
C
      OPEN (UNIT=3,FILE=NAMEOUT)

      DO 555 N=1,NREC1

       INDB=INDBEG(N)
       INDE=INDEND(N)

C
C  NOW FIND CORRESPONDING VALUES OF THE SOLAR WIND PARAMETERS W1 - W6
C  BY GOING OVER THE ARRAYS, PREVIOUSLY READ IN THE RAM, AND WRITE THEM IN THE OUTPUT FILE:
C
      DO 41 IND=INDB,INDE

         Pdyn=1.937D-6*DEN(IND)*V_SW(IND)**2
         By=BYGSM(IND)
         Bz=BZGSM(IND)
         DstSYM=SYMH(IND)

      CALL RECALC_08 (IYEAR,IDAY(IND),IHOUR(IND),MIN(IND),0,VXX,VYY,VZZ)

C    CALCULATE W1 - W6 INDICES:

                W1=0.
                W2=0.
                W3=0.
                W4=0.
                W5=0.
                W6=0.

                KEY1=1
                KEY2=1
                KEY3=1
                KEY4=1
                KEY5=1
                KEY6=1

            DO 42 KK=IND,INDB,-1

                   Vnorm=   V_SW(KK)/400.
                   Dennorm= DEN(KK)*1.16/5. !  ASSUME HeH=0.04, HENCE 1.16
                   Bsnorm= -BZGSM(KK)/5.

                   IF (Bsnorm.LE.0.) THEN
                      Bs1=0.D0
                      Bs2=0.D0
                      Bs3=0.D0
                      Bs4=0.D0
                      Bs5=0.D0
                      Bs6=0.D0
                   ELSE
                      Bs1=Bsnorm**A(53)
                      Bs2=Bsnorm**A(56)
                      Bs3=Bsnorm**A(59)
                      Bs4=Bsnorm**A(62)
                      Bs5=Bsnorm**A(65)
                      Bs6=Bsnorm**A(68)
                   ENDIF

            FAC1=Dennorm**A(51) *Vnorm**A(52) *Bs1
            FAC2=Dennorm**A(54) *Vnorm**A(55) *Bs2
            FAC3=Dennorm**A(57) *Vnorm**A(58) *Bs3
            FAC4=Dennorm**A(60) *Vnorm**A(61) *Bs4
            FAC5=Dennorm**A(63) *Vnorm**A(64) *Bs5
            FAC6=Dennorm**A(66) *Vnorm**A(67) *Bs6

            TAUMT=(IND-KK)*5.

            ARG1=-TAUMT*DT1
            ARG2=-TAUMT*DT2
            ARG3=-TAUMT*DT3
            ARG4=-TAUMT*DT4
            ARG5=-TAUMT*DT5
            ARG6=-TAUMT*DT6

                IF (ARG1.GT.-10..AND.KEY1.EQ.1) THEN
                  W1=W1+FAC1*EXP(ARG1)
                ELSE
                  KEY1=0
                ENDIF

                IF (ARG2.GT.-10..AND.KEY2.EQ.1) THEN
                  W2=W2+FAC2*EXP(ARG2)
                ELSE
                  KEY2=0
                ENDIF

                IF (ARG3.GT.-10..AND.KEY3.EQ.1) THEN
                  W3=W3+FAC3*EXP(ARG3)
                ELSE
                  KEY3=0
                ENDIF

                IF (ARG4.GT.-10..AND.KEY4.EQ.1) THEN
                  W4=W4+FAC4*EXP(ARG4)
                ELSE
                  KEY4=0
                ENDIF

                IF (ARG5.GT.-10..AND.KEY5.EQ.1) THEN
                  W5=W5+FAC5*EXP(ARG5)
                ELSE
                  KEY5=0
                ENDIF

                IF (ARG6.GT.-10..AND.KEY6.EQ.1) THEN
                  W6=W6+FAC6*EXP(ARG6)
                ELSE
                  KEY6=0
                ENDIF


           IF (KEY1.EQ.0.AND.KEY2.EQ.0.AND.KEY3.EQ.0.AND.KEY4.EQ.0.AND.
     +      KEY5.EQ.0.AND.KEY6.EQ.0) GOTO 43


 42         CONTINUE


 43         W1=W1*DT1*5.
            W2=W2*DT2*5.
            W3=W3*DT3*5.
            W4=W4*DT4*5.
            W5=W5*DT5*5.
            W6=W6*DT6*5.

      WRITE(3,444) IYEAR,IDAY(IND),IHOUR(IND),MIN(IND),BXGSM(IND),
     * BYGSM(IND),BZGSM(IND),VXGSE(IND),VYGSE(IND),VZGSE(IND),DEN(IND),
     * TEMP(IND),SYMH(IND),IMFFLAG(IND),ISWFLAG(IND),
     * TILT,Pdyn,W1,W2,W3,W4,W5,W6
 444  FORMAT (2I4,2I3,3F8.2,3F8.1,F7.2,F9.0,F7.1,2(3X,I2),F8.4,7F7.2)
c
c  Each data record in the output file contains full set of parameters,
c   including those to be  used as input for the TS05 model:
c   Year, Day of Year, Hour of Day, Minute of Hour, BX_IMF, BY_IMF, BZ_IMF,
C   solar wind VX_GSE, VY_GSE, VZ_GSE, proton density DEN, temperature TEMP,
c   SYM-H index, IMF and SW data availability flags, dipole tilt angle (RADIANS),
c   solar wind ram pressure (nPa), and 6 driving variables W1, W2, W3, W4, W5, W6.
c
  41   CONTINUE
 555   CONTINUE

      CLOSE(3)

 777  CONTINUE
C
      END

C**************************************************************************
