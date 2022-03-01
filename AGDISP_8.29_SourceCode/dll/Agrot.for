C**AGROT
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 11/16/16
C
      SUBROUTINE AGROT(HHK,IUNIT,LFL,ICLS,NPTS,DDV,XXV,IER,
     $                 REALWD,CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGROT
!MS$ATTRIBUTES REFERENCE :: HHK
!MS$ATTRIBUTES REFERENCE :: IUNIT
!MS$ATTRIBUTES REFERENCE :: LFL
!MS$ATTRIBUTES REFERENCE :: ICLS
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DDV
!MS$ATTRIBUTES REFERENCE :: XXV
!MS$ATTRIBUTES REFERENCE :: IER
!MS$ATTRIBUTES REFERENCE :: REALWD
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGROT runs the USDA FS rotary atomizer analysis, then reconstructs
C  the drop size distribution by calling the appropriate function
C
C  HHK    - HKDATA data structure
C  IUNIT  - Units flag: 0 = English; 1 = metric
C  LFL    - Operations flag: 0 = initialization of calculation
C  ICLS   - Size class flag: -1 = no; 0-12 = class to use
C  NPTS   - Number of points in drop size distribution
C  DDV    - Drop size distribution array
C  XXV    - Volume fraction array
C  IER    - Error flag: 0 = No errors and return
C                       1 = Write warning information
C                       2 = Write error information and continue
C                       3 = Write error information and return
C  REALWD - Real data array (value, minimum, maximum)
C  CHSTR  - Character string
C  JCHSTR - Length of character string
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /HKDATA/ HHK
C
      CHARACTER*40 CHSTR
C
      DIMENSION REALWD(3),DDV(2),XXV(2)
C
      DATA SMIN,SMAX,DMIN,DMAX,QMIN,QMAX,RMIN,RMAX,TMIN,TMAX,EMIN,EMAX /
     $  22.35,75.99,35.0,75.0,3.785,45.420,
     $  2480.0,11200.0,61.7,72.4,3.0,200.0 /
C
C  Set all of the necessary parameters for USDA FS regressions
C
      IER=0
C
      N=HHK.MATTYPE
      M=HHK.ROTTYPE
C
      S=HHK.SPEED
      IF (LFL.EQ.1) THEN
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=1.0/0.447
        CALL AGCHK(S,SMIN,SMAX,0.9*SMIN,1.1*SMAX,1,1,IER,FAC,REALWD)
        IF (IER.GT.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Air Speed (mph)'
          ELSE
            CHSTR='Air Speed (m/s)'
          ENDIF
          JCHSTR=15
          RETURN
        ENDIF
        LFL=2
      ENDIF
C
      A=HHK.BLADEANG
      IF (LFL.EQ.2) THEN
        IF (M.EQ.1) THEN
          FAC=1.0
          CALL AGCHK(A,DMIN,DMAX,0.9*DMIN,1.1*DMAX,1,1,IER,FAC,REALWD)
          IF (IER.GT.0) THEN
            CHSTR='Blade Angle (deg)'
            JCHSTR=17
            RETURN
          ENDIF
        ENDIF
        LFL=3
      ENDIF
C
      R=HHK.RPM
      IF (LFL.EQ.3) THEN
        FAC=1.0
C        IF (R.EQ.0.0) THEN
C          RR=5695.8991+178.3315*S-122.9546*A-1.00641*S*A
C          CALL AGCHK(RR,RMIN,RMAX,0.9*RMIN,1.1*RMAX,1,1,IER,FAC,REALWD)
C          IF (IER.GT.0) THEN
C            CHSTR='Computed Rotation Rate (rpm)'
C            JCHSTR=28
C            RETURN
C          ENDIF
C        ELSE
          CALL AGCHK(R,RMIN,RMAX,0.9*RMIN,1.1*RMAX,1,1,IER,FAC,REALWD)
          IF (IER.GT.0) THEN
            CHSTR='Rotation Rate (rpm)'
            JCHSTR=19
            RETURN
          ENDIF
C        ENDIF
        LFL=4
      ENDIF
C      RR=5695.8991+178.3315*S-122.9546*A-1.00641*S*A
C      IF (R.EQ.0.0) R=RR
C
      Q=HHK.FLOWRATE
      IF (LFL.EQ.4) THEN
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=0.2642
        CALL AGCHK(Q,QMIN,QMAX,0.9*QMIN,1.1*QMAX,1,1,IER,FAC,REALWD)
        IF (IER.GT.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Flow Rate (gpm)'
          ELSE
            CHSTR='Flow Rate (lpm)'
          ENDIF
          JCHSTR=15
          RETURN
        ENDIF
        LFL=5
      ENDIF
C
C  Compute the USDA FS parameters and construct the distribution
C
      IF (LFL.EQ.5) THEN
        IF (N.EQ.0) THEN
          DST=72.4
          EVS=3.0
        ELSEIF (N.EQ.1) THEN
          DST=65.0
          EVS=200.0
        ELSEIF (N.EQ.2) THEN
          DST=61.7
          EVS=3.0
        ELSE
          DST=71.5
          EVS=9.6
        ENDIF
C
        SPEED=(2.0*S-SMAX-SMIN)/(SMAX-SMIN)
        DEG=(2.0*A-DMAX-DMIN)/(DMAX-DMIN)
        GPM=(2.0*Q-QMAX-QMIN)/(QMAX-QMIN)
        RPM=(2.0*R-RMAX-RMIN)/(RMAX-RMIN)
        DST=(2.0*DST-TMAX-TMIN)/(TMAX-TMIN)
        EVS=(2.0*EVS-EMAX-EMIN)/(EMAX-EMIN)
C
        IF (M.EQ.0) THEN
          D25=45.081927+5.5995755*GPM-54.89117*RPM+34.577333*SPEED*RPM
     $        +24.478165*RPM*RPM
          D50=67.73417+7.1991397*GPM-66.66639*RPM+48.938422*SPEED*RPM
     $        +33.671598*RPM*RPM
          D75=89.63745+11.305544*GPM-89.62636*RPM+68.311524*SPEED*RPM
     $        +45.299831*RPM*RPM
        ELSE
          D25=14.110018-84.30651*DEG-139.3413*RPM+7.5939378*DST
     $        +27.572685*SPEED*SPEED-52.6154*SPEED*DEG
     $        -26.06713*SPEED*RPM-7.651296*SPEED*DST
     $        +15.187793*SPEED*EVS-21.35982*DEG*DEG-12.05377*DEG*GPM
     $        -43.08659*DEG*RPM+6.3503666*DEG*DST-28.16186*DEG*EVS
     $        -11.08876*GPM*RPM+6.7492162*GPM*EVS+41.098194*RPM*RPM
     $        -45.04848*RPM*EVS
          D50=145.49303-90.5219*SPEED+55.896759*DEG+23.397212*GPM
     $        -48.97168*RPM+43.23432*EVS+38.530391*SPEED*SPEED
     $        -63.38954*SPEED*DEG-24.7293*SPEED*EVS+38.377697*DEG*RPM
     $        -14.13247*GPM*DST+14.95168*GPM*EVS+71.742191*RPM*RPM
     $        -27.89545*RPM*EVS
          D75=220.3937-123.3389*SPEED+75.293664*DEG+33.892283*GPM
     $        -74.22471*RPM+75.547661*EVS+32.224854*SPEED*SPEED
     $        -66.53212*SPEED*DEG-46.69371*SPEED*EVS+74.119644*DEG*RPM
     $        -18.14506*GPM*DST+25.8779*GPM*EVS+121.8762*RPM*RPM
     $        -68.01278*RPM*EVS
        ENDIF
C
C  Compute with DV0.25, DV0.5, and DV0.75
C
        Y1=SQRT(D25/D50)
        X1=FX(0.25)
        Y2=SQRT(D75/D50)
        X2=FX(0.75)
        NP=3
        SUMX=X1+X2
        SUMXY=X1*Y1+X2*Y2
        SUMXX=X1*X1+X2*X2
        SUMY=1.0+Y1+Y2
        SL=(SUMXY-SUMX*SUMY/NP)/(SUMXX-SUMX*SUMX/NP)
        RSNEW=5.126915*SL
C
        LTEM=HHK.SPRTYPE
        CALL AGPARX(LTEM,ICLS,D50,RSNEW,NPTS,DDV,XXV)
      ENDIF
      LFL=6
      RETURN
      END