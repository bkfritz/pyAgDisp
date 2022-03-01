C**AGDSRN
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.11 08/04/04
C
      SUBROUTINE AGDSRN(LFLAG,NPTS,DKV,XKV,VMD,XRS,XND10,XND90,XF141,DP)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGDSRN
!MS$ATTRIBUTES REFERENCE :: LFLAG
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DKV
!MS$ATTRIBUTES REFERENCE :: XKV
!MS$ATTRIBUTES REFERENCE :: VMD
!MS$ATTRIBUTES REFERENCE :: XRS
!MS$ATTRIBUTES REFERENCE :: XND10
!MS$ATTRIBUTES REFERENCE :: XND90
!MS$ATTRIBUTES REFERENCE :: XF141
!MS$ATTRIBUTES REFERENCE :: DP
C
C  AGDSRN recovers the VMD and relative span for the
C  user-entered drop size distribution or determines the
C  distribution from the VMD and relative span (by root-normal)
C
C  LFLAG  - Input type (0=distribution known; 1,2=VMD and RS known
C  NPTS   - Number of drop sizes
C  DKV    - Drop size distribution
C  XKV    - Volume fractions
C  VMD    - Volume median diameter (micrometers)
C  XRS    - Relative span
C  XND10  - Drop diameter at 10 percent of volume (micrometers)
C  XND90  - Drop diameter at 90 percent of volume (micrometers)
C  XF141  - Percentage volume contained in up to 141 micrometers
C  DP     - Drift potential
C
      DIMENSION DKV(2),XKV(2)
C
      CALL AGDSRX(LFLAG,NPTS,DKV,XKV,VMD,XRS,XND10,XND90,XF141,DP)
      RETURN
      END
C**AGDSRX
      SUBROUTINE AGDSRX(LFLAG,NPTS,DKV,XKV,VMD,XRS,XND10,XND90,XF141,DP)
C
      DIMENSION DKV(2),XKV(2),TKV(100),CKV(100),DDV(32,3)
C
      DATA DDV /
     $    10.77,   16.73,   19.39,   22.49,   26.05,   30.21,   35.01,
     $    40.57,   47.03,   54.50,   63.16,   73.23,   84.85,   98.12,
     $   113.71,  131.73,  152.79,  177.84,  205.84,  238.45,  276.48,
     $   320.60,  372.18,  430.74,  498.91,  578.54,  670.72,  777.39,
     $   900.61, 1044.42, 1210.66, 1403.04 ,
     $    13.92,   20.84,   24.20,   28.15,   32.55,   37.72,   43.73,
     $    50.64,   58.76,   68.12,   78.99,   91.62,  106.30,  123.22,
     $   142.76,  165.29,  191.34,  221.91,  256.94,  298.07,  345.60,
     $   400.75,  464.83,  538.47,  623.61,  722.82,  838.00,  971.78,
     $  1126.51, 1305.88, 1513.71, 1754.16,
     $    13.61,   20.07,   24.06,   28.05,   33.09,   40.13,   48.11,
     $    57.15,   68.18,   80.15,   93.18,  110.30,  135.55,  165.45,
     $   195.38,  230.58,  275.76,  330.91,  390.77,  461.16,  551.51,
     $   661.81,  792.06,  942.26, 1122.97, 1343.57, 1604.07, 1904.48,
     $  2265.89, 2707.09, 3228.10, 3828.91 /
C
C  Construct profile from given drop information
C
      IF (LFLAG.EQ.1) THEN
        NPTS=32
        CC=XRS/5.126915
        PSAVE=0.0
        I=1
        XP=(SQRT(DDV(NPTS,1)/VMD)-1.0)/CC
        PP=FP(XP)
        IF (PP.LT.0.999) THEN
          I=2
          XP=(SQRT(DDV(NPTS,2)/VMD)-1.0)/CC
          PP=FP(XP)
          IF (PP.LT.0.999) I=3
        ENDIF
        DO N=1,NPTS
          DKV(N)=DDV(N,I)
          XP=(SQRT(DKV(N)/VMD)-1.0)/CC
          PROB=FP(XP)
          XKV(N)=PROB-PSAVE
          PSAVE=PROB
        ENDDO
        IF (PSAVE.LT.1.0) THEN
          DO N=1,NPTS
            XKV(N)=XKV(N)/PSAVE
          ENDDO
        ENDIF
C
C  Optimize profile from given drop information
C
      ELSEIF (LFLAG.EQ.2) THEN
        CALL AGDSFN(VMD,XRS,0.001,DMIN,FMIN)
        CALL AGDSFN(VMD,XRS,0.999,DMAX,FMAX)
        NPTS=32
        DTEM=DMAX-(NPTS-1)*DMIN
        IF (DTEM.LT.0.0) NPTS=IFIX(DMAX/DMIN)+1
        DBEG=1.0
        DEND=2.0
        K=0
10      DTEM=0.5*(DBEG+DEND)
        K=K+1
        SUM=(DTEM**(NPTS-1)-1.0)/(DTEM-1.0)
        IF (K.LT.20) THEN
          IF (SUM.LT.DMAX/DMIN) THEN
            DBEG=DTEM
          ELSE
            DEND=DTEM
          ENDIF
          GOTO 10
        ENDIF
        DSUM=1.0
        PSAVE=0.0
        DO NN=1,NPTS
          DV=DMIN*DSUM
          CC=XRS/5.126915
          XP=(SQRT(DV/VMD)-1.0)/CC
          PP=FP(XP)
          IF (NN.EQ.NPTS) PP=1.0
          DKV(NN)=DV
          XKV(NN)=PP-PSAVE
          PSAVE=PP
          DSUM=DSUM+DTEM**NN
        ENDDO
      ENDIF
C
      IF (NPTS.GT.1) THEN
        IPTS=NPTS
        CSUM=XKV(1)
        DO N=2,NPTS
          CSUM=CSUM+XKV(N)
          IF (CSUM.GT.0.999) IPTS=MIN0(IPTS,N)
        ENDDO
        NPTS=IPTS
      ENDIF
C
C  Compute cumulative volume fractions
C
      IF (NPTS.GT.1) THEN
        TKV(1)=DKV(1)**3
        CKV(1)=AMIN1(1.0,AMAX1(0.0,XKV(1)))
        DO N=2,NPTS
          TKV(N)=DKV(N)**3
          CKV(N)=AMIN1(1.0,AMAX1(0.0,CKV(N-1)+XKV(N)))
        ENDDO
C
C  Compute drop size distribution numerics
C
        XND10=AGINT(NPTS,CKV,TKV,0.1)**0.33333
        IF (LFLAG.EQ.0) VMD=AGINT(NPTS,CKV,TKV,0.5)**0.33333
        XND90=AGINT(NPTS,CKV,TKV,0.9)**0.33333
        IF (LFLAG.EQ.0) XRS=(XND90-XND10)/VMD
        XF141=100.0*AGINT(NPTS,TKV,CKV,141.0**3)
C
        DP=0.0977052-0.000446*XND10+0.0001007*VMD-0.000065*XND90
     $    +0.004092001*XF141+0.0000002*XND10*XND10
     $    -0.0000003477*XND10*VMD+0.0000003*XND10*XND90
     $    -0.0000339*XND10*XF141-0.00000313*VMD*XF141
     $    +0.000001116*XND90*XF141+0.00000399416*XF141*XF141
        DP=AMAX1(DP,0.0)
C
      ENDIF
      RETURN
      END
C**AGDSFN
      SUBROUTINE AGDSFN(VMD,XRS,CUM,DIAM,FRAC)
C
      CC=XRS/5.126915
      DMIN=0.0
      DMAX=50.0
10    DMAX=2.0*DMAX
      XP=(SQRT(DMAX/VMD)-1.0)/CC
      PP=FP(XP)
      IF (PP.LT.CUM) GOTO 10
      K=0
20    DIAM=0.5*(DMIN+DMAX)
      K=K+1
      XP=(SQRT(DIAM/VMD)-1.0)/CC
      PP=FP(XP)
      IF (K.LT.20) THEN
        IF (PP.LT.CUM) THEN
          DMIN=DIAM
        ELSE
          DMAX=DIAM
        ENDIF
        GOTO 20
      ENDIF
      IF (DIAM.LT.0.01*VMD) THEN
        DIAM=0.01*VMD
        XP=(SQRT(DIAM/VMD)-1.0)/CC
        PP=FP(XP)
      ENDIF
      IF (DIAM.LT.0.15874) THEN
        DIAM=0.15874
        XP=(SQRT(DIAM/VMD)-1.0)/CC
        PP=FP(XP)
      ENDIF
      FRAC=PP
      RETURN
      END