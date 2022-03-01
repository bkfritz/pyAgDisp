C**AGKRN
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGKRN(NPTS,DKV,CKV,VMD,DNV,XNV,PSAVE,IER)
C
C  AGKRN reconstructs the root/normal drop size distribution
C
C  NPTS   - Number of user-defined drop sizes
C  DKV    - User-defined drop size distribution
C  CKV    - User-defined cumulative volume fraction
C  VMD    - Volume median diameter
C  DNV    - Drop size distribution
C  XNV    - Volume fraction array
C  PSAVE  - Total cumulative volume fraction
C  IER    - Error flag
C
      DIMENSION DKV(2),CKV(2),XKV(32),DNV(2),XNV(2)
C
      DO N=1,NPTS
        CKV(N)=AMIN1(1.0,AMAX1(0.0,CKV(N)))
      ENDDO
      XKV(1)=CKV(1)
      DO N=2,NPTS
        XKV(N)=CKV(N)-CKV(N-1)
      ENDDO
      NS=0
10    NS=NS+1
      IF (CKV(NS).LT.0.00001) GO TO 10
      NE=NPTS+1
20    NE=NE-1
      IF (CKV(NE).GT.0.99999) GO TO 20
      IF (NE-NS+1.LT.2) THEN
        IER=4
        RETURN
      ENDIF
C
C  Compute least squares line through data
C
      SUMN=0.0
      SUMX=0.0
      SUMY=0.0
      SUMXX=0.0
      SUMXY=0.0
      DO N=NS,NE
        Y=SQRT(DKV(N)/VMD)
        X=FX(CKV(N))
        SUMN=SUMN+1.0
        SUMX=SUMX+X
        SUMY=SUMY+Y
        SUMXX=SUMXX+X*X
        SUMXY=SUMXY+X*Y
      ENDDO
      BB=(SUMXY-SUMX*SUMY/SUMN)/(SUMXX-SUMX*SUMX/SUMN)
      AA=(SUMY-BB*SUMX)/SUMN
C
C  With AA and BB compute area error
C
      DO N=NS,NE
        XP=(SQRT(DKV(N)/VMD)-AA)/BB
        PROB=FP(XP)
        IF (N.EQ.NS) THEN
          XPROB=PROB
          AREA=0.0
          EMIN=0.0
        ELSE
          XPROB=PROB-PSAVE
          AREA=AREA+(DKV(N)-DKV(N-1))*(XKV(N)+XKV(N-1))
          EMIN=EMIN+(DKV(N)-DKV(N-1))*ABS(XKV(N)+XKV(N-1)-XPROB-XSAVE)
        ENDIF
        PSAVE=PROB
        XSAVE=XPROB
      ENDDO
C
C  Loop for fixed VMD
C
      ICC=10000.0*BB
      ICSAVE=ICC
      ICMN=ICC-1000
      IF (ICMN.LE.0) THEN
        N=(-ICMN+9)/10+1
        ICMN=ICMN+10*N
      ENDIF
      ICMX=ICC+1000
      DO IC=ICMN,ICMX,10
        CC=0.0001*IC
        DO N=NS,NE
          XP=(SQRT(DKV(N)/VMD)-1.0)/CC
          PROB=FP(XP)
          IF (N.EQ.NS) THEN
            XPROB=PROB
            ERR=0.0
          ELSE
            XPROB=PROB-PSAVE
            ERR=ERR+(DKV(N)-DKV(N-1))*ABS(XKV(N)+XKV(N-1)-XPROB-XSAVE)
          ENDIF
          PSAVE=PROB
          XSAVE=XPROB
        ENDDO
        IF (ERR.LT.EMIN) THEN
          EMIN=ERR
          ICSAVE=IC
        ENDIF
      ENDDO
      CC=0.0001*ICSAVE
C
C  Construct profile with given drop sizes
C
      PSAVE=0.0
      DO N=1,32
        XP=(SQRT(DNV(N)/VMD)-1.0)/CC
        PROB=FP(XP)
        XNV(N)=PROB-PSAVE
        PSAVE=PROB
      ENDDO
      IF (PSAVE.LT.1.0) THEN
        DO N=1,32
          XNV(N)=XNV(N)/PSAVE
        ENDDO
      ENDIF
      RETURN
      END
C**FX
      FUNCTION FX(P)
C
C  Probability argument (Abramowitz and Stegun 26.2.23)
C
      DATA C0,C1,C2 / 2.515517, 0.802853, 0.010328 /
      DATA D1,D2,D3 / 1.432788, 0.189269, 0.001308 /
C
      X=P
      SIGN=-1.0
      IF (X.GT.0.5) THEN
        X=1.0-X
        SIGN=1.0
      ENDIF
      T=SQRT(-ALOG(X*X))
      FX=SIGN*(T-(C0+T*(C1+T*C2))/(1.0+T*(D1+T*(D2+T*D3))))
      RETURN
      END
C**FP
      FUNCTION FP(X)
C
C  Probability function (Abramowitz and Stegun 26.2.18)
C
      DATA C1,C2,C3,C4 / 0.196854, 0.115194, 0.000344, 0.019527 /
C
      P=ABS(X)
      FP=1.0-0.5/(1.0+P*(C1+P*(C2+P*(C3+P*C4))))**4
      IF (X.LT.0.0) FP=1.0-FP
      RETURN
      END