C**AGKLN
C  Continuum Dynamics, Inc.
C  Version 1.08 03/01/00
C
      SUBROUTINE AGKLN(NPTS,DKV,CKV,VMD,DNV,XNV,PSAVE,IER,LFL)
C
C  AGKLN reconstructs the log normal drop size distribution
C
C  NPTS   - Number of user-defined drop sizes
C  DKV    - User-defined drop size distribution
C  CKV    - User-defined cumulative volume fraction
C  VMD    - Volume median diameter
C  DNV    - Drop size distribution
C  XNV    - Volume fraction array
C  PSAVE  - Total cumulative volume fraction
C  IER    - Error flag
C  LFL    - Type flag: 0 = LN; 1 = ULLN; 2 = BLN
C
      DIMENSION DKV(2),CKV(2),XKV(32),DNV(2),XNV(2)
C
      DO N=1,NPTS
        CKV(N)=AMIN1(1.0,AMAX1(0.0,CKV(N)))
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
C  Find inverse of error function
C
      DO N=NS,NE
        F=2.0*CKV(N)-1.0
        XMIN=-20.0
        XMAX=20.0
        IX=0
30      X=(XMIN+XMAX)/2.0
        IX=IX+1
        E=ERF(X)
        IF (IX.LT.20.AND.ABS(E-F).GT.0.001) THEN
          IF (E.GT.F) THEN
            XMAX=X
          ELSE
            XMIN=X
          ENDIF
          GO TO 30
        ENDIF
        XKV(N)=X
      ENDDO
C
C  Construct log normal solution
C
      XFK=0.0
      XFF=0.0
      DO N=NS,NE
        F=ALOG(DKV(N)/VMD)
        XFK=XFK+F*XKV(N)
        XFF=XFF+F*F
      ENDDO
      DELTS=XFK/XFF
      LFL=0
C
C  Test for upper limit log normal solution
C
      IF (NE-NS+1.GT.2) THEN
        DELTO=DELTS
        DMIN=DNV(32)
        DMAX=10000.0
        ID=0
40      ID=ID+1
        DNEW=0.5*(DMIN+DMAX)
        AA=DNEW/VMD-1.0
        XFK=0.0
        XFF=0.0
        DO N=NS,NE
          F=ALOG(AA*DKV(N)/(DNEW-DKV(N)))
          XFK=XFK+F*XKV(N)
          XFF=XFF+F*F
        ENDDO
        DELTA=XFK/XFF
        EMIN=0.0
        EMAX=0.0
        AAMIN=(DNEW-0.5)/VMD-1.0
        AAMAX=(DNEW+0.5)/VMD-1.0
        DO N=NS,NE
          QMIN=0.5*(1.0+ERF(DELTA*ALOG(AAMIN*DKV(N)/(DNEW-0.5-DKV(N)))))
          EMIN=EMIN+(QMIN-CKV(N))**2
          QMAX=0.5*(1.0+ERF(DELTA*ALOG(AAMAX*DKV(N)/(DNEW+0.5-DKV(N)))))
          EMAX=EMAX+(QMAX-CKV(N))**2
        ENDDO
        IF (ID.LT.20.AND.ABS(DELTA/DELTO-1.0).GT.0.0001) THEN
          DELTO=DELTA
          IF (EMAX.GT.EMIN) THEN
            DMAX=DNEW
          ELSE
            DMIN=DNEW
          ENDIF
          GO TO 40
        ENDIF
        IF (DNEW.GT.DNV(32)+1.0.AND.DNEW.LT.5000.0) THEN
          DDMAX=DNEW
          LFL=1
C
C  Test for bounded log normal solution
C
          IF (NE-NS+1.GT.3) THEN
            DELTO=DELTA
            DMIN=0.0
            DMAX=DNV(1)
            ID=0
50          ID=ID+1
            DNEW=0.5*(DMIN+DMAX)
            AA=(DDMAX-VMD)/(VMD-DNEW)
            XFK=0.0
            XFF=0.0
            DO N=NS,NE
              F=ALOG(AA*(DKV(N)-DNEW)/(DDMAX-DKV(N)))
              XFK=XFK+F*XKV(N)
              XFF=XFF+F*F
            ENDDO
            DELTB=XFK/XFF
            EMIN=0.0
            EMAX=0.0
            AAMIN=(DDMAX-VMD)/(VMD-0.5*DNEW)
            AAMAX=(DDMAX-VMD)/(VMD-1.5*DNEW)
            DO N=NS,NE
              QMIN=0.5*(1.0+ERF(DELTB*ALOG(AAMIN*(DKV(N)-0.5*DNEW)/
     $             (DDMAX-DKV(N)))))
              EMIN=EMIN+(QMIN-CKV(N))**2
              QMAX=0.5*(1.0+ERF(DELTB*ALOG(AAMAX*(DKV(N)-1.5*DNEW)/
     $             (DDMAX-DKV(N)))))
              EMAX=EMAX+(QMAX-CKV(N))**2
            ENDDO
            IF (ID.LT.20.AND.ABS(DELTB/DELTO-1.0).GT.0.0001) THEN
              DELTO=DELTB
              IF (EMAX.GT.EMIN) THEN
                DMIN=DNEW
              ELSE
                DMAX=DNEW
              ENDIF
              GO TO 50
            ENDIF
            IF (DNEW.GT.0.0001.AND.DNEW.LT.DNV(1)-1.0) THEN
              DDMIN=DNEW
              LFL=2
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C  Construct profile with given drop sizes
C
      PSAVE=0.0
      DO N=1,32
        IF (LFL.EQ.0) THEN
          V=0.5*(1.0+ERF(DELTS*ALOG(DNV(N)/VMD)))
        ELSEIF (LFL.EQ.1) THEN
          AA=DDMAX/VMD-1.0
          V=0.5*(1.0+ERF(DELTA*ALOG(AA*DNV(N)/(DDMAX-DNV(N)))))
        ELSE
          AA=(DDMAX-VMD)/(VMD-DDMIN)
          V=0.5*(1.0+ERF(DELTB*ALOG(AA*(DNV(N)-DDMIN)/(DDMAX-DNV(N)))))
        ENDIF
        XNV(N)=V-PSAVE
        PSAVE=V
      ENDDO
      IF (PSAVE.LT.1.0) THEN
        DO N=1,32
          XNV(N)=XNV(N)/PSAVE
        ENDDO
      ENDIF
      RETURN
      END