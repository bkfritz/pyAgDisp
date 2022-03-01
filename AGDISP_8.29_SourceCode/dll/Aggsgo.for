C**AGGSGO
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGGSGO(NNVEC,YYVEC,DDVEC)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGGSGO
!MS$ATTRIBUTES REFERENCE :: NNVEC
!MS$ATTRIBUTES REFERENCE :: YYVEC
!MS$ATTRIBUTES REFERENCE :: DDVEC
C
C  AGGSGO completes the Gaussian toolbox calculation
C
C  NNVEC  - Number of points
C  YYVEC  - Distance array (m)
C  DDVEC  - Deposition (fraction applied)
C
      DIMENSION YYVEC(2),DDVEC(2)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA TPI / 6.283185307 /
C
C  Layered average wind speed
C
      HMAX=0.0
      DO NN=1,NNDRPT
        DO NV=1,NVAR
          HMAX=AMAX1(HMAX,XGV(2,NV,NN))
        ENDDO
      ENDDO
      TEM=WINDSP/(ALOG((WINDHT+ZO)/ZO)-PSTAB)
      UBAR=TEM*((1.0+ZO/HMAX)*ALOG((HMAX+ZO)/ZO)-1.0-PSTAB)
C
C  Terminal velocity
C
      DO NN=1,NNDRPT
        DO NV=1,NVAR
          WTN=1.0
          DTEM=XGV(4,NV,NN)
          DTAU=3.12E-06*DTEM*DTEM*DENN
10        WTO=WTN
          REYNO=0.0688*DTEM*WTO
          WTN=9.8*DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
          IF (ABS(WTN/WTO-1.0).GT.0.0001) GOTO 10
          XGV(4,NV,NN)=WTN
        ENDDO
      ENDDO
C
C  Need deposition grid plus zeroed values
C
      DX=2.0
      F=1.005
      XXMAX=XYMAX+(NSWTH-1)*SWATH
      NGPTS=0
20    NGPTS=NGPTS+1
      GXV(NGPTS)=XYMIN+(NGPTS-1)*DX
      GDV(NGPTS)=0.0
      DX=DX*F
      IF (GXV(NGPTS).LT.XYMAX) NGMAX=NGPTS+1
      IF (GXV(NGPTS).LT.XXMAX) GOTO 20
      RATE=1.0
      WTEM=270.0-WINDDR
      IF (WTEM.GT.90.0) WTEM=WTEM-360.0
      RATE=1.0/COS(0.017453293*WTEM)
C
C  Move through all drop sizes and all nozzles, for one flight line
C
      DO NN=1,NNDRPT
        DO NV=1,NVAR
          YP=XGV(1,NV,NN)
          H=XGV(2,NV,NN)
          SO=XGV(3,NV,NN)
          V=XGV(4,NV,NN)
          CALL AGISY(LSTAB,SO,XY,CTEM,DTEM,HM)
C          IF (HMIX.NE.0.0) HM=HMIX
          HM=HMIX
          XP=0.0
          DO NG=1,NGPTS
            XG=0.0
            YG=GXV(NG)
            BX=YG-YP
            BY=XG-XP
            BR=SQRT(ABS(BX*BX+BY*BY))
            PHI=ATAN2(BY,BX)-0.017453293*WTEM
            X=BR*COS(PHI)
            Y=BR*SIN(PHI)
            IF (H.EQ.0.0) THEN
              EXPX=EXP(-AMIN1(0.5*(X/SO)**2,25.0))
              SY=SO
              VDEP=2.15*DMASSN(NN)*EXPX/SO
            ELSE
              IF (X+XY.LE.0.0) GOTO 30
              TH=0.017453293*(CTEM-DTEM*ALOG(X+XY))
              SY=0.46511628*(X+XY)*TAN(TH)
              CALL AGISZ(LSTAB,SO,X,XZ,SZ,BTEM)
              IF (SZ.LE.0.0) GOTO 30
              CALL AGSMS(BTEM,H,X,XZ,V,UBAR,SZ,HM,XMSUM)
              VDEP=2.15*SO*DMASSN(NN)*XMSUM/SY/SZ
            ENDIF
            Y=2.15*SO
            EXPT=EXP(-AMIN1(0.5*(Y/SY)**2,25.0))
            EXPY=1.0+2.0*EXPT
            GDV(NG)=GDV(NG)+VDEP*EXPY
30        ENDDO
        ENDDO
      ENDDO
      DO NPTS=1,NGPTS
        GDV(NPTS)=GDV(NPTS)*RATE*SWATH/TPI/NVAR
      ENDDO
C
C  Add effects of all other swaths
C
      DO NPTS=1,NGMAX
        GTV(NPTS)=GDV(NPTS)
      ENDDO
      DO NS=2,NSWTH
        DO NPTS=1,NGMAX
          Y=GXV(NPTS)+(NS-1)*SWATH
          GTV(NPTS)=GTV(NPTS)+AGINT(NGPTS,GXV,GDV,Y)
        ENDDO
      ENDDO
C
C  Save the deposition result
C
      DDFACT=DDHAND/GTV(1)
      NNVEC=NGMAX
      DO NPTS=1,NGMAX
        YYVEC(NPTS)=GXV(NPTS)
        DDVEC(NPTS)=GTV(NPTS)*DDFACT
      ENDDO
      RETURN
      END