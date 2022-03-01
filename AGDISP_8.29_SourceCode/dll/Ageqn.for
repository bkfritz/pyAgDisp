C**AGEQN
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 11/16/16
C
      SUBROUTINE AGEQN(XOV)
C
C  AGEQN integrates the equations of motion
C
C  XOV    - Array of locations, velocities, etc.
C
      DIMENSION XOV(9,60),XNV(9,60),DV(6,60)
      DIMENSION YNR(2),ZNR(2),YNL(2),ZNL(2),YNP(4),ZNP(4)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /DVFV/ IYDV(60,51),IYFV(60,51)  !,IYHV(60,51)
C
C  Save initial positions
C
      ISWC=NVAR
      SFAC=SDISP+0.5*(1+IBOOM)
C      IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
      NSWTM=NSWTH
      IF (IBOOM.EQ.1) NSWTM=NSWTH+1
      DO I=1,NVAR
        DO J=1,NSWTM
          IYDV(I,J)=0
          IYFV(I,J)=0
C          IYHV(I,J)=1
        ENDDO
      ENDDO
      NSTEP=0
      LCPEND=0
      LSPEND=0
      IVTT=NVAR
      CALL AGSAV(XOV,0.0)
C
C  Initialize integration
C
      DT=0.0
      T=0.0
      DMIN=DIAM
      ISTT=0
      IF (NPRP.NE.0) THEN
        DO N=1,NPRP
          CPXI(N)=CPXI(N)+XO-XPRP(N)
        ENDDO
      ENDIF
      IF (LMVEL.EQ.2.AND.XO.GT.0.0) THEN
        FN=EXP(-AMIN1(CHF*XO,25.0))
        G2PI(1)=CHG*(1.0-FN)
        ZBAR(1)=HHEL
        ZBAL(1)=HHEL
        WHEL=CHW*SQRT(FN)
      ENDIF
C
C  Integrate to TMAX
C
10    NSTEP=NSTEP+1
      UTERM=9.58*(1.0-EXP(-(DMIN/1770.0)**1.147))
      DT=0.0002*DMIN/UTERM
      CALL AGBKG(XOV,DV,T)
C
C  Solve the equations of motion for the DT time step
C    X  Y  Z  U  V  W  XX  XU  UU
C
      DO I=1,NVAR
        IF (ISW(I).NE.0) THEN
          EXPT=0.0
          IF (DV(1,I).GT.0.0) EXPT=EXP(-AMIN1(DT/DV(1,I),25.0))
          TEM1=DV(2,I)+9.8*STU*DV(1,I)
          TEM2=XOV(4,I)-TEM1
          XNV(1,I)=XOV(1,I)+TEM1*DT+TEM2*DV(1,I)*(1.0-EXPT)
          XNV(4,I)=TEM1+TEM2*EXPT
          TEM1=DV(3,I)-9.8*CTU*STS*DV(1,I)
          TEM2=XOV(5,I)-TEM1
          XNV(2,I)=XOV(2,I)+TEM1*DT+TEM2*DV(1,I)*(1.0-EXPT)
          XNV(5,I)=TEM1+TEM2*EXPT
          TEM1=DV(4,I)-9.8*CTU*CTS*DV(1,I)
          TEM2=XOV(6,I)-TEM1
          XNV(3,I)=XOV(3,I)+TEM1*DT+TEM2*DV(1,I)*(1.0-EXPT)
          XNV(6,I)=TEM1+TEM2*EXPT
          TEM1=DV(5,I)+DV(1,I)*DV(6,I)
          TEM2=XOV(8,I)-DV(5,I)+DV(1,I)*(XOV(9,I)-2.0*DV(6,I))
          TEM3=XOV(9,I)-DV(6,I)
          XNV(9,I)=DV(6,I)+TEM3*EXPT*EXPT
          XNV(8,I)=TEM1+TEM2*EXPT-TEM3*DV(1,I)*EXPT*EXPT
          XNV(7,I)=XOV(7,I)+2.0*TEM1*DT+2.0*TEM2*DV(1,I)*(1.0-EXPT)
     $             -TEM3*DV(1,I)*DV(1,I)*(1.0-EXPT*EXPT)
          XNV(7,I)=AMAX1(0.0,XNV(7,I))
          XNV(9,I)=AMAX1(0.0,XNV(9,I))
C
          IF (XNV(3,I).LE.ZREF) THEN
            RATE=(XOV(3,I)-ZREF)/(XOV(3,I)-XNV(3,I))
            DO J=1,9
              XNV(J,I)=XOV(J,I)+RATE*(XNV(J,I)-XOV(J,I))
            ENDDO
            XNV(3,I)=ZREF
            ISW(I)=-1
          ENDIF
C
C  Canopy deposition and total accountancy in height
C
          CNEW=CMASS(I)
          IF (LCANF.GT.0) THEN
            CALL AGCAN(XOV(1,I),XNV(1,I),EDOV(I),EDNV(I),CNEW)
          ENDIF
          ETEM=YMASS*CMASS(I)*((EDOV(I)/DIAM)**3-(EDNV(I)/DIAM)**3)
          CTEM=YMASS*(CMASS(I)-CNEW)*(EDNV(I)/DIAM)**3
          ATEM=YMASS*(CMASS(I)-CNEW)
          DH=XNV(3,I)-XOV(3,I)
          NH=MAX0(IFIX(ABS(DH)/0.1)+1,2)
          DH=DH/(NH-1)
          IF (CTEM.GT.0.0) THEN
            NC=0
            DO N=2,NH
              HTEM=XOV(3,I)-ZREF+(N-1)*DH
              IF (HTEM.GT.0.0.AND.HTEM.LT.HCAN-ZREF) NC=NC+1
            ENDDO
            IF (NC.GT.0) THEN
              TEMNC=1.0/NC
            ELSE
              TEMNC=0.0
            ENDIF
          ENDIF
          DO N=2,NH
            HTEM=XOV(3,I)-ZREF+(N-1)*DH
            IH=MAX0(MIN0(IFIX(HTEM/DAHH)+1,NAHH),1)
            TAHFV(1,IH)=TAHFV(1,IH)+ETEM/(NH-1)
            IF (CTEM.GT.0.0) THEN
              IF (HTEM.GT.0.0.AND.HTEM.LT.HCAN-ZREF) THEN
                TAHFV(2,IH)=TAHFV(2,IH)+TEMNC*CTEM
C                TAHFV(3,IH)=TAHFV(3,IH)+TEMNC*ATEM
              ENDIF
            ENDIF
            IF (ISW(I).LT.0) TAHFV(3,IH)=TAHFV(3,IH)
     $                                  +YMASS*CNEW*(EDNV(I)/DIAM)**3
          ENDDO
C
C  Total accountancy in distance
C
          ND=1
20        IF (XNV(2,I).GT.TADDV(ND)) THEN
            IF (ND.LT.NADD) THEN
              ND=ND+1
              GOTO 20
            ENDIF
          ENDIF
          TADFV(1,ND)=TADFV(1,ND)+ETEM
          TADFV(2,ND)=TADFV(2,ND)+CTEM
          IF (ISW(I).LT.0) TADFV(3,ND)=TADFV(3,ND)
     $                                +YMASS*CNEW*(EDNV(I)/DIAM)**3
C
C  Total accountancy in time
C
          NT=1
30        IF (T.GT.TATTV(NT)) THEN
            IF (NT.LT.NATT) THEN
              NT=NT+1
              GOTO 30
            ENDIF
          ENDIF
          IF (XNV(2,I).LT.YGRID2) INMAX=MAX0(INMAX,NT)
          DO N=NT,NATT
            TATFV(1,N)=TATFV(1,N)+ETEM
            TATFV(2,N)=TATFV(2,N)+CTEM
            IF (ISW(I).LT.0) TATFV(3,N)=TATFV(3,N)
     $                                 +YMASS*CNEW*(EDNV(I)/DIAM)**3
          ENDDO
          EFRAC=EFRAC+ETEM
          CALL AGDSD(YMASS*(CMASS(I)-CNEW),EDNV(I),DSCP)  !canopy DSD
          CMASS(I)=CNEW
C
          IF (ISW(I).LT.0) XDTOT=XDTOT+YMASS*CMASS(I)
C
          DO J=1,NSWTM
            YTEM=0.0
            IF (J.EQ.1.AND.IBOOM.EQ.1) THEN
              IF (IHALF(I).EQ.1) YTEM=1.0
            ELSEIF (J.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
              IF (IHALF(I).EQ.0) YTEM=1.0
            ELSE
              YTEM=1.0
            ENDIF
            TEM=YEDGE2+(J-1)*SWATH
            IF (XNV(2,I).GT.TEM.AND.IYDV(I,J).EQ.0) THEN
              YDRFT=YDRFT+YTEM*YMASS*CMASS(I)
              IYDV(I,J)=1
            ENDIF
            TEM=YFLXV+(J-1)*SWATH
            IF (XNV(2,I).GT.TEM.AND.IYFV(I,J).EQ.0) THEN
              FDTOT=FDTOT+YTEM*YMASS*CMASS(I)
              IYFV(I,J)=1
              CALL AGDSD(YTEM*YMASS*CMASS(I),EDNV(I),DSVP)  !transport DSD
            ENDIF
C
            IF (ISW(I).LT.0) THEN
              TEF=(J-SFAC)*SWATH
              IF (XNV(2,I).LT.TEF) THEN
                CALL AGDSD(YTEM*YMASS*CMASS(I),EDNV(I),DSSB)  !spray block DSD
              ELSE  !IF (XNV(2,I).LT.TEM) THEN
                CALL AGDSD(YTEM*YMASS*CMASS(I),EDNV(I),DSDW)  !downwind DSD
              ENDIF
C
              TEM=YFLXV+(J-1)*SWATH
              TEMY=0.5*(XNV(2,I)-TEM)**2/XNV(7,I)
              TEMM=YTEM*YMASS*CMASS(I)*EXP(-AMIN1(TEMY,25.0))
     $             /SQRT(XNV(7,I))
              CALL AGDSD(TEMM,EDNV(I),DSDP)  !point DSD
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C  Determine new positions of vortices
C
      IF (LMVEL.NE.0) THEN
        IF (LMVEL.EQ.2) JHEL=1
        DO N=1,NVOR
          CALL AGVEL(XO,YBAR(N),ZBAR(N),TEM,VBAR,WBAR)
          YNR(N)=YBAR(N)+DT*VBAR
          ZNR(N)=ZBAR(N)+DT*WBAR
          CALL AGVEL(XO,YBAL(N),ZBAL(N),TEM,VBAL,WBAL)
          YNL(N)=YBAL(N)+DT*VBAL
          ZNL(N)=ZBAL(N)+DT*WBAL
        ENDDO
        IF (NPRP.NE.0) THEN
          DO N=1,NPRP
            CALL AGVEL(XO-XPRP(N),YPRP(N),ZPRP(N),TEM,VBAR,WBAR)
            YNP(N)=YPRP(N)+DT*VBAR
            ZNP(N)=ZPRP(N)+DT*WBAR
          ENDDO
        ENDIF
        IF (LMVEL.EQ.2) THEN
          NVOR=0
          CALL AGVEL(XO,YHEL,ZHEL,TEM,VBAR,WBAR)
          YHEL=YHEL+DT*VBAR
          ZHEL=ZHEL+DT*WBAR
          NVOR=1
          JHEL=0
        ENDIF
        DO N=1,NVOR
          YBAR(N)=YNR(N)
          ZBAR(N)=ZNR(N)
          YBAL(N)=YNL(N)
          ZBAL(N)=ZNL(N)
        ENDDO
        IF (NPRP.NE.0) THEN
          DO N=1,NPRP
            YPRP(N)=YNP(N)
            ZPRP(N)=ZNP(N)
          ENDDO
        ENDIF
C
C  Correct circulation decay
C
        IF (XO.GT.0.0) THEN
          DO N=1,NVOR
            IF (ABS(GDKV(N)).GT.1.0E-10) THEN
              TEM=AMAX1(1.0,YBAR(N)-YBAL(N))
              HTEM=2.0*ZBAR(N)/TEM
              IF (HTEM.GT.2.0) THEN
                GDKT=GDKO+(GDK-GDKO)/0.102167/HTEM**3.291
              ELSE
                GDKT=GDK
              ENDIF
              GDKV(N)=GDKV(N)*EXP(-AMIN1(ABS(GDKT*BSTAB*DT/TEM),25.0))
            ENDIF
            IF (GDKV(N).GT.0.001) THEN
              DO I=1,NVAR
                IF (ISW(I).NE.0) YGAUS1=AMAX1(YGAUS1,XNV(2,I))
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
C  Correct model parameters
C
      XO=XO+UO*DT
      IF (NPRP.NE.0) THEN
        DO N=1,NPRP
          CPXI(N)=CPXI(N)+UO*DT
          RN=CPXI(N)/11.785
          VPRP(N)=VPRP(N)*(RPRP(N)/RN)**2
          RPRP(N)=RN
        ENDDO
      ENDIF
      IF (LMVEL.EQ.2.AND.XO.GT.0.0) THEN
        IF (XO.LT.40.0*S) THEN
          FN=EXP(-AMIN1(0.01*CHF*XO,25.0))
          G2PI(1)=CHG*(1.0-FN)
          ZBAR(1)=ZHEL
          ZBAL(1)=ZHEL
          WHEL=CHW*SQRT(FN)
        ELSE
          WHEL=0.0
        ENDIF
      ENDIF
C
C  Check solution and continue
C
      T=T+DT
      ISWC=0
      MSWC=0
      IVTT=0
      DO I=1,NVAR
        IF (ISW(I).NE.0) THEN
          ISWC=ISWC+1
          DO J=1,9
            XOV(J,I)=XNV(J,I)
          ENDDO
          IF (LEVAP.NE.0) THEN
            EDOV(I)=AMAX1(EDNV(I),DCUT)
            DMIN=AMIN1(DMIN,EDOV(I))
            IF (IVT(I).EQ.1.AND.
     $         (EDOV(I).EQ.DCUT.OR.XOV(2,I).GT.YGRID2)) THEN
              IVT(I)=0  !turn off save for vapor tracking
              TVTMAX=AMAX1(TVTMAX,T)
            ENDIF
          ENDIF
          IF (ABS(XNV(2,I)).GT.GRDMX) ISW(I)=-2
          IF (ISW(I).LT.0) MSWC=1
          IF (EDOV(I).LT.2.0) ISW(I)=0  !stop small droplets < 2 um
          IVTT=IVTT+IVT(I)
        ENDIF
      ENDDO
      IF (T.GE.TMAX) THEN
        ISWC=0
        MSWC=1
      ENDIF
C
C  Check CALPUFF data collection for later export
C
      IF (LCPFLG.EQ.1.AND.LCPEND.EQ.0) THEN
        IF (LMVEL.EQ.1.OR.(LMVEL.EQ.2.AND.WHEL/CHW.LT.0.1)) THEN
          IF ((ABS(G2PI(1)*GDKV(1)).LE.G2PIMN).AND.(T.GE.ETAU)) THEN
            ISWC=0  !was commented out
            MSWC=1
            LCPEND=1
          ENDIF
        ENDIF
      ENDIF
C
C  Check SCIPUFF end of calculation for this droplet size
C
      IF (LSPFLG.EQ.1) THEN
        IF (LMVEL.EQ.1.OR.(LMVEL.EQ.2.AND.WHEL/CHW.LT.0.1)) THEN
          IF (ABS(G2PI(1)*GDKV(1)).LE.G2PIMN) THEN
            ISWC=0
            MSWC=1
            LSPEND=1
          ENDIF
        ENDIF
      ENDIF
C
      I=MAX0(1,IFIX(200.0/DMIN))
      IF (MOD(NSTEP,I).EQ.0) MSWC=1
      IF (MSWC.EQ.1) CALL AGSAV(XNV,T)
      IF (ISWC.NE.0) GOTO 10
      CALL AGSAV(XNV,-T)
      RETURN
      END