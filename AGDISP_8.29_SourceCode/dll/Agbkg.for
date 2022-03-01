C**AGBKG
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGBKG(XV,DV,T)
C
C  AGBKG evaluates the background at every drop location
C
C  XV     - Array of current locations, velocities, etc.
C  DV     - Array of background information (determined here)
C  T      - Time
C
      DIMENSION XV(9,2),DV(6,2)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA UXI,UVI / 0.0 , 0.0 /
C
      DATA AEVAP,BEVAP / 0.240 , 0.240 /
C
C  Loop for all drops
C
      VMAX=0.1
      DO I=1,NVAR
        IF (ISW(I).NE.0) THEN
          X=XO+XV(1,I)
          Y=XV(2,I)
          Z=XV(3,I)
C
C  Determine mean velocity at the drop position
C
          CALL AGVEL(X,Y,Z,U,V,W)
          VMAX=AMAX1(VMAX,
     $         SQRT(ABS(XV(5,I)**2+XV(6,I)**2)),SQRT(ABS(V*V+W*W)))
C
C  Determine decay constant
C
          VREL=SQRT(ABS((XV(4,I)-U)**2+(XV(5,I)-V)**2+(XV(6,I)-W)**2))
C
C  Time decay evaluation
C
          D=EDOV(I)
          DENC=((D**3-DCUT**3)*DENF+DCUT**3*DENN)/D**3
          DTAU=3.12E-06*D*D*DENC
          ETAU=0.0  !PROTECT FOR DRY EVAPORATION AND CALPUFF
          REYNO=0.0688*D*VREL
          IF (VREL.GT.0.0) THEN
            IF (LDRY.EQ.0) THEN
              DTAU=DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
            ELSE
              DTAU=DTAU/(1.0+APSPH*REYNO**BPSPH+CPSPH*REYNO/
     $             (1.0+DPSPH/REYNO))
            ENDIF
          ENDIF
          IF (LEVAP.NE.0) THEN
            IF (D.GT.DCUT) THEN
              EFACT=0.5+0.25*AMIN1(REYNO,2.0)
              DTEM=DTEMP
              IF (LCANF.GT.0.AND.Z.LE.HCAN) DTEM=DTEMC
              ETAU=D*D/DTEM/ERATE/EFACT
              IF (VREL.GT.0.0) ETAU=ETAU/(1.0+0.27*SQRT(REYNO))
              DT=AMIN1(DT,0.002*ETAU)
	        IF (LSPFLG.EQ.1) THEN
                CALL AGDISPcalcEvaporation(D,VREL,DCUT,DT,EDNV(I))
              ELSE
                IF (ETAU.EQ.0.0) THEN
                  EDNV(I)=DCUT
                ELSE
                  EDNV(I)=D*SQRT(AMAX1(1.0-DT/ETAU,(DCUT/DIAM)**2))
                  ETAUN=DIAM*DIAM/DTEM/ERATE/EFACT
                  IF (VREL.GT.0.0) ETAUN=ETAUN/(1.0+0.27*SQRT(REYNO))
                  TEM=T/ETAUN
                  TEM=AEVAP*TEM*(1.0+BEVAP*TEM)
                  EDNV(I)=DIAM*SQRT(AMAX1(1.0-TEM,(DCUT/DIAM)**2))
                ENDIF
                ETAU=ETAUN  !PROTECT FOR CALPUFF OUTPUT
              ENDIF
            ENDIF
          ENDIF
C
C  Scale length
C
          SL=0.65*Z
          QQ=0.0
          IF (NVOR.GT.0) THEN
            DO N=1,NVOR
              R=SQRT(ABS((Y-YBAR(N))**2+(Z-ZBAR(N))**2))
              SL=AMIN1(SL,0.6*R)
              R=SQRT(ABS((Y-YBAL(N))**2+(Z-ZBAL(N))**2))
              SL=AMIN1(SL,0.6*R)
            ENDDO
          ENDIF
          IF (SL.EQ.0.0) GOTO 10
C
C  Turbulence
C
          IF (LMCRS.EQ.1) THEN
            QQ=QQMX
          ELSE
            QQ=AGINT(NWIND,WINDHTV,WINDQQV,Z)
          ENDIF
          IF (LCANF.GT.0) THEN
            IF (Z.LE.HCAN) THEN
              QQ=QQMC*Z*Z*EXP(2.0*ALPHAC*(Z/HCAN-1.0))
            ELSE
              QQ=QQMX*(Z/(Z/HCAN-DOC+ZOC))**2
            ENDIF
          ENDIF
          IF (NPRP.NE.0) THEN
            DO N=1,NPRP
              R=SQRT(ABS((Y-YPRP(N))**2+(Z-ZPRP(N))**2))
              E=15.174*R/CPXI(N)
              UA=11.785*CPUR/CPXI(N)/(1.0+0.25*E*E)**2
              QQ=QQ+0.2034*UA*UA
            ENDDO
          ENDIF
C
C  Determine analytic turbulent correlations with the droplet
C
          IF (QQ.NE.0.0) THEN
            WTAU=SL/(VREL+0.375*SQRT(QQ))
            C=T/WTAU
            EXPC=EXP(-AMIN1(C,25.0))
            EXPT=0.0
            IF (D.GT.0.0) EXPT=EXP(-AMIN1(T/DTAU,25.0))
            B=(DTAU/WTAU)**2
            IF (ABS(B-1.0).GT.0.01) THEN
              SUM1=0.5*(3.0-B)/(B-1.0)**2
              SUM2=0.5/(B-1.0)
              XK1=SUM1*(1.0-DTAU/WTAU)+SUM2
              XK2=SUM1*(EXPC-EXPT*DTAU/WTAU)+SUM2*EXPC*(1.0+C)
              XK3=SUM1*(EXPC-EXPT)+SUM2*EXPC*C
            ELSE
              XK1=0.375
              XK2=(3.0+3.0*C-C*C)*EXPC/8.0
              XK3=(5.0-C)*C*EXPC/8.0
            ENDIF
            XK4=0.5*(1.0+EXPC*(C-1.0))
            UXI=-DTAU*XK1+DTAU*EXPT*(XK2-XK3*DTAU/WTAU)+WTAU*XK4
            UVI=XK1-EXPT*(XK2-XK3*DTAU/WTAU)
          ENDIF
C
C  Evaluate background parameters
C
10        DV(1,I)=DTAU
          DV(2,I)=U
          DV(3,I)=V
          IF (LMVEL.EQ.0) THEN
            DV(4,I)=0.0
          ELSE
            DV(4,I)=W
          ENDIF
          DV(5,I)=UXI*QQ/3.0
          DV(6,I)=UVI*QQ/3.0
        ENDIF
      ENDDO
      RETURN
      END