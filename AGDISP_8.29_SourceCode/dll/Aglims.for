C**AGLIMS
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 11/16/16
C
      SUBROUTINE AGLIMS(NPTS,DV,PV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGLIMS
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: PV
C
C  AGLIMS finishes the initialization
C
C  NPTS   - Number of drop sizes
C  DV     - Drop size diameter array (micrometers)
C  PV     - Percentage completed array (%)
C
      DIMENSION DV(2),PV(2)
C
      CALL AGLIMX(NPTS,DV,PV)
      RETURN
      END
C**AGLIMX
      SUBROUTINE AGLIMX(NPTS,DV,PV)
C
      DIMENSION DV(2),PV(2),DSDV(36)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA DSDV /    8.00,    9.27,   10.75,   12.45,   14.43,   16.73,
     $              19.39,   22.49,   26.05,   30.21,   35.01,   40.57,
     $              47.03,   54.50,   63.16,   73.23,   84.85,   98.12,
     $             113.71,  131.73,  152.79,  177.84,  205.84,  238.45,
     $             276.48,  320.60,  372.18,  430.74,  498.91,  578.54,
     $             670.72,  777.39,  900.61, 1044.42, 1210.66, 1403.04 / 
C
C  Horizontal deposition planes
C
      YMIN=YDEPN
      YMAX=YDEPX
      NVEC=NDEPR
      DY=(YMAX-YMIN)/(NVEC-1)
      DO N=1,NVEC
        YDEPR(N)=YMIN+(N-1)*DY
        ZDEPR(N)=0.0
      ENDDO
      DDEPR=DY
      YMAX=YMAX+(NSWTH+2.5)*SWATH
      NVEC=(YMAX-YMIN)/DY+2
      NDEPS=NVEC
      DO N=1,NVEC
        YDEPS(N)=YMIN+(N-1)*DY
        ZDEPS(N)=0.0
        ZDEPT(N)=0.0
        ZDEPH(N)=0.0
        ZDEPI(N)=0.0
        ZDEPN(N)=0.0
C        ZFLXV(N)=0.0  !WHY FLUXES
C        ZFLXE(N)=0.0
C        ZFLXH(N)=0.0
      ENDDO
      NDEPS2=(YDEPX2+(NSWTH+2.5)*SWATH-YDEPN)/DY+2
C
C  Vertical flux
C
      YMIN=ZFLXN
      YMAX=ZFLXX
      NVEC=NFLXR
      DY=(YMAX-YMIN)/NVEC
      DO N=1,NVEC
        YFLXR(N)=YMIN+N*DY
        ZFLXR(N)=0.0
        ZFLXT(N)=0.0
        ZFLXD(N)=0.0
      ENDDO
      DFLXR=DY
C
C  Transfer drop size distribution
C
      NPTS=0
      PNOZ=0
      DMAX=0.0
      DO N=1,NDRP
        NPTS=NPTS+1
        DDIAMN(NPTS)=DIAMV(N)
        DMAX=AMAX1(DMAX,DDIAMN(NPTS))
        DMASSN(NPTS)=DMASS(N)
        PNOZ=PNOZ+1
        PV(NPTS)=PNOZ
      ENDDO
      NNDRPT=NPTS
      DO N=1,NPTS
        DV(N)=DDIAMN(N)
        PV(N)=100.0*PV(N)/PV(NPTS)
      ENDDO
C
C  Inilalize computed drop size distributions
C
      NDSD=0
10    NDSD=NDSD+1
      IF (NDSD.LE.36) THEN
        DSDC(NDSD)=DSDV(NDSD)
      ELSE
        DSDC(NDSD)=1.159*DSDC(NDSD-1)
      ENDIF
      IF (DSDC(NDSD).LT.DMAX.AND.NDSD.LT.75) GOTO 10
      DO N=1,NDSD
        DSSB(N)=0.0
        DSDW(N)=0.0
        DSVP(N)=0.0
        DSCP(N)=0.0
        DSDP(N)=0.0
      ENDDO
C
C  Set initial values
C
      SWATH=SWATH/CTS
      SFAC=SDISP+0.5*(1+IBOOM)
C      IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
      YEDGE2=YGRID2+(1.0-SFAC)*SWATH
      YDRFT=0.0
      EFRAC=0.0
      ALEFT=0.0
      YGAUS1=0.0
C
C  Set turbulence level in discrete wind speed
C
      IF (LMCRS.EQ.2) THEN
        DO N=1,NWIND
          WINDHTV(N+10)=WINDHTV(N)
          WINDSPV(N+10)=WINDSPV(N)
        ENDDO
        NWIND=NWIND+10
        USK=WINDSPV(11)/ALOG((WINDHTV(11)+ZO)/ZO)
        QQMX=0.845*QSTAB*USK**2
        DO N=1,10
          WINDHTV(N)=0.1*(N-1)*WINDHTV(11)
          WINDSPV(N)=USK*ALOG((WINDHTV(N)+ZO)/ZO)-PSTAB
          WINDQQV(N)=QQMX
        ENDDO
        WINDQQV(11)=QQMX
        IF (NWIND.GT.12) THEN
          DO N=12,NWIND-1
            DZM=WINDHTV(N)-WINDHTV(N-1)
            DZP=WINDHTV(N+1)-WINDHTV(N)
            DZT=DZM+DZP
            FZM=-DZP/DZM/DZT
            FZP=DZM/DZP/DZT
            FZ=-FZM-FZP
            DUDZ=FZM*WINDSPV(N-1)+FZ*WINDSPV(N)+FZP*WINDSPV(N+1)
            WINDQQV(N)=0.845*QSTAB*(WINDHTV(N)*DUDZ)**2
          ENDDO
        ENDIF
        WINDQQV(NWIND)=WINDQQV(NWIND-1)
      ENDIF
C
C  Set canopy constants
C
      IF (LCANF.NE.0) THEN
        WINDHT=2.0*HCAN
        WINDSP=USK*(ALOG((WINDHT+ZO)/ZO)-PSTAB)
        TEM=1.0-DOC+ZOC
        ALPHAC=1.0/TEM/(ALOG(TEM/ZOC)-PSTAB)
        UOPN=WINDSP/(ALOG((WINDHT/HCAN-DOC+ZOC)/ZOC)-PSTAB)
        UCAN=UOPN*(ALOG(TEM/ZOC)-PSTAB)
        QQMX=0.845*(UOPN/HCAN)**2
        QQMC=QQMX/TEM**2
        TEM=WINDDR*6.2831853/360.0
        CCW=COS(TEM)
        SCW=SIN(TEM)
      ENDIF
C
C  Set total accountancy arrays
C
      TATTV(1)=0.0
      TATTV(2)=0.1
      NATT=2
      DT=0.1
      FT=1.05
20    DT=DT*FT
      NATT=NATT+1
      TATTV(NATT)=TATTV(NATT-1)+DT
C      IF (TATTV(NATT).LT.300.0.AND.NATT.LT.200) GOTO 20
      IF (TATTV(NATT).LT.TMAX.AND.NATT.LT.200) GOTO 20
C      TATTV(NATT)=AMIN1(TATTV(NATT),300.0)
      DO N=1,NATT
        DO I=1,3
          TATFV(I,N)=0.0
        ENDDO
      ENDDO
      INMAX=2
C
      DO N=1,27
        TADDV(N)=2.0*(N-26)
      ENDDO
      NADD=27
      DD=2.0
      FD=1.05
30    DD=DD*FD
      NADD=NADD+1
      TADDV(NADD)=TADDV(NADD-1)+DD
C      IF (TADDV(NADD).LT.300.0.AND.NADD.LT.200) GOTO 30
      IF (TADDV(NADD).LT.YGRID2.AND.NADD.LT.200) GOTO 30
C      TADDV(NADD)=AMIN1(TADDV(NADD),300.0)
      DO N=1,NADD
        DO I=1,3
          TADFV(I,N)=0.0
        ENDDO
      ENDDO
C
      DAHH=0.005
40    DAHH=2.0*DAHH
      NAHH=(BOOMHT-ZREF+0.5*S)/DAHH+1
      IF (NAHH.GT.200) GOTO 40
      NAHH=MAX0(NAHH,2)
      DO N=1,NAHH
        TAHHV(N)=(N-1)*DAHH+ZREF
        DO I=1,3
          TAHFV(I,N)=0.0
        ENDDO
      ENDDO
C
C  Set CALPUFF on
C
      IF (LCPFLG.EQ.1) THEN
        IF (LMVEL.EQ.1) THEN
          G2PIMN=0.005*ABS(G2PIS(1))
        ELSE
          G2PIMN=0.005*ABS(CHG)
        ENDIF
        NPUFF=0
      ENDIF
C
C  Set SCIPUFF end of calculation condition
C
      IF (LSPFLG.EQ.1) THEN
        IF (LMVEL.EQ.1) THEN
          G2PIMN=FSPFLG*ABS(G2PIS(1))
        ELSE
          G2PIMN=FSPFLG*ABS(CHG)
        ENDIF
        LDRP=0
        FSPV(1:5)=0.0
      ENDIF
C
C  Set Vapor Tracking
C
      NVTRK=0
      LVTFLG=0  !LEVAP
      XVTMAX=0.0
      YVTMIN=0.0
      YVTMAX=0.0
      ZVTMAX=0.0
      TVTMAX=0.0      
C
      RETURN
      END