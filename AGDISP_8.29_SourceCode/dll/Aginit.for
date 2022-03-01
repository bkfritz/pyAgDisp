C**AGINIT
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGINIT(UD,MAA)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGINIT
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: MAA
C
C  AGINIT sets up all default pointers for data input
C
C  UD     - USERDATA data structure
C  MAA    - Multiple application flag: -1,0=no;#=wind speed index
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      CALL AGINIX(UD,MAA)
      RETURN
      END
C**AGINIX
      SUBROUTINE AGINIX(UD,MAA)
C
      DIMENSION XNOZ(60),YNOZ(60),ZNOZ(60),UNOZ(60),VNOZ(60)
      DIMENSION KSTV(5,7),PSTV(6),QSTV(6),BSTV(6)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      INCLUDE 'AGCOMMON.INC'
      INCLUDE 'AGSAMPLE.INC'
C
      DATA TPI / 6.2831853 /
      DATA KSTV / 1,1,2,3,3,1,2,2,3,4,2,3,3,4,4,3,4,4,4,4,
     $            4,4,4,4,4,6,5,4,4,4,6,6,5,4,4 /
      DATA PSTV / 0.524, 0.373, 0.211, 0.0,-0.533,-3.175 /
      DATA QSTV / 2.207, 1.693, 1.309, 1.0, 0.734, 0.500 /
      DATA BSTV / 1.911, 1.393, 1.161, 1.0, 0.893, 0.786 /
C
C  Set all necessary default flags
C
C      GRDMX=0.0
      NPRP=0
      NVOR=0
      DZBP=0.0
      PSBP=0.0
      PGBP=0.0
      JHEL=0
      QQMX=1.0
      SDISP=0.0
      JSMO=0  !1  !TEST THIS VARIABLE
      LCANF=0
      HCAN=0.0
      CTU=1.0
      STU=0.0
      CTS=1.0
      STS=0.0
      IIDEP=-1
      IIDIS=-1  !WHAT IS THIS POINTING TO
      IIGAU=-1
C
      IF (MAA.LT.0) THEN
        LFMAA=1
        LFMET=0
        LFMAC=0
      ELSEIF (MAA.EQ.0) THEN
        LFMAA=1
        LFMET=1
        LFMAC=0
      ELSE
        LFMAA=0
        LFMET=1
        LFMAC=MAA
      ENDIF
C
C  Establish initial values based on data structure
C
C      DO ND=1,3
        NDRP=UD.DSD.NUMDROP
        DO N=1,NDRP
          DIAMV(N)=UD.DSD.DIAM(N)
          DMASS(N)=UD.DSD.MASSFRAC(N)
        ENDDO
C        NZTYPE=0
C      ENDDO
C      ITRTYP=UD.TIER
C      IF (ITRTYP.EQ.2) THEN
C        IACTYP=UD.AC.BASICTYP+1
C      ELSE
        IACTYP=UD.AC.WINGTYPE-2  !fixed=1 heli=2
C      ENDIF
      LMVEL=1-UD.APPLMETH
      IF (LMVEL.EQ.0) THEN
        CACNM='Ground Sprayer'
        LACNM=14
        PGJET=UD.GA.PRESSURE
        UGJET=0.88*SQRT(200.0*PGJET)
        IGJET=UD.GA.NOZTYPE
        IF (IGJET.EQ.0) THEN
          XGJET=0.14
          CGJET=0.57
        ELSE
          XGJET=0.02
          CGJET=2.04
        ENDIF
        S=0.5*UD.CTL.SWATHWID
        UO=1.0
      ELSE
        CACNM=UD.AC.NAME
        LACNM=UD.AC.LNAME
        S=UD.AC.SEMISPAN
        UO=UD.AC.TYPSPEED
        WT=9.81*UD.AC.WEIGHT
        LMVEL=UD.AC.WINGTYPE-2  !fixed=1 heli=2
        IF (LMVEL.EQ.1) THEN
          DZBPD=UD.AC.BIPLSEP
          AS=UD.AC.PLANAREA
          TDOT=UD.AC.PROPRPM
          RPRPS=UD.AC.PROPRAD
          DZPRP=UD.AC.ENGVERT
          XPRPS=-UD.AC.ENGFWD
          NPRP=UD.AC.NUMENG
          IF (NPRP.EQ.1) THEN
            YPRPS(1)=0.0
          ELSEIF (NPRP.EQ.2) THEN
            YPRPS(1)=UD.AC.ENGHORIZ(1)
            YPRPS(2)=-UD.AC.ENGHORIZ(1)
          ELSE
            YPRPS(1)=UD.AC.ENGHORIZ(1)
            YPRPS(2)=-UD.AC.ENGHORIZ(1)
            YPRPS(3)=UD.AC.ENGHORIZ(2)
            YPRPS(4)=-UD.AC.ENGHORIZ(2)
          ENDIF
        ELSE
          BDOT=UD.AC.PROPRPM
        ENDIF
      ENDIF
C
C      ISMKY=UD.SMOKEY
C      IF (ITRTYP.EQ.3.AND.ISMKY.EQ.1) THEN
        ANGTU=UD.TRN.UPSLOPE
        IF (ANGTU.NE.0.0) THEN
          CTU=COS(ANGTU*TPI/360.0)
          STU=SIN(ANGTU*TPI/360.0)
        ENDIF
        ANGTS=UD.TRN.SIDESLOPE
        IF (ANGTS.NE.0.0) THEN
          CTS=COS(ANGTS*TPI/360.0)
          STS=SIN(ANGTS*TPI/360.0)
        ENDIF
C      ENDIF
C
      LDRY=UD.AIRTYPE
      IF (LDRY.EQ.0) THEN
        NVAR=UD.NZ.NUMNOZ
        DO N=1,NVAR
          XNOZ(N)=UD.NZ.POSFWD(N)
          YNOZ(N)=UD.NZ.POSHORIZ(N)
          ZNOZ(N)=UD.NZ.POSVERT(N)
          UNOZ(N)=0.0
          VNOZ(N)=0.0
        ENDDO
C
C  Dry deposition settings
C
      ELSE
        SPHERE=UD.DRY.SPHERICITY
        APSPH=EXP(2.3288-6.4581*SPHERE+2.4486*SPHERE**2)
        BPSPH=0.0964+0.5565*SPHERE
        CPSPH=EXP(4.905-13.8944*SPHERE+18.4222*SPHERE**2
     $        -10.2599*SPHERE**3)/24.0
        DPSPH=EXP(1.4681+12.2584*SPHERE-20.7322*SPHERE**2
     $        +15.8855*SPHERE**3)
        LDRY=1+UD.DRY.TYPE
        IF (LDRY.EQ.1) THEN
          WDRY=UD.DRY.WIDTH
          ADRY=UD.DRY.ANGLE
          VDRY=UD.DRY.VELOCITY
          NVAR=MAX0(MIN0(IFIX(10.0*WDRY+1),60),3)
          DN=WDRY/(NVAR-1)
          DA=2.0*ADRY/(NVAR-1)
          DO N=1,NVAR
            XNOZ(N)=0.0
            YNOZ(N)=-0.5*WDRY+(N-1)*DN
            ZNOZ(N)=0.0
            TEMA=(-ADRY+(N-1)*DA)/57.29578
            UNOZ(N)=VDRY*COS(TEMA)
            VNOZ(N)=VDRY*SIN(TEMA)
          ENDDO
        ELSEIF (LDRY.EQ.2) THEN
          HDRY=UD.DRY.HUB
          RDRY=UD.DRY.RPM
          NVAR=48
          DA=7.5/57.29578
          TEMV=0.10472*HDRY*RDRY
          DO N=1,NVAR
            TEMA=(N-1)*DA
            XNOZ(N)=-HDRY*SIN(TEMA)
            YNOZ(N)=HDRY*COS(TEMA)
            ZNOZ(N)=0.0
            UNOZ(N)=-TEMV*COS(TEMA)
            VNOZ(N)=-TEMV*SIN(TEMA)
          ENDDO
        ELSE
          SDRY=UD.DRY.LENGTH
          NVAR=MAX0(MIN0(IFIX(10.0*SDRY+1),59),3)
          NVAR=NVAR/2
          NVAR=2*NVAR+1
          DN=SDRY/(NVAR-1)
          DO N=1,NVAR
            XNOZ(N)=0.0
            YNOZ(N)=-0.5*SDRY+(N-1)*DN
            ZNOZ(N)=0.0
            UNOZ(N)=0.0
            VNOZ(N)=0.0
          ENDDO
        ENDIF
      ENDIF
C
      XOSMN=1.0E+10
      XOSMX=-1.0E+10
      ZOSMN=1.0E+10
      ZOSMX=-1.0E+10
      DO N=1,NVAR
        XOSMN=AMIN1(XOSMN,XNOZ(N))
        XOSMX=AMAX1(XOSMX,XNOZ(N))
        ZOSMN=AMIN1(ZOSMN,ZNOZ(N))
        ZOSMX=AMAX1(ZOSMX,ZNOZ(N))
      ENDDO
      BOOMHT=UD.CTL.HEIGHT
      RGJET=BOOMHT
      IF (LMVEL.EQ.0) THEN
        BOOMVT=0.0
        BOOMFD=0.0
      ELSE
        BOOMVT=UD.AC.BOOMVERT
        BOOMFD=UD.AC.BOOMFWD
        WINGVT=UD.AC.WINGVERT
      ENDIF
      DIST=BOOMHT-BOOMVT+WINGVT
      XOS=-BOOMFD-XOSMX
      IF (LMVEL.EQ.1) ZOSMN=AMIN1(ZOSMN,DZPRP-BOOMVT-RPRPS)
      HOSMN=DIST
      DO N=1,NVAR
        XS(1,N)=-BOOMFD-XNOZ(N)
        XS(2,N)=YNOZ(N)
        XS(3,N)=BOOMHT+ZNOZ(N)
        HOSMN=AMIN1(HOSMN,XS(3,N)*CTS-XS(2,N)*STS)
        XS(4,N)=-UO+UNOZ(N)
        XS(5,N)=VNOZ(N)
        DO K=6,9
          XS(K,N)=0.0
        ENDDO
c
c        xs(5,n)=xs(5,n)+4.75  !fix for spray gun for journal
c        xs(6,n)=xs(6,n)+4.75  !fix for spray gun for journal
c
        IF (LMVEL.EQ.0) THEN
          XS(6,N)=-UGJET
          XS(7,N)=0.001
        ENDIF
        IF (LDRY.NE.0) XS(7,N)=1.0
C        NSD(N)=UD.NZ.NOZTYP(N)+1
C        NZTYPE(NSD(N))=NZTYPE(NSD(N))+1
        IF (XS(2,N).LT.0.0) THEN
          IHALF(N)=1
        ELSE
          IHALF(N)=0
        ENDIF
      ENDDO
C      IF (UD.CTL.SWTYPE.EQ.0) THEN
        SWATH=UD.CTL.SWATHWID
C      ELSE
C        SWATH=2.0*S*UD.CTL.SWATHWID
C      ENDIF
      LFLOW=UD.SM.FLOWUNIT+1
      IF (LFLOW.EQ.1) THEN
        FLOW=0.001585*UO*UD.SM.FLOWRATE*SWATH
        FLOWN=UD.SM.FLOWRATE
      ELSE
        FLOW=0.2642*UD.SM.FLOWRATE
        FLOWN=UD.SM.FLOWRATE/0.006/UO/SWATH
      ENDIF
      DENF=UD.SM.SPECGRAV
      DENN=UD.SM.NONVGRAV
      IF (UD.SM.BASICTYP.EQ.0) THEN
        ERATE=0.0
        LEVAP=0
      ELSE
        ERATE=UD.SM.EVAPRATE
        LEVAP=1
      ENDIF
      VFRAC=1.0-UD.SM.NVFRAC
C      IF (ABS(VFRAC).LT.0.001.OR.LDRY.EQ.1) LEVAP=0
      AFRAC=UD.SM.ACFRAC
      WINDSP=UD.MET.WINDSPD
C      LWDIR=UD.CTL.ITSTWDIR
      ZO=UD.MET.SURFRUFF
      TEMPTR=UD.MET.TEMP
      RHUMTR=UD.MET.HUMIDITY
      NSWTH=UD.CTL.NUMLINES
      IBOOM=UD.CTL.HALFBOOM
C      ISDTYP=UD.CTL.SDTYPE
C      IF (ISDTYP.EQ.0) THEN
C        SDISP=-UD.CTL.SDVALUE
C      ELSEIF (ISDTYP.EQ.1) THEN
C        SDISP=UD.CTL.SDVALUE
C      ELSEIF (ISDTYP.EQ.2) THEN
        SDISP=-UD.CTL.SDVALUE/AMAX1(SWATH,1.0)+0.5*(UD.CTL.SWATHOFF-1.0)
C      ELSE
C        SDISP=0.5*(1-IBOOM)
C      ENDIF
      YFLXV=UD.CTL.FLXPLANE
C      HFLXV=UD.CTL.HGTRN
C      RBDST=YFLXV
C
C      IF (ISDTYP.NE.1) THEN
        SWDISP=-SDISP*SWATH
C      ELSE
C        SWDISP=0.0
C      ENDIF
C
      LFOPT=UD.CTL.LINEOPT
      DO N=1,NSWTH
        NFREP(N)=UD.CTL.LINEREPS(N)
      ENDDO
      NFREP(NSWTH+1)=1
C
      ITEM=MIN0(IFIX((WINDSP+1.0)/2.0)+1,5)
      KSTAB=UD.MET.INSOL+1
      LSTAB=KSTV(ITEM,KSTAB)
      PSTAB=PSTV(LSTAB)
      QSTAB=QSTV(LSTAB)
      BSTAB=BSTV(LSTAB)
C
C  Advanced settings
C
      TMAX=UD.CTL.MAXTIME
      GDK=2.0*UD.MET.VDECAYIG
      GDKO=2.0*UD.MET.VDECAYOG
      IF (LFMAA.EQ.1) THEN
        WINDDR=UD.MET.WINDDIR
      ELSE
        WINDDR=-90.0
      ENDIF
      WINDHT=UD.MET.WINDHGT
      DRAG=UD.AC.DRAG
      PROP=UD.AC.PROPEFF
      PRTR=UD.MET.PRESSURE
      ZREF=UD.TRN.ZREF
C
      NDEPR2=0.5*(UD.CTL.MAXDWND+155.0)+1
      YDEPX2=2.0*(NDEPR2-31)
      YGRID2=UD.CTL.MAXDWND  !YDEPX2-95.0
      NGRID2=NDEPR2-46
      NDEPR=0.5*(4000.0+155.0)+1
      YDEPX=2.0*(NDEPR-31)
      YGRID=4000.0  !YDEPX-95.0
      NGRID=NDEPR-46
C
      IDSB=UD.CTL.SAVETRAJ
C
C  CALPUFF input
C
      LCPFLG=UD.CALPUFFFLAG
C
C  SCIPUFF inputs
C
      LSPFLG=UD.SCIPUFFFLAG
      FSPFLG=UD.SCIPUFFCUTOFF
C
      LMCRS=UD.MET.WINDTYPE+1
      IF (LMCRS.EQ.2) THEN
        NWIND=UD.MET.NUMWINDT
        DO N=1,NWIND
          WINDHTV(N)=UD.MET.WINDTWH(N)
          WINDSPV(N)=UD.MET.WINDTWS(N)
        ENDDO
      ENDIF
C
C  Canopy settings
C
C      IF (ISMKY.EQ.1) THEN
C        IF (ITRTYP.EQ.2) THEN
C          HCAN=UD.CAN.HEIGHT
C          ZREF=AMAX1(ZREF,HCAN)
C          IF (HCAN.GT.0.0) THEN
C            LCANF=-1
C            ZOC=UD.CAN.NDRUFF
C            DOC=UD.CAN.NDDISP
C          ENDIF
C        ELSE
          LCANF=UD.CAN.TYPE
          IF (LCANF.NE.0) THEN
            HCAN=UD.CAN.HEIGHT
            ZOC=UD.CAN.NDRUFF
            DOC=UD.CAN.NDDISP
            IF (LCANF.EQ.3) THEN
              LCANF=-1
              ZREF=AMAX1(ZREF,HCAN)
            ELSE
              ESIZE=UD.CAN.ELESIZ
              LTYPE=UD.CAN.ELETYP
              TEMPC=UD.CAN.TEMP
              RHUMC=UD.CAN.HUMIDITY
              ICANHT=0
              IF (LCANF.EQ.1) THEN
                STEMS=UD.CAN.STANDEN
                NCAN=UD.CAN.NUMENV
                CANIMN=1.0E+10
                CANIMX=-1.0E+10
                CANPMN=1.0E+10
                CANPMX=-1.0E+10
                DO N=1,NCAN
                  CANHV(N)=UD.CAN.ENVHGT(N)
                  CANIV(N)=UD.CAN.ENVDIA(N)
                  CANPV(N)=UD.CAN.ENVPOP(N)
                  IF (N.GT.1) THEN
                    IF (CANHV(N).LT.CANHV(N-1)) ICANHT=N
                  ENDIF
                  CANIMN=AMIN1(CANIMN,CANIV(N))
                  CANIMX=AMAX1(CANIMX,CANIV(N))
                  CANPMN=AMIN1(CANPMN,CANPV(N))
                  CANPMX=AMAX1(CANPMX,CANPV(N))
                ENDDO
              ELSE
                LCANF=LCANF+UD.CAN.OPTYPE-1
                IF (LCANF.EQ.2) THEN
                  NCAN=UD.CAN.NUMLAI
                  CANIMN=1.0E+10
                  CANIMX=-1.0E+10
                  ICANMN=0
                  DO N=1,NCAN
                    CANHV(N)=UD.CAN.LAIHGT(N)
                    CANIV(N)=UD.CAN.LAICUM(N)
                    CANIMN=AMIN1(CANIMN,CANIV(N))
                    CANIMX=AMAX1(CANIMX,CANIV(N))
                    IF (N.GT.1) THEN
                      IF (CANHV(N).LT.CANHV(N-1)) ICANHT=N
                      IF (CANIV(N).GT.CANIV(N-1)) ICANMN=N
                    ENDIF
                  ENDDO
                ELSE
                  NCAN=1
                  CANHV(1)=UD.CAN.LIBHGT
                  CANIV(1)=UD.CAN.LIBLAI
                  BBULL=UD.CAN.LIBB
                  CBULL=UD.CAN.LIBC
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C        ENDIF
C      ENDIF
C
C  Multiple application assessment settings
C
      IF (LFMAA.EQ.0) THEN
        WINDSP=LFMAC
        WINDDR=-90.0
        TEMPTR=TEMPAA
        RHUMTR=RHUMAA
      ENDIF
C
C  Set ICV array for displaying initial data and testing limits
C
      DO N=1,150
        ICV(N)=0
      ENDDO
      ICV(1)=20*LFMAA
      IF (LMVEL.NE.0) ICV(2)=20*LFMAA
      ICV(3)=20*LFMAA
      IF (LMVEL.NE.0) ICV(4)=20*LFMAA
      IF (LMVEL.NE.0) ICV(5)=20*LFMAA
      IF (LMVEL.NE.0) ICV(6)=20*LFMAA
      IF (LMVEL.NE.0) ICV(7)=20*LFMAA
      IF (LMVEL.EQ.0) THEN
        ICV(8)=35
        ICV(9)=35
      ELSEIF (LMVEL.EQ.1) THEN
        ICV(8)=21
        ICV(9)=23
        ICV(10)=40*LFMAA
        ICV(11)=40*LFMAA
        ICV(12)=40*LFMAA
        ICV(13)=40*LFMAA
        ICV(14)=40*LFMAA
        ICV(15)=40*LFMAA
        ICV(16)=40
        ICV(17)=45*LFMAA
        IF (NPRP.EQ.4) ICV(18)=45*LFMAA
      ELSE
        ICV(8)=30*LFMAA
        ICV(9)=30
        ICV(10)=30
      ENDIF
      ICV(19)=15*LFMAA
      IF (LMVEL.NE.0) ICV(20)=15*LFMAA
      IF (LMVEL.NE.0) ICV(21)=15*LFMAA
      ICV(22)=28*LFMAA
      ICV(23)=28*(2-LMCRS)*LFMAA
      ICV(24)=28*LFMAA*LFMET
      ICV(25)=28*(2-LMCRS)*LFMET
      ICV(26)=28*LFMAA
      DO N=1,NWIND
        ICV(26+N)=29*(LMCRS-1)*LFMET
        ICV(36+N)=29*(LMCRS-1)*LFMET
      ENDDO
      IF (NWIND.EQ.0) ICV(27)=29*(LMCRS-1)*LFMET
      ICV(47)=60*LFMAA
      ICV(48)=60*LFMAA
      IF (LEVAP.EQ.1) THEN
        ICV(49)=60*LFMAA
        ICV(50)=60*LFMAA
        ICV(51)=60*LFMAA
      ENDIF
      ICV(52)=61*LFMAA
      ICV(53)=61*LFMAA
      ICV(54)=62*LFMAA
      ICV(55)=62*LFMAA
      ICV(56)=62*LFMAA
      ICV(57)=62*LFMAA
C      DO ND=1,3
C        JCARD=66  !+4*(ND-1)
C        IF (NZTYPE(ND).GT.0) THEN  !IS THIS GAP GOING TO BE A PROBLEM?
          ICV(58)=64*LFMAA
          ICV(59)=64*LFMAA
          ICV(60)=64*LFMAA
          ICV(61)=64
C        ENDIF
C      ENDDO
      ICV(62)=65*LFMAA
      ICV(63)=65*LFMAA
      ICV(64)=66*LFMAA
      ICV(65)=66*LFMET
      ICV(66)=66*LFMET
      IF (ZREF.NE.0.0) ICV(67)=70*LFMAA
      ICV(68)=72*LFMAA*IBOOM
      IF (LFMAA.EQ.0) THEN
        ICV(69)=75
        ICV(70)=75
      ELSE
        ICV(69)=85
        ICV(70)=85
      ENDIF
      ICV(71)=86*LFMAA
      DO NF=1,NSWTH
        IF (NFREP(NF).NE.1) ICV(NF+71)=87*LFMAA*(1-LFOPT)
      ENDDO
      IF (LFOPT.EQ.1) ICV(122)=88*LFMAA
      ICV(123)=90
      ICV(124)=95
C      ICV(142)=100
C      IF (ISMKY.EQ.1) THEN
        IF (LCANF.EQ.-1) THEN
          ICV(125)=105*LFMAA
          ICV(129)=110*LFMAA
          ICV(130)=110*LFMAA
        ELSE
          IF (LCANF.NE.0) THEN
            ICV(125)=110*LFMAA
            ICV(126)=110*LFMAA
            ICV(127)=110*LFMAA
            ICV(128)=110*LFMAA
            ICV(129)=110*LFMAA
            ICV(130)=110*LFMAA
            IF (LCANF.EQ.1) THEN
              ICV(131)=120*LFMAA
              ICV(132)=120*LFMAA
              ICV(133)=120*LFMAA
              ICV(134)=120*LFMAA
              ICV(135)=120*LFMAA
              ICV(136)=120*LFMAA
              ICV(137)=120*LFMAA
            ELSEIF (LCANF.EQ.2) THEN
              ICV(131)=125*LFMAA
              ICV(132)=125*LFMAA
              ICV(133)=125*LFMAA
              ICV(134)=125*LFMAA
              ICV(135)=125*LFMAA
            ELSEIF (LCANF.EQ.3) THEN
              ICV(131)=130*LFMAA
              ICV(132)=130*LFMAA
            ENDIF
          ENDIF
          IF (ANGTU.NE.0.0) ICV(138)=140*LFMAA
          IF (ANGTS.NE.0.0) THEN
            ICV(139)=140*LFMAA
            ICV(140)=140*LFMAA
          ENDIF
        ENDIF
C      ENDIF
      IF (LDRY.NE.0) THEN
        ICV(141)=150
        IF (LDRY.EQ.1) THEN
          ICV(142)=150
          ICV(143)=150
          ICV(144)=150
        ELSEIF (LDRY.EQ.2) THEN
          ICV(142)=150
          ICV(143)=150
        ELSE
          ICV(142)=150
        ENDIF
      ENDIF
      IF (IDSB.EQ.1) ICV(145)=160*LFMAA
      IF (LCPFLG.EQ.1) ICV(146)=165*LFMAA
      IF (IACTYP.EQ.2) ICV(147)=170*LFMAA
      IF (LSPFLG.EQ.1) ICV(148)=175*LFMAA
      ICV(149)=180
C
      ICARD=0
      RETURN
      END