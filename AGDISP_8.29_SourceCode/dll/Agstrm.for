C**AGSTRM
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGSTRM(UD,NUMSS,SGLD,SGLV,SGLH,ISTYPE,INTYPE,
     $                  XWIDE,XDEEP,XTREE,XDIST,XSRATE,XSLENG,
     $                  XSTURN,XRIPAR,XDECAY,XCHARG,XINPTS,
     $                  IUNIT,LFL,XSNGL,NAUTO,XAUTO,RAUTO,
     $                  NNVEC,YYVEC,CCVEC,NSBL,TV,XV,CV,IER,
     $                  REALWD,CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSTRM
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: NUMSS
!MS$ATTRIBUTES REFERENCE :: SGLD
!MS$ATTRIBUTES REFERENCE :: SGLV
!MS$ATTRIBUTES REFERENCE :: SGLH
!MS$ATTRIBUTES REFERENCE :: ISTYPE
!MS$ATTRIBUTES REFERENCE :: INTYPE
!MS$ATTRIBUTES REFERENCE :: XWIDE
!MS$ATTRIBUTES REFERENCE :: XDEEP
!MS$ATTRIBUTES REFERENCE :: XTREE
!MS$ATTRIBUTES REFERENCE :: XDIST
!MS$ATTRIBUTES REFERENCE :: XSRATE
!MS$ATTRIBUTES REFERENCE :: XSLENG
!MS$ATTRIBUTES REFERENCE :: XSTURN
!MS$ATTRIBUTES REFERENCE :: XRIPAR
!MS$ATTRIBUTES REFERENCE :: XDECAY
!MS$ATTRIBUTES REFERENCE :: XCHARG
!MS$ATTRIBUTES REFERENCE :: XINPTS
!MS$ATTRIBUTES REFERENCE :: IUNIT
!MS$ATTRIBUTES REFERENCE :: LFL
!MS$ATTRIBUTES REFERENCE :: XSNGL
!MS$ATTRIBUTES REFERENCE :: NAUTO
!MS$ATTRIBUTES REFERENCE :: XAUTO
!MS$ATTRIBUTES REFERENCE :: RAUTO
!MS$ATTRIBUTES REFERENCE :: NNVEC
!MS$ATTRIBUTES REFERENCE :: YYVEC
!MS$ATTRIBUTES REFERENCE :: CCVEC
!MS$ATTRIBUTES REFERENCE :: NSBL
!MS$ATTRIBUTES REFERENCE :: TV
!MS$ATTRIBUTES REFERENCE :: XV
!MS$ATTRIBUTES REFERENCE :: CV
!MS$ATTRIBUTES REFERENCE :: IER
!MS$ATTRIBUTES REFERENCE :: REALWD
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGSTRM performs the stream assessment calculations
C
C  UD     - USERDATA data structure
C  NUMSS  - Number of points in single swath deposition array
C  SGLD   - Downwind distance array (m)
C  SGLV   - Single swath deposition array (fraction applied)
C  SGLH   - Upwind half boom single swath deposition array (fraction applied)
C  ISTYPE - Selection type: 0 = Single point result
C                           1 = Times for distance range
C                           2 = Distances for time range
C  INTYPE - Input type for ISTYPE=0: 0 = Time known
C                                    1 = Distance known
C         - Number of times or distances for ISTYPE=1,2 (0=auto)
C  XWIDE  - Width of stream (m)
C  XDEEP  - Stream depth (m)
C  XTREE  - Distance to tree line (m)
C  XDIST  - Distance to stream centerline (m)
C  XSRATE - Stream flow rate (m3/s)
C  XSLENG - Spray line length (m)
C  XSTURN - Turn-around time (s)
C  XRIPAR - Riparian removal fraction
C  XDECAY - Chemical decay rate (1/day)
C  XCHARG - Recharge rate (m3/s/km)
C  XINPTS - Values of time, distance, or concentration for ISTYPE=0
C         - Values of times or distances for ISTYPE=1,2
C  IUNIT  - Units flag: 0 = English; 1 = metric
C  LFL    - Operations flag (0 = initialization of calculation)
C  XSNGL  - Time, distance, concentration computed for ISTYPE=0
C  NAUTO  - Number of times or distances for ISTYPE=1,2
C  XAUTO  - Times or distances automatically generated for ISTYPE=1,2
C  RAUTO  - Discharge rates for EXAMS distances (UNITS)
C  NNVEC  - Number of points in arrays for ISTYPE=1,2
C  YYVEC  - Arrays of times or distances for ISTYPE=1,2
C  CCVEC  - Concentration arrays for ISTYPE=1,2
C  NSBL   - EXAMS number of spray block lines entering stream (0=no export)
C  TV     - EXAMS time array entering stream (s)
C  XV     - EXAMS distance array entering stream (m)
C  CV     - EXAMS concentration array entering stream (ng/L)
C  IER    - Error flag: 0 = No warning or error message
C                       1 = Write warning information real
C                       3 = Write error information real
C                       4 = Write error information string
C  REALWD - Real data array (value, minimum, maximum)
C  CHSTR  - Character string
C  JCHSTR - Length of character string
C
      CHARACTER*40 CHSTR
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      DIMENSION SGLD(2),SGLV(2),SGLH(2),XINPTS(6),REALWD(3)
      DIMENSION XSNGL(3),XAUTO(4),RAUTO(4)
      DIMENSION NNVEC(4),YYVEC(1200,4),CCVEC(1200,4)
      DIMENSION TV(2),XV(2),CV(2),AV(11)
C
      COMMON /STRM/ ATEM,DTEM,UTEM
      COMMON /TBLK/ YN(4900),ZN(4900)
      COMMON /SSBL/ SSBLF,SSBLM,SSBLS,SSBLT
C
C  Check input data
C
C      ITRTYP=UD.TIER
C      IF (ITRTYP.EQ.1) ITRTYP=2
      IER=0
      DISTMX=UD.CTL.MAXDWND
      IF (LFL.EQ.0) THEN
        LFL=1
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(XWIDE,0.1,304.8,0.1,DISTMX,1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Stream Width (ft)'
            JCHSTR=17
          ELSE
            CHSTR='Stream Width (m)'
            JCHSTR=16
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.1) THEN
        LFL=2
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(XDEEP,0.01,100.0,0.01,100.0,1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Stream Depth (ft)'
            JCHSTR=17
          ELSE
            CHSTR='Stream Depth (m)'
            JCHSTR=16
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
C      IF (LFL.EQ.2) THEN
C        LFL=3
C        IF (UD.TIER.EQ.1.AND.XACT.LE.0.0) THEN
C          IER=4
C          IF (UD.SMOKEY.EQ.0) THEN
C            CHSTR='Active Rate must be positive'
C            JCHSTR=28
C          ELSE
C            CHSTR='Active Fraction must be positive'
C            JCHSTR=32
C          ENDIF
C          RETURN
C        ENDIF
C      ENDIF
C
      IF (LFL.EQ.2) THEN
        LFL=3
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(XDIST,0.5*XWIDE,304.8-0.5*XWIDE,
     $             0.5*XWIDE,DISTMX-0.5*XWIDE,1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Distance to Center of Stream (ft)'
            JCHSTR=33
          ELSE
            CHSTR='Distance to Center of Stream (m)'
            JCHSTR=32
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.3) THEN
        LFL=4
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(XTREE,0.0,XDIST-0.5*XWIDE,
     $             0.0,XDIST-0.5*XWIDE,1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Distance to Riparian Barrier (ft)'
            JCHSTR=33
          ELSE
            CHSTR='Distance to Riparian Barrier (m)'
            JCHSTR=32
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.4) THEN
        LFL=5
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=35.3134
        CALL AGCHK(XSRATE,0.01,100000.0,0.001,250000.0,
     $             1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Stream Flow Rate (ft'//CHAR(179)//'/s)'
            JCHSTR=24
          ELSE
            CHSTR='Stream Flow Rate (m'//CHAR(179)//'/s)'
            JCHSTR=23
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.5) THEN
        LFL=6
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(XSLENG,10.0,1000.0,1.0,100000.0,
     $             1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Spray Line Length (ft)'
            JCHSTR=22
          ELSE
            CHSTR='Spray Line Length (m)'
            JCHSTR=21
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.6) THEN
        LFL=7
        CALL AGCHK(XSTURN,0.0,60.0,0.0,300.0,1,1,IER,1.0,REALWD)
        IF (IER.NE.0) THEN
          CHSTR='Turn-Around Time (s)'
          JCHSTR=20
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.7) THEN
        LFL=8
        CALL AGCHK(XRIPAR,0.0,1.0,0.0,1.0,1,1,IER,1.0,REALWD)
        IF (IER.NE.0) THEN
          CHSTR='Riparian Interception Factor'
          JCHSTR=28
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.8) THEN
        LFL=9
        CALL AGCHK(XDECAY,0.0,5.0,0.0,25.0,1,1,IER,1.0,REALWD)
        IF (IER.NE.0) THEN
          CHSTR='Instream Chemical Decay Rate (1/day)'
          JCHSTR=36
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.9) THEN
        LFL=10
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=56.8321
        CALL AGCHK(XSRATE,0.01,100000.0,0.001,250000.0,
     $             1,1,IER,FAC,REALWD)
        IF (IER.NE.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Recharge Rate (ft'//CHAR(179)//'/s/mi)'
            JCHSTR=24
          ELSE
            CHSTR='Recharge Rate (m'//CHAR(179)//'/s/km)'
            JCHSTR=23
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
      IF (LFL.EQ.10) THEN
        LFL=11
        IF (ISTYPE.EQ.0) THEN
          IF (INTYPE.EQ.0) THEN
            IF (XINPTS(1).LT.0.0) THEN
              IER=4
              CHSTR='Single Point Time must be positive'
              JCHSTR=34
              RETURN
            ENDIF
          ENDIF
        ELSEIF (ISTYPE.EQ.1) THEN
          IF (XINPTS(2).LE.XINPTS(1)) THEN
            IER=4
            CHSTR='Distance Range not minimum to maximum'
            JCHSTR=37
            RETURN
          ELSEIF (INTYPE.GT.0) THEN
            DO I=1,INTYPE
              IF (XINPTS(I+2).LT.0.0) THEN
                IER=4
                CHSTR='Time Values must be positive'
                JCHSTR=28
                RETURN
              ENDIF
            ENDDO
          ENDIF
        ELSE
          IF (XINPTS(2).LE.XINPTS(1)) THEN
            IER=4
            CHSTR='Time Range not minimum to maximum'
            JCHSTR=33
            RETURN
          ELSEIF (XINPTS(1).LT.0.0) THEN
            IER=4
            CHSTR='Time Values must be positive'
            JCHSTR=28
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
C  Set single swath parameters
C
C      IF (UD.TIER.EQ.1) THEN
C        IBOOM=0
C        IF (UD.APPLMETH.EQ.0) THEN
C          NSBL=20
C          SWATH=18.2882
C          UO=53.64
C          NUMS=NUMSS
C          SFAC=1.0
C          IF (UD.DSD(1).BASICTYP.EQ.0) THEN
C            SFACB=-0.5
C          ELSEIF (UD.DSD(1).BASICTYP.EQ.1) THEN
C            SFACB=0.0
C          ELSE
C            SFACB=0.5
C          ENDIF
C        ELSEIF (UD.APPLMETH.EQ.1) THEN
C          NSBL=UD.GA.NUMSWATH
C          UO=2.235
C          ITYPE=UD.GA.BASICTYP
C          XDWND=UD.CTL.MAXDWND
C          CALL AGGRNX(ITYPE,1,XDWND,NSBL,-1,NUMS,YN,ZN)
C          SWATH=SSBLS
C        ELSE
C          NSBL=UD.OA.ENDTROW
C          UO=2.235
C          ITYPE=UD.OA.BASICTYP
C          XDWND=UD.CTL.MAXDWND
C          IBROW=UD.OA.BEGTROW
C          CALL AGORCX(ITYPE,1,XDWND,IBTROW,NSBL,-1,NUMS,YN,ZN)
C          SWATH=SSBLS
C        ENDIF
C        SDISP=0.0
C        ACTIVE=XACT*UD.SM.FLOWRATE
C        WINDSP=4.47
C        WINDDR=0.0
C      ELSE
        IBOOM=UD.CTL.HALFBOOM
        NSBL=UD.CTL.NUMLINES
        IF (IBOOM.EQ.1) NSBL=NSBL+1
C        IF (UD.CTL.SWTYPE.EQ.0) THEN
          SWATH=UD.CTL.SWATHWID
C        ELSE
C          SWATH=2.0*UD.AC.SEMISPAN*UD.CTL.SWATHWID
C        ENDIF
C        ISDTYP=UD.CTL.SDTYPE
C        IF (ISDTYP.EQ.0) THEN
C          SDISP=-UD.CTL.SDVALUE
C        ELSEIF (ISDTYP.EQ.1) THEN
C          SDISP=UD.CTL.SDVALUE
C        ELSEIF (ISDTYP.EQ.2) THEN
           SDISP=-UD.CTL.SDVALUE/SWATH
C        ELSE
C          SDISP=0.5*(1-IBOOM)
C        ENDIF
        UO=UD.AC.TYPSPEED
        NUMS=NUMSS
        SFAC=SDISP+0.5*(1+IBOOM)
C        IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
C        SFAC=SFAC+SDISP
        ACTIVE=UD.SM.ACFRAC*UD.SM.FLOWRATE*UD.SM.NONVGRAV
        WINDSP=UD.MET.WINDSPD
C        IF (UD.TIER.EQ.2) THEN
C          WINDDR=0.0
C        ELSE
          WINDDR=-UD.MET.WINDDIR-90.0
C        ENDIF
C      ENDIF
C
C  Set parameters for all swaths
C
      TTIME=XSTURN+XSLENG/UO+SWATH/WINDSP/COS(0.01745329*WINDDR)
      TANGL=TAN(0.01745329*WINDDR)
      DO NS=1,NSBL
C
C        IF (UD.APPLMETH.EQ.0) THEN
C          IF (UD.TIER.EQ.1) THEN
C            SSBLF=(NS-SFACB)*SWATH
C          ELSE
            SSBLF=(NS-SFAC)*SWATH
C          ENDIF
          DO N=1,NUMS
            YN(N)=SGLD(N)
          ENDDO
          IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
            DO N=1,NUMS
              ZN(N)=0.5*SGLH(N)
            ENDDO
          ELSEIF (NS.EQ.NSBL.AND.IBOOM.EQ.1) THEN
            DO N=1,NUMS
              ZN(N)=0.5*(SGLV(N)-SGLH(N))
            ENDDO
          ELSE
            DO N=1,NUMS
              ZN(N)=SGLV(N)
            ENDDO
          ENDIF
C        ELSEIF (UD.APPLMETH.EQ.1) THEN
C          ITYPE=UD.GA.BASICTYP
C          XDWND=UD.CTL.MAXDWND
C          ISWTH=UD.GA.NUMSWATH
C          CALL AGGRNX(ITYPE,1,XDWND,ISWTH,-NS,NUMS,YN,ZN)
C        ELSE
C          ITYPE=UD.OA.BASICTYP
C          XDWND=UD.CTL.MAXDWND
C          IBROW=UD.OA.BEGTROW
C          IEROW=UD.OA.ENDTROW
C          CALL AGORCX(ITYPE,1,XDWND,IBROW,IEROW,-NS,NUMS,YN,ZN)
C        ENDIF
C
        TV(NS)=(NS-1)*TTIME
        XV(NS)=(XDIST+SSBLF)*TANGL
        IF (UD.APPLMETH.EQ.0) THEN
          XBEG=XDIST-0.5*XWIDE+(NS-SFAC)*SWATH
          XEND=XDIST+0.5*XWIDE+(NS-SFAC)*SWATH
        ELSE
          XBEG=XDIST-0.5*XWIDE
          XEND=XDIST+0.5*XWIDE
        ENDIF
C
        NB=1
10      IF (YN(NB+1).LE.XBEG) THEN
          NB=NB+1
          IF (NB.LT.NUMS) GOTO 10
        ENDIF
        NE=NB-1
20      IF (YN(NE+1).LE.XEND) THEN
          NE=NE+1
          IF (NE.LT.NUMS) GOTO 20
        ENDIF
        CBEG=AGINT(NUMS,YN,ZN,XBEG)
        CEND=AGINT(NUMS,YN,ZN,XEND)
        IF (NE.EQ.NB) THEN
          XAPPL=0.5*(XEND-XBEG)*(CBEG+CEND)
        ELSE
          XAPPL=0.5*(YN(NB+1)-XBEG)*(CBEG+ZN(NB+1))
          IF (NE.GT.NB+1) THEN
            DO N=NB+1,NE-1
              XAPPL=XAPPL+0.5*(YN(N+1)-YN(N))*(ZN(N)+ZN(N+1))
            ENDDO
          ENDIF
          XAPPL=XAPPL+0.5*(XEND-YN(NE))*(ZN(NE)+CEND)
        ENDIF
        XAPPL=XAPPL*(1.0-XRIPAR)
        CV(NS)=100000.0*UD.CTL.LINEREPS(NS)*ACTIVE*XAPPL/XWIDE/XDEEP
      ENDDO
      XDK=0.000693*XCHARG/XWIDE/XDEEP+XDECAY/86400.0
      XSPEED=XSRATE/XWIDE/XDEEP
      ATEM=0.5*XSLENG
      UTEM=XSPEED/ATEM
      DTEM=0.11*XSPEED*XWIDE*XWIDE/XDEEP/ATEM/ATEM
C
C  Develop single point solution
C
      IF (ISTYPE.EQ.0) THEN
        IF (INTYPE.EQ.0) THEN
          XSNGL(1)=XINPTS(1)
          NN=1
          DO NS=2,NSBL
            IF (TV(NS).LE.XSNGL(1)) NN=NS
          ENDDO
          IF (NN.EQ.1) THEN
            XSNGL(2)=XV(1)+XSPEED*(XSNGL(1)-TV(1))
            TEM=CV(1)*EXP(-XDK*(XSNGL(1)-TV(1)))
            IF (XSNGL(1)-TV(1).LT.1.0E-06) THEN
              XSNGL(3)=TEM
            ELSE
              XSNGL(3)=TEM*ERF(0.5/SQRT(DTEM*(XSNGL(1)-TV(1))))
            ENDIF
          ELSE
            XS=XV(1)+XSPEED*(XSNGL(1)-TV(1))
            XE=XV(NN)+XSPEED*(XSNGL(1)-TV(NN))
            NP=20*(NN-1)+1
            DX=(XE-XS)/(NP-1)
            XSNGL(2)=0.0
            XSNGL(3)=0.0
            DO N=1,NP
              CTEM=0.0
              XTEM=XS+(N-1)*DX
              DO NS=1,NN
                TEM=CV(NS)*EXP(-XDK*(XSNGL(1)-TV(NS)))
                CTEM=CTEM+TEM*AGSTR(XSNGL(1)-TV(NS),XTEM-XV(NS))
              ENDDO
              IF (CTEM.GT.XSNGL(3)) THEN
                XSNGL(2)=XTEM
                XSNGL(3)=CTEM
              ENDIF
            ENDDO
          ENDIF
        ELSE
          XSNGL(2)=XINPTS(1)
          NN=1
          DO NS=2,NSBL
            IF (XV(NS).LE.XSNGL(2)) NN=NS
          ENDDO
          IF (NN.EQ.1) THEN
            XSNGL(1)=TV(1)+(XSNGL(2)-XV(1))/XSPEED
            TEM=CV(1)*EXP(-XDK*AMAX1(XSNGL(1)-TV(1),0.0))
            IF (XSNGL(1)-TV(1).LT.1.0E-06) THEN
              XSNGL(3)=TEM
            ELSE
              XSNGL(3)=TEM*ERF(0.5/SQRT(DTEM*(XSNGL(1)-TV(1))))
            ENDIF
          ELSE
            TS=TV(1)+(XSNGL(2)-XV(1))/XSPEED
            TE=TV(NN)+(XSNGL(2)-XV(NN))/XSPEED
            NP=20*(NN-1)+1
            DT=(TE-TS)/(NP-1)
            XSNGL(1)=0.0
            XSNGL(3)=0.0
            DO N=1,NP
              CTEM=0.0
              TTEM=TS+(N-1)*DT
              DO NS=1,NN
                IF (TTEM.GE.TV(NS)) THEN
                  TEM=CV(NS)*EXP(-XDK*(TTEM-TV(NS)))
                  CTEM=CTEM+TEM*AGSTR(TTEM-TV(NS),XSNGL(2)-XV(NS))
                ENDIF
              ENDDO
              IF (CTEM.GT.XSNGL(3)) THEN
                XSNGL(1)=TTEM
                XSNGL(3)=CTEM
              ENDIF
            ENDDO
          ENDIF
        ENDIF
C        NSBL=0
C
C  Develop times for distance range
C
      ELSEIF (ISTYPE.EQ.1) THEN
        IF (INTYPE.EQ.0) THEN
          NN=1
          DO NS=2,NSBL
            IF (XV(NS).LE.XINPTS(1)) NN=NS
          ENDDO
          IF (NN.EQ.1) THEN
            TTEM=TV(1)+(XINPTS(1)-XV(1))/XSPEED
          ELSE
            TSTEM=TV(1)+(XINPTS(1)-XV(1))/XSPEED
            TETEM=TV(NN)+(XINPTS(1)-XV(NN))/XSPEED
            TTEM=AMAX1(TSTEM,TETEM)
          ENDIF
          TTEM=AMAX1(TTEM,0.0)
          IF (TTEM.GT.1.0) THEN
            TINC=0.0
          ELSE
            TINC=1.0-TTEM
          ENDIF
          TMIN=ALOG10(TTEM+TINC)
          NN=1
          DO NS=2,NSBL
            IF (XV(NS).LE.XINPTS(2)) NN=NS
          ENDDO
          IF (NN.EQ.1) THEN
            TTEM=TV(1)+(XINPTS(2)-XV(1))/XSPEED
          ELSE
            TSTEM=TV(1)+(XINPTS(2)-XV(1))/XSPEED
            TETEM=TV(NN)+(XINPTS(2)-XV(NN))/XSPEED
            TTEM=AMAX1(TSTEM,TETEM)
          ENDIF
          TTEM=AMAX1(TTEM,0.0)
          TMAX=ALOG10(TTEM+TINC)
          XAUTO(1)=10.0**(0.8*TMIN+0.2*TMAX)-TINC
          XAUTO(2)=10.0**(0.6*TMIN+0.4*TMAX)-TINC
          XAUTO(3)=10.0**(0.4*TMIN+0.6*TMAX)-TINC
          XAUTO(4)=10.0**(0.2*TMIN+0.8*TMAX)-TINC
          NAUTO=4
        ELSE
          NAUTO=0
          DO I=1,INTYPE
            IAUTO=0
            DO NS=1,NSBL
              TSTEM=TV(NS)+AMAX1(0.0,XINPTS(1)-XV(NS))/XSPEED
              TETEM=TV(NS)+AMAX1(0.0,XINPTS(2)-XV(NS))/XSPEED
              IF (TSTEM.LE.XINPTS(I+2).AND.XINPTS(I+2).LE.TETEM)
     $          IAUTO=IAUTO+1
            ENDDO
            IF (IAUTO.NE.0) THEN
              NAUTO=NAUTO+1
              XAUTO(NAUTO)=XINPTS(I+2)
            ENDIF
          ENDDO
          IF (NAUTO.EQ.0) THEN
            NSBL=0
            RETURN
          ENDIF
        ENDIF
C
        DO I=1,NAUTO
          NN=1
          DO NS=2,NSBL
            IF (TV(NS).LE.XAUTO(I)) NN=NS
          ENDDO
          XSTEM=XV(1)+XSPEED*(XAUTO(I)-TV(1))
          XETEM=XV(NN)+XSPEED*(XAUTO(I)-TV(NN))
          IF (XSTEM.LE.XETEM) THEN
            XS=XSTEM
            XE=XETEM
            IS=1
            IE=NN
            IA=1
          ELSE
            XS=XETEM
            XE=XSTEM
            IS=NN
            IE=1
            IA=-1
          ENDIF
          IF (NN.EQ.1) THEN
            DX=0.0
          ELSE
            DX=(XE-XS)/(NN-1)
          ENDIF
          XMIN=XINPTS(1)
          IF (XMIN.LT.XE) THEN
30          IF (XS.LT.XMIN) THEN
              XS=XS+DX
              IS=IS+IA
              GO TO 30
            ENDIF
          ENDIF
          XMAX=XINPTS(2)
          IF (XMAX.GT.XS) THEN
40          IF (XE.GT.XMAX) THEN
              XE=XE-DX
              IE=IE-IA
              GO TO 40
            ENDIF
          ENDIF
          NNN=0
          CALL AGSCL(XAUTO(I)-TV(IS),XS-XV(IS),2,
     $               2.0*XS-XMIN-XV(IS),NP,NA,AV)
          DO N=NA,1,-1
            NNN=NNN+1
            YYVEC(NNN,I)=2.0*XS-AV(N)-XV(IS)
          ENDDO
          IT=IA*(IE-IS+IA)
          DO II=2,IT
            XH=XS+0.5*DX
            CALL AGSCL(XAUTO(I)-TV(IS),XS-XV(IS),2,XH-XV(IS),NP,NA,AV)
            DO N=2,NA
              NNN=NNN+1
              YYVEC(NNN,I)=AV(N)+XV(IS)
            ENDDO
            XS=XS+DX
            IS=IS+IA
            CALL AGSCL(XAUTO(I)-TV(IS),XS-XV(IS),2,
     $                 2.0*XS-XH-XV(IS),NP,NA,AV)
            DO N=NA-NP,1,-1
              NNN=NNN+1
              YYVEC(NNN,I)=2.0*XS-AV(N)-XV(IS)
            ENDDO
          ENDDO
          CALL AGSCL(XAUTO(I)-TV(IS),XS-XV(IS),2,XMAX-XV(IS),NP,NA,AV)
          DO N=2,NA
            NNN=NNN+1
            YYVEC(NNN,I)=AV(N)+XV(IS)
          ENDDO
          NNVEC(I)=NNN
          DO N=1,NNN
            CTEM=0.0
            DO NS=1,NN
              TEM=CV(NS)*EXP(-XDK*(XAUTO(I)-TV(NS)))
              CTEM=CTEM+TEM*AGSTR(XAUTO(I)-TV(NS),YYVEC(N,I)-XV(NS))
            ENDDO
            CCVEC(N,I)=CTEM
          ENDDO
        ENDDO
C        NSBL=0
C
C  Develop distances for time range
C
      ELSE
        IF (INTYPE.EQ.0) THEN
          NN=1
          DO NS=2,NSBL
            IF (TV(NS).LE.XINPTS(1)) NN=NS
          ENDDO
          IF (NN.EQ.1) THEN
            XTEM=XV(1)+XSPEED*(XINPTS(1)-TV(1))
          ELSE
            XSTEM=XV(1)+XSPEED*(XINPTS(1)-TV(1))
            XETEM=XV(NN)+XSPEED*(XINPTS(1)-TV(NN))
            XTEM=AMAX1(XSTEM,XETEM)
          ENDIF
          IF (XTEM.GT.1.0) THEN
            XINC=0.0
          ELSE
            XINC=1.0-XTEM
          ENDIF
          XMIN=ALOG10(XTEM+XINC)
          NN=1
          DO NS=2,NSBL
            IF (TV(NS).LE.XINPTS(2)) NN=NS
          ENDDO
          IF (NN.EQ.1) THEN
            XTEM=XV(1)+XSPEED*(XINPTS(2)-TV(1))
          ELSE
            XSTEM=XV(1)+XSPEED*(XINPTS(2)-TV(1))
            XETEM=XV(NN)+XSPEED*(XINPTS(2)-TV(NN))
            XTEM=AMAX1(XSTEM,XETEM)
          ENDIF
          XMAX=ALOG10(XTEM+XINC)
          XAUTO(1)=10.0**(0.8*XMIN+0.2*XMAX)-XINC
          XAUTO(2)=10.0**(0.6*XMIN+0.4*XMAX)-XINC
          XAUTO(3)=10.0**(0.4*XMIN+0.6*XMAX)-XINC
          XAUTO(4)=10.0**(0.2*XMIN+0.8*XMAX)-XINC
          NAUTO=4
        ELSE
          NAUTO=0
          DO I=1,INTYPE
            IAUTO=0
            DO NS=1,NSBL
              XSTEM=XV(NS)+XSPEED*AMAX1(0.0,XINPTS(1)-TV(NS))
              XETEM=XV(NS)+XSPEED*AMAX1(0.0,XINPTS(2)-TV(NS))
              IF (XSTEM.LE.XINPTS(I+2).AND.XINPTS(I+2).LE.XETEM)
     $          IAUTO=IAUTO+1
            ENDDO
            IF (IAUTO.NE.0) THEN
              NAUTO=NAUTO+1
              XAUTO(NAUTO)=XINPTS(I+2)
            ENDIF
          ENDDO
          IF (NAUTO.EQ.0) THEN
            NSBL=0
            RETURN
          ENDIF
        ENDIF
C
        IAUTO=0
        DO I=1,NAUTO
          NN=1
          DO NS=2,NSBL
            IF (XV(NS).LE.XAUTO(I)) NN=NS
          ENDDO
          TSTEM=AMAX1(TV(1)+(XAUTO(I)-XV(1))/XSPEED,0.0)
          TETEM=AMAX1(TV(NN)+(XAUTO(I)-XV(NN))/XSPEED,0.0)
          IF (TSTEM.LE.TETEM) THEN
            TS=TSTEM
            TE=TETEM
            IS=1
            IE=NN
            IA=1
          ELSE
            TS=TETEM
            TE=TSTEM
            IS=NN
            IE=1
            IA=-1
          ENDIF
          IF (NN.EQ.1) THEN
            DT=0.0
          ELSE
            DT=(TE-TS)/(NN-1)
          ENDIF
          TMIN=XINPTS(1)
          IF (TMIN.LT.TE) THEN
50          IF (TS.LT.TMIN) THEN
              TS=TS+DT
              IS=IS+IA
              GO TO 50
            ENDIF
          ENDIF
          TMAX=XINPTS(2)
          IF (TMAX.GT.TS) THEN
60          IF (TE.GT.TMAX) THEN
              TE=TE-DT
              IE=IE-IA
              GO TO 60
            ENDIF
          ENDIF
          NNN=0
          CALL AGSCL(TS-TV(IS),XAUTO(I)-XV(IS),1,
     $               2.0*TS-TMIN-TV(IS),NP,NA,AV)
          DO N=NA,1,-1
            NNN=NNN+1
            YYVEC(NNN,I)=2.0*TS-AV(N)-TV(IS)
          ENDDO
          IT=IA*(IE-IS+IA)
          DO II=2,IT
            TH=TS+0.5*DT
            CALL AGSCL(TS-TV(IS),XAUTO(I)-XV(IS),1,TH-TV(IS),NP,NA,AV)
            DO N=2,NA
              NNN=NNN+1
              YYVEC(NNN,I)=AV(N)+TV(IS)
            ENDDO
            TS=TS+DT
            IS=IS+IA
            CALL AGSCL(TS-TV(IS),XAUTO(I)-XV(IS),1,
     $                 2.0*TS-TH-TV(IS),NP,NA,AV)
            DO N=NA-NP,1,-1
              NNN=NNN+1
              YYVEC(NNN,I)=2.0*TS-AV(N)-TV(IS)
            ENDDO
          ENDDO
          CALL AGSCL(TS-TV(IS),XAUTO(I)-XV(IS),1,TMAX-TV(IS),NP,NA,AV)
          DO N=2,NA
            NNN=NNN+1
            YYVEC(NNN,I)=AV(N)+TV(IS)
          ENDDO
          NNVEC(I)=NNN
          CMAX=0.0
          DO N=1,NNN
            CTEM=0.0
            DO NS=1,NN
              IF (YYVEC(N,I).GE.TV(NS)) THEN
                TEM=CV(NS)*EXP(-XDK*(YYVEC(N,I)-TV(NS)))
                CTEM=CTEM+TEM*AGSTR(YYVEC(N,I)-TV(NS),XAUTO(I)-XV(NS))
              ENDIF
            ENDDO
            CCVEC(N,I)=CTEM
            CMAX=AMAX1(CMAX,CCVEC(N,I))
          ENDDO
          CMIN=0.001*CMAX
          IF (CCVEC(1,I).LT.CMIN.AND.CCVEC(NNN,I).LT.CMIN) IAUTO=IAUTO+1
          RAUTO(I)=XSRATE+XAUTO(I)*XCHARG/1000.0
        ENDDO
C        IF (IAUTO.NE.NAUTO) NSBL=0
      ENDIF
      RETURN
      END
C**AGSCL
      SUBROUTINE AGSCL(T,X,LFL,YMAXX,NDUP,NPTS,YV)
C
C  AGSCL evaluates the scale size
C
C  T      - Time (s)
C  X      - Distance (m)
C  LFL    - Operations flag: 1 = time; 2 = distance
C  YMAXX  - Maximum time / distance
C  NDUP   - Duplicate point flag: 1 = duplicate; 0 = no duplicate
C  NPTS   - Number of points
C  YV     - Resulting scale values
C
      DIMENSION YV(2)
C
      COMMON /STRM/ ATEM,DTEM,UTEM
C
      IF (LFL.EQ.1) THEN
        YS=T
      ELSE
        YS=X
      ENDIF
      IF (YS.GT.1.0) THEN
        YINC=0.0
      ELSE
        YINC=1.0-YS
      ENDIF
      YE=ALOG10(YMAXX+YINC)
      DY=0.001*(YE-ALOG10(YS+YINC))
      DO N=1,1000
        YMAX=10.0**(YE-(N-1)*DY)-YINC
        IF (LFL.EQ.1) THEN
          IF (YMAX.GE.0.0) THEN
            CNEW=AGSTR(YMAX,X)
          ELSE
            CNEW=0.0
          ENDIF
        ELSE
          IF (T.GE.0.0) THEN
            CNEW=AGSTR(T,YMAX)
          ELSE
            CNEW=0.0
          ENDIF
        ENDIF
        IF (CNEW.GT.0.0001) GO TO 10
      ENDDO
C
10    NDUP=0
      IF (N.EQ.1) NDUP=1
      NPTS=11
      DY=(YMAX-YS)/(NPTS-1)
      IF (DY.LT.1.0) THEN
        NPTS=MAX0(2,IFIX(YMAX-YS+1))
        DY=(YMAX-YS)/(NPTS-1)
      ENDIF
      DO N=1,NPTS
        YV(N)=YS+(N-1)*DY
      ENDDO
      RETURN
      END
C**AGSTR
      FUNCTION AGSTR(T,X)
C
C  AGSTR evaluates the exact solution at the time and distance desired
C
C  T      - Time (s)
C  X      - Distance (m)
C
      COMMON /STRM/ ATEM,DTEM,UTEM
C
      YA=X/ATEM-UTEM*T
      DA=2.0*SQRT(DTEM*T)
      IF (T.LT.1.0E-06) THEN
        AGSTR=1.0
      ELSE
        AGSTR=0.5*(ERF((1.0-YA)/DA)+ERF((1.0+YA)/DA))
      ENDIF
      RETURN
      END
C**ERF
      FUNCTION ERF(XTEM)
C
C  Error function
C
      TTEM=1.0/(1.0+0.47047*ABS(XTEM))
      ETEM=1.0-TTEM*(0.3480242+TTEM*(-0.0958798+TTEM*0.7478556))
     $     *EXP(-AMIN1(XTEM*XTEM,25.0))
      IF (XTEM.LT.0.0) ETEM=-ETEM
      ERF=ETEM
      RETURN
      END