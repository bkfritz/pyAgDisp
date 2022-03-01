C**AGSBIN
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGSBIN(UD,LUDBND,UDXBND,UDYBND,UDFDIR,LUDCON,
     $                  UDVCON,LUDNM,LUDDN,LUDPT,NPTS,DV,PV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSBIN
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: LUDBND
!MS$ATTRIBUTES REFERENCE :: UDXBND
!MS$ATTRIBUTES REFERENCE :: UDYBND
!MS$ATTRIBUTES REFERENCE :: UDFDIR
!MS$ATTRIBUTES REFERENCE :: LUDCON
!MS$ATTRIBUTES REFERENCE :: UDVCON
!MS$ATTRIBUTES REFERENCE :: LUDNM
!MS$ATTRIBUTES REFERENCE :: LUDDN
!MS$ATTRIBUTES REFERENCE :: LUDPT
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: PV
C
C  AGSBIN initializes the spray block details toolbox calculation
C
C  UD     - USERDATA data structure
C  LUDBND - Number of spray block points
C  UDXBND - X array of spray block points (m)
C  UDYBND - Y array of spray block points (m)
C  UDFDIR - Flight angle (deg from North)
C  LUDCON - Contour level flag (1 = auto)
C  UDVCON - Contour level array (in user units)
C  LUDNM  - Deposition numerator units flag (0-6)
C  LUDDN  - Deposition denominator units flag (0-5)
C  LUDPT  - Deposition component type (0-2)
C  NPTS   - Number of drop sizes
C  DV     - Drop size diameter array (micrometers)
C  PV     - Percentage completed array (%)
C
      CHARACTER*40 CHSTR
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD / USERDATA / UD
C
      DIMENSION UDXBND(2),UDYBND(2),UDVCON(2),DV(2),PV(2)
      DIMENSION REALWD(3),XBND(100),YBND(100),XEV(100),VTEM(5)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /RTRN/ CTH,STH
C
C  Initialize all variables
C
      CALL AGINIX(UD,0)
10    CALL AGREAX(1,IER,IWR,REALWD,CHSTR,JCHSTR)
      IF (IER.NE.4) GOTO 10
      CALL AGLIMX(NPTS,DV,PV)
C
C  Verify FLOW rate
C
      IF (LFLOW.EQ.1) THEN
        FCON=1.0
      ELSE
        FCON=0.006*UO*SWATH
      ENDIF
C
C  Set deposition units
C
      TEM=0.1*FLOWN*SWATH/FCON
      IF (LUDNM.EQ.0) THEN
        IIDEP=0
        LNM=0
      ELSE
        IIDEP=LUDPT+1
        LNM=LUDNM
      ENDIF
      IF (LNM.EQ.0) THEN
        TEMN=1.90986E+12
      ELSEIF (LNM.EQ.1) THEN
        TEMN=0.033814
      ELSEIF (LNM.EQ.2) THEN
        TEMN=0.00026417
      ELSEIF (LNM.EQ.3) THEN
        TEMN=0.002205*DENF
      ELSEIF (LNM.EQ.4) THEN
        TEMN=0.001
      ELSEIF (LNM.EQ.5) THEN
        TEMN=DENF
      ELSE
        TEMN=0.001*DENF
      ENDIF
      LDN=LUDDN
      IF (LDN.EQ.0) THEN
        TEMD=1550.0
      ELSEIF (LDN.EQ.1) THEN
        TEMD=10.76365
      ELSEIF (LDN.EQ.2) THEN
        TEMD=0.000247105
      ELSEIF (LDN.EQ.3) THEN
        TEMD=10000.0
      ELSEIF (LDN.EQ.4) THEN
        TEMD=1.0
      ELSE
        TEMD=0.0001
      ENDIF
      FFDEP=TEM*TEMN/TEMD
C
C  Set contour levels
C
      NCON=LUDCON
      IF (NCON.EQ.0) THEN
        CONMN=1.0
        DO N=1,5
          VCON(N)=AMAX1(UDVCON(N),0.0)
          IF (VCON(N).GT.0.0) CONMN=AMIN1(CONMN,VCON(N))
        ENDDO
        DO N=1,5
          VTEM(N)=0.0
          MM=0
          DO M=1,5
            IF (VCON(M).GT.VTEM(N)) THEN
              VTEM(N)=VCON(M)
              MM=M
            ENDIF
          ENDDO
          IF (MM.GT.0) VCON(MM)=-VCON(MM)
        ENDDO
        DO N=1,5
          VCON(N)=VTEM(N)
        ENDDO
      ENDIF
C
C  Initialize deposit arrays
C
      DO NN=1,NVAR
        DO N=1,NPTS
          XPOSV(NN,N)=0.0
          YPOSV(NN,N)=0.0
          SPRDV(NN,N)=0.0
          VOLRV(NN,N)=0.0
        ENDDO
      ENDDO
C
C  Process spray boundary information
C
      THETA=6.2831853*(90.0-UDFDIR)/360.0
      CTH=COS(THETA)
      STH=SIN(THETA)
C
      YMIN=1.0E+20
      YMAX=-1.0E+20
      DO NN=1,LUDBND
        XBND(NN)=-UDXBND(NN)*CTH-UDYBND(NN)*STH
        YBND(NN)=UDXBND(NN)*STH-UDYBND(NN)*CTH
        YMIN=AMIN1(YMIN,YBND(NN))
        YMAX=AMAX1(YMAX,YBND(NN))
      ENDDO
      XBND(LUDBND+1)=XBND(1)
      YBND(LUDBND+1)=YBND(1)
      SFAC=SDISP+0.5*(1+IBOOM)
C      IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
      NSFLT=MIN0(IFIX((YMAX-YMIN)/SWATH+SFAC),200)
      NSFLTN=NSFLT
      IF (SCW.LE.0.0) THEN
        DO NS=1,NSFLT
          YSFLT(NS)=YMAX-(NS-SFAC)*SWATH
        ENDDO
      ELSE
        DO NS=1,NSFLT
          YSFLT(NS)=YMIN+(NS-SFAC)*SWATH
        ENDDO
      ENDIF
C
      DO NS=1,NSFLT
        NEV=0
        DO NN=1,LUDBND
          Y=YSFLT(NS)
          IF ((Y.LE.YBND(NN).AND.Y.GE.YBND(NN+1)).OR.
     $        (Y.GE.YBND(NN).AND.Y.LE.YBND(NN+1))) THEN
            X=(XBND(NN)*(YBND(NN+1)-Y)+XBND(NN+1)*(Y-YBND(NN)))
     $        /(YBND(NN+1)-YBND(NN))
            NEV=NEV+1
            XEV(NEV)=X
          ENDIF
        ENDDO
C
        DO NJ=2,NEV
          X=XEV(NJ)
          DO NI=NJ-1,1,-1
            IF (XEV(NI).LE.X) GOTO 20
            XEV(NI+1)=XEV(NI)
          ENDDO
          NI=0
20        XEV(NI+1)=X
        ENDDO
        XSBEG(NS)=XEV(1)
        XSEND(NS)=XEV(2)
        IF (NEV.GT.2) THEN
          IF (NSFLTN+NEV/2-1.LE.200) THEN
            DO NJ=3,NEV,2
              NSFLTN=NSFLTN+1
              YSFLT(NSFLTN)=YSFLT(NS)
              XSBEG(NSFLTN)=XEV(NJ)
              XSEND(NSFLTN)=XEV(NJ+1)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      NSFLT=NSFLTN
      LFGPS=0
C
C  Fill error function array
C
      NERF=81
      YERF=-4.0
      DERF=0.1
      DO N=1,NERF
        Y=(N-41)*DERF
        ERFV(N)=ERF(Y)
      ENDDO
      RETURN
      END