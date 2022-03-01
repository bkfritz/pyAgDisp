C**AGSGPS
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGSGPS(UD,NSFLTN,UDXBEG,UDYBEG,UDXEND,UDYEND,
     $                  NSFLG,NSGRP,LUDCON,
     $                  UDVCON,LUDNM,LUDDN,LUDPT,NPTS,DV,PV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSGPS
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: NSFLTN
!MS$ATTRIBUTES REFERENCE :: UDXBEG
!MS$ATTRIBUTES REFERENCE :: UDYBEG
!MS$ATTRIBUTES REFERENCE :: UDXEND
!MS$ATTRIBUTES REFERENCE :: UDYEND
!MS$ATTRIBUTES REFERENCE :: NSFLG
!MS$ATTRIBUTES REFERENCE :: NSGRP
!MS$ATTRIBUTES REFERENCE :: LUDCON
!MS$ATTRIBUTES REFERENCE :: UDVCON
!MS$ATTRIBUTES REFERENCE :: LUDNM
!MS$ATTRIBUTES REFERENCE :: LUDDN
!MS$ATTRIBUTES REFERENCE :: LUDPT
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: PV
C
C  AGSGPS initializes the GPS toolbox calculation
C
C  UD     - USERDATA data structure
C  NSFLTN - Number of flight lines
C  UDXBEG - X array of flight line beginning points (m)
C  UDYBEG - Y array of flight line beginning points (m)
C  UDXEND - X array of flight line ending points (m)
C  UDYEND - Y array of flight line ending points (m)
C  NSFLG  - Flight line group identifiers
C  NSGRP  - Group identifier to process
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
      DIMENSION NSFLG(2),UDXBEG(2),UDYBEG(2),UDXEND(2),UDYEND(2)
      DIMENSION UDVCON(2),DV(2),PV(2),REALWD(3),VTEM(5)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /RTRN/ CTH,STH   !temporarily here
      COMMON /RGRP/ ANGRP(500)
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
C  Process flight line information
C
      NSFLT=NSFLTN
      LFGPS=NSGRP
      IF (LFGPS.EQ.1) THEN
        DO NS=1,NSFLT
          LSFLG(NS)=NSFLG(NS)
          XSBEG(NS)=UDXBEG(NS)
          YSBEG(NS)=UDYBEG(NS)
          XSEND(NS)=UDXEND(NS)
          YSEND(NS)=UDYEND(NS)
          DXTEM=XSEND(NS)-XSBEG(NS)
          DYTEM=YSEND(NS)-YSBEG(NS)
          COSFL(NS)=(COS(ANGRP(NSFLG(NS)))*DXTEM
     $              -SIN(ANGRP(NSFLG(NS)))*DYTEM)
     $              /SQRT(ABS(DXTEM*DXTEM+DYTEM*DYTEM))
        ENDDO
      ENDIF
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