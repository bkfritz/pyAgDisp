C**AGDAPT
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGDAPT(UD,NUMD,DEPD,DEPV,ISTYPE,INTYPE,
     $                  XPOND,XDEEP,XLENG,XLAND,XAPPL,XCONC,
     $                  NUMU,USRD,USRV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGDAPT
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: NUMD
!MS$ATTRIBUTES REFERENCE :: DEPD
!MS$ATTRIBUTES REFERENCE :: DEPV
!MS$ATTRIBUTES REFERENCE :: ISTYPE
!MS$ATTRIBUTES REFERENCE :: INTYPE
!MS$ATTRIBUTES REFERENCE :: XPOND
!MS$ATTRIBUTES REFERENCE :: XDEEP
!MS$ATTRIBUTES REFERENCE :: XLENG
!MS$ATTRIBUTES REFERENCE :: XLAND
!MS$ATTRIBUTES REFERENCE :: XAPPL
!MS$ATTRIBUTES REFERENCE :: XCONC
!MS$ATTRIBUTES REFERENCE :: NUMU
!MS$ATTRIBUTES REFERENCE :: USRD
!MS$ATTRIBUTES REFERENCE :: USRV
C
C  AGDAPT performs the deposition assessment calculations
C
C  UD     - USERDATA data structure
C  NUMD   - Number of points in deposition array
C  DEPD   - Downwind distance array (m)
C  DEPV   - Deposition array (fraction applied)
C  ISTYPE - Selection type: 0 = Aquatic pond model
C                           1 = Terrestrial point model
C                           2 = Terrestrial area model
C  INTYPE - Input type: 0 = Downwind distance known
C                       1 = Fraction applied known
C                       2 = L/ha known
C  XPOND  - Length of pond (m)
C  XDEEP  - Pond depth (m)
C  XLENG  - Length of area (m)
C  XLAND  - Distance to pond/point/area (m)
C  XAPPL  - Average deposition (fraction applied)
C  XCONC  - Average concentration (ng/L)
C  NUMU   - Number of points in averaged deposition array
C  USRD   - Downwind distance array (m)
C  USRV   - Averaged deposition array (fraction applied)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      DIMENSION DEPD(2),DEPV(2),USRD(2),USRV(2)
C
      COMMON /TEMP/ NTEMP,YTEMP(3500),ZTEMP(3500)
      COMMON /TBLK/ YN(4900),ZN(4900)
C
C  Test deposition type
C
      ACTIVE=UD.SM.ACFRAC*UD.SM.FLOWRATE*UD.SM.NONVGRAV
      IF (ISTYPE.EQ.1) THEN
        NUMU=0
        NTEMP=NUMD
        DO N=1,NTEMP
          YTEMP(N)=DEPD(N)
          ZTEMP(N)=DEPV(N)
        ENDDO
      ELSE
        IF (ISTYPE.EQ.0) THEN
          CALL AGEXTD(NUMD,DEPD,DEPV,XPOND,NNTEMP,YTEMP,ZTEMP)
          XTEM=XPOND
          IF (XDEEP.LE.0.01.OR.XDEEP.GT.100.0) ACTIVE=-1.0
        ELSE
          CALL AGEXTD(NUMD,DEPD,DEPV,XLENG,NNTEMP,YTEMP,ZTEMP)
          XTEM=XLENG
        ENDIF
        NUMU=NUMD
        NTEMP=NUMD
        DO N=1,NTEMP
          USRD(N)=YTEMP(N)
          NN=N
          XAVE=0.5*(YTEMP(NN+1)-YTEMP(NN))*(ZTEMP(NN+1)+ZTEMP(NN))
10        NN=NN+1
          IF (NN.LT.NNTEMP) THEN
            IF (YTEMP(NN+1).LT.DEPD(N)+XTEM) THEN
              XAVE=XAVE+0.5*(YTEMP(NN+1)-YTEMP(NN))
     $                     *(ZTEMP(NN+1)+ZTEMP(NN))
              GOTO 10
            ELSE
              DD=AGINT(NNTEMP,YTEMP,ZTEMP,DEPD(N)+XTEM)
              XAVE=XAVE+0.5*(DEPD(N)+XTEM-YTEMP(NN))*(DD+ZTEMP(NN))
            ENDIF
          ENDIF
          ZN(N)=XAVE/AMAX1(XTEM,0.1)
        ENDDO
        DO N=1,NTEMP
          ZTEMP(N)=ZN(N)
          USRV(N)=ZTEMP(N)
        ENDDO
        IF (XTEM.LT.0.1.OR.XTEM.GT.YTEMP(NTEMP)) ACTIVE=-1.0
      ENDIF
      X2=100000.0*ACTIVE
C
C  Convert to fraction applied
C
      IF (INTYPE.EQ.0) THEN
        IF (ACTIVE.LE.0.0.OR.XLAND.LT.0.0.OR.XLAND.GT.YTEMP(NTEMP)) THEN
          XAPPL=-1.0
        ELSE
          XAPPL=AGINT(NTEMP,YTEMP,ZTEMP,XLAND)
        ENDIF
      ELSEIF (INTYPE.EQ.2) THEN
        IF (ACTIVE.LE.0.0.OR.XCONC.LT.0.0) THEN
          XAPPL=-1.0
        ELSE
          XAPPL=XCONC*XDEEP/X2
        ENDIF
      ENDIF
C
C  Compute distance to fraction applied
C
      IF (INTYPE.NE.0) THEN
        TEMM=1.0
        DO N=1,NTEMP
          TEMM=AMIN1(TEMM,ZTEMP(N))
          ZTEMP(N)=-ZTEMP(N)
        ENDDO
        IF (XAPPL.LT.TEMM) THEN
          XLAND=-1.0
        ELSE
          XLAND=AGINT(NTEMP,ZTEMP,YTEMP,-XAPPL)
          ILAND=IFIX(XLAND)
          IF (XLAND.GT.FLOAT(ILAND)) ILAND=ILAND+1
          XLAND=FLOAT(ILAND)
        ENDIF
      ENDIF
C
C  Compute concentration
C
      IF (ISTYPE.EQ.0.AND.INTYPE.NE.2) THEN
        IF (ACTIVE.LE.0.0.OR.XAPPL.LT.0.0) THEN
          XCONC=-1.0
        ELSE
          XCONC=X2*XAPPL/XDEEP
        ENDIF
      ENDIF
      RETURN
      END