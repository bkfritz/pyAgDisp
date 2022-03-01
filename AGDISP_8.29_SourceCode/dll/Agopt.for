C**AGOPT
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGOPT
C
C  AGOPT optimizes the flight line reps
C
      INCLUDE 'AGCOMMON.INC'
C
      COMMON /TEMP/ NTEMP,YTEMP(3500),ZTEMP(3500)
      COMMON /TBLK/ YN(4900),ZN(4900)
C
      DO NS=1,NSWTH
        IF (NFREP(NS).GT.0) NFREP(NS)=1
      ENDDO
      DS=SWATH/(IFIX(SWATH/2.0)+1)
      SFAC=SDISP+0.5*(1+IBOOM)
C      IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
      NDS=IFIX((NSWTH-SDISP)*SWATH/DS)+1
      DO N=1,NDS
        YN(N)=-DS*(N-1)
        ZN(N)=0.0
      ENDDO
      N1=IFIX((1-SDISP)*SWATH/DS)+1
      N2=IFIX((2-SDISP)*SWATH/DS)+1
C
      NSWTM=NSWTH
      IF (IBOOM.EQ.1) NSWTM=NSWTH+1
      DO NS=1,NSWTM
        DO N=1,NDS
          Y=YN(N)+(NS-SFAC)*SWATH
          IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
            Z=0.5*AGINT(NDEPS,YDEPS,ZDEPH,Y)
          ELSEIF (NS.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
            Z=0.5*(AGINT(NDEPS,YDEPS,ZDEPS,Y)
     $        -AGINT(NDEPS,YDEPS,ZDEPH,Y))
          ELSE
            Z=AGINT(NDEPS,YDEPS,ZDEPS,Y)
          ENDIF
          ZN(N)=ZN(N)+NFREP(NS)*Z
        ENDDO
      ENDDO
      DO N=1,NDS
        IF (N.LE.N2) THEN
          ZTEMP(N)=ZN(N)
        ELSE
          NN=N
10        NN=NN-N2+N1
          IF (NN.GT.N2) GOTO 10
          ZTEMP(N)=ZN(NN)
        ENDIF
      ENDDO
C
      XSAVE=0.0
      DO N=1,NDS
        XSAVE=XSAVE+(ZN(N)-ZTEMP(N))**2
      ENDDO
C
C  Loop for improved spray block deposition
C
20    ICHG=0
      DO IS=2,NSWTH
        IF (NFREP(IS).GT.1) THEN
          ISAV=NFREP(IS)
          NFREP(IS)=NFREP(IS)-1
          DO N=1,NDS
            ZN(N)=0.0
          ENDDO
C
          DO NS=1,NSWTM
            DO N=1,NDS
              Y=YN(N)+(NS-SFAC)*SWATH
              IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
                Z=0.5*AGINT(NDEPS,YDEPS,ZDEPH,Y)
              ELSEIF (NS.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
                Z=0.5*(AGINT(NDEPS,YDEPS,ZDEPS,Y)
     $            -AGINT(NDEPS,YDEPS,ZDEPH,Y))
              ELSE
                Z=AGINT(NDEPS,YDEPS,ZDEPS,Y)
              ENDIF
              ZN(N)=ZN(N)+NFREP(NS)*Z
            ENDDO
          ENDDO
C
          XDIFF=0.0
          DO N=1,NDS
            XDIFF=XDIFF+(ZN(N)-ZTEMP(N))**2
          ENDDO
C
          IF (XDIFF.LT.XSAVE) THEN
            XSAVE=XDIFF
            ICHG=1
          ELSE
            NFREP(IS)=ISAV
          ENDIF
        ENDIF
C
        IF (NFREP(IS).GT.0) THEN
          ISAV=NFREP(IS)
          NFREP(IS)=NFREP(IS)+1
          DO N=1,NDS
            ZN(N)=0.0
          ENDDO
C
          DO NS=1,NSWTM
            DO N=1,NDS
              Y=YN(N)+(NS-SFAC)*SWATH
              IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
                Z=0.5*AGINT(NDEPS,YDEPS,ZDEPH,Y)
              ELSEIF (NS.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
                Z=0.5*(AGINT(NDEPS,YDEPS,ZDEPS,Y)
     $            -AGINT(NDEPS,YDEPS,ZDEPH,Y))
              ELSE
                Z=AGINT(NDEPS,YDEPS,ZDEPS,Y)
              ENDIF
              ZN(N)=ZN(N)+NFREP(NS)*Z
            ENDDO
          ENDDO
C
          XDIFF=0.0
          DO N=1,NDS
            XDIFF=XDIFF+(ZN(N)-ZTEMP(N))**2
          ENDDO
C
          IF (XDIFF.LT.XSAVE) THEN
            XSAVE=XDIFF
            ICHG=1
          ELSE
            NFREP(IS)=ISAV
          ENDIF
        ENDIF
      ENDDO
      IF (ICHG.EQ.1) GOTO 20
      RETURN
      END