C**AGCON
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.07 06/18/03
C
      SUBROUTINE AGCON(TNEW,ANS)
C
C  AGCON computes the continuous ground deposition pattern
C
C  TNEW   - Time
C  ANS    - Trajectory results array
C
      DIMENSION ANS(4,60),XV(3)
C
      INCLUDE 'AGCOMMON.INC'
C
C  ISW =  1  Active drop above the surface
C         0  Drop hits the surface and penetrates
C        -1  Four standard deviations below the surface and finish
C
      IF (TNEW.GE.0.0) THEN
        DTE=TNEW-TOLD
        DO N=1,NVAR
          IF (ISW(N).NE.0) THEN
            XNDEP(1,N)=ANS(1,N)
            XNDEP(2,N)=ANS(2,N)-ZREF
            XNDEP(3,N)=ANS(3,N)
            DSDEP(N)=AFRAC
          ELSE
            DO I=1,3
              XNDEP(I,N)=XNDEP(I,N)+DTE*DNDEP(I,N)
            ENDDO
          ENDIF
          IS=0
          IF (IDEPV(N).GE.0) THEN
            XV(1)=XNDEP(1,N)
            XV(2)=XNDEP(2,N)
            XV(3)=XNDEP(3,N)
            CALL AGDEP(XV,DNDEP(1,N),DTE,DSDEP(N),YDEPS,DDEPR,
     $                 NDEPS,TEMND*CMASS(N),ZDEPS,ZDEPH,IHALF(N),I)
            IF (I.EQ.0.AND.IDEPV(N).EQ.0) IS=1
          ENDIF
          IF (IS.EQ.1) IDEPV(N)=-1
          IF (ISW(N).LT.0.AND.IDEPV(N).GT.0) IDEPV(N)=0
        ENDDO
C
C  Extend deposition for active drops below the surface
C
      ELSE
        TIMEE=TOLD
        TMAXE=10.0*TIMEE
        DTEE=DTE
10      TIMEE=TIMEE+DTEE
        L=0
        DO N=1,NVAR
          IF (ISW(N).EQ.0) THEN
            DO I=1,3
              XNDEP(I,N)=XNDEP(I,N)+DTEE*DNDEP(I,N)
            ENDDO
            IF (IDEPV(N).EQ.0) THEN
              L=L+1
              XV(1)=XNDEP(1,N)
              XV(2)=XNDEP(2,N)
              XV(3)=XNDEP(3,N)
              CALL AGDEP(XV,DNDEP(1,N),DTEE,DSDEP(N),YDEPS,DDEPR,
     $                   NDEPS,TEMND*CMASS(N),ZDEPS,ZDEPH,IHALF(N),I)
              IF (I.EQ.0) IDEPV(N)=-1
            ENDIF
          ENDIF
        ENDDO
        DTEE=1.1*DTEE
        IF (L.NE.0.AND.TIMEE.LT.TMAXE) GOTO 10
      ENDIF
      RETURN
      END