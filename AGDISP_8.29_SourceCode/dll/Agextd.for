C**AGEXTD
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.28 04/14/13
C
      SUBROUTINE AGEXTD(NPTS,YV,DV,XLENG,NTEMP,YTEMP,ZTEMP)
C
C  AGEXTD extends the deposition profile for pond integration
C
C  NPTS   - Number of points in deposition array
C  YV     - Downwind distance array (m)
C  DV     - Deposition array (fraction of applied)
C  XLENG  - Length of pond (m)
C  NTEMP  - Number of extended points in deposition array
C  YTEMP  - Extended downwind distance array (m)
C  ZTEMP  - Extended deposition array (fraction of applied)
C
      DIMENSION YV(2),DV(2),YTEMP(2),ZTEMP(2)
C
      COMMON /EXTD/ AA,BB  !WHY HERE
C
      SUMY=0.0
      SUMD=0.0
      SUMYY=0.0
      SUMYD=0.0
C
      NUM=-1
      J=0
10    NUM=NUM+1
      IF (DV(NPTS-NUM).EQ.0.0) GOTO 10
      J=J+1
      IF (J.LT.16) GOTO 10
C
      YMAG=YV(NPTS-NUM)
      DMAG=DV(NPTS-NUM)
      DO N=NPTS-NUM+1,NPTS
        IF (DV(N).GT.0.0) THEN
          AY=ALOG(YV(N)-YMAG)
          AD=ALOG(DV(N)/DMAG)
          SUMY=SUMY+AY
          SUMD=SUMD+AD
          SUMYY=SUMYY+AY*AY
          SUMYD=SUMYD+AY*AD
        ENDIF
      ENDDO
      BB=(SUMYD-SUMY*SUMD/16)/(SUMYY-SUMY*SUMY/16)
      AA=EXP((SUMD-BB*SUMY)/16)
      DO N=1,NPTS
        YTEMP(N)=YV(N)
        ZTEMP(N)=DV(N)
      ENDDO
      NTEMP=NPTS+AMAX1(XLENG,2.0)/2.0+1
      DO N=NPTS+1,NTEMP
        YTEMP(N)=YTEMP(N-1)+2.0
        ZTEMP(N)=DMAG*AA*(YTEMP(N)-YMAG)**BB
      ENDDO
      RETURN
      END