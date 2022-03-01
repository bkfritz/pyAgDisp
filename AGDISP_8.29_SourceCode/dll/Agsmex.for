C**AGSMEX
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGSMEX(NEXCT,NPTS,YV,DV,RAR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSMEX
!MS$ATTRIBUTES REFERENCE :: NEXCT
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: RAR
C
C  AGSMEX recovers the EXAMS data values
C
C  NEXCT  - EXAMS record to recover
C  NPTS   - Number of data points
C  YV     - Downwind distance array (m)
C  DV     - Deposition array (fraction applied)
C  RAR    - Realized application rate (fraction applied)
C
      DIMENSION YV(2),DV(2)
C
      INCLUDE 'AGSAMPLE.INC'
C
      COMMON /SBLK/ ZT(4900),ZL(4900)
      COMMON /TBLK/ YN(4900),ZN(4900)
C
      IDV=IXAMV(NEXCT)
      NPTS=NEXPT
      DO N=1,NEXPT
        YV(N)=YEXPT(N)
        DV(N)=EXAMV(N,NEXCT)
      ENDDO
      CALL AGSPLN(NPTS,YV,DV,NUMD,YN,ZN,NUMP,ZT,ZL,DTEM)
      RAR=1.0-DTEM/BLKSIZ
      IF (IDV.EQ.0) THEN
        DO N=1,NPTS
          DV(N)=0.0
        ENDDO
      ENDIF
      RETURN
      END