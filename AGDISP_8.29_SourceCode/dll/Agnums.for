C**AGNUMS
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 12/16/13
C
      SUBROUTINE AGNUMS(XNSD,XCOV,XMEAN,XAPEF,XDDEP,XAIR,XEVAP,XCAN)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGNUMS
!MS$ATTRIBUTES REFERENCE :: XNSD
!MS$ATTRIBUTES REFERENCE :: XCOV
!MS$ATTRIBUTES REFERENCE :: XMEAN
!MS$ATTRIBUTES REFERENCE :: XAPEF
!MS$ATTRIBUTES REFERENCE :: XDDEP
!MS$ATTRIBUTES REFERENCE :: XAIR
!MS$ATTRIBUTES REFERENCE :: XEVAP
!MS$ATTRIBUTES REFERENCE :: XCAN
C
C  AGNUMS transfers results back to the numerics screen
C
C  XNSD   - Swath displacement (m)
C  XCOV   - COV in spray block
C  XMEAN  - Mean deposition in spray block (fraction applied)
C  XAPEF  - Application efficiency (%)
C  XDDEP  - Percentage downwind drift
C  XAIR   - Percentage airborne at end of calculations
C  XEVAP  - Percentage evaporated
C  XCAN   - Percentage canopy deposition
C
      INCLUDE 'AGCOMMON.INC'
C
      XNSD=SWDISP
      XCOV=SBCOV
      XMEAN=SBMEAN
C
      NF=0
      DO N=1,NSWTH
        NF=NF+NFREP(N)
      ENDDO
      XNF=FLOAT(NSWTH)/NF
C
      XCAN=AMAX1(100.0*CDEPS,0.0)
      XDDEP=AMAX1(100.0*ALEFT*XNF/MAX0(NSWTH,1)/AMAX1(SWATH,1.0),0.0)
      XAIR=AMAX1(100.0*YDRFT/MAX0(NSWTH,1),0.0)
      XAPEF=100.0-XAIR-XDDEP-XCAN
      IF (XAPEF.LT.0.0) THEN
        XAPEF=0.0
        XCAN=AMAX1(100.0-XAIR-XDDEP,0.0)
      ENDIF
      XEVAP=100.0*EFRAC
      RETURN
      END