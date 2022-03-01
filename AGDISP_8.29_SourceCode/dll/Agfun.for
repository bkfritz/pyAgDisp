C**AGFUN
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGFUN(LFLOW,UO,SWATH,XFLOW,ACTIVE,XDENF,LUFNM,LUFDN,
     $           FLUX)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGFUN
!MS$ATTRIBUTES REFERENCE :: LFLOW
!MS$ATTRIBUTES REFERENCE :: UO
!MS$ATTRIBUTES REFERENCE :: SWATH
!MS$ATTRIBUTES REFERENCE :: XFLOW
!MS$ATTRIBUTES REFERENCE :: ACTIVE
!MS$ATTRIBUTES REFERENCE :: XDENF
!MS$ATTRIBUTES REFERENCE :: LUFNM
!MS$ATTRIBUTES REFERENCE :: LUFDN
!MS$ATTRIBUTES REFERENCE :: FLUX
C
C  AGFUN sets the units on fraction of applied for flux
C
C  LFLOW  - Flag (1=L/ha; 2=L/min)
C  UO     - Flight speed (m/s)
C  SWATH  - Swath width (m)
C  XFLOW  - Flow rate (L/ha)
C  ACTIVE - Active fraction
C  XDENF  - Specific gravity
C  LUFNM  - Flux numerator units flag (1-9)
C  LUFDN  - Flux denominator units flag (0-5) !(0-4)
C  FLUX   - Multiplicative factor on flux
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
      TEM=0.1*XFLOW*ACTIVE/FCON
      IF (LUFNM.EQ.1) THEN
        TEMN=0.033814
      ELSEIF (LUFNM.EQ.2) THEN
        TEMN=0.00026417
      ELSEIF (LUFNM.EQ.3) THEN
        TEMN=0.002205*XDENF
      ELSEIF (LUFNM.EQ.4) THEN
        TEMN=0.001
      ELSEIF (LUFNM.EQ.5) THEN
        TEMN=XDENF
      ELSEIF (LUFNM.EQ.6) THEN
        TEMN=0.001*XDENF
      ELSEIF (LUFNM.EQ.7) THEN
        TEMN=1000.0*XDENF
      ELSEIF (LUFNM.EQ.8) THEN
        TEMN=1000000.0*XDENF
      ELSE
        TEMN=1000000000.0*XDENF
      ENDIF
      IF (LUFDN.EQ.0) THEN
        TEMD=1550.0
      ELSEIF (LUFDN.EQ.1) THEN
        TEMD=10.76365
      ELSEIF (LUFDN.EQ.2) THEN
        TEMD=0.000247105
      ELSEIF (LUFDN.EQ.3) THEN
        TEMD=10000.0
      ELSEIF (LUFDN.EQ.4) THEN
        TEMD=1.0
      ELSE
        TEMD=0.0001
      ENDIF
      FLUX=TEM*TEMN/TEMD
      RETURN
      END