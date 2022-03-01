C**AGSWD
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.16 06/22/05
C
      SUBROUTINE AGSWD(WDIR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSWD
!MS$ATTRIBUTES REFERENCE :: WDIR
C
C  AGSWD stores the corrected wind directions
C
C  WDIRN  - Wind direction (deg)
C
      DIMENSION WDIR(500)
C
      INCLUDE 'AGCOMMON.INC'
C
C  SBIN
C
      IF (LFGPS.EQ.0) THEN
        QTMP=SIN(0.0174533*WDIR(1))
        DO NS=1,NSFLT
          QMOD(NS)=QTMP
        ENDDO
C
C  SGPS
C
      ELSE
        DO NS=1,NSFLT
          QMOD(NS)=SIN(0.0174533*WDIR(NS))
        ENDDO
      ENDIF
      RETURN
      END