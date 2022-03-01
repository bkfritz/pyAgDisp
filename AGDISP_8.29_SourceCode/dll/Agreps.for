C**AGREPS
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.05 09/01/02
C
      SUBROUTINE AGREPS(NPTS,NREPV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGREPS
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: NREPV
C
C  AGREPS transfers the flight line reps back to the program
C
C  NPTS   - Number of flight lines
C  NREPV  - Number of reps of each flight line
C
      INTEGER*2 NPTS,NREPV(2)
C
      INCLUDE 'AGCOMMON.INC'
C
      NPTS=NSWTH
      DO NS=1,NSWTH
        NREPV(NS)=NFREP(NS)
      ENDDO
      RETURN
      END