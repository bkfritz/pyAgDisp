C**AGDSD
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGDSD(XMASS,EDN,DSDV)
C
C  AGDSD interpolates for the proper drop size category
C
C  XMASS  - Fraction of mass carried by drop
C  EDN    - Drop size (microns)
C  DSDV   - Drop size distribution
C
      DIMENSION DSDV(2)
C
      INCLUDE 'AGCOMMON.INC'
C
      N=0
10    N=N+1
      IF (EDN.LE.DSDC(N)) THEN
        DSDV(N)=DSDV(N)+XMASS
        RETURN
      ENDIF
      GOTO 10
      END