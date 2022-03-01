C AGDISPexportResults
C Continuum Dynamics, Inc.
C SCIPUFF Implementation: 8.27 04/01/12
C
      SUBROUTINE AGDISPexportResults(AGDISPoutput)
!MS$ATTRIBUTES DLLEXPORT :: AGDISPexportResults
C
C PURPOSE: export results to SCIPUFF
C
      TYPE AGDISPdropletData
        SEQUENCE
        REAL X,Y,Z
        REAL SPREAD
        REAL DI,DF,DN
        REAL XNDROPS
      END TYPE AGDISPdropletData
C
      TYPE (AGDISPdropletData),DIMENSION(*)::AGDISPoutput
C
      INCLUDE 'AGCOMMON.INC'
C
      DO L=1,LDRP
        AGDISPoutput(L)%X=XLSPN(1,L)
        AGDISPoutput(L)%Y=XLSPN(2,L)
        AGDISPoutput(L)%Z=XLSPN(3,L)
        AGDISPoutput(L)%SPREAD=XLSPN(4,L)
        AGDISPoutput(L)%DI=ELSPN(1,L)
        AGDISPoutput(L)%DF=ELSPN(2,L)
        AGDISPoutput(L)%DN=ELSPN(3,L)
        AGDISPoutput(L)%XNDROPS=SLSPN(L)
      ENDDO
      RETURN
      END