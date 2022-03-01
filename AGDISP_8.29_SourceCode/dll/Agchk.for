C**AGCHK
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGCHK(X,XMIN,XMAX,XXMIN,XXMAX,LFLL,LFLH,LFL,FAC,RV)
C
C  AGCHK checks limits on input values
C
C  X      - Input value
C  XMIN   - Minimum value
C  XMAX   - Maximum value
C  XXMIN  - Absolute minimum value
C  XXMAX  - Absolute maximum value
C  LFLL   - Low boundary limit (0 or 1)
C  LFLH   - High boundary limit (0 or 1)
C  LFL    - Error flag
C  FAC    - Units conversion factor
C  RV     - Value in user units
C
      DIMENSION RV(3)
C
      LFL=0
      RV(1)=FAC*X
      RV(2)=FAC*XMIN
      RV(3)=FAC*XMAX
      IF (X.LT.XMIN.OR.X.GT.XMAX) THEN
        LFL=1
C        IF (X.LT.XXMIN.OR.X.GT.XXMAX) THEN
C          LFL=2+IMAX0(LFLL,LFLH)
        IF (X.LT.XXMIN) THEN
          LFL=2+LFLL
        ELSEIF (X.GT.XXMAX) THEN
          LFL=2+LFLH
        ENDIF
        IF (LFL.GE.2) THEN
          RV(2)=FAC*XXMIN
          RV(3)=FAC*XXMAX
        ENDIF
      ENDIF
      RETURN
      END