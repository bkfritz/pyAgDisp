C**AGINT
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      FUNCTION AGINT(N,YV,ZV,Y)
C
C  AGINT interpolates within arrays
C
C  N      - Length of YV and ZV arrays
C  YV     - Independent array
C  ZV     - Dependent array
C  Y      - Value in YV to return ZV answer
C
      DIMENSION YV(2),ZV(2)
C
      IF (Y.LE.YV(1)) THEN
        AGINT=ZV(1)
      ELSE
        DO I=2,N
          IF (Y.LE.YV(I)) THEN
            AGINT=(ZV(I-1)*(YV(I)-Y)+ZV(I)*(Y-YV(I-1)))/(YV(I)-YV(I-1))
            RETURN
          ENDIF
        ENDDO
        AGINT=ZV(N)
      ENDIF
      RETURN
      END