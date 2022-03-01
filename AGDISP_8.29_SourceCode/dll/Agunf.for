C**AGUNF
C  Continuum Dynamics, Inc.
C  AGDISP Version 1.08 03/01/00
C
      FUNCTION AGUNF(NB,YB,DB,DV,YP)
C
C  AGUNF interpolates within a uniform array
C
C  NB     - Length of DV array
C  YB     - Y beginning point
C  DB     - Y increment between points
C  DV     - Dependent array
C  YP     - Y value to return DV answer
C
      DIMENSION DV(2)
C
      NP=IFIX((YP-YB)/DB)+1
      IF (NP.LT.1) THEN
        AGUNF=DV(1)
      ELSEIF (NP.LT.NB) THEN
        AGUNF=(DV(NP)*(YB+NP*DB-YP)+DV(NP+1)*(YP-YB-(NP-1)*DB))/DB
      ELSE
        AGUNF=DV(NB)
      ENDIF
      RETURN
      END