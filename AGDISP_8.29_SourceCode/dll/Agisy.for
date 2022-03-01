C**AGISY
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGISY(LSTAB,SO,XY,C,D,HM)
C
C  AGISY determines the ISC coefficients for Y
C
C  LSTAB  - Stability identifier
C  SO     - Initial spread (m)
C  XY     - Virtual distance (m)
C  C      - Power law coefficient
C  D      - Power law coefficient
C  HM     - Surface layer mixing height (m)
C
      DIMENSION CSDC(6),DSDC(6),PSDC(6),QSDC(6),HMIX(6)
C
      DATA CSDC / 41.6671,30.8333,20.0   ,13.3333,10.0   , 6.6667 /
      DATA DSDC /  2.5334, 1.8096, 1.0857, 0.7238, 0.5429, 0.3619 /
      DATA PSDC /  2.4703, 3.7452, 6.3651,10.0982,13.9755,21.6128 /
      DATA QSDC /  1.1236, 1.1086, 1.0905, 1.0881, 1.0858, 1.0881 /
C
      DATA HMIX /  1500.0, 1000.0,  500.0,  300.0,  200.0,  100.0 /
C
      C=CSDC(LSTAB)
      D=DSDC(LSTAB)
      P=PSDC(LSTAB)
      Q=QSDC(LSTAB)
      XY=P*SO**Q
      HM=HMIX(LSTAB)
      RETURN
      END