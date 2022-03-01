C**AGWPLT
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGWPLT(PROB,NPTS,DEG,PCT10,PCT30,PCT50,PCT70,PCT90)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGWPLT
!MS$ATTRIBUTES REFERENCE :: PROB
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DEG
!MS$ATTRIBUTES REFERENCE :: PCT10
!MS$ATTRIBUTES REFERENCE :: PCT30
!MS$ATTRIBUTES REFERENCE :: PCT50
!MS$ATTRIBUTES REFERENCE :: PCT70
!MS$ATTRIBUTES REFERENCE :: PCT90
C
C  AGWPLT develops the wind rose plot curves
C
C  PROB   - Probability distribution array
C  NPTS   - Number of points
C  DEG    - Wind direction (deg from)
C  PCT10  - 10th percentile wind speed array (m/s)
C  PCT30  - 30th percentile wind speed array (m/s)
C  PCT50  - 50th percentile wind speed array (m/s)
C  PCT70  - 70th percentile wind speed array (m/s)
C  PCT90  - 90th percentile wind speed array (m/s)
C
      DIMENSION PROB(36,20),WS(20),CS(20)
      DIMENSION DEG(2),PCT10(2),PCT30(2),PCT50(2),PCT70(2),PCT90(2)
C
      NPTS=37
      DEG(1)=0.0
      DO N=1,36
        WS(1)=1.0
        CS(1)=0.0
        DEG(N+1)=10.0*N
        DO NN=2,20
          WS(NN)=NN
          CS(NN)=CS(NN-1)+PROB(N,NN)
        ENDDO
        PCT10(N+1)=AGINT(20,CS,WS,0.1*CS(20))
        PCT30(N+1)=AGINT(20,CS,WS,0.3*CS(20))
        PCT50(N+1)=AGINT(20,CS,WS,0.5*CS(20))
        PCT70(N+1)=AGINT(20,CS,WS,0.7*CS(20))
        PCT90(N+1)=AGINT(20,CS,WS,0.9*CS(20))
      ENDDO
      PCT10(1)=PCT10(37)
      PCT30(1)=PCT30(37)
      PCT50(1)=PCT50(37)
      PCT70(1)=PCT70(37)
      PCT90(1)=PCT90(37)
      RETURN
      END