C**AGRBR
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.29 06/16/16
C
      SUBROUTINE AGRBR(RBHT,POROS,RBESZ,IRBETP,XTREE,XDIST,RIF)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGRBR
!MS$ATTRIBUTES REFERENCE :: RBHT
!MS$ATTRIBUTES REFERENCE :: POROS
!MS$ATTRIBUTES REFERENCE :: RBESZ
!MS$ATTRIBUTES REFERENCE :: IRBETP
!MS$ATTRIBUTES REFERENCE :: XTREE
!MS$ATTRIBUTES REFERENCE :: XDIST
!MS$ATTRIBUTES REFERENCE :: RIF
C
C  AGRBR handles riparian barrier impaction
C
C  RBHT   - Barrier height (m)
C  POROS  - Barrier porosity
C  RBESZ  - Element size (m)
C  IRBETP - Element type (0=ribbon;1=cylinder;2=sphere)
C  XTREE  - Distance to tree line (m)
C  XDIST  - Distance to stream centerline (m)
C  RIF    - Riparian interception factor
C
      INCLUDE 'AGCOMMON.INC'
C
C  Check limits on all inputs
C
      XINCR=XDIST-XTREE
      PK=AMAX1(0.0,AMIN1(1.0,POROS))
      BSIZE=AMAX1(0.001,AMIN1(0.25,RBESZ))
      IF (XINCR.LE.0.0.OR.RBHT.LE.0.0.OR.PK.EQ.1.0) THEN
        RIF=0.0
        RETURN
      ENDIF
C
C  Volume average wind speed
C
      BS=(1.0+ZO/RBHT)*ALOG((RBHT+ZO)/ZO)-1.0-PSTAB
      UTEM=-BS*SCW
C
C  Collection efficiency
C
      PTKSUM=0.0
      DO NN=1,NDSD
        DTEM=DSDC(NN)
        DCUT=DTEM*(1.0-VFRAC)**0.33333
        DENC=((DTEM**3-DCUT**3)*DENF+DCUT**3*DENN)/DTEM**3
        STK=DENC*DTEM*DTEM*UTEM/BSIZE/1600.0
        CALL FSEFF(IRBETP,STK,EFF)
        PTK=1.0-EFF*(1.0-PK)
        PTKSUM=PTKSUM+PTK*DSVP(NN)
      ENDDO
C
      PTKSUM=AMIN1(1.0,AMAX1(0.0,PTKSUM))
      RTEM=1.0-1.6308*PTKSUM+0.6308*PTKSUM*PTKSUM
      RTEM=AMIN1(1.0,AMAX1(0.0,RTEM))
      RIF=RTEM*AMAX1(0.0,1.0-0.2*XINCR/RBHT)
      RETURN
      END