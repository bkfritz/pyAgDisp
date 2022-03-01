C**AGGROR
C  Continuum Dynamics, Inc.
C  Version 8.29 06/16/16
C
      SUBROUTINE AGGROR(ITYPE,XDWND,ISWTH,SWATH,IDEP,NPTS,YV,DV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGGROR
!MS$ATTRIBUTES REFERENCE :: ITYPE
!MS$ATTRIBUTES REFERENCE :: XDWND
!MS$ATTRIBUTES REFERENCE :: ISWTH
!MS$ATTRIBUTES REFERENCE :: SWATH
!MS$ATTRIBUTES REFERENCE :: IDEP
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: DV
C
C  AGGROR transfers ground and orchard deposition profiles
C
C  ITYPE  - Type: 0 = Ground sprayer Fine/High
C                 1 = Ground sprayer Fine/Low
C                 2 = Ground sprayer Medium/High
C                 3 = Ground sprayer Medium/Low
C                 4 = Orchard sprayer High
C                 5 = Orchard sprayer Low
C  XDWND  - Maximum downwind direction (m)
C  ISWTH  - Number of swaths
C  SWATH  - Swath width (m)
C  IDEP   - Deposition flag: 0 = deposition; 1 = pond-integrated deposition
C  NPTS   - Number of points in deposition array
C  YV     - Y distance array (m)
C  DV     - Deposition array (fraction applied)
C
      DIMENSION YV(2),DV(2)
C
      CALL AGGROX(ITYPE,XDWND,ISWTH,SWATH,IDEP,NPTS,YV,DV)
      RETURN
      END
C**AGGROX
      SUBROUTINE AGGROX(ITYPE,XDWND,ISWTH,SWATH,IDEP,NPTS,YV,DV)
C
      DIMENSION YV(2),DV(2),YYV(10),AV(6),BV(6)
C
      COMMON /TEMP/ NTEMP,YTEMP(3500),ZTEMP(3500)
C
      DATA YYV / 0.0, 0.03125, 0.0625, 0.125, 0.25,
     $           0.5, 1.0    , 1.5   , 2.0  , 3.0  /
      DATA AV  /  3.1350,  1.2168,  0.7700, -0.4479,  2.8019, -0.1397 /
      DATA BV  /  0.4610,  0.0166,  0.3646,  0.2775,  0.4846,  0.3703 /
C
C  Set limits
C
      I=ITYPE+1
      NPTS=XDWND/2.0+33
      NSWTH=50
      ISWMN=1
      ISWMX=MIN0(NSWTH,MAX0(1,ISWTH))
C
C  Ground and orchard sprayers
C
      DO N=1,NPTS
        IF (N.LE.10) THEN
          YV(N)=YYV(N)
        ELSE
          YV(N)=2.0*(N-9)
        ENDIF
        DV(N)=0.0
        DO NS=ISWMN,ISWMX
          YSD=YV(N)+(NS-1)*SWATH
          DV(N)=DV(N)+0.01*EXP(AV(I)-BV(I)*SQRT(YSD))
        ENDDO
      ENDDO
C
C  Compute pond-integrated deposition
C
      IF (IDEP.EQ.1) THEN
        CALL AGAVE(NPTS,YV,DV,NTEMP,YTEMP,ZTEMP)
        NPTS=NTEMP
        DO N=1,NPTS
          YV(N)=YTEMP(N)
          DV(N)=ZTEMP(N)
        ENDDO
      ELSE
        NPTS=NPTS-32
      ENDIF
      RETURN
      END