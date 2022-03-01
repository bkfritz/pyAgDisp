C**AGFILL
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.02 12/28/01
C
      SUBROUTINE AGFILL(ITYPE,NOLD,DKV,XKV,NPTS,DDV,XXV,IER,
     $                  CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGFILL
!MS$ATTRIBUTES REFERENCE :: ITYPE
!MS$ATTRIBUTES REFERENCE :: NOLD
!MS$ATTRIBUTES REFERENCE :: DKV
!MS$ATTRIBUTES REFERENCE :: XKV
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DDV
!MS$ATTRIBUTES REFERENCE :: XXV
!MS$ATTRIBUTES REFERENCE :: IER
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGFILL sets up the user-entered drop size distribution for
C  reconstruction by the appropriate function
C
C  ITYPE  - Solution type: 0 = root-normal
C                          1 = Rosin-Rammler
C                          2 = log normal
C  NOLD   - Number of user-entered drop sizes
C  DKV    - User-entered drop size distribution
C  XKV    - User-entered volume fractions
C  NPTS   - Number of points in drop size distribution
C  DDV    - Drop size distribution array
C  XXV    - Volume fraction array
C  IER    - Error flag: 0 = no error -- result acceptable
C                       4 = error with character string only
C                       5 = information with character string
C  CHSTR  - Character string
C  JCHSTR - Length of character string
C
      CHARACTER*40 CHSTR
C
      DIMENSION DDV(32),XXV(32),DKV(32),XKV(32),CKV(32),TNV(32)
      DIMENSION DNV(32,3),XNV(32)
C
      DATA DNV /
     $    10.77,   16.73,   19.39,   22.49,   26.05,   30.21,   35.01,
     $    40.57,   47.03,   54.50,   63.16,   73.23,   84.85,   98.12,
     $   113.71,  131.73,  152.79,  177.84,  205.84,  238.45,  276.48,
     $   320.60,  372.18,  430.74,  498.91,  578.54,  670.72,  777.39,
     $   900.61, 1044.42, 1210.66, 1403.04 ,
     $    13.92,   20.84,   24.20,   28.15,   32.55,   37.72,   43.73,
     $    50.64,   58.76,   68.12,   78.99,   91.62,  106.30,  123.22,
     $   142.76,  165.29,  191.34,  221.91,  256.94,  298.07,  345.60,
     $   400.75,  464.83,  538.47,  623.61,  722.82,  838.00,  971.78,
     $  1126.51, 1305.88, 1513.71, 1754.16,
     $    13.61,   20.07,   24.06,   28.05,   33.09,   40.13,   48.11,
     $    57.15,   68.18,   80.15,   93.18,  110.30,  135.55,  165.45,
     $   195.38,  230.58,  275.76,  330.91,  390.77,  461.16,  551.51,
     $   661.81,  792.06,  942.26, 1122.97, 1343.57, 1604.07, 1904.48,
     $  2265.89, 2707.09, 3228.10, 3828.91 /
C
C  Compute cumulative volume fractions and determine VMD
C
      IER=0
      IF (NOLD.LT.2) THEN
        IER=4
        CHSTR='Insufficient data points for curvefit'
        JCHSTR=37
        RETURN
      ENDIF
      TNV(1)=DKV(1)**3
      CKV(1)=XKV(1)
      DO N=2,NOLD
        TNV(N)=DKV(N)**3
        CKV(N)=CKV(N-1)+XKV(N)
      ENDDO
      VMD=AGINT(NOLD,CKV,TNV,0.5)**0.33333
      IF (VMD.LT.25.0.OR.VMD.GT.2500.0) THEN
        IER=4
      ELSE
C
C  Check data consistency and run interpolate function
C
        DO N=2,NOLD
          IF (DKV(N)-DKV(N-1).LT.1.0) IER=4
        ENDDO
        IF (IER.EQ.0) THEN
C          IF (ITYPE.EQ.0) THEN
            I=1
            CALL AGKRN(NOLD,DKV,CKV,VMD,DNV(1,1),XNV,PSAVE,IER)
            IF (PSAVE.LT.0.99999) THEN
              I=2
              CALL AGKRN(NOLD,DKV,CKV,VMD,DNV(1,2),XNV,PSAVE,IER)
              IF (PSAVE.LT.0.99999) THEN
                I=3
                CALL AGKRN(NOLD,DKV,CKV,VMD,DNV(1,3),XNV,PSAVE,IER)
              ENDIF
            ENDIF
C          ELSEIF (ITYPE.EQ.1) THEN
C            I=1
C            CALL AGKRR(NOLD,DKV,CKV,DNV(1,1),XNV,PSAVE,IER)
C            IF (PSAVE.LT.0.99999) THEN
C              I=2
C              CALL AGKRR(NOLD,DKV,CKV,DNV(1,2),XNV,PSAVE,IER)
C              IF (PSAVE.LT.0.99999) THEN
C                I=3
C                CALL AGKRR(NOLD,DKV,CKV,DNV(1,3),XNV,PSAVE,IER)
C              ENDIF
C            ENDIF
C          ELSE
C            I=1
C            CALL AGKLN(NOLD,DKV,CKV,VMD,DNV(1,1),XNV,PSAVE,IER,L)
C            IF (PSAVE.LT.0.99999) THEN
C              I=2
C              CALL AGKLN(NOLD,DKV,CKV,VMD,DNV(1,2),XNV,PSAVE,IER,L)
C              IF (PSAVE.LT.0.99999) THEN
C                I=3
C                CALL AGKLN(NOLD,DKV,CKV,VMD,DNV(1,3),XNV,PSAVE,IER,L)
C              ENDIF
C            ENDIF
C          ENDIF
        ENDIF
      ENDIF
      IF (IER.NE.0) THEN
        CHSTR='Insufficient accuracy for curvefit'
        JCHSTR=34
C
C  Solution is acceptable
C
      ELSE
C        IF (ITYPE.EQ.2) THEN
C          IF (L.EQ.1) THEN
C            IER=5
C            CHSTR='Upper Limit Log Normal solution invoked'
C            JCHSTR=39
C          ELSEIF (L.EQ.2) THEN
C            IER=5
C            CHSTR='Bounded Log Normal solution invoked'
C            JCHSTR=35
C          ENDIF
C        ENDIF
C
C  Check drop sizes
C
        NNEW=0
        DO N=1,32
          IF (XNV(N).GT.0.00001) THEN
            NNEW=NNEW+1
            TNV(NNEW)=DNV(N,I)
            XNV(NNEW)=XNV(N)
          ENDIF
        ENDDO
      ENDIF
C
      IF (IER.EQ.0.OR.IER.EQ.5) THEN
        NPTS=NNEW
        DO N=1,NPTS
          DDV(N)=TNV(N)
          XXV(N)=XNV(N)
        ENDDO
      ENDIF
      RETURN
      END