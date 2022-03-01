C**AGSCF
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.25 12/22/10
C
      SUBROUTINE AGSCF(IFLG,ASCF,BSCF,CSCF,NNVEC,YYVEC,DDVEC)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSCF
!MS$ATTRIBUTES REFERENCE :: IFLG
!MS$ATTRIBUTES REFERENCE :: ASCF
!MS$ATTRIBUTES REFERENCE :: BSCF
!MS$ATTRIBUTES REFERENCE :: CSCF
!MS$ATTRIBUTES REFERENCE :: NNVEC
!MS$ATTRIBUTES REFERENCE :: YYVEC
!MS$ATTRIBUTES REFERENCE :: DDVEC
C
C  AGSCF computes the desired sprayer deposition curve-fit pattern
C
C  IFLG   - Curve-fit option flag: 0=inverse; 1=exponential; 2=power
C  ASCF   - A coefficient (1/m,nondim,nondim)
C  BSCF   - B coefficient (nondim,1/sqrt(m),nondim)
C  CSCF   - C coefficient (nondim,nondim,nondim)
C  NNVEC  - Number of points
C  YYVEC  - Distance array (m)
C  DDVEC  - Deposition array (fraction of applied)
C
      DIMENSION YYTEM(3000),DDTEM(3000)
      DIMENSION YYVEC(3000),DDVEC(3000)
C
      INCLUDE 'AGCOMMON.INC'
C
      NNTEM=GRDMX/2.0+1
      DO NN=1,NNTEM
        YYTEM(NN)=(NN-1)*2.0
        IF (IFLG.EQ.0) THEN
          DDTEM(NN)=CSCF/(1.0+ASCF*YYTEM(NN))**BSCF
        ELSEIF (IFLG.EQ.1) THEN
          DDTEM(NN)=EXP(ASCF-BSCF*SQRT(ABS(YYTEM(NN))))+CSCF
        ELSE
          DDTEM(NN)=ASCF*YYTEM(NN)**BSCF+CSCF
        ENDIF
      ENDDO
C
      NNVEC=YGRID/2.0+1
      DO NN=1,NNVEC
        YYVEC(NN)=(NN-1)*2.0
        DDVEC(NN)=0.0
      ENDDO
C
      SFAC=0.5+SDISP
      DO NS=1,NSWTH
        DO NN=1,NNVEC
          Y=YYVEC(NN)+(NS-SFAC)*SWATH
          Z=AGINT(NNTEM,YYTEM,DDTEM,Y)
          DDVEC(NN)=DDVEC(NN)+NFREP(NS)*Z
        ENDDO
      ENDDO
C
      RETURN
      END