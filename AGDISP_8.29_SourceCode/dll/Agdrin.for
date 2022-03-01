C**AGDRIN
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.25 07/29/10
C
      SUBROUTINE AGDRIN(NNDISC,ITDISC,XXDISC,YYDISC,ZZDISC,
     $                  XXNORM,YYNORM,ZZNORM,ESDISC)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGDRIN
!MS$ATTRIBUTES REFERENCE :: NNDISC
!MS$ATTRIBUTES REFERENCE :: ITDISC
!MS$ATTRIBUTES REFERENCE :: XXDISC
!MS$ATTRIBUTES REFERENCE :: YYDISC
!MS$ATTRIBUTES REFERENCE :: ZZDISC
!MS$ATTRIBUTES REFERENCE :: XXNORM
!MS$ATTRIBUTES REFERENCE :: YYNORM
!MS$ATTRIBUTES REFERENCE :: ZZNORM
!MS$ATTRIBUTES REFERENCE :: ESDISC
C
C  AGDRIN sets up the discrete receptor calculation
C
C  NNDISC - Number of active discrete receptors
C  ITDISC - Receptor type array (1-4)
C  XXDISC - X location array of receptors (m)
C  YYDISC - Y location array of receptors (m)
C  ZZDISC - Z location array of receptors (m)
C  XXNORM - X outward normal array of receptor surfaces
C  YYNORM - Y outward normal array of receptor surfaces
C  ZZNORM - Z outward normal array of receptor surfaces
C  ESDISC - Element size of receptors (cm)
C
      DIMENSION ITDISC(2),XXDISC(2),YYDISC(2),ZZDISC(2)
      DIMENSION XXNORM(2),YYNORM(2),ZZNORM(2),ESDISC(2)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /RTRN/ CTH,STH
C
      IF (NNDISC.EQ.0) RETURN
      IIDIS=1
      NNDSR=NNDISC
      DO N=1,NNDSR
        ITDSR(N)=ITDISC(N)+1
        XXDSR(N)=-XXDISC(N)*CTH-YYDISC(N)*STH
        YYDSR(N)=XXDISC(N)*STH-YYDISC(N)*CTH
        ZZDSR(N)=ZZDISC(N)
        XNORM(N)=-XXNORM(N)*CTH-YYNORM(N)*STH
        YNORM(N)=XXNORM(N)*STH-YYNORM(N)*CTH
        ZNORM(N)=ZZNORM(N)
        ESDSR(N)=ESDISC(N)
        DPDSR(N)=0.0
      ENDDO
      RETURN
      END