C**AGSFL
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.12 09/25/04
C
      SUBROUTINE AGSFL(LACTFL,NSFLT,NSGRP,LUDSEG,UDXSEG,UDYSEG,
     $                 XSBEG,XSEND,YSBEG,YSEND,ANGLE,ANFLT)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSFL
!MS$ATTRIBUTES REFERENCE :: LACTFL
!MS$ATTRIBUTES REFERENCE :: NSFLT
!MS$ATTRIBUTES REFERENCE :: NSGRP
!MS$ATTRIBUTES REFERENCE :: LUDSEG
!MS$ATTRIBUTES REFERENCE :: UDXSEG
!MS$ATTRIBUTES REFERENCE :: UDYSEG
!MS$ATTRIBUTES REFERENCE :: XSBEG
!MS$ATTRIBUTES REFERENCE :: XSEND
!MS$ATTRIBUTES REFERENCE :: YSBEG
!MS$ATTRIBUTES REFERENCE :: YSEND
!MS$ATTRIBUTES REFERENCE :: ANGLE
!MS$ATTRIBUTES REFERENCE :: ANFLT
C
C  AGSFL computes the nominal flight line direction
C
C  The Cartesian coordinate system is (X,Y). GPS flight line increments are quizzed
C  to generate consistent flight line start and end locations, in a coordinate system
C  parallel and perpendicular to the nominal flight direction. These data can then be
C  sent to AGDISP to generate the deposition.
C
C  Output from AGDISP need to be inverse transformed with CTH and STH.
C
C  INPUTS:
C
C  LACTFL - Action flag: 0 = sum flight line info; 1 = transform flight lines
C  NSFLT  - Flight line number
C  NSGRP  - Group number
C  LUDSEG - Number of segments in flight line
C  UDXSEG - X grid array of segments in local coordinates
C  UDYSEG - Y grid array of segments in local coordinates
C
C  OUTPUTS:
C
C  XSBEG  - X starting location of flight line (transformed)
C  XSEND  - X ending location of flight line (transformed)
C  YSBEG  - Y starting location of flight line (transformed)
C  YSEND  - Y ending location of flight line (transformed)
C  ANGLE  - Nominal flight direction (radians)
C  ANFLT  - Flight direction for this group (radians)
C
      SAVE
C
      DIMENSION UDXSEG(2),UDYSEG(2),NNGRP(500)
C
      COMMON /RTRN/ CTH,STH
      COMMON /RGRP/ ANGRP(500)
C
      IF (LACTFL.EQ.0) THEN
C
C  Construct transformation angle
C
        IF (NSFLT.EQ.1) THEN
          ANGLE=0.0
          NSTOT=0
          DO NS=1,500
            ANGRP(NS)=0.0
            NNGRP(NS)=0
          ENDDO
        ENDIF
        XC=UDXSEG(1)
        YC=UDYSEG(1)
        XSBEG=UDXSEG(1)
        YSBEG=UDYSEG(1)
        XPTS=UDXSEG(1)
        YPTS=UDYSEG(1)
        SUMXX=0.0
        SUMXY=0.0
        DO N=2,LUDSEG
          XPTS=XPTS+UDXSEG(N)
          YPTS=YPTS+UDYSEG(N)
          SUMXX=SUMXX+(UDXSEG(N)-XC)*(UDXSEG(N)-XC)
          SUMXY=SUMXY+(UDXSEG(N)-XC)*(UDYSEG(N)-YC)
        ENDDO
        XSEND=UDXSEG(LUDSEG)
        YSEND=UDYSEG(LUDSEG)
        SUMN=(XPTS-LUDSEG*XC)*(YPTS-LUDSEG*YC)/LUDSEG
        SUMD=(XPTS-LUDSEG*XC)*(XPTS-LUDSEG*XC)/LUDSEG
        AA=(SUMXY-SUMN)/(SUMXX-SUMD)
        THETA=ATAN(AA)
        ANGLE=ANGLE+THETA
        NSTOT=NSTOT+1
        ANGRP(NSGRP)=ANGRP(NSGRP)+THETA
        NNGRP(NSGRP)=NNGRP(NSGRP)+1
C
C  Transform coordinates
C
      ELSE
        IF (NSFLT.EQ.1) THEN
          ANGLE=ANGLE/NSTOT
          CTH=COS(ANGLE)
          STH=SIN(ANGLE)
          AA=TAN(ANGLE)
          DO NS=1,500
            IF (NNGRP(NS).NE.0) ANGRP(NS)=ANGLE-ANGRP(NS)/NNGRP(NS)
          ENDDO
        ENDIF
        ANFLT=ANGRP(NSGRP)
        XC=UDXSEG(1)
        YC=UDYSEG(1)
        XSBEG=UDXSEG(1)
        YSBEG=UDYSEG(1)
        XPTS=UDXSEG(1)
        YPTS=UDYSEG(1)
        DO N=2,LUDSEG
          XPTS=XPTS+UDXSEG(N)
          YPTS=YPTS+UDYSEG(N)
        ENDDO
        XSEND=UDXSEG(LUDSEG)
        YSEND=UDYSEG(LUDSEG)
        TEM=1.0+AA*AA
        BB=(YPTS-AA*XPTS)/LUDSEG
        X=(XSBEG+AA*(YSBEG-BB))/TEM
        Y=AA*X+BB
        XSBEG=-X*CTH-Y*STH
        YSBEG=X*STH-Y*CTH
        X=(XSEND+AA*(YSEND-BB))/TEM
        Y=AA*X+BB
        XSEND=-X*CTH-Y*STH
        YSEND=X*STH-Y*CTH
        IF (XSEND.LT.XSBEG) THEN
          X=XSBEG
          Y=YSBEG
          XSBEG=XSEND
          YSBEG=YSEND
          XSEND=X
          YSEND=Y
        ENDIF
      ENDIF
      RETURN
      END