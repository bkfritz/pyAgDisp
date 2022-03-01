C**AGAREA
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGAREA(NXPTS,NYPTS,XGRDV,YGRDV,DGRDV,NACB,
     $                  UDXACB,UDYACB,AREA,COVER)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGAREA
!MS$ATTRIBUTES REFERENCE :: NXPTS
!MS$ATTRIBUTES REFERENCE :: NYPTS
!MS$ATTRIBUTES REFERENCE :: XGRDV
!MS$ATTRIBUTES REFERENCE :: YGRDV
!MS$ATTRIBUTES REFERENCE :: DGRDV
!MS$ATTRIBUTES REFERENCE :: NACB
!MS$ATTRIBUTES REFERENCE :: UDXACB
!MS$ATTRIBUTES REFERENCE :: UDYACB
!MS$ATTRIBUTES REFERENCE :: AREA
!MS$ATTRIBUTES REFERENCE :: COVER
C
C  AGAREA computes the area coverage
C
C  NXPTS  - Number of grid points in X direction
C  NYPTS  - Number of grid points in Y direction
C  XGRDV  - X grid point array (m)
C  YGRDV  - Y grid point array (m)
C  DGRDV  - Deposition array (in user units)
C  NACB   - Number of area coverage boundary points
C  UDXACB - X array of area coverage boundary points (m)
C  UDYACB - Y array of area coverage boundary points (m)
C  AREA   - Computed area of area coverage (sq m)
C  COVER  - Computed average deposition within area (in user units)
C
      DIMENSION XGRDV(NXPTS),YGRDV(NYPTS),DGRDV(NXPTS,NYPTS)
      DIMENSION UDXACB(2),UDYACB(2),XACB(100),YACB(100)
C
      COMMON /RTRN/ CTH,STH
C
C  Transform area coverage boundary
C
      AREA=0.0
      COVER=0.0
      IF (NACB.EQ.0) RETURN
      XMIN=1.0E+20
      XMAX=-1.0E+20
      YMIN=1.0E+20
      YMAX=-1.0E+20
      DO NN=1,NACB
        XACB(NN)=-UDXACB(NN)*CTH-UDYACB(NN)*STH
        YACB(NN)=UDXACB(NN)*STH-UDYACB(NN)*CTH
        XMIN=AMIN1(XMIN,XACB(NN))
        XMAX=AMAX1(XMAX,XACB(NN))
        YMIN=AMIN1(YMIN,YACB(NN))
        YMAX=AMAX1(YMAX,YACB(NN))
      ENDDO
      XACB(NACB+1)=XACB(1)
      YACB(NACB+1)=YACB(1)
C
C  Compute area
C
      DO NN=1,NACB
        AREA=AREA+XACB(NN)*YACB(NN+1)-XACB(NN+1)*YACB(NN)
      ENDDO
      AREA=0.5*ABS(AREA)
C
C  Set sweep through area
C
      NAREA=0
      DTEM=0.1*(XGRDV(2)-XGRDV(1))
      XBEG=XMIN-DTEM
      XEND=XMAX+DTEM
      NXT=(XEND-XBEG)/DTEM+1
      YBEG=YMIN-DTEM
      YEND=YMAX+DTEM
      NYT=(YEND-YBEG)/DTEM+1
      DO NX=2,NXT
        LFL=0
        XPT=XBEG+(NX-0.5)*DTEM
        NXG=MIN0(MAX0(IFIX((XPT-XGRDV(1))/DTEM/10.0)+1,1),NXPTS-1)
        DO NY=2,NYT
          YPT=YBEG+(NY-0.5)*DTEM
          NYG=MIN0(MAX0(IFIX((YPT-YGRDV(1))/DTEM/10.0)+1,1),NYPTS-1)
          YMN=YPT-DTEM
C
C  Check every boundary segment for a crossing
C
          DO N=1,NACB-1
            XB1=XACB(N)
            YB1=YACB(N)
            XB2=XACB(N+1)
            YB2=YACB(N+1)
            IF (XB1.NE.XB2) THEN
              IF (XPT.GT.AMIN1(XB1,XB2).AND.XPT.LT.AMAX1(XB1,XB2)) THEN
                YTEM=((YB2-YB1)*XPT+YB1*XB2-YB2*XB1)/(XB2-XB1)
                IF (YTEM.GE.YMN.AND.YTEM.LE.YPT) LFL=1-LFL
              ENDIF
            ENDIF
          ENDDO
C
C  If inside the boundary, add the deposition
C
          IF (LFL.EQ.1) THEN
            DMM=DGRDV(NXG,NYG)
            DPM=DGRDV(NXG+1,NYG)
            DMP=DGRDV(NXG,NYG+1)
            DPP=DGRDV(NXG+1,NYG+1)
            DM=(DPM*(XPT-XGRDV(NXG))+DMM*(XGRDV(NXG+1)-XPT))/
     $         (XGRDV(NXG+1)-XGRDV(NXG))
            DP=(DPP*(XPT-XGRDV(NXG))+DMP*(XGRDV(NXG+1)-XPT))/
     $         (XGRDV(NXG+1)-XGRDV(NXG))
            D=(DP*(YPT-YGRDV(NYG))+DM*(YGRDV(NYG+1)-YPT))/
     $        (YGRDV(NYG+1)-YGRDV(NYG))
            NAREA=NAREA+1
            COVER=COVER+D
          ENDIF
        ENDDO
      ENDDO
      IF (NAREA.GT.0) COVER=COVER/NAREA
      RETURN
      END