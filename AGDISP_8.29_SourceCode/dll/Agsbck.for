C**AGSBCK
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGSBCK(LUDBND,UDXBND,UDYBND,IFLG)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSBCK
!MS$ATTRIBUTES REFERENCE :: LUDBND
!MS$ATTRIBUTES REFERENCE :: UDXBND
!MS$ATTRIBUTES REFERENCE :: UDYBND
!MS$ATTRIBUTES REFERENCE :: IFLG
C
C  AGSBCK checks the spray block details toolbox inputs
C
C  LUDBND - Number of spray block points
C  UDXBND - X array of spray block points (m)
C  UDYBND - Y array of spray block points (m)
C  IFLG   - Error flag (0 = OK; 1 = boundary crossing)
C
      DIMENSION UDXBND(2),UDYBND(2)
      DIMENSION XBND(101),YBND(101)
C
C  Check spray boundary information
C
      IFLG=0
      IF (LUDBND.EQ.0) RETURN
      NBND=LUDBND
      DO N=1,NBND
        XBND(N)=UDXBND(N)
        YBND(N)=UDYBND(N)
      ENDDO
      IF (XBND(1).EQ.XBND(NBND).AND.YBND(1).EQ.YBND(NBND)) THEN
        NBND=NBND-1
      ELSE
        XBND(NBND+1)=XBND(1)
        YBND(NBND+1)=YBND(1)
      ENDIF
      DO N=1,NBND-2
        XB1=XBND(N)
        YB1=YBND(N)
        XE1=XBND(N+1)
        YE1=YBND(N+1)
        DX1=XE1-XB1
        DY1=YE1-YB1
        DO M=N+2,NBND
          XB2=XBND(M)
          YB2=YBND(M)
          XE2=XBND(M+1)
          YE2=YBND(M+1)
          DX2=XE2-XB2
          DY2=YE2-YB2
          XD=DY2*DX1-DY1*DX2
          IF (XD.NE.0.0) THEN
            XN=(YE2-YE1)*DX1*DX2-XE2*DY2*DX1+XE1*DY1*DX2
            X=-XN/XD
            IF (X.GT.AMIN1(XB1,XE1).AND.X.LT.AMAX1(XB1,XE1).AND.
     $          X.GT.AMIN1(XB2,XE2).AND.X.LT.AMAX1(XB2,XE2)) THEN
              IFLG=1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END