C**AGSGRD
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.11 06/28/04
C
      SUBROUTINE AGSGRD(NXPTS,NYPTS,NNSFLT,YYSFLT,XXSBEG,XXSEND)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSGRD
!MS$ATTRIBUTES REFERENCE :: NXPTS
!MS$ATTRIBUTES REFERENCE :: NYPTS
!MS$ATTRIBUTES REFERENCE :: NNSFLT
!MS$ATTRIBUTES REFERENCE :: YYSFLT
!MS$ATTRIBUTES REFERENCE :: XXSBEG
!MS$ATTRIBUTES REFERENCE :: XXSEND
C
C  AGSGRD recovers the grid mesh for spray block details
C
C  NXPTS  - Number of grid points in X direction
C  NYPTS  - Number of grid points in Y direction
C  NNSFLT - Number of flight lines
C  YYSFLT - Y location array of flight lines (m)
C  XXSBEG - X beginning location array of flight lines (m)
C  XXSEND - X ending location array of flight lines (m)
C
      DIMENSION YYSFLT(2),XXSBEG(2),XXSEND(2)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /GRDV/ DEPV(60,300)
C
C  Compute the deposition at each deposited point
C
      DMAX=0.0
      DO N=1,NVAR
        DO NN=1,NNDRPT
          DEPV(N,NN)=0.0
          VOLNN=VOLRV(N,NN)
          YY=YPOSV(N,NN)
          IF (VOLNN.GT.0.0) THEN
            DO M=1,NVAR
              DO MM=1,NNDRPT
                VOLMM=VOLRV(M,MM)
                Y=YPOSV(M,MM)
                SS=SPRDV(M,MM)
                IF (VOLMM.GT.0.0) THEN
                  YEXP=EXP(-AMIN1(0.5*((Y-YY)/SS)**2,25.0))
                  DTEM=DMASSN(MM)*VOLMM*YEXP/SS
                  DEPV(N,NN)=DEPV(N,NN)+DTEM
                ENDIF
              ENDDO
            ENDDO
          ENDIF
          DEPV(N,NN)=FFDEP*DEPV(N,NN)/NVAR/2.50663
          DMAX=AMAX1(DMAX,DEPV(N,NN))
        ENDDO
      ENDDO
C
C  Set limits on deposition breadth
C
      IF (NCON.EQ.0) THEN
        DMIN=0.5*CONMN
      ELSE
        DMIN=0.0005*DMAX
      ENDIF
      XMIN=1.0E+20
      XMAX=-1.0E+20
      YMIN=1.0E+20
      YMAX=-1.0E+20
      DO N=1,NVAR
        DO NN=1,NNDRPT
          VOLNN=VOLRV(N,NN)
          DTEM=DEPV(N,NN)
          SY=SPRDV(N,NN)
          XX=XPOSV(N,NN)
          YY=YPOSV(N,NN)
          IF (VOLNN.GT.0.0) THEN
            IF (DTEM.GT.DMIN) THEN
              SX=SY
              XMIN=AMIN1(XMIN,XX-4.0*SX)
              XMAX=AMAX1(XMAX,XX+4.0*SX)
              YMIN=AMIN1(YMIN,YY-4.0*SY)
              YMAX=AMAX1(YMAX,YY+4.0*SY)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C  Construct grid mesh points
C
      XFMIN=1.0E+20
      XFMAX=-1.0E+20
      XGMIN=1.0E+20
      XGMAX=-1.0E+20
      IF (LFGPS.EQ.0) THEN
        CALL HSORT(NSFLT,YSFLT,XSBEG,XSEND)
        DGSD=SWATH
      ELSE
        YFMIN=1.0E+20
        YFMAX=-1.0E+20
        YGMIN=1.0E+20
        YGMAX=-1.0E+20
        DGSD=SWATH
      ENDIF
      DINF=800.0
C
      DO NS=1,NSFLT
        XFMIN=AMIN1(XFMIN,XSBEG(NS),XSEND(NS))
        XFMAX=AMAX1(XFMAX,XSBEG(NS),XSEND(NS))
        XGMIN=AMIN1(XGMIN,XSBEG(NS)+XMIN,XSEND(NS)+XMIN)
        XGMAX=AMAX1(XGMAX,XSBEG(NS)+XMAX,XSEND(NS)+XMAX)
        IF (LFGPS.NE.0) THEN
          YFMIN=AMIN1(YFMIN,YSBEG(NS),YSEND(NS))
          YFMAX=AMAX1(YFMAX,YSBEG(NS),YSEND(NS))
          YGMIN=AMIN1(YGMIN,YSBEG(NS)+YMIN,YSEND(NS)+YMIN)
          YGMAX=AMAX1(YGMAX,YSBEG(NS)+YMAX,YSEND(NS)+YMAX)
        ENDIF
      ENDDO
      IF (LFGPS.EQ.0) THEN
        YGMIN=YSFLT(1)+YMIN
        YGMAX=YSFLT(NSFLT)+YMAX
      ENDIF
C
      XGMIN=AMAX1(XGMIN,XFMIN-DINF)
      XGMAX=AMIN1(XGMAX,XFMAX+DINF)
      IF (LFGPS.EQ.0) THEN
        YGMIN=AMAX1(YGMIN,YSFLT(1)-DINF)
        YGMAX=AMIN1(YGMAX,YSFLT(NSFLT)+DINF)
      ELSE
        YGMIN=AMAX1(YGMIN,YFMIN-DINF)
        YGMAX=AMIN1(YGMAX,YFMAX+DINF)
      ENDIF
      NXPTS=IFIX((XGMAX-XGMIN)/DGSD)+1
      IF (LFGPS.EQ.0) THEN
        NYGB=IFIX(AMAX1((YSFLT(1)-YGMIN)/DGSD,0.0))+1
        YGMIN=YSFLT(1)-NYGB*DGSD
        NYGM=IFIX((YSFLT(NSFLT)-YSFLT(1))/DGSD)+1
        NYGE=IFIX(AMAX1((YGMAX-YSFLT(NSFLT))/DGSD,0.0))+1
        NYPTS=NYGB+NYGM+NYGE
      ELSE
        NYPTS=IFIX((YGMAX-YGMIN)/DGSD)+1
      ENDIF
C
C  Return flight line data
C
      IF (LFGPS.EQ.0) THEN
        NNSFLT=NSFLT
        DO NS=1,NSFLT
          YYSFLT(NS)=YSFLT(NS)
          XXSBEG(NS)=XSBEG(NS)
          XXSEND(NS)=XSEND(NS)
        ENDDO
      ENDIF
C
C  Construct cross flight line deposition parameters
C
      SS=2.0
      J=0
10    J=J+1
      DDDEP=J*SS
      NNDEP=(YMAX-YMIN)/DDDEP+1
      IF (NNDEP.GT.1000) GOTO 10
      NNDEP=MAX0(NNDEP,2)
      DDDEP=(YMAX-YMIN)/(NNDEP-1)
      YYDEP=YMIN
      RETURN
      END
C**HSORT - Heapsort ordering of flight lines
      SUBROUTINE HSORT(N,YS,XB,XE)
C
      DIMENSION YS(2),XB(2),XE(2)
C
      IF (N.LT.2) RETURN
      L=N/2+1
      IR=N
10    IF (L.GT.1) THEN
        L=L-1
        YYS=YS(L)
        XXB=XB(L)
        XXE=XE(L)
      ELSE
        YYS=YS(IR)
        XXB=XB(IR)
        XXE=XE(IR)
        YS(IR)=YS(1)
        XB(IR)=XB(1)
        XE(IR)=XE(1)
        IR=IR-1
        IF (IR.EQ.1) THEN
          YS(1)=YYS
          XB(1)=XXB
          XE(1)=XXE
          RETURN
        ENDIF
      ENDIF
      I=L
      J=L+L
20    IF (J.LE.IR) THEN
        IF (J.LT.IR) THEN
          IF (YS(J).LT.YS(J+1)) J=J+1
        ENDIF
        IF (YYS.LT.YS(J)) THEN
          YS(I)=YS(J)
          XB(I)=XB(J)
          XE(I)=XE(J)
          I=J
          J=J+J
        ELSE
          J=IR+1
        ENDIF
        GOTO 20
      ENDIF
      YS(I)=YYS
      XB(I)=XXB
      XE(I)=XXE
      GOTO 10
      END