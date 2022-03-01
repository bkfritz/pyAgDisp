C**AGDSR
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.06 03/24/03
C
      SUBROUTINE AGDSR(NN,NNOZ,XV,VOLNN)
C
C  AGDSR computes the discrete receptor deposition
C
C  NN     - Discrete receptor identifier
C  NNOZ   - Nozzle number
C  XV     - Trajectory array
C  VOLNN  - Volume fraction
C
      DIMENSION XV(9)
C
      INCLUDE 'AGCOMMON.INC'
C
      XX=XV(1)
      YY=XV(2)
      SY=SQRT(ABS(XV(7)))
      XDR=XXDSR(NN)
      YDR=YYDSR(NN)
C
      SX=SY  !AMAX1(SY,16.0)
      ST=1.414214*SX
      DO NS=1,NSFLT
        YTEM=0.0
        IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
          IF (IHALF(NNOZ).EQ.1) YTEM=1.0
        ELSEIF (NS.EQ.NSFLT.AND.IBOOM.EQ.1) THEN
          IF (IHALF(NNOZ).EQ.0) YTEM=1.0
        ELSE
          YTEM=1.0
        ENDIF
        HFLEN=0.5*(XSEND(NS)-XSBEG(NS))
        XC=0.5*(XSEND(NS)+XSBEG(NS))+XX
        X=XDR-XC
        XERF=0.5*(AGUNF(NERF,YERF,DERF,ERFV,(HFLEN-X)/ST)
     $           +AGUNF(NERF,YERF,DERF,ERFV,(HFLEN+X)/ST))
        Y=YDR-YSFLT(NS)
        YEXP=EXP(-AMIN1(0.5*((Y-YY)/SY)**2,25.0))
        DEP=FFDEP*DMASSN(NNDRP)*VOLNN*YEXP/SY/NVAR/2.50663
        CALL FSDISC(NN,NNOZ,XV,EFF)
        DPDSR(NN)=DPDSR(NN)+YTEM*XERF*DEP*EFF
      ENDDO
      RETURN
      END
C**FSDISC
      SUBROUTINE FSDISC(NN,NNOZ,XV,EFF)
C
C  Determine collection efficiency of discrete receptors
C
C  NN     - Discrete receptor number
C  NNOZ   - Nozzle number
C  XV     - Trajectory array
C  EFF    - Collection efficiency (0.0 to 1.0)
C
      DIMENSION XV(9)
C
      INCLUDE 'AGCOMMON.INC'
C
C  NTDSR = 1: default; 2: ribbon; 3: cylinder; 4: sphere
C
C  Default receptors
C
      IF (NTDSR(NN,NNOZ).EQ.1) THEN
        EFF=1.0
      ELSE
C
C  Compute normal velocity to receptor
C
        XTEM=SQRT(ABS(XNORM(NN)**2+YNORM(NN)**2+ZNORM(NN)**2))
        IF (XTEM.EQ.0.0) XTEM=1.0
        UNORM=-(XNORM(NN)*XV(4)+YNORM(NN)*XV(5)+ZNORM(NN)*XV(6))/XTEM
        IF (UNORM.LE.0.0) THEN
          EFF=0.0
        ELSE
C
C  Compute Stokes parameter
C
          EDS=EDOV(NNOZ)
          DENC=((EDS**3-DCUT**3)*DENF+DCUT**3*DENN)/EDS**3
          IF (ESDSR(NN).LE.0.0) THEN
            EFF=0.0
          ELSE
            STK=DENC*EDS*EDS*UNORM/ESDSR(NN)/1600.0
            CALL FSEFF(NTDSR(NN,NNOZ)-2,STK,EFF)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END