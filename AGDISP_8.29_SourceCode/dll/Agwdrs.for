C**AGWDRS
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGWDRS(NFLDIR,TEMP,RHUM,NXSPD,NFREQ,
     $                  MONB,MONE,TEMPG,RHUMG,PROB)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGWDRS
!MS$ATTRIBUTES REFERENCE :: NFLDIR
!MS$ATTRIBUTES REFERENCE :: TEMP
!MS$ATTRIBUTES REFERENCE :: RHUM
!MS$ATTRIBUTES REFERENCE :: NXSPD
!MS$ATTRIBUTES REFERENCE :: NFREQ
!MS$ATTRIBUTES REFERENCE :: MONB
!MS$ATTRIBUTES REFERENCE :: MONE
!MS$ATTRIBUTES REFERENCE :: TEMPG
!MS$ATTRIBUTES REFERENCE :: RHUMG
!MS$ATTRIBUTES REFERENCE :: PROB
C
C  AGWDRS generates the probability distribution from the interim
C  frequency distribution and user-defined limits
C
C  NFLDIR - Direction angle to sensitive area (0=off, 1 to 36)
C  TEMP   - Temperature array (deg C)
C  RHUM   - Relative humidity array (%)
C  NXSPD  - Maximum wind speed entered by user
C  NFREQ  - Frequency distribution array
C  MONB   - Beginning month
C  MONE   - Ending month
C  TEMPG  - Average generated temperature (deg C)
C  RHUMG  - Average generated relative humidity (%)
C  PROB   - Probability distribution array
C
      INTEGER*2 NFREQ(36,12,20)
C
      DIMENSION TEMP(12),RHUM(12)
      DIMENSION NDIR(36),FDIR(36),PROB(36,20)
C
C  Find dominant wind direction for the months and speeds selected
C
      DO N=1,36
        NDIR(N)=0
      ENDDO
      DO NSPD=2,NXSPD
        DO M=MONB,MONE
          DO N=1,36
            NDIR(N)=NDIR(N)+NFREQ(N,M,NSPD)
          ENDDO
        ENDDO
      ENDDO
C
      DO N=1,36
        FDIR(N)=0.0
        DO NN=N-6,N+6
          NPT=NN
          IF (NPT.LT.1) NPT=NPT+36
          IF (NPT.GT.36) NPT=NPT-36
          NANG=IABS(NN-N)
          FDIR(N)=FDIR(N)+FLOAT(NDIR(NPT))*COS(0.1745329*FLOAT(NANG))
        ENDDO
      ENDDO
      FMAX=0.0
      DO N=1,36
        IF (FDIR(N).GT.FMAX) THEN
          FMAX=FDIR(N)
          NMAX=N
        ENDIF
      ENDDO
C
      IF (NFLDIR.NE.0) NMAX=NFLDIR
C
C  Set up wind rose
C
      DO J=2,20
        DO N=1,36
          PROB(N,J)=0.0
        ENDDO
      ENDDO
      TOT=0.0
      TEMPG=0.0
      RHUMG=0.0
      NTTR=0
      DO NSPD=2,NXSPD
        DO M=MONB,MONE
          DO N=1,36
            IDIR=N-NMAX
            IF (IDIR.LT.-17) IDIR=IDIR+36
            IF (IDIR.GT.18) IDIR=IDIR-36
            IDIR=IDIR+18
            TOT=TOT+NFREQ(N,M,NSPD)
            PROB(IDIR,NSPD)=PROB(IDIR,NSPD)+NFREQ(N,M,NSPD)
            NTTR=NTTR+1
            TEMPG=TEMPG+TEMP(M)
            RHUMG=RHUMG+RHUM(M)
          ENDDO
        ENDDO
      ENDDO
C
      TEMPG=TEMPG/NTTR
      RHUMG=RHUMG/NTTR
      DO NSPD=2,NXSPD
        DO N=1,36
          PROB(N,NSPD)=PROB(N,NSPD)/TOT
        ENDDO
      ENDDO
      RETURN
      END