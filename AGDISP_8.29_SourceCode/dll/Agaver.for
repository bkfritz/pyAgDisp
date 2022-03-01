C**AGAVER
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.00 09/01/01
C
      SUBROUTINE AGAVER(NPTS,DV,DMIN,DAV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGAVER
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: DMIN
!MS$ATTRIBUTES REFERENCE :: DAV
C
C  AGAVER computes the average drop size distribution from the
C  upper drop size distribution supplied by the user
C
C  NPTS   - Number of drop sizes
C  DV     - Upper diameter drop sizes
C  DMIN   - Lower diameter for first drop size
C  DAV    - Average diameter drop sizes
C
      DIMENSION DV(NPTS),DAV(NPTS)
C
      DTEM=DMIN
      DO N=1,NPTS
        DAV(N)=DV(N)*((1.0+DTEM/DV(N))
     $              *(1.0+(DTEM/DV(N))**2)/4.0)**0.33333
        DTEM=DV(N)
      ENDDO
      RETURN
      END