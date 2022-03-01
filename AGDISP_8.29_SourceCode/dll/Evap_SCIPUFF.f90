MODULE struct_fd

  TYPE material_str
    SEQUENCE
    REAL a          !Antoine coefficients
    REAL b
    REAL c
    REAL w          !Molecular weight
    REAL st         !Surface tension
    REAL rho        !Liquid density
    REAL rhob       !Density variation with temperature
    LOGICAL IsWater !Flag to indicate if carrier liquid is water
  END TYPE material_str

  TYPE met_str
    SEQUENCE
    REAL xReference     !Release location provided in AGDISP setup call
    REAL yReference
    REAL x                      ! (x,y,z) offset in meters
    REAL y
    REAL z
    REAL u                      ! (u,v,w) velocity in m/s
    REAL v
    REAL w
    REAL T                      ! Temperature (K)
    REAL h                      ! humidity mixing ratio (g/g)
    REAL p                      ! Pressure (mb)
    REAL tke                    ! TKE – may be useful for termination criterion?
  END TYPE met_str

  REAL,    PARAMETER :: PI      = 3.141592653
  REAL,    PARAMETER :: ABSZERO = -273.15       !deg-K
  REAL,    PARAMETER :: PSURF   = 1013.25       !mb (Standard Atmosphere)
  REAL,    PARAMETER :: RGAS    = 287.04        !Gas constant (J/mole/K)
  REAL,    PARAMETER :: RMUAIR  = 1.6E-5
  REAL,    PARAMETER :: PR    = 0.71
  REAL,    PARAMETER :: TEPS  = 1.0E-4    !Convergence test
  REAL,    PARAMETER :: DELTA = 2.16E-7
  REAL,    PARAMETER :: THIRD = 1.0/3.0
  INTEGER, PARAMETER :: MAXI  = 10

END MODULE struct_fd

!----------------------------------------------------------------------

SUBROUTINE Evaporation( diam,d_solid,vfall,matl,met,dt )

USE struct_fd

IMPLICIT NONE

REAL               ,  INTENT( INOUT ) :: diam     !Droplet diameter (m)
REAL               ,  INTENT( IN    ) :: d_solid  !Diameter of solid particle (m)
REAL               ,  INTENT( IN    ) :: vfall    !Droplet fall velocity (velocity relative to air) (m/s)
TYPE( material_str ), INTENT( IN    ) :: matl
TYPE( met_str ),      INTENT( IN    ) :: met
REAL,                 INTENT( IN    ) :: dt       !Timestep (s)

IF( d_solid <= 0.0 )THEN
  CALL EvaporationLiquid( diam,vfall,matl,met,dt )
ELSE
  CALL EvaporationWetPart( diam,d_solid,vfall,matl,met,dt,matl%IsWater )
END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE EvaporationLiquid( diam,vfall,matl,met,dt )

USE struct_fd

IMPLICIT NONE

REAL               ,  INTENT( INOUT ) :: diam     !Droplet diameter (m)
REAL               ,  INTENT( IN    ) :: vfall    !Droplet fall velocity (velocity relative to air) (m/s)
TYPE( material_str ), INTENT( IN    ) :: matl
TYPE( met_str ),      INTENT( IN    ) :: met
REAL,                 INTENT( IN    ) :: dt       !Timestep (s)

REAL tamb, tinf, pb, KA
REAL mass, xmass, nu, rfac, ddrop, difd, dmass, hvd
REAL rhoa, rhod, ret, sc, sh, cs, cvhat
REAL dpower, onemdp, pamb, psat, rlqd, plqd, psatprime
REAL alpha, gama, pdenom, denom
REAL termf1, termf2, termd1, termd2
REAL xxx, xp1, ps, deltax
REAL gasctl, radius
REAL rhov, tdrop, tboil, mv, rhocpm

INTEGER iter

REAL, EXTERNAL :: sherwd

!----- Ambient agent vapor concentration (assumed zero)

cvhat = 0.0

!----- Ambient temperature

tboil = matl%b/(matl%a - LOG10(pb*0.7500616)) - matl%c - ABSZERO

tamb = met%T
pb   = met%p

tinf = MIN(tamb,tboil)

KA = 0.023815 + 7.1E-5*(tamb+ABSZERO) ! Air thermal conductivity (W/m-K)

!-----  Calculate drop Reynolds number

ddrop  = diam
radius = 0.5*ddrop

rlqd   = 8.31436E3/matl%w   ! gas constant for liquid
gasctl = rlqd*tamb

rhoa = pb*100./(RGAS*tamb)
ret  = rhoa*vfall*ddrop/RMUAIR
nu   = sherwd( ret,PR )

!------ Calculate agent properties

dpower = 0.94     ! temperature exponent of liquid minus 1.0
onemdp = 1.0 - dpower
pamb   = 100.*pb  !1.0E5*pb  *** units of kPa?
CALL lqd( matl,tinf,rhod,cs,hvd,difd )

sc = RMUAIR/(rhoa*difd)
sh = sherwd( ret,sc )

!------ Adjust the diffusivity into air for ambient pressure

difd = difd * PSURF/pb

psat = MIN(0.9999*pamb,cs*tinf*rlqd)

!-----  Ambient agent vapor pressure

plqd = MIN(psat,cvhat*tamb*rlqd)

!------ Calculate agent vapor pressures

psatprime = MIN(0.9999*pamb,psat*EXP(2.0*matl%st/(gasctl*radius*rhod)))

pdenom = pamb - psatprime   !0.5*(plqd+psatprime)

alpha = hvd/gasctl                      !  L/RT
gama  = alpha*pamb*difd*sh/(KA*nu*tinf) !  PLD/KRT**2

termf2 = gama/pdenom
denom  = 1.0 + termf2*alpha*psatprime*(pamb-plqd)/pdenom

!------ Calculate initial scaled temperature departure, xxx

xxx = termf2*(plqd-psatprime)/denom

!----- Iteration loop

iter = 0

IterationLoop: DO

  iter = iter + 1

  xp1  = xxx + 1.0

  ps     = MIN(0.9999*pamb,psatprime*EXP(alpha*xxx/xp1))  !  effective surface pressure
  pdenom = pamb - ps   !0.5*(plqd+ps)

  termf1 = (xp1**onemdp-1.0)/onemdp      !  1st term of function
  termd1 = xp1**(-dpower)                !  1st term of deriv
  termf2 = gama/pdenom
  termd2 = termf2*alpha/pdenom*(pamb-plqd)*ps/(xp1*xp1) ! 2nd term of deriv
  termf2 = termf2*(plqd-ps)                             ! 2nd term of function

!----- Calculate increment to xxx

  deltax = (termf2-termf1)/(termd1+termd2)
  xxx    = xxx + deltax

!----- Check for convergence

  IF( ABS(deltax) <= TEPS .OR. iter >= MAXI )EXIT

END DO IterationLoop

xp1 = xxx + 1.0
ps  = psatprime*EXP(alpha*xxx/xp1)  ! effective saturated pressure at the surface

!----- Calculate mass increment of water component

IF( ABS(xxx) > 1.0E-4 )THEN
  denom  = xp1**onemdp - 1.0
  termf1 = onemdp*xxx/denom
ELSE
  termf1 = 1.0
END IF
termd1 = pamb*difd*sh/gasctl*(ps-plqd)/(pamb-ps)     !0.5*(plqd+ps))
dmass  = PI*ddrop*termf1*termd1*dt

!------ Calculate liquid drop mass

rfac   = (PI/6.)*rhod
mass   = rfac*(ddrop**3)

!-----  Update drop size

IF( dmass >= mass )THEN
  diam = 0.
ELSE
  xmass = mass - dmass
  diam  = (xmass/rfac)**THIRD
END IF

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE EvaporationWetPart( diam,d_solid,vfall,matl,met,dt,IsWater )

USE struct_fd

IMPLICIT NONE

REAL               ,  INTENT( INOUT ) :: diam     !Droplet diameter (m)
REAL               ,  INTENT( IN    ) :: d_solid  !Diameter of solid particle (m)
REAL               ,  INTENT( IN    ) :: vfall    !Droplet fall velocity (velocity relative to air) (m/s)
TYPE( material_str ), INTENT( IN    ) :: matl
TYPE( met_str ),      INTENT( IN    ) :: met
REAL,                 INTENT( IN    ) :: dt       !Timestep (s)
LOGICAL,              INTENT( IN    ) :: IsWater  !Flag to indicate liquid is water

REAL tamb, pb, KA
REAL mass, nu, rfac, ddrop, difd, dmass, hvd
REAL rhoa, rhod, ret, sc, sh
REAL apower, dpower, onemdp, pamb, psat, rlqd, plqd, psatprime
REAL alpha, gama, pdenom, denom
REAL termf1, termf2, termd1, termd2
REAL xxx, xp1, ps, deltax
REAL gasctl, radius
REAL wvfrac, wvlim

INTEGER iter
!DEC$ IF DEFINED (NGIC)
INTEGER ID
REAL    mwl, stl
!DEC$ ENDIF

REAL, EXTERNAL :: sherwd

!------ Set ambient temp

tamb = met%T
pb   = met%p

KA = 0.023815 + 7.1E-5*(tamb+ABSZERO) ! Air thermal conductivity (W/m-K)

!-----  Calculate drop Reynolds number

ddrop   = diam
wvfrac  = 1.0 - (d_solid/ddrop)**3
radius  = 0.5*ddrop

rlqd    = 8.31436E3/matl%w    ! gas constant for liquid
gasctl  = rlqd*tamb

rhoa   = pb*100./(RGAS*tamb)

ret  = rhoa*vfall*ddrop/RMUAIR
nu   = sherwd( ret,PR )

!------ Calculate agent properties

apower = 1.0      ! activity reduction exponent (due to water dilution)
dpower = 0.94     ! temperature exponent of water minus 1.0
onemdp = 1.0 - dpower
pamb   = 100.*pb  !1.0E5*pb

IF( IsWater )THEN
  CALL set_water_props( tamb,rhod,pb/PSURF,psat,hvd,difd )
  plqd = MIN(psat,met%h*rhoa*gasctl)
ELSE
  CALL lqd( matl,tamb,rhod,psat,hvd,difd )
  difd = difd * PSURF/pb
  psat = psat*tamb*rlqd
  plqd = 0.0
END IF

sc = RMUAIR/(rhoa*difd)
sh = sherwd( ret,sc )

!------ Calculate water vapor pressures

psatprime = psat*(wvfrac**apower) ! reduced effective saturated pressure in impure drop
psatprime = psatprime*EXP(2.0*matl%st/(gasctl*radius*rhod))

pdenom = pamb - 0.5*(plqd+psatprime)
!  dfunct = difd*pamb/pdenom

alpha = hvd/gasctl                      !  L/RT
gama  = alpha*pamb*difd*sh/(KA*nu*tamb) !  PLD/KRT**2

termf2 = gama/pdenom
denom  = 1.0 + termf2*alpha*psatprime*(pamb-plqd)/pdenom

!------ Calculate initial scaled temperature departure, xxx

xxx = termf2*(plqd-psatprime)/denom

!----- Iteration loop

iter = 0

IterationLoop: DO

  iter = iter + 1

  xp1   = xxx + 1.0

  ps     = psatprime*EXP(alpha*xxx/xp1)  !  effective surface pressure
  pdenom = pamb - 0.5*(plqd+ps)

  termf1 = (xp1**onemdp-1.0)/onemdp      !  1st term of function
  termd1 = xp1**(-dpower)                !  1st term of deriv
  termf2 = gama/pdenom
  termd2 = termf2*alpha/pdenom*(pamb-plqd)*ps/(xp1*xp1) ! 2nd term of deriv
  termf2 = termf2*(plqd-ps)                             ! 2nd term of function

!----- Calculate increment to xxx

  deltax = (termf2-termf1)/(termd1+termd2)
  xxx    = xxx + deltax

!----- Check for convergence

  IF( ABS(deltax) <= TEPS .OR. iter >= MAXI )EXIT

END DO IterationLoop

xp1 = xxx + 1.0
ps  = psatprime*EXP(alpha*xxx/xp1)  ! effective saturated pressure at the surface

!----- Calculate mass increment of water component

IF( ABS(xxx) > 1.0E-4 )THEN
  denom  = xp1**onemdp - 1.0
  termf1 = onemdp*xxx/denom
ELSE
  termf1 = 1.0
END IF
termd1 = pamb*difd*sh/gasctl*(ps-plqd)/(pamb-0.5*(plqd+ps))
dmass  = PI*ddrop*termf1*termd1*dt

!------ Calculate liquid drop mass

rfac = (PI/6.)*rhod
mass = rfac*(ddrop**3-d_solid**3)

IF( dmass > 0. )THEN
  wvlim = MAX(MIN(plqd/psat,wvfrac),0.01)
  xxx   = 1. - wvlim*(1.-wvfrac)/(wvfrac*(1.-wvlim))
  dmass = MIN(dmass,mass*xxx)
ELSE
  wvlim = MAX(plqd/psat,wvfrac)
  xxx   = 1. - wvlim*(1.-wvfrac)/(wvfrac*(1.-wvlim))
  dmass = MAX(dmass,mass*xxx)
END IF

diam = (diam**3-dmass/rfac)**THIRD

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE lqd( m,tkdrop,rhod,cs,hv,dif )

USE struct_fd

IMPLICIT NONE

TYPE( material_str ), INTENT( IN  ) :: m
REAL,                 INTENT( IN  ) :: tkdrop
REAL,                 INTENT( OUT ) :: cs, dif, hv, rhod

REAL, PARAMETER :: DIFFAC = 3.1038
REAL, PARAMETER :: WA     = 0.03448
REAL, PARAMETER :: HVFAC  = 1.9148e4

REAL difa, tc

!-----  Convert droplet temperature from Kelvin to Celsius

tc = tkdrop + ABSZERO

!-----  Calculate liquid properties as a function of temperature
!-----  Units are MKS
!-----  In cgs units 1.6e-5 = 1.0e+6/760/R

rhod = MAX(m%rho - m%rhob*tc,1.0E-6)
tc   = tc + m%c

IF( tc < 1.0E-6 )THEN
  cs  = 0.0
  hv  = 0.0
  dif = 0.0
ELSE
  cs = MIN(20.,MAX(m%a-m%b/tc,-20.0))
  cs = 1.6E-2*m%w*(10.**cs)/tkdrop !kg/m3
  hv   = HVFAC*m%b*(tkdrop/tc)**2/m%w
  difa = 4.3E-7*SQRT(tkdrop*(WA+1./m%w))*tkdrop
  dif  = difa/(DIFFAC+(1000.*m%w/rhod)**0.3333)**2
END IF

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE set_water_props( tamb,rhod,pb,psat,hv,dif )

USE struct_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: tamb,pb
REAL, INTENT( OUT ) :: psat, dif, hv, rhod

REAL tc

!-----  Convert droplet temperature from Kelvin to Celsius

tc = tamb + ABSZERO

!-----  Calculate liquid properties as a function of temperature
!-----  Units are MKS
!-----  In cgs units 1.6e-5 = 1.0e+6/760/R

rhod = MAX(1000.78 - 0.197*tc,1.0E-6)
psat = 1.0E+2 *10.**((0.7859+0.03477*tc)/(1.0+0.00412*tc)) ! Pa
dif  = 2.11E-5*(tamb/273.2)**1.94/pb ! (m2/s)
hv   = 2.5008E6 - 2.3E3*tc ! (j/Kg)

RETURN
END

!-----------------------------------------------------------------------

REAL FUNCTION sherwd( re,sc )

IMPLICIT NONE

REAL, INTENT( IN ) :: re, sc

!  Sherwood number for falling droplet. Original NUSSE model
!          sherwd = 2. + 0.552*SQRT(re)*sc**0.33333333
!  Modified 03/21/00 to use 
!     Beard & Pruppacher, (1971), JAS, 28, 1455-1464

REAL xxx

xxx = SQRT(re)*sc**0.33333333

IF( xxx < 1.4259 )THEN
  sherwd = 2.000000 + 0.216*xxx**2
ELSE
  sherwd = 1.560815 + 0.616*xxx
END IF
 
RETURN
END
