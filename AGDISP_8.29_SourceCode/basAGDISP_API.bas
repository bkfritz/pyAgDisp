Attribute VB_Name = "basAGDISP_API"
'$Id: basAGDISP_API.bas,v 1.15 2016/12/05 15:36:29 tom Exp $
'API for agdisp32.dll

Option Explicit

'
' Globals
'

'AGINIT MAA flags
Public Const AGINIT_NORMAL = 0
Public Const AGINIT_MAA = 1

'AGENDS results flags
Public Const AGENDS_DEPOS = 0      'Deposition
Public Const AGENDS_PID = 1        'Pond-Integrated Deposition
Public Const AGENDS_FLUX = 2       'Vertical Flux
Public Const AGENDS_1HRCON = 3     '1-hour average concentration
Public Const AGENDS_COV = 4        'COV
Public Const AGENDS_MEAN = 5       'COV Mean Deposition
Public Const AGENDS_ALOFT = 6      'Fraction Aloft
Public Const AGENDS_SGLDEP = 7     'Single-Swath Deposition
Public Const AGENDS_SGLHAF = 8     'Single-Swath upwind half-boom deposition
Public Const AGENDS_MULDEP = 9     'Multiple deposition setup
Public Const AGENDS_SBLOCK = 10    'Spray block deposition
Public Const AGENDS_CANOPY = 11    'Canopy deposition
Public Const AGENDS_TAALOFT = 12   'Time accountancy aloft
Public Const AGENDS_TAVAPOR = 13   'Time accountancy vapor
Public Const AGENDS_TACANOPY = 14  'Time accountancy canopy
Public Const AGENDS_TAGROUND = 15  'Time accountancy ground
Public Const AGENDS_HAALOFT = 16   'Height accountancy aloft
Public Const AGENDS_HAVAPOR = 17   'Height accountancy vapor
Public Const AGENDS_HACANOPY = 18  'Height accountancy canopy
Public Const AGENDS_SBLOCKDSD = 19 'Spray block drop size distribution
Public Const AGENDS_DWINDDSD = 20  'Downwind drop size distribution
Public Const AGENDS_FLUXDSD = 21   'Vertical flux drop size distribution
Public Const AGENDS_DAALOFT = 22   'Time accountancy aloft
Public Const AGENDS_DAVAPOR = 23   'Time accountancy vapor
Public Const AGENDS_DACANOPY = 24  'Time accountancy canopy
Public Const AGENDS_DAGROUND = 25  'Time accountancy ground
Public Const AGENDS_SBCOVER = 26   'Spray Block Area Coverage
Public Const AGENDS_CANDSD = 27    'Canopy drop size distribution
Public Const AGENDS_LAYOUT = 28    'Application Layout
Public Const AGENDS_DEPDROPS = 29  'Deposition in drops/cm2
Public Const AGENDS_POINTDSD = 30  'Point drop size distribution

'AGGRND results flags
Public Const AGGRND_DEPOS = 0      'Deposition
Public Const AGGRND_PID = 1        'Pond-Integrated Deposition

'
' Function Declarations
'
Declare Sub agarea Lib "agdisp32.dll" Alias "_agarea@40" ( _
  nxpts&, nypts&, xgrdv!, ygrdv!, dgrdv!, _
  nacb&, xacb!, yacb!, area!, cover!)
Declare Sub agaver Lib "agdisp32.dll" Alias "_agaver@16" ( _
  npts&, dv!, dmin!, dav!)
Declare Sub agcov Lib "agdisp32.dll" Alias "_agcov@32" ( _
  ncov&, COVVal!, COVESW!, COVDep!, _
  INTYPE&, COV!, ESW!, dep!)
Declare Sub agdapt Lib "agdisp32.dll" Alias "_agdapt@60" ( _
  UD As UserData, _
  ndep&, DepDist!, DepVal!, _
  ISTYPE&, INTYPE&, XPOND!, XDEEP!, _
  XLENG!, XLAND!, XAPPL!, XCONC!, _
  nusr&, UsrDist!, UsrVal!)
Declare Sub agdrin Lib "agdisp32.dll" Alias "_agdrin@36" ( _
  nd&, Typ&, X!, Y!, Z!, XN!, YN!, ZN!, Size!)
Declare Sub agdrop Lib "agdisp32.dll" Alias "_agdrop@4" (n&)
Declare Sub agdrot Lib "agdisp32.dll" Alias "_agdrot@4" (dep!)
'Declare Sub agdry
Declare Sub agdsrn Lib "agdisp32.dll" Alias "_agdsrn@40" ( _
  lflg&, nusr&, dkv!, xkv!, VMD!, xrs!, _
  D10!, D90!, F141!, dp!)
Declare Sub agdun Lib "agdisp32.dll" Alias "_agdun@36" ( _
  LFLOW&, U0!, SWATH!, FLOW!, ACTIVE!, DENF!, LNM&, LDN&, FFDEP!)
Declare Sub agends Lib "agdisp32.dll" Alias "_agends@16" ( _
  IFLG&, nv&, yv!, dv!)
Declare Sub agfill Lib "agdisp32.dll" Alias "_agfill@40" ( _
  itype&, nusr&, div!, xiv!, _
  npts&, dv!, xv!, _
  ier&, ByVal chstr$, jchstr&)
Declare Sub agfun Lib "agdisp32.dll" Alias "_agfun@36" ( _
  LFLOW&, U0!, SWATH!, FLOW!, ACTIVE!, DENF!, LNM&, LDN&, FFLUX!)
Declare Sub aggsin Lib "agdisp32.dll" Alias "_aggsin@36" ( _
  UD As UserData, lopts&, xopts!, ndep&, ydep!, ddep!, npts&, dv!, pv!)
Declare Sub aggsgo Lib "agdisp32.dll" Alias "_aggsgo@12" ( _
  nnvec&, yyvec!, ddvec!)
Declare Sub aginit Lib "agdisp32.dll" Alias "_aginit@8" ( _
  UD As UserData, maaflg&)
Declare Sub agars Lib "agdisp32.dll" Alias "_agars@44" ( _
  BK As DropKirkData, iunit&, lfl&, iqual&, _
  npts&, dv!, xv!, _
  ier&, realwd!, ByVal chstr$, jchstr&)
Declare Sub aglims Lib "agdisp32.dll" Alias "_aglims@12" ( _
  np&, dv!, pv!)
Declare Sub agnums Lib "agdisp32.dll" Alias "_agnums@32" ( _
  xnsd!, xcov!, xSM!, xae!, XDE!, xab!, xev!, xcn!)
Declare Sub agparm Lib "agdisp32.dll" Alias "_agparm@28" ( _
  lflg&, lcls&, VMD!, xrs!, nusr&, dkv!, xkv!)
Declare Sub agpout Lib "agdisp32.dll" Alias "_agpout@24" ( _
  xref!, yref!, zbase!, xlen!, angle!, ByVal fn$)
Declare Sub agpuff Lib "agdisp32.dll" Alias "_agpuff@40" ( _
  lflg&, xref!, yref!, zbase!, xlen!, angle!, _
  ier&, realwd!, ByVal chstr$, jchstr&)
Declare Sub agread Lib "agdisp32.dll" Alias "_agread@24" ( _
  iunits&, stat&, ityp&, adat!, ByVal cdat$, clen&)
Declare Sub agreps Lib "agdisp32.dll" Alias "_agreps@8" ( _
  npts%, reps%)
Declare Sub agrot Lib "agdisp32.dll" Alias "_agrot@44" ( _
  HK As HKData, iunit&, lfl&, icls&, npts&, dv!, xv!, _
  ier&, realwd!, ByVal chstr$, jchstr&)
Declare Sub agrbr Lib "agdisp32.dll" Alias "_agrbr@28" ( _
  rbht!, poros!, esize!, etype&, XTREE!, XDIST!, rif!)
Declare Sub agrtrn Lib "agdisp32.dll" Alias "_agrtrn@8" ( _
  xh!, yv!)
Declare Sub agsbck Lib "agdisp32.dll" Alias "_agsbck@16" ( _
  nbnd&, xbnd!, ybnd!, IFLG&)
Declare Sub agsbin Lib "agdisp32.dll" Alias "_agsbin@52" ( _
  UD As UserData, _
  nbnd&, xbnd!, ybnd!, fdir!, ncon&, conv!, _
  LNM&, LDN&, lpt&, npts&, dv!, pv!)
Declare Sub agscf Lib "agdisp32.dll" Alias "_agscf@28" ( _
  IFLG&, ascf!, bscf!, cscf!, _
  ndep&, DepDist!, DepVal!)
Declare Sub agsend Lib "agdisp32.dll" Alias "_agsend@28" ( _
  nxpts&, nypts&, xgrdv!, ygrdv!, dgrdv!, conv!, dmax!)
Declare Sub agsetl Lib "agdisp32.dll" Alias "_agsetl@28" ( _
  xSD As SprayMaterialData, xvmd!, npts&, dtv!, stv!, dnv!, snv!)
Declare Sub agsgrd Lib "agdisp32.dll" Alias "_agsgrd@24" ( _
  nxpts&, nypts&, nsflt&, ysflt!, xsbeg!, xsend!)
Declare Sub agsmck Lib "agdisp32.dll" Alias "_agsmck@40" ( _
  TEMPA!, RHUMA!, _
  NEVNTS&, NYEARS&, PROB!, NTSPD&, _
  ier&, realwd!, ByVal chstr$, jchstr&)
Declare Sub agsmex Lib "agdisp32.dll" Alias "_agsmex@20" ( _
  nexct&, npts&, yv!, dv!, rar!)
Declare Sub agsmpl Lib "agdisp32.dll" Alias "_agsmpl@40" ( _
  npts&, yv!, dv!, nexam&, _
  ndep&, DepDist!, DepVal!, _
  npid&, PIDDist!, PIDVal!)
Declare Sub agstrm Lib "agdisp32.dll" Alias "_agstrm@140" ( _
  UD As UserData, _
  nsgl&, SglDist!, SglVal!, SglHalf!, _
  ISTYPE&, INTYPE&, XWIDE!, XDEEP!, _
  XTREE!, XDIST!, XSRATE!, XSLENG!, XSTURN!, _
  XRIPAR!, XDECAY!, XCHARG!, XINPTS!, _
  iunit&, lfl&, xsngl!, Nauto&, Xauto!, Rauto!, _
  npts&, yv!, cv!, _
  NSBL&, TTV!, XXV!, CCV!, _
  ier&, realwd!, ByVal chstr$, jchstr&)
Declare Sub agswd Lib "agdisp32.dll" Alias "_agswd@4" ( _
  wdir!)
Declare Sub agtraj Lib "agdisp32.dll" Alias "_agtraj@24" ( _
  UD As UserData, drop!, NTR&, slv!, apv!, japv&)
Declare Sub agtrgo Lib "agdisp32.dll" Alias "_agtrgo@12" ( _
  ntrgo&, apv!, japv&)
Declare Sub agwdrs Lib "agdisp32.dll" Alias "_agwdrs@40" ( _
  nfldir&, temp!, rhum!, nxspd&, nfreq%, _
  monb&, mone&, tempg!, rhumg!, PROB!)
Declare Sub agwplt Lib "agdisp32.dll" Alias "_agwplt@32" ( _
  PROB!, npts&, deg!, p10!, p30!, p50!, p70!, p90!)

