Attribute VB_Name = "basAGDISP"
' $Id: basAGDISP.bas,v 1.35 2016/12/05 15:36:29 tom Exp $
'basAGDISP.BAS - main code module for AGDISP
'
'Current AGDISP version
Public Const AGDISPVERSION = 8.29      'Current AGDISP Verion

'**************** User-defined data types ****************
'
'======= User Input data area ============================
'
' The structures defined below contain all of the input data
' that is specified by the user. These structures are duplicated
' for the fortran part of this program in a file called
' "agdstruc.inc" in the dll directory.
'
' IF CHANGES ARE MADE TO THE USER DATA STRUCTURES HERE, MAKE
' COMPLIMENTARY CHANGES TO THE FORTRAN (AND VICE VERSA).
'
'define constants for sizing arrays
Public Const MAX_DROPS = 100  'max number of drop categories
Public Const MAX_NOZZLES = 60 'max number of nozzles
Public Const MAX_ENGINES = 2  'max number of aircraft engines
Public Const MAX_FLTLINES = 50 'max number of spray lines
Public Const MAX_ENVELOPE = 25 'Max canopy envelope levels
Public Const MAX_LAI = 10     'max canopy LAI levels
Public Const MAX_CALCDATA = 4900 'max size of calc arrays
Public Const MAX_ASAE = 6     'Max ASAE drop distributions
Public Const MAX_WINDS = 10   'Max number of heights/wind speeds
'tbc add these sometime
'public const MAX_BASICAC
'etc
'
'Ground Application Data
Public Type GroundApplData
  NumSwaths As Integer     'Number of Swaths for ground sprayer
  NozType As Integer       'Ground nozzle type 0=Flat Fan 1=Air Injection
  Pressure As Single       'Boom Pressure (bar)
End Type

'Drop Size Distribution Data
Public Type DropSizeDistData
  Type As Integer          '0=basic (ASAE) 1=(not used) 2=user-def 3=lib (no 4) 5=ARS 6=FS
  BasicType As Integer     'Basic DSD category: 0-17
  Name As String * 40      'Name for DSD
  LName As Integer         'Length of Name
  NumDrop As Integer       'Number of Drop Categories
  Diam(MAX_DROPS - 1) As Single     'Drop Diameters (um)
  MassFrac(MAX_DROPS - 1) As Single 'Drop Volume Fractions
End Type

'USDA ARS Nozzle Models data
Public Type DropKirkData
  MaxErrorLevel As Integer 'Max error level encountered during run 0=none 1=warn 2=err,cont 3=err,stop
  NozType As Integer       'Nozzle Type: 0-3
  NameNoz As String * 40   'Nozzle Name
  LNameNoz As Integer      'Length of Name
  NameType As String * 40  'Nozzle body angle
  LNameType As Integer     'Length of NameType
  Orifice As Single        'Orifice identifier
  Speed As Single          'Tunnel Speed (m/s)
  NozAngle As Single       'Nozzle angle (deg) or Restrictor Number
  Pressure As Single       'Nozzle pressure (bar)
  SprayType As Integer     'Model Output Style: 0=Spray Quality 1=DSD
End Type

'USDA FS Rotary Atomizer Data
Public Type HKData
  MaxErrorLevel As Integer 'Max error level encountered during run 0=none 1=warn 2=err,cont 3=err,stop
  MatType As Integer       'Material Type 0=Water 1=Water/1% StaPut 2=Water/0.25% Hasten
  RotType As Integer       'Atomizer Type: 0=AU4000 1=AU5000
  Speed As Single          'Tunnel Speed (m/s)
  BladeAngle As Single     'Blade angle (deg)
  BladeRPM As Single       'Rotation Rate (rpm)
  Flowrate As Single       'Nozzle flowrate (L/min)
  SprayType As Integer     'Model Output Style: 0=Spray Quality 1=DSD
End Type

'Spray Material Data
Public Type SprayMaterialData
  Type As Integer          'Type: 0=basic 1=user-def 2=lib
  BasicType As Integer     'Basic Type: 0=oil 1=water
  Name As String * 40      'Spray Mat Library Substance Name
  LName As Integer         'Length of Name
  CalcInputSelect As Integer 'Calulation input selection 0=rates 1=tank mix
  NVfrac As Single         'Nonvolatile Fraction
  ACfrac As Single         'Active fraction
  ActSolFrac As Single     'Fraction of tank mix that is Active solution
  AddSolFrac As Single     'Fraction of tank mix that is Additive Solution
  ActNVFrac As Single      'Fraction of Active solution that is nonvolatile
  AddNVFrac As Single      'Fraction of Additive solution that is nonvolatile
  Flowrate As Single       'Flow Rate (L/ha or L/min)
  FlowrateUnits As Integer 'Flow Rate units Flag (0=L/ha 1=L/min)
  SpecGrav As Single       'Specific Gravity of carrier
  NonVGrav As Single       'Specific Gravity of active and additive
  EvapRate As Single       'Evaporation Rate ()
End Type

'Aircraft Data
Public Type AircraftData
  Type As Integer          '0=Basic 1=user-def 2=lib
  Name As String * 40      'Aircraft Name
  LName As Integer         'Length of Name
  WingType As Integer      'Aircraft Wing Type: 3=fixed 4=heli (MET converts to 1 and 2)
  SemiSpan As Single       'Semispan or Rotor Radius (m)
  TypSpeed As Single       'Typical Spraying Speed (m/s)
  BiplSep As Single        'Biplane Distance Between Wings (m)
  Weight As Single         'Weight (kg)
  PlanArea As Single       'Planform Area (m2)
  PropRPM As Single        'Propeller/Rotor RPM
  PropRad As Single        'Propeller Radius (m)
  PropEff As Single        'Propeller Efficiency
  EngVert As Single        'Engine Vertical position (m)
  EngFwd As Single         'Engine Forward position (m)
  NumEng As Integer        'Number of engines
  EngHoriz(MAX_ENGINES - 1) As Single  'Engine Horizontal positions (m)
  WingVert As Single       'Dist from wingtip vortex to trailing edge (m)
  BoomVert As Single       'Vert dist from boom to trailing edge (m)
  BoomFwd As Single        'Fwd dist from boom to trailing edge (m)
  DragCoeff As Single      'Drag Coefficient
End Type

'Nozzle data
Public Type NozzleData
  Type As Integer          'Distribution type: 0=basic 1=user-def
  Name As String * 40      'Distribution Name
  LName As Integer         'Length of Name
  NumNoz As Integer        'Number of Nozzles
  PosHoriz(MAX_NOZZLES - 1) As Single 'Horizontal Positions (m)
  PosVert(MAX_NOZZLES - 1) As Single  'Vertical Position (m)
  PosFwd(MAX_NOZZLES - 1) As Single   'Forward (Axial) Position (m)
  PosHorizLimit As Single  'Wingspan % for user-def Dist Limit
  BoomWidth As Single      'Dist Limit % for basic dists.
End Type
  
'Dry delivery data
Public Type DryData
  Type As Integer          'Spreader Type: 0=venturi 1=radial 2=bucket
  Sphericity As Single     'Particle Sphericity
  Width As Single          'Spreader Exit Width (m)       (Type=1)
  angle As Single          'Spreader Exit Angle (deg)     (Type=1)
  Velocity As Single       'Spreader Exit Velocity (m/s)  (Type=1)
  Hub As Single            'Spreader Hub Radius (m)       (Type=2)
  RPM  As Single           'Spreader RPM                  (Type=2)
  Length As Single         'Spreader Swaying Distance (m) (Type=3)
End Type

'Meteorological Data
Public Type MetData
  WindType As Integer      'Wind Type: 0=Single Height 1=Wind Table
  NumWinds As Integer      'Number of Wind Table Entries
  WindHeight(MAX_WINDS - 1) As Single 'Wind Table Height (m)
  WindSpeed(MAX_WINDS - 1) As Single  'Wind Table Speeds (m/s)
  WS As Single             'Wind Speed (m/s)
  WD As Single             'Wind Direction (deg)
  WH As Single             'Height of WS (m)
  temp As Single           'Temperature (deg C)
  Humidity As Single       'Humidity (%)
  Pressure As Single       'Barometric Pressure (mb)
  VortexDecayIGE As Single 'Vortex Decay Rate in ground effect (m/s)
  VortexDecayOGE As Single 'Vortex Decay Rate out of ground effect (m/s)
  SurfRough As Single      'Surface Roughness (m)
  Insolation As Integer    'Insolation index (0-7: Strong, Mod, Slight, etc.)
End Type

'Canopy Data
Public Type CanopyData
  Type As Integer          'Canopy Type 0=none 1=story 2=optical 3=basic
  Name As String * 42      'Canopy Name
  LName As Integer         'Length of Name
  EleSiz As Single         'Element Size (m)
  EleTyp As Integer        'Element Type (0=plate 1=cyl 2=sph)
  StanDen As Single        'Stand Density (stems/ha)
  NumEnv As Integer        'Number of tree envelope levels
  EnvHgt(MAX_ENVELOPE - 1) As Single 'Height (m)
  EnvDiam(MAX_ENVELOPE - 1) As Single 'Diameter (m)
  EnvPop(MAX_ENVELOPE - 1) As Single 'Probability of Penetration
  optType As Integer       'Optical Canopy Type 1=user-def 2=library
  LibHgt As Single         'Height (m)
  LibLAI As Single         'Leaf Area Index
  LibB As Single           'Coefficient B
  LibC As Single           'Coefficient C
  LibDataQuality As Integer 'Data Quality 1=High 2=Med 3=Low
  NumLAI As Integer        'Number of LAI levels
  LAIHgt(MAX_LAI - 1) As Single 'Height (m)
  LAICum(MAX_LAI - 1) As Single 'Cumulative LAI
  temp As Single           'Temperature within canopy (deg C)
  Humidity As Single       'Relative Humidity Within canopy (%)
  NDRuff As Single         'Nondimensional Surface Roughness
  NDDisp As Single         'Nondimensional Displacement
  Height As Single         'Canopy Height (m)
End Type

'Terrain Data
Public Type TerrainData
  Zref As Single           'Ground Reference (m)
  Upslope As Single        'Ground upslope angle (deg)
  Sideslope As Single      'Ground sideslope angle (deg)
End Type

'Control Data
Public Type ControlData
  Height As Single         'Boom Height (m)
  NumLines As Integer      'Number of Spray Lines
  LineReps(MAX_FLTLINES - 1) As Integer 'Number of repetitions of each Spray Line
  LineOptimize As Integer  'Spray Line optimization flag 0=off 1=on
  SwathWidth As Single     'Swath Width (none or m)
  SwathDisp As Single      'Swath Displacement (none or m)
  SwathOffset As Integer   'Swath Offset (0=1/2 Swath, 1=0 Swath)
  FluxPlane As Single      'Flux Plane Location (m)
  MaxComputeTime As Single 'Max Compute time (sec)
  MaxDownwindDist As Single 'Max Downwind distance for computations (m)
  HalfBoom As Integer      'Half-boom application flag 0=full boom 1=half boom
  SaveTraj As Integer      'Save trajectory files 0=don't save 1=save
End Type

'define a data type to hold the user input data
'This type is passed to the Fortran DLL. If you change
'its structure, you must also change it in the DLL.
Public Type UserData
  ApplMethod As Integer    'Application Method 0=aerial 1=ground
  AerialType As Integer    'Aerial Delivery Type 0=liquid 1=dry
  Title As String          'Run title
  Notes As String          'User notes
  CALPUFFFLAG As Integer   'Invoke CALPUFF 0= no 1=yes (NOT SAVED IN DATA FILE)
  SCIPUFFFLAG As Integer   'Invoke SCIPUFF 0=no 1=yes (NOT SAVED IN DATA FILE)
  SCIPUFFCUTOFF As Single  'SCIPUFF Cutoff Factor (NOT SAVED IN DATA FILE)
  GA As GroundApplData     'Ground Application Data
  DSD As DropSizeDistData  'Drop Szie Distribution Data
  BK As DropKirkData       'DropKirk Data
  HK As HKData             'FS Rotary Atomizer Data
  SM As SprayMaterialData  'Spray Material Data
  AC As AircraftData       'Aircraft Data
  NZ As NozzleData         'Nozzle Data
  DRY As DryData           'Dry Delivery data
  MET As MetData           'Meteorological Data
  CAN As CanopyData        'Canopy Data
  TRN As TerrainData       'Terrain Data
  CTL As ControlData       'Control Data
End Type

'======= end of User Input data area =====================

'======= User Calculation data area ============================
' The structures defined below are also duplicated in agdstruc.inc

'define a type to hold user calculation results
Public Type UserCalc
  Valid As Integer         'If true, the calcs have been performed
  CodeVersion As Single    'the version of AGDISP that performed these calcs
  StartDate As String      'the date when the calcs were started
  StartTime As String      'the time when the calcs were started
  MaxErrorLevel As Integer 'Max error level encountered during run 0=none 1=warn 2=err
  MessageLog As String     'A log of calculation messages
  NumDep As Integer        'Number of Deposition Data Points
  DepDist() As Single      'Deposition Distance (m)
  DepVal() As Single       'Deposition (fraction of applied)
  DepExtrap As Single      'if > 0, X value at which extrapolation begins
  NumDrp As Integer        'Number of Number Deposition Data Points
  DrpDist() As Single      'Number Deposition Distance (m)
  DrpVal() As Single       'Number Deposition (drops/cm2)
  NumPID As Integer        'Number of Pond-Int Depos Data Points
  PIDDist() As Single      'PID Distance (m)
  PIDVal() As Single       'PID (fraction of applied)
  PIDExtrap As Single      'if > 0, X value at which extrapolation begins
  NumFlux As Integer       'Number of Flux Data Points
  FluxDist() As Single     'Flux Distance (m)
  FluxVal() As Single      'Flux (mg/m2)
  NumCOV As Integer        'Number of COV/ESW data points
  COVVal() As Single       'COV points
  COVESW() As Single       'Effective Swath Width values for COV's
  NumConc As Integer       'Number of concentration data points
  ConcDist() As Single     'Conc distance (m)
  ConcVal() As Single      'Conc values (ng/l)(ppt)
  SwathDisp As Single      'Swath Displacement (m)
  SBCOV As Single          'Spray Block COV
  SBMeanDep As Single      'Spray Block Mean Deposition
  SWATH As Single          'Swath Width (m)
  AirborneDrift As Single  '% Airborne Drift
  EvapFrac As Single       'Evaporated Fraction
  AppEff As Single         'Application Efficiency (%)
  DownwindDep As Single    'Downwind Deposition (%)
  CanopyDep As Single      'Canopy Deposition
  NumSgl As Integer        'Number of Single-Line Depos Points
  SglDist() As Single      'Single-Line Depos Distance (m)
  SglVal() As Single       'Single-Line Depos (%)
  NumFA As Integer         'Number of Fraction Aloft Points
  FADist() As Single       'Fraction Aloft Distance (m)
  FAVal() As Single        'Fraction Aloft
  NumCOVM As Integer       'Number of COV Mean Deposition Points
  COVMDist() As Single     'COV Mean Deposition Distance (m)
  COVMVal() As Single      'COV Mean Deposition
  NumHalf As Integer       'Number Sgl Swath Upwind Half Boom
  HalfDist() As Single     'Sgl Swath Upwind Half Boom Dist(m)
  HalfVal() As Single      'Sgl Swath Upwind Half Boom depos
  NumSBD As Integer        'Number of Spray Block Depos pts
  SBDDist() As Single      'Spray Block Depos Distance (m)
  SBDVal() As Single       'Spray Block Depos
  NumCAN As Integer        'Number of Canopy Depos Points
  CANDist() As Single      'Canopy Deposition Distance (m)
  CANVal() As Single       'Canopy Deposition
  NumTAA As Integer        'Num Time Accountancy Aloft Pts
  TAATime() As Single      'Time Accountancy Aloft Time (sec)
  TAAVal() As Single       'Time Accountancy Aloft
  NumTAV As Integer        'Num Time Accountancy Vapor Pts
  TAVTime() As Single      'Time Accountancy Vapor Time (sec)
  TAVVal() As Single       'Time Accountancy Vapor
  NumTAC As Integer        'Num Time Accountancy Canopy Pts
  TACTime() As Single      'Time Accountancy Canopy Time (sec)
  TACVal() As Single       'Time Accountancy Canopy
  NumTAG As Integer        'Num Time Accountancy Ground Pts
  TAGTime() As Single      'Time Accountancy Ground Time (sec)
  TAGVal() As Single       'Time Accountancy Ground
  NumDAA As Integer        'Num Distance Accountancy Aloft Pts
  DAADist() As Single      'Distance Accountancy Aloft Distance (sec)
  DAAVal() As Single       'Distance Accountancy Aloft
  NumDAV As Integer        'Num Distance Accountancy Vapor Pts
  DAVDist() As Single      'Distance Accountancy Vapor Distance (sec)
  DAVVal() As Single       'Distance Accountancy Vapor
  NumDAC As Integer        'Num Distance Accountancy Canopy Pts
  DACDist() As Single      'Distance Accountancy Canopy Distance (sec)
  DACVal() As Single       'Distance Accountancy Canopy
  NumDAG As Integer        'Num Distance Accountancy Ground Pts
  DAGDist() As Single      'Distance Accountancy Ground Distance (sec)
  DAGVal() As Single       'Distance Accountancy Ground
  NumHAA As Integer        'Num Height Accountancy Aloft Pts
  HAAHgt() As Single       'Height Accountancy Aloft Height (m)
  HAAVal() As Single       'Height Accountancy Aloft
  NumHAV As Integer        'Num Height Accountancy Vapor Pts
  HAVHgt() As Single       'Height Accountancy Vapor Height (m)
  HAVVal() As Single       'Height Accountancy Vapor
  NumHAC As Integer        'Num Height Accountancy Canopy Pts
  HACHgt() As Single       'Height Accountancy Canopy Height (m)
  HACVal() As Single       'Height Accountancy Canopy
  NumSBDSD As Integer      'Num Spray Block Drop Size Dist Pts
  SBDSDDiam() As Single    'Spray Block DSD Diameter (micron)
  SBDSDFrac() As Single    'Spray Block DSD Mass Frac
  NumDWDSD As Integer      'Num Downwind Drop Size Dist Pts
  DWDSDDiam() As Single    'Downwind DSD Diameter (micron)
  DWDSDFrac() As Single    'Downwind DSD Mass Frac
  NumFXDSD As Integer      'Num Vert Flux Drop Size Dist Pts
  FXDSDDiam() As Single    'Vert Flux DSD Diameter (micron)
  FXDSDFrac() As Single    'Vert Flux DSD Mass Frac
  NumCNDSD As Integer      'Num Canopy Drop Size Dist Pts
  CNDSDDiam() As Single    'Canopy DSD Diameter (micron)
  CNDSDFrac() As Single    'Canopy DSD Mass Frac
  NumPTDSD As Integer      'Num Canopy Drop Size Dist Pts
  PTDSDDiam() As Single    'Canopy DSD Diameter (micron)
  PTDSDFrac() As Single    'Canopy DSD Mass Frac
  NumSBAC As Integer       'Num Spray Block Area Coverage Pts
  SBACRate() As Single     'Spray Block Fraction of Application Rate
  SBACFrac() As Single     'Spray Block Fraction of Area Covered
  NumLAY As Integer        'Num Application Layout Pts
  LAYDist() As Single      'Application Layout Distance (m)
  LAYFrac() As Single      'Application Layout Frac Tank Mix
  CalpuffCalcsAvailable As Boolean 'NOT SAVED. If true, CALPUFF calcs are available for export
End Type

'======= end of User Calculation data area ============================

'Interface-related data
Public Type InterfaceData
  HasConfidentialData As Boolean  'True if SDTF library is present
  FileName As String         'Current user data file name
  LibraryPath As String      'Full path to library database
  MAALibraryPath As String   'Full path to MAA library database
  DataChanged As Integer     'If True, data was changed, but not saved
  CalcsBatchMode As Integer  'If true, bring up calcs form in batch mode
  OkToDoCalcs As Integer     'Used to monitor calc progress
  DataNeedsChecking As Integer 'Check data if true
  CalcsInProgress As Integer 'the calculations are happening
  StartCalcsOnLoad As Integer 'if true, start calcs right away
  RevertCalcsAvailable As Integer 'If true, a saved set of calcs is available for reloading
  PlotVar As Long         'current plot variable
  LastPlotVar As Long     'previous value of PlotVar
  PuffSprayLineBeginningX As Single
  PuffSprayLineBeginningY As Single
  PuffBaseElevationZ As Single
  PuffSprayLineLength As Single
  PuffFlightDirection As Single
  PuffFileName As String
End Type

'define a type to hold preferences
Public Type UserPrefsData
  ShowAboutOnStartup As Integer 'If true, show the About form on program startup
  PauseBeforeCalc As Integer    'If true, the user starts the calcs
  SuppressCalcWarnings As Integer 'if true, don't ask user about calc warnings
  SuppressCalcErrors As Integer   'If true, don't ask user about calc errors
  SuppressModelFeatureWarnings As Integer 'If true, don't warn on feature use.
  Units As Integer              'Units System 0=imperial 1=metric
  DepUnitsFraction As Integer   'Use Fraction of Applied for Depos Units instead of Num/Den
  DepUnitsNum As Integer        'Active Fraction Deposition Units Numerator   (1-9)
  DepUnitsDen As Integer        'Active Fraction Deposition Units Denominator (0-5)
  FluxUnitsFraction As Integer  'Use Fraction of Applied for Flux Units instead of Num/Den
  FluxUnitsNum As Integer       'Flux Units Numerator   (1-9)
  FluxUnitsDen As Integer       'Flux Units Denominator (0-4)
  UserLib As String             'path to user library file
End Type

'******************** Global variables *******************
Public UI As InterfaceData
Public UD As UserData
Public UC As UserCalc
Public UP As UserPrefsData
Public NZ2 As NozzleData    'work area used by frmNozzles
Public BK2 As DropKirkData  'work area used by frmDropSize and frmDropKirk

Public gobjErrors As clsErrors 'For error handling

'*************** AGDISP Global constants ****************
'User file format version
'0 -> AGDISP 8.0          : Initial file format
'See UserDataWrite for more format history
'
Public Const USERFILEVERSION = 9   'version number for user data files

'Library database format version
'Libraries must have a table "Info" with an integer field "Version"
'which matches this number.
'
'1 -> 8.0: Began file version checking
'---------
'Vers 5: remove dropsize and related tables, basicdsd: remove swathdisp
'Vers 4: Canopy: Extended Name to 42 chars, added DataQuality field.
'Vers 3: Combined SDTF/FS dropsize, dropdiam tables
'Vers 2: Added to ARS table ofcflag, ofcprefix, modflag, modprefix
'Vers 1: Baseline library format
Public Const LIBRARYVERSION = 5  'version number for all libraries

'Tiers
Public Const TIER_1 = 1
Public Const TIER_2 = 2
Public Const TIER_3 = 3

'Application Method
Public Const AM_AERIAL = 0
Public Const AM_GROUND = 1
Public Const AM_ORCHARD = 2

'Aerial Delivery Type
Public Const AD_LIQUID = 0
Public Const AD_DRY = 1

'Wind Speed Type
Public Const WIND_TYPE_SINGLE = 0
Public Const WIND_TYPE_TABLE = 1

'Options for FileDialog
Public Const FD_OPEN = 1
Public Const FD_SAVEAS = 2
Public Const FD_TYPE_USER = 1
Public Const FD_TYPE_TEXT = 2
Public Const FD_TYPE_LIB = 3

'**************** Links to DLLs and APIs *****************
Public Const HH_DISPLAY_TOPIC As Long = 0
Public Const HH_HELP_CONTEXT As Long = &HF
Public Const HH_CLOSE_ALL = &H12

Public Declare Function HtmlHelp Lib "HHCtrl.ocx" Alias "HtmlHelpA" _
    (ByVal hWndCaller As Long, _
     ByVal pszFile As String, _
     ByVal uCommand As Long, _
     dwData As Any) As Long

Public Declare Sub CopyMemory Lib "Kernel32" Alias "RtlMoveMemory" (hpvDest As Any, hpvSource As Any, ByVal cbCopy As Long)
Public Declare Function GetPrivateProfileInt Lib "Kernel32" Alias "GetPrivateProfileIntA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal nDefault As Long, ByVal lpFileName As String) As Long
Public Declare Function GetPrivateProfileString Lib "Kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Public Declare Function GetProfileInt Lib "Kernel32" Alias "GetProfileIntA" (ByVal lpAppName As String, ByVal lpKeyName As String, ByVal nDefault As Long) As Long
Public Declare Function GetProfileString Lib "Kernel32" Alias "GetProfileStringA" (ByVal lpAppName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long) As Long
Public Declare Function WriteProfileString Lib "Kernel32" Alias "WriteProfileStringA" (ByVal lpszSection As String, ByVal lpszKeyName As String, ByVal lpszString As String) As Long
Public Declare Function WritePrivateProfileString Lib "Kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpString As Any, ByVal lpFileName As String) As Long

Public Function CheckData(outctl As Control) As Integer
'Init fortran calc routines and perform a data check
'
'outctl - a listbox to receive status messages
'
  Dim strErrLocation As String
  On Error GoTo Error_Handler
  
  'args for agread
  Dim iunits As Long
  Dim istat As Long
  Dim itype As Long
  ReDim idat(2) As Long
  ReDim adat(2) As Single
  Dim cdat As String
  Dim clen As Long
  Dim s As String
  Dim squal As Integer
  
  Dim DKAction As Integer
  Dim DKshowmsg As Integer
  Dim DKanswer As Integer
  Dim np As Integer
  ReDim Diam(MAX_DROPS - 1) As Single
  ReDim mfrac(MAX_DROPS - 1) As Single
  Dim Msg As String
  Dim MBType As Integer
  Dim outstr As String
  Dim minstr As String
  Dim maxstr As String

  'Add a message to the calc log
  Msg = ""
  AppendStr Msg, Date$ & " " & Time$, False
  AppendStr Msg, " Checking data", False
  AddToLog outctl, Msg
  AppendStr UC.MessageLog, Msg, True

  CheckData = False 'default return value

  'Pass the User Data to the fortran
  Call aginit(UD, AGINIT_NORMAL)

  'Get and display summary messages
  ' agread info: istat 0=OK, keep reading
  '                    1=warning, supressible: keep reading
  '                    2=error, suppressible: keep reading
  '                    3=error, non-suppressible: STOP
  '                    4=End of data, STOP
  '              itype 0=char data only
  '                    1=int and char data
  '                    2=real and char data
  '          adat/idat Limits in arrays: (0)=value (1)=min (2)=max
  UC.MaxErrorLevel = 0 'reset the max error level
  iunits = UP.Units    'set the units flag
  cdat = Space$(40)    'make space for the return string
  
  Do
    Call agread(iunits, istat, itype, adat(0), cdat, clen)
    Select Case istat  'error level
    Case 0: 'normal, keep reading
      Select Case itype
      Case 0: 'no message or data
      Case 1: 'message
        Msg = Trim$(cdat)
        AddToLog outctl, Msg
        AppendStr UC.MessageLog, Msg, True
      Case 2: 'message and float data
        Msg = cdat & " " & AGFormat$(adat(0))
        AddToLog outctl, Msg
        AppendStr UC.MessageLog, Msg, True
      Case 3: 'message and integer data
        Msg = cdat & " " & Format$(CInt(adat(0)))
        AddToLog outctl, Msg
        AppendStr UC.MessageLog, Msg, True
      End Select
    Case 1: 'warning, suppressible
      If UC.MaxErrorLevel < 1 Then UC.MaxErrorLevel = 1
      Msg = ""
      AppendStr Msg, "Warning!", True
      Select Case itype
      Case 0: 'no message or data
      Case 1: 'message
        AddToLog outctl, Trim$(cdat)
        AppendStr Msg, cdat, True
      Case 2: 'message and float data
        AddToLog outctl, cdat & " " & AGFormat$(adat(0))
        AppendStr Msg, "Although calculations may continue, ", True
        AppendStr Msg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr Msg, "is beyond recommended range. The limits are:", True
        AppendStr Msg, "", True
        AppendStr Msg, "Min: " & AGFormat$(adat(1)), True
        AppendStr Msg, "Val: " & AGFormat$(adat(0)), True
        AppendStr Msg, "Max: " & AGFormat$(adat(2)), True
      Case 3: 'message and integer data
        AddToLog outctl, cdat & " " & Format$(CInt(adat(0)))
        AppendStr Msg, "Although calculations may continue, ", True
        AppendStr Msg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr Msg, "is beyond recommended range. The limits are:", True
        AppendStr Msg, "", True
        AppendStr Msg, "Min: " & Format$(CInt(adat(1))), True
        AppendStr Msg, "Val: " & Format$(CInt(adat(0))), True
        AppendStr Msg, "Max: " & Format$(CInt(adat(2))), True
      End Select
      'Add the Warning to the log
      AppendStr UC.MessageLog, Msg, False
      'Warn the user, if appropriate
      If Not UP.SuppressCalcWarnings And Not UI.CalcsBatchMode Then
        AppendStr Msg, "", True
        AppendStr Msg, "Continue with calculations?", True
        If MsgBox(Msg, vbExclamation + vbYesNo) = vbNo Then
          Exit Do
        End If
      End If
    Case 2: 'error, suppressible
      If UC.MaxErrorLevel < 2 Then UC.MaxErrorLevel = 2
      Msg = ""
      AppendStr Msg, "Error!", True
      Select Case itype
      Case 0: 'no message or data
      Case 1: 'message
        AddToLog outctl, Trim$(cdat)
        AppendStr Msg, cdat, True
      Case 2: 'message and float data
        AddToLog outctl, cdat & " " & AGFormat$(adat(0))
        AppendStr Msg, "Although calculations may continue, ", True
        AppendStr Msg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr Msg, "is beyond recommended range. The limits are:", True
        AppendStr Msg, "", True
        AppendStr Msg, "Min: " & AGFormat$(adat(1)), True
        AppendStr Msg, "Val: " & AGFormat$(adat(0)), True
        AppendStr Msg, "Max: " & AGFormat$(adat(2)), True
      Case 3: 'message and integer data
        AddToLog outctl, cdat & " " & Format$(CInt(adat(0)))
        AppendStr Msg, "Although calculations may continue, ", True
        AppendStr Msg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr Msg, "is beyond recommended range. The limits are:", True
        AppendStr Msg, "", True
        AppendStr Msg, "Min: " & Format$(CInt(adat(1))), True
        AppendStr Msg, "Val: " & Format$(CInt(adat(0))), True
        AppendStr Msg, "Max: " & Format$(CInt(adat(2))), True
      End Select
      'Add the Error to the log
      AppendStr UC.MessageLog, Msg, False
      'Warn the user, if appropriate
      If Not UP.SuppressCalcErrors And Not UI.CalcsBatchMode Then
        AppendStr Msg, "", True
        AppendStr Msg, "Continue with calculations?", True
        If MsgBox(Msg, vbExclamation + vbYesNo) = vbNo Then
          Exit Do
        End If
      End If
    Case 3: 'error, non-suppressible: stop
      If UC.MaxErrorLevel < 3 Then UC.MaxErrorLevel = 3
      Msg = ""
      AppendStr Msg, "Error!", True
      Select Case itype
      Case 0: 'no message or data
      Case 1: 'message
        AddToLog outctl, Trim$(cdat)
        AppendStr Msg, cdat, True
      Case 2: 'message and float data
        AddToLog outctl, cdat & " " & AGFormat$(adat(0))
        AppendStr Msg, "Calculations cannot continue because", True
        AppendStr Msg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr Msg, "is out of range. The limits are:", True
        AppendStr Msg, "", True
        AppendStr Msg, "Min: " & AGFormat$(adat(1)), True
        AppendStr Msg, "Val: " & AGFormat$(adat(0)), True
        AppendStr Msg, "Max: " & AGFormat$(adat(2)), True
      Case 3: 'message and integer data
        AddToLog outctl, cdat & " " & Format$(CInt(adat(0)))
        AppendStr Msg, "Calculations cannot continue because", True
        AppendStr Msg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr Msg, "is out of range. The limits are:", True
        AppendStr Msg, "", True
        AppendStr Msg, "Min: " & Format$(CInt(adat(1))), True
        AppendStr Msg, "Val: " & Format$(CInt(adat(0))), True
        AppendStr Msg, "Max: " & Format$(CInt(adat(2))), True
      End Select
      'Add the Error to the log
      AppendStr UC.MessageLog, Msg, False
      'Tell the user about the error and stop
      If Not UI.CalcsBatchMode Then
        MsgBox Msg, vbCritical + vbOKOnly
      End If
      Exit Do
    Case 4: 'normal end of data
      UI.DataNeedsChecking = False 'data has been checked and is okay
      CheckData = True
      Exit Do
    Case Else 'just in case...
      Exit Do
    End Select
    DoEvents
  Loop While True

'====================================================
'Exit Point for CheckData
'====================================================
Exit_CheckData:
  Exit Function


'====================================================
'            ERROR HANDLER ROUTINE(S)
'====================================================
Error_Handler:
  gobjErrors.Append Err, "CheckData", "basAGDISP", strErrLocation

  gobjErrors.UserMessage
  gobjErrors.WriteToErrorLog
  gobjErrors.Clear
  Resume Exit_CheckData
End Function

Public Sub AddToLog(c As Control, s As String)
'Add a string to the Log control
  On Error Resume Next
  Dim fsave As Control
  c.AddItem s
  c.Refresh
  Set fsave = ActiveControl 'save current control
  c.SetFocus           'set focus to list box
  SendKeys "{END}"                  'send an END key to the list box
  DoEvents
  c.Selected(c.ListIndex) = False   'turn off highlight
  fsave.SetFocus                    'restore original focus
End Sub

Public Function CalcARS(xBK As DropKirkData, showmsg As Integer, _
  MaxErrLev As Integer, squal As Integer, _
  np As Integer, Diam() As Single, mfrac() As Single) As Integer
'Calculate ARS drop distribution
'
' xBK       i   DropKirkData (ARS) structure containing input data
' showmsg   i   if true, show calc warning and error messages
' MaxErrLev o   maximum error level during calculations
' squal     o   spray quality -1:none,use np/diam/mfrac  0-10:basic dsd
' np        o   number of points in diam/mfrac arrays
' Diam      o   array of drop diameters
' mfrac     o   array of mass fractions
'
' function returns true if calcs are successful, false if not
'
  'agars arguments
  Dim nd As Long
  Dim ier As Long
  Dim iunits As Long
  Dim lfl As Long
  Dim iqual As Long
  ReDim realwd(2) As Single
  Dim cdat As String
  Dim clen As Long
  
  Dim minstr As String
  Dim maxstr As String
  Dim outstr As String

  'call agars to do the dropkirk calcs
  '
  'AGARS: first call with LFL=0 to get back the nozzle/deflector text.
  '       Then call the subroutine with LFL=1.
  '       If there is NO WARNING/ERROR with the first checked variable,
  '       the code will INTERNALLY increment LFL and check the next variable, etc.
  '       If there is NO WARNING/ERROR to any inputs, the code will complete the
  '       calculation and return LFL=6!  However, if there is a WARNING/ERROR that
  '       the user can decide to CONTINUE, then you (TBC) must increment the
  '       present value of LFL, then call the subroutine.
  '       If the user makes a change to the offending variable, either by WARNING
  '       or ERROR, you do NOT increment LFL but simply call AGARS.
  '       The code will go directly to the variable that previously offended us.
  '       In this way I hope we will navigate through the inputs to run the ARS model.
  '       -MET
  '
  '  xBK    - BKDATA data structure
  '  iunits - Units flag: 0 = English; 1 = metric
  '  lfl    - Operations flag: 0 = initialization of calculation
  '  iqual  - Size class flag: -1 = no; 0-10XX = class to use
  '  nd     - Number of points in drop size distribution
  '  Diam() - Drop size distribution array
  '  mfrac()- Cumulative Volume fraction array
  '  ier    - Error flag: 0 = No warning or error message
  '                       1 = Write warning information
  '                       2 = Write error information and continue
  '                       3 = Write error information and return
  '  realwd - Real data array (value, minimum, maximum)
  '  cdat   - Character string
  '  clen   - Length of character string
  '
  CalcARS = False           'default return value
  MaxErrLev = 0             'reset max error level
  iunits = UP.Units         'get units from preferences
  lfl = 0                   '1-4 are the calculation levels
  cdat = Space$(40)         'allocate string space
  'initialize
  Call agars(xBK, iunits, lfl, iqual, _
              nd, Diam(0), mfrac(0), ier, realwd(0), cdat, clen)
  lfl = 1
  Do
    Call agars(xBK, iunits, lfl, iqual, _
                nd, Diam(0), mfrac(0), ier, realwd(0), cdat, clen)
    squal = CInt(iqual) 'convert to integer
    np = CInt(nd) 'convert to integer
    Select Case ier
      Case 0  'OK. No warning or error
        'The calcs are complete and successful.
        CalcARS = True  'Success!
        Exit Do
      Case 1, 2 'warning/error with msg and data. Can contine.
        If MaxErrLev < ier Then MaxErrLev = ier
        If showmsg Then
          outstr = AGFormat$(realwd(0))
          minstr = AGFormat$(realwd(1))
          maxstr = AGFormat$(realwd(2))
          Select Case ier
          Case 1
            Msg = "Warning!" + Chr$(13)
          Case 2
            Msg = "Error!" + Chr$(13)
          End Select
          Msg = Msg + "Although calculations may continue," + Chr$(13)
          Msg = Msg + Chr$(34) + Trim$(cdat) + Chr$(34) + Chr$(13)
          Msg = Msg + "is beyond recommended limits. The limits are:" + Chr$(13)
          Msg = Msg + Chr$(13)
          Msg = Msg + "Min: " + minstr + Chr$(13)
          Msg = Msg + "Val: " + outstr + Chr$(13)
          Msg = Msg + "Max: " + maxstr + Chr$(13)
          Msg = Msg + Chr$(13)
          Msg = Msg + "Continue with calculations?"
          If MsgBox(Msg, vbExclamation + vbYesNo) = vbNo Then
            Exit Do
          End If
        End If
      Case 3 'error with msg and data. Stop.
        If MaxErrLev < ier Then MaxErrLev = ier
        If showmsg Then
          outstr = AGFormat$(realwd(0))
          minstr = AGFormat$(realwd(1))
          maxstr = AGFormat$(realwd(2))
  
          Msg = "Error!" + Chr$(13)
          Msg = Msg + "Calculations cannot continue because" + Chr$(13)
          Msg = Msg + Chr$(34) + Trim$(cdat) + Chr$(34) + Chr$(13)
          Msg = Msg + "is out of range. The limits are:" + Chr$(13)
          Msg = Msg + Chr$(13)
          Msg = Msg + "Min: " + minstr + Chr$(13)
          Msg = Msg + "Val: " + outstr + Chr$(13)
          Msg = Msg + "Max: " + maxstr
          MsgBox Msg, vbCritical + vbOKOnly
        End If
        Exit Do
      Case Else 'just in case...
        MsgBox "Unexpected ier value in CalcARS: " & CStr(ier), vbCritical + vbOKOnly
        Exit Do
    End Select
  Loop
'TBC  'Convert agars's Cumulative mass fractions to Incremental mass fractions
'TBC  If np > 0 Then
'TBC    For i = np - 1 To 1 Step -1
'TBC      mfrac(i) = mfrac(i) - mfrac(i - 1)
'TBC    Next i
'TBC  End If
End Function

Function AGFormat$(X)
'Format numbers for display using special rules
  Dim s As String
  If Abs(X) > 999999.9999 Then                'huge numbers
    s = Format$(X, "Scientific")
  ElseIf Abs(X) > 0 And Abs(X) < 0.0001 Then  'tiny numbers
    s = Format$(X, "Scientific")
  ElseIf Abs(X) >= 1 Then                     'smallish numbers
    s = Format$(X, "#####0.##")
  Else                                        '0 and everything else
    s = Format$(X, "#####0.####")
  End If
  'get rid of hanging decimal points
  If Right$(s, 1) = "." Then s = Left$(s, Len(s) - 1)
  'return the formatted string
  AGFormat$ = s
End Function

Sub AppendStr(buf As String, newstr As String, addcr As Integer)
'append newstr to buf, add a <crlf> optionally
  buf = buf & newstr
  If addcr Then buf = buf & vbCrLf
End Sub

Sub CenterForm(f As Form)
'Center the form on the screen
  f.Left = (Screen.Width / 2) - (f.Width / 2)
  f.Top = (Screen.Height / 2) - (f.Height / 2)
End Sub

Public Sub UserDataDefault(xUD As UserData)
'Set program data to Forestry default values
  Dim dum1 As Single, dum2 As Single
  
  'The Basic DSD is referenced in a couple of places
  'here. This constant makes sure that use is consistant.
  Const DEFAULT_BASICDSD = 4 'ASAE Fine to Medium
  
  'General
  With xUD
    .Title = "Untitled"
    .Notes = ""
    .SCIPUFFFLAG = 0          'Always 0 for Agdisp use
    .SCIPUFFCUTOFF = 0        'Always 0 for Agdisp use
    .ApplMethod = AM_AERIAL
    .AerialType = AD_LIQUID
  End With

  'Ground
  With xUD.GA
    .NumSwaths = 20      'Swaths for ground sprayer
    .NozType = 0         'Flat Fan
    .Pressure = 4.137    'bar (60 psig)
  End With

  'DSD
  GetBasicDataDSD DEFAULT_BASICDSD, xUD.DSD

  'Rotary Atomizer
  With xUD.HK
    .MaxErrorLevel = 0
    .MatType = 0 'Water
    .RotType = 0 'AU4000
    .Speed = 0   'will be set to match aircraft
    .BladeAngle = 50 'deg
    .BladeRPM = 5000 'rpm
    .Flowrate = 0 'will be set to match SM flowrate
    .SprayType = 1   'DSD
  End With
  
  'DropKirk
  With xUD.BK
    .MaxErrorLevel = 0
    .NozType = 0
    .NameNoz = "Ceramic Disk Core 45"
    .LNameNoz = 20
    .Orifice = 6 'was 0.061
    .Speed = 44.7    'm/s (100 mph)
    .NozAngle = 30
    .Pressure = 4.137 'bar (60 psig)
    .SprayType = 1  'DSD
  End With

  'Spray Mat
  With xUD.SM
    .Type = 0        'Basic
    .BasicType = 1   'Water
    .Name = GetBasicNameSM(.BasicType)
    .LName = Len(.Name)
    .CalcInputSelect = 0 'Input rates
    .NVfrac = 0.03
    .ACfrac = 0.015
    .ActSolFrac = 0.015
    .AddSolFrac = 0.015
    .ActNVFrac = 1
    .AddNVFrac = 1
    .Flowrate = 18.71 'L/ha
    .FlowrateUnits = 0 'L/ha
    .SpecGrav = 1
    .NonVGrav = 1
    .EvapRate = 84.76
  End With
  
  'Aircraft Data
  GetLibraryAircraft "Air Tractor AT-401", xUD.AC
  'Fill in advanced items not set above
  With xUD.AC
    .PropEff = 0.8
    .DragCoeff = 0.1
  End With

  'Nozzle Distribution Data
  With xUD.NZ
    .Type = 1 'user-def
    .Name = "Nozzle Distribution"
    .LName = Len(.Name)
    'These values are for the AT-401
    .NumNoz = 42
    For i = 0 To MAX_NOZZLES - 1
      .PosHoriz(i) = 0
      .PosVert(i) = 0
      .PosFwd(i) = 0
    Next
    .PosHoriz(0) = -4.8585
    .PosHoriz(1) = -4.6215
    .PosHoriz(2) = -4.3845
    .PosHoriz(3) = -4.1475
    .PosHoriz(4) = -3.9105
    .PosHoriz(5) = -3.6735
    .PosHoriz(6) = -3.4365
    .PosHoriz(7) = -3.1995
    .PosHoriz(8) = -2.9625
    .PosHoriz(9) = -2.7255
    .PosHoriz(10) = -2.4885
    .PosHoriz(11) = -2.2515
    .PosHoriz(12) = -2.0145
    .PosHoriz(13) = -1.7775
    .PosHoriz(14) = -1.5405
    .PosHoriz(15) = -1.3035
    .PosHoriz(16) = -1.0665
    .PosHoriz(17) = -0.8295001
    .PosHoriz(18) = -0.5925001
    .PosHoriz(19) = -0.3555001
    .PosHoriz(20) = -0.1185001
    .PosHoriz(21) = 0.1184999
    .PosHoriz(22) = 0.3554999
    .PosHoriz(23) = 0.5925
    .PosHoriz(24) = 0.8295
    .PosHoriz(25) = 1.0665
    .PosHoriz(26) = 1.3035
    .PosHoriz(27) = 1.5405
    .PosHoriz(28) = 1.7775
    .PosHoriz(29) = 2.0145
    .PosHoriz(30) = 2.2515
    .PosHoriz(31) = 2.4885
    .PosHoriz(32) = 2.7255
    .PosHoriz(33) = 2.9625
    .PosHoriz(34) = 3.1995
    .PosHoriz(35) = 3.4365
    .PosHoriz(36) = 3.6735
    .PosHoriz(37) = 3.9105
    .PosHoriz(38) = 4.1475
    .PosHoriz(39) = 4.3845
    .PosHoriz(40) = 4.6215
    .PosHoriz(41) = 4.8585
    .PosHorizLimit = 0
    UpdateBoomWidth xUD.NZ, xUD.AC.SemiSpan, dum1, dum2 '.BoomWidth
  End With

  'Dry Delivery
  With xUD.DRY
    .Type = 0                 'venturi
    .Sphericity = 1
    .Width = 4
    .angle = 30
    .Velocity = 10
    .Hub = 0.35
    .RPM = 500
    .Length = 6
  End With
  
  'Met
  With xUD.MET
    .WindType = 0         'Single Height
    .NumWinds = 0         'Wind Table Entries
    For i = 0 To MAX_WINDS - 1
      .WindHeight(i) = 0
      .WindSpeed(i) = 0
    Next i
    .WS = 2.235           '5 mph
    .WD = -90             '-90 deg
    .WH = 2               '2 m
    .temp = 18.333        '65 deg F
    .Humidity = 50
    .Pressure = 1013
    .VortexDecayIGE = 0.56
    .VortexDecayOGE = 0.15
    .SurfRough = 0.0075   '.0246 ft
    .Insolation = 4       'Overcast (Day)
  End With

  'Canopy
  With xUD.CAN
    .Type = 3 'Basic
    .Name = ""
    .LName = 0
    .EleSiz = 0.02
    .EleTyp = 0 'flat plate
    .StanDen = 494.2088  '200 st/ac
    .NumEnv = 0
'    .EnvHgt()
'    .EnvDiam()
'    .EnvPop()
    .optType = 1
    .LibHgt = 24.32
    .LibLAI = 2.57
    .LibB = 0.426
    .LibC = 1.856
    .LibDataQuality = 1
    .NumLAI = 0
'    .LAIHgt()
'    .LAICum()
    .temp = xUD.MET.temp          'same as Met
    .Humidity = xUD.MET.Humidity  'same as Met
    .NDRuff = 0.14
    .NDDisp = 0.7
    .Height = 21.336  '70 ft
  End With
  
  'Terrain
  With xUD.TRN
    .Zref = 0
    .Upslope = 0
    .Sideslope = 0
  End With
  
'  'Riparian Barrier
'  With xUD.RIP
'    .Dist = 0
'    .Height = 0
'    .Porosity = 0
'    .EleSiz = 0
'  End With
  
  'Control
  With xUD.CTL
    .Height = 30.48       '100 ft
    .NumLines = 20
    For i = 0 To MAX_FLTLINES - 1
      .LineReps(i) = 1
    Next
    .LineOptimize = 0
    .SwathWidth = 18.2882 '60 ft
    .SwathDisp = 0        '0 ft
    .SwathOffset = 0      '1/2 Swath
    .FluxPlane = 0
    .MaxComputeTime = 6000 '6000 sec
    .MaxDownwindDist = 795 '795 m
    .HalfBoom = 0 'full boom
    .SaveTraj = 0 'don't save traj files
  End With

End Sub

Function FileDialog(act, ftype, fname As String) As Integer
'Dialog box for obtaining a file name for UserData
' act - common dialog box action
'       1=FD_OPEN=Open 2=FD_SAVEAS=Save As
' ftype file filter type
'       1=FD_TYPE_USER=.ag 2=FD_TYPE_TEXT=.txt 3=FD_TYPE_LIB=.mdb
' fname the selected file name, if selection is successful.
'       fname is unchanged otherwise
'
' returns: true on OK, false on Cancel
'
  Dim d As Control
  Dim fn As String

  Set d = frmMain.CMDialog1
  '
  On Error GoTo ErrHandlerFND
  'Turn on CancelError
  d.CancelError = True
  'Set Default Extension
  Select Case ftype
  Case FD_TYPE_USER 'AGDISP user files
    'added if a file name is entered without an extension
    d.DefaultExt = "agd"
    'Set filter list
    d.Filter = "All Files (*.*)|*.*|Data Files (*.ag)|*.ag"
  Case FD_TYPE_TEXT 'Text files
    'added if a file name is entered without an extension
    d.DefaultExt = "txt"
    'Set filter list
    d.Filter = "All Files (*.*)|*.*|Text Files (*.txt)|*.txt"
  Case FD_TYPE_LIB 'User library
    'added if a file name is entered without an extension
    d.DefaultExt = "mdb"
    'Set filter list
    d.Filter = "All Files (*.*)|*.*|Library Files (*.mdb)|*.mdb"
  End Select
  'Specify current filter
  d.FilterIndex = 2
  'Set the default file name
  d.FileName = fname
  'Set dialog flags
  d.Flags = cdlOFNHideReadOnly
  'Display the dialog box
  Select Case act
  Case FD_OPEN
    d.ShowOpen
  Case FD_SAVEAS
    d.ShowSave
  End Select
  'full file path is CMDialog1.FileName
  'file name only is CMDialog1.FileTitle
  
  'check for file existance for Save As Function
  fn = d.FileTitle
  If act = FD_SAVEAS And FileExists(fn) Then
    If MsgBox("File exists. Replace?", vbQuestion + vbYesNo) = vbNo Then
      FileDialog = False
      Exit Function
    End If
  End If
  
  If ftype = FD_TYPE_USER Then UI.FileName = d.FileTitle
  
  If ftype = FD_TYPE_LIB Then
    fname = d.FileName 'save whole path
  Else
    fname = d.FileTitle
  End If
  FileDialog = True
  Exit Function

ErrHandlerFND:
'User pressed cancel button
  FileDialog = False
  Exit Function

End Function

Function FileExists(fn)
'return true if the given file exists, false otherwise
  On Error GoTo ErrHandlerFE  'start error trapping
  X = GetAttr(fn) 'if this fails, the file doesn't exist
  FileExists = True
  Exit Function
  
ErrHandlerFE:
  FileExists = False
  Exit Function

End Function

Public Sub UpdateBoomWidth(xNZ As NozzleData, SemiSpan As Single, _
                LeftExtent As Single, RightExtent As Single)
'calculate Nozzle Distribution Extents and update the BoomWidth
'
'xNZ          I  NozzleData stucture containing NozzleData
'SemiSpan     I  aircraft semispan (or rotor radius)
'LeftExtent   O  leftmost nozzle extent as a percent of semispan
'RightExtent  O  rightmost nozzle extent as a percent of semispan
'
'Modifies: xNZ.BoomWidth

  Dim farleft As Single
  Dim farright As Single
  Dim PosHoriz As Single
  Dim i As Integer
  
  'find the farthest-out nozzle
  farleft = 0
  farright = 0
  For i = 0 To xNZ.NumNoz - 1
    PosHoriz = xNZ.PosHoriz(i)
    If PosHoriz > farright Then farright = PosHoriz
    If PosHoriz < farleft Then farleft = PosHoriz
  Next
  'calculate the extents
  LeftExtent = -farleft / SemiSpan * 100
  RightExtent = farright / SemiSpan * 100
  'Select the larger one for the BoomWidth
  If LeftExtent > RightExtent Then
    xNZ.BoomWidth = LeftExtent
  Else
    xNZ.BoomWidth = RightExtent
  End If
End Sub

Public Function GenRegNozDist(xNZ As NozzleData, SemiSpan As Single, _
  sNozzles As String, sExtent As String, sSpacing As String) As Boolean
'generate a regularly-spaced nozzle distribution
'
'Two of the three parameters (nozzles, extent, spacing) must be provided.
'
'When the input parameters cannot be implemented exactly, Spacing is given
'priority over Extent or Number of Nozzles.
'
'The nozzle distribution in xNZ is replaced by the new one.
'If the routine is unsuccessful, it returns False and the nozzle distribution
'is unchanged.

  Dim Msg As String
  Dim inflag As Integer
  Dim limit As Single
  Dim nnoz As Integer
  Dim spacing As Single
  Dim tmpspc As Single
  
  Dim hmax As Single
  Dim i As Integer
  Dim n As Integer
  Dim dh As Single
  
  GenRegNozDist = False 'default value
  
  'extract parameter values
  inflag = 0
  'Extent
  If Len(Trim$(sExtent)) > 0 Then
    inflag = inflag + 1  'Bit 1: Extent
    limit = SemiSpan * (Abs(Val(sExtent)) / 100)
  End If
  'Number of Nozzles
  If Len(Trim$(sNozzles)) > 0 Then
    inflag = inflag + 2  'Bit 2: Number
    nnoz = Abs(Val(sNozzles))
    If nnoz = 0 Then
      Msg = "Nozzles must be at least 1"
      MsgBox Msg, vbInformation + vbOKOnly
      nnoz = 1
    ElseIf nnoz > MAX_NOZZLES Then
      Msg = "Nozzles may not be greater than " & Format$(MAX_NOZZLES)
      MsgBox Msg, vbInformation + vbOKOnly
      nnoz = MAX_NOZZLES
    End If
  End If
  'Spacing
  If Len(Trim$(sSpacing)) > 0 Then
    inflag = inflag + 4 'Bit 3: Spacing
    spacing = UnitsInternal(Abs(Val(sSpacing)), UN_LENGTH)
  End If
    
  'set up for the generation
  'The first 3 bits of inflag show which parameters were supplied.
  Select Case inflag
  Case 3 '(011) limit, number: need spacing
    If nnoz > 1 Then
      spacing = 2 * limit / (nnoz - 1)
    Else
      spacing = 0
    End If
  Case 5 '(101) limit, spacing: need number
    If spacing = 0 Or limit = 0 Then
      MsgBox "Enter positive Extent and Spacing.", vbCritical + vbOKOnly
      Exit Function
    End If
    'Find the number of nozzles we need
    nnoz = (2 * limit / spacing) + 1
    'don't exceed the max number of nozzles
    If nnoz > MAX_NOZZLES Then
      Msg = "This distribution requires more than " + _
            Format$(MAX_NOZZLES) + " nozzles. The Extent " + _
            "will be reduced to match this limit."
      MsgBox Msg, vbInformation + vbOKOnly
      nnoz = MAX_NOZZLES
    End If
    'adjust limit to exactly match spacing
    limit = (nnoz - 1) * spacing * 0.5
  Case 6 '(110) number, spacing: need limit
    limit = (nnoz - 1) * spacing * 0.5
  Case 7 '(111) all three specified - too many!
    Msg = "Only two values should be specified to generate a regular distribution."
    MsgBox Msg, vbCritical + vbOKOnly
    Exit Function
  Case Else 'zero or 1 specified
    Msg = "Two values are needed to generate a regular distribution."
    MsgBox Msg, vbCritical + vbOKOnly
    Exit Function
  End Select

  'generate the distribution
  For i = 0 To nnoz - 1
    xNZ.PosHoriz(i) = limit - spacing * (nnoz - i - 1)
    If Abs(xNZ.PosHoriz(i)) < 0.001 Then xNZ.PosHoriz(i) = 0
    xNZ.PosVert(i) = 0
    xNZ.PosFwd(i) = 0
  Next
  xNZ.NumNoz = nnoz
  xNZ.Type = 1 'user-defined

  'Success
  GenRegNozDist = True
End Function

Sub GetBasicDataDSD(BasicType, xDSD As DropSizeDistData)
'Get Basic Drop Distribution data
'
  Dim DB As Database
  Dim DS As Recordset
  Dim s As String

  If Not LibOpenDB(DB) Then Exit Sub
  If Not LibOpenRS(DB, "BasicDSD", DS) Then Exit Sub
  
  'position the Recordset to the correct record
  DS.FindFirst "Type = " & Format$(BasicType)
  If DS.NoMatch Then Exit Sub
  
  With xDSD
    .Type = 0   'basic
    .BasicType = BasicType
    .Name = GetBasicNameDSD(BasicType)
    .LName = Len(.Name)
    .NumDrop = DS.Fields("NumDrop")
    FieldToArray DS.Fields("Diam"), .Diam()
    FieldToArray DS.Fields("Frac"), .MassFrac()
  End With
End Sub

Sub GetBasicDataDSDSwathDisp(BasicType, SwathDisp)
'Get Swath displacement information associated with Basic Drop Distribution data
'
'returns: Swath Displacement as a fraction of swath width
'
  Dim DB As Database
  Dim DS As Recordset
  Dim s As String

  If Not LibOpenDB(DB) Then Exit Sub
  If Not LibOpenRS(DB, "BasicDSD", DS) Then Exit Sub
  
  'position the Recordset to the correct record
  DS.FindFirst "Type = " & Format$(BasicType)
  If DS.NoMatch Then Exit Sub
  
  SwathDisp = DS.Fields("SwathDisp")
  
  DS.Close
  DB.Close
End Sub

Sub GetBasicDataSM(BasicType As Integer, xSM As SprayMaterialData)
'Get Basic Spray Material data
  With xSM
    Select Case BasicType
    Case 0 'Oil
      .SpecGrav = 0.92
      .NonVGrav = 0.92
      .EvapRate = 0
    Case 1 'water
      .SpecGrav = 1
      .NonVGrav = 1
      .EvapRate = 84.76
    End Select
  End With
End Sub

Function GetBasicNameAM(BasicType As Integer)
'return the name of the Application Method
  ReDim Basiclbl(1) As String
  Basiclbl(0) = "Aerial"
  Basiclbl(1) = "Ground"
  
  GetBasicNameAM = Basiclbl(BasicType)
End Function

Function GetBasicNameDSD(BasicType) As String
'return the name of the Basic Drop Size Distribution of
'the specified type
  Dim DB As Database
  Dim DS As Recordset
  Dim crit As String
  
  If Not LibOpenDB(DB) Then GetBasicNameDSD = "": Exit Function
  
  Set DS = DB.OpenRecordset("BasicDSD", dbOpenDynaset)
  
  'position the Recordset to the correct record
  crit = "Type = " & Format$(BasicType)
  DS.FindFirst crit
  If DS.NoMatch Then GetBasicNameDSD = "": Exit Function
  GetBasicNameDSD = Trim$(DS.Fields("Name"))
  DS.Close
  DB.Close
End Function

Function GetBasicNameSM(BasicType)
'return the name of the Basic Spray Material
  ReDim Basiclbl(1) As String
  Basiclbl(0) = "Oil"
  Basiclbl(1) = "Water"
  
  GetBasicNameSM = Basiclbl(BasicType)
End Function

Public Function GetLibraryAircraft(ACName As String, xAC As AircraftData) As Boolean
'Retrieve an aircraft from the library and transfer the data to xAC
  Dim DB As Database
  Dim RS As Recordset
  
  GetLibraryAircraft = False 'default return value
  
  If LibOpenDB(DB) Then
    If LibOpenRS(DB, "Aircraft", RS) Then
      'Find the aircraft
      RS.FindFirst "Name='" & ACName & "'"
      If Not (RS.BOF And RS.EOF) Then
       With xAC
          .Type = 2 'library
          '.BasicType  (not set)
          .Name = RS("Name")
          .LName = Len(Trim$(.Name))
          .WingType = RS("Type")
          .SemiSpan = RS("SemiSpan")
          .TypSpeed = RS("TypSpeed")
          .BiplSep = RS("BiplSep")
          .Weight = RS("Weight")
          .PlanArea = RS("PlanArea")
          .PropRPM = RS("PropRPM")
          .PropRad = RS("PropRad")
          '.PropEff
          .EngVert = RS("EngVert")
          .EngFwd = RS("EngFwd")
          .NumEng = RS("NumEng")
          FieldToArray RS("EngHoriz"), .EngHoriz()
          .WingVert = RS("WingVert")
          .BoomVert = RS("BoomVert")
          .BoomFwd = RS("BoomFwd")
          '.DragCoeff
        End With
        GetLibraryAircraft = True
      End If
      RS.Close
    End If
    DB.Close
  End If
End Function

Function GetARSNozName(Index) As String
'return the name of the ARS Nozzle
  Dim DB As Database
  Dim RS As Recordset
  
  GetARSNozName = "" 'default return value
  
  If Not LibOpenDB(DB) Then Exit Function
  If Not LibOpenRS(DB, "ARSNozzle", RS) Then Exit Function
  
  'position the Recordset to the correct record
  RS.FindFirst "Index=" & Format$(Index)
  If RS.NoMatch Then Exit Function
  GetARSNozName = Trim$(RS.Fields("Name"))
  RS.Close
  DB.Close
End Function

Function GetARSNozNameType(Index) As String
  Dim xBK As DropKirkData
  Dim iunits As Long
  Dim lfl As Long
  Dim iqual As Long
  Dim nd As Long
  Dim Diam(MAX_DROPS - 1) As Single
  Dim mfrac(MAX_DROPS - 1) As Single
  Dim ier As Long
  ReDim realwd(2) As Single
  Dim cdat As String
  Dim clen As Long
  
  'Call agars with lfl=0 simply to get the Type Name string
  'The only input needed is the index
  'The only output of significance is cdat
  
  xBK.NozType = Index
  iunits = UP.Units         'get units from preferences
  lfl = 0                   'reset agkirk counter
  cdat = Space$(40)         'allocate string space
 
  Call agars(xBK, iunits, lfl, iqual, _
              nd, Diam(0), mfrac(0), ier, realwd(0), cdat, clen)
  GetARSNozNameType = Trim$(cdat)
End Function

Sub GetARSNozData(Index, _
                  OfcUnits As Integer, OfcLabel As String, OfcPrefix As String, _
                  NumOfc As Integer, OfcVal() As Single, _
                  ModUnits As Integer, ModLabel As String, ModPrefix As String, _
                  NumMod As Integer, ModVal() As Single)
'Get ARS Nozzle Data from agars
  Dim iunit As Long
  Dim lfl As Long
  Dim iqual As Long
  Dim npts As Long
  
'Get ARS Nozzle Data from the library
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  
  If Not LibOpenDB(DB) Then Exit Sub
  If Not LibOpenRS(DB, "ARSNozzle", RS) Then Exit Sub
  
  'position the Recordset to the correct record
  RS.FindFirst "Index = " & Format$(Index)
  If RS.NoMatch Then Exit Sub
  
'  OfcUnits = RS.Fields("OfcUnits")
'  OfcLabel = Trim$(RS.Fields("OfcLabel"))
'  OfcPrefix = Trim$(RS.Fields("OfcPrefix"))
'  NumOfc = RS.Fields("NumOfc")
'  FieldToArray RS.Fields("OfcVal"), OfcVal()
'  ModUnits = RS.Fields("ModUnits")
'  ModLabel = Trim$(RS.Fields("ModLabel"))
'  ModPrefix = Trim$(RS.Fields("ModPrefix"))
'  NumMod = RS.Fields("NumMod")
'  FieldToArray RS.Fields("ModVal"), ModVal()
  
  RS.Close
  DB.Close
End Sub

Sub GetHelp(Topic As Integer)
' Displays the selected help topic selected from either
' Editors;' or Viewer's help menu.
'
    If HelpTopic = -1 Then
        '
        ' "Using Help" was selected so display the Standard Windows Help
        ' Topic for "Using Help".
        '
        HtmlHelp frmMain.hWnd, App.HelpFile, HH_HELP_CONTEXT, 0&
    Else
        ' A help topic other the "Using help" was selected.
        '
        HtmlHelp frmMain.hWnd, App.HelpFile, HH_HELP_CONTEXT, ByVal CLng(Topic)
    End If

End Sub

Sub GetLibDataDSD(key As String, nd As Integer, Diam() As Single, frac() As Single)
'Get Library Drop Distribution data
'
' key  i  a "master" search key comprised of a set of 6
'         subkeys separated by commas. This is exactly what
'         frmDropLib returns.
'
' xDSD o  a fully formatted Drop Size Distribution retrieved
'         from the library.
'
  Dim DB As Database
  Dim DS As Recordset
  Dim crit As String
  Dim s As String
  Dim keyLib As String
  Dim keySubst As String
  Dim keyNoz As String
  Dim keyNozAng As String
  Dim keyNozRPM As String
  Dim keyPressure As String
  Dim keyWS As String
  Dim ndlong As Long
  Dim dslflag As Integer

  If Trim$(key) = "" Then Exit Sub

  'Extract search keys from the master key
  start = 1
  comma = InStr(start, key, ",")
  keylen = comma - start
  keyLib = Mid$(key, start, keylen)
  start = comma + 1
  comma = InStr(start, key, ",")
  keylen = comma - start
  keySubst = Mid$(key, start, keylen)
  start = comma + 1
  comma = InStr(start, key, ",")
  keylen = comma - start
  keyNoz = Mid$(key, start, keylen)
  start = comma + 1
  comma = InStr(start, key, ",")
  keylen = comma - start
  keyNozAng = Mid$(key, start, keylen)
  start = comma + 1
  comma = InStr(start, key, ",")
  keylen = comma - start
  keyNozRPM = Mid$(key, start, keylen)
  start = comma + 1
  comma = InStr(start, key, ",")
  keylen = comma - start
  keyPressure = Mid$(key, start, keylen)
  start = comma + 1
  keyWS = Mid$(key, start)

  If Not LibOpenDB(DB) Then Exit Sub
  If keyLib = "0" Then
    Set DS = DB.OpenRecordset("Dropsize", dbOpenDynaset)
  Else
    Set DS = DB.OpenRecordset("DropsizeFS", dbOpenDynaset)
  End If
  
  'position the Recordset to the correct record
  crit = "Substance = '" & keySubst & "'"
  crit = crit & " and Nozzle = '" & keyNoz & "'"
  crit = crit & " and NozzleAngle = " & keyNozAng
  crit = crit & " and NozzleRPM = " & keyNozRPM
  crit = crit & " and Pressure = " & keyPressure
  crit = crit & " and WindSpeed = " & keyWS
  DS.FindFirst crit
  If DS.NoMatch Then
    nd = 0
    Exit Sub
  End If

  'Retrieve the data from the library
  nd = 32 'all lib entries have 32 categories
  FieldToArray DS.Fields("MassFrac"), frac()
  dslflag = DS.Fields("DSLflag") 'flag used to get diams below
  DS.Close

  'retrieve the diameter data from a different table
  If keyLib = "0" Then
    Set DS = DB.OpenRecordset("Dropdiam", dbOpenDynaset)
  Else
    Set DS = DB.OpenRecordset("DropdiamFS", dbOpenDynaset)
  End If
  DS.FindFirst "DSLflag = " & CStr(dslflag)
  If DS.NoMatch Then
    nd = 0
    Exit Sub
  End If
  FieldToArray DS.Fields("Diameter"), Diam()
  DS.Close
  DB.Close
End Sub

Function GetRunID() As String
'return a runid associated with the current data set.
'the ID is a chracter string consisting of:
' - the file name
' - the version of AGDISP that performed the calculations
' - the date and time when the calculations were started
  Dim s As String
  s = App.Title & " " & UI.FileName
  If UC.Valid Then
    s = s & " " & Format$(UC.CodeVersion, "#0.00")
    s = s & " " & UC.StartDate
    s = s & " " & UC.StartTime
  Else
    s = s & " " & Format$(AGDISPVERSION, "#0.00")
    s = s & " 00-00-0000"
    s = s & " 00:00:00"
  End If
  GetRunID = s
End Function

Function GetTypeNameAC(INTYPE As Integer)
'return the name of the Aircraft type
  ReDim Typelbl(3) As String
  Typelbl(0) = "Reference"
  Typelbl(1) = "User-defined"
  Typelbl(2) = "Library" 'SDTF
  Typelbl(3) = "Library" 'FS
  
  GetTypeNameAC = Typelbl(INTYPE)
End Function

Function GetTypeNameACWing(INTYPE As Integer)
'return the name of the Aircraft WingType
  ReDim Typelbl(3 To 4) As String
  Typelbl(3) = "Fixed-Wing"
  Typelbl(4) = "Helicopter"
  
  GetTypeNameACWing = Typelbl(INTYPE)
End Function

Function GetTypeNameAerial(INTYPE As Integer)
  Dim s As String
  Select Case INTYPE
  Case 0: s = "Liquid"
  Case 1: s = "Dry"
  Case Else: s = ""
  End Select
  GetTypeNameAerial = s
End Function

Function GetTypeNameGANoz(INTYPE As Integer)
  Dim s As String
  Select Case INTYPE
  Case 0: s = "Flat Fan"
  Case 1: s = "Air Injection"
  Case Else: s = ""
  End Select
  GetTypeNameGANoz = s
End Function

Function GetTypeNameSpreader(INTYPE As Integer)
  Dim s As String
  Select Case INTYPE
  Case 0: s = "Venturi"
  Case 1: s = "Radial"
  Case 2: s = "Bucket"
  Case Else: s = ""
  End Select
  GetTypeNameSpreader = s
End Function

Function GetTypeNameStability(INTYPE As Integer)
  Dim s As String
  Select Case INTYPE
  Case 0: s = "Strong"
  Case 1: s = "Moderate"
  Case 2: s = "Slight"
  Case 3: s = "Weak"
  Case 4: s = "Overcast"
  Case 5: s = "Thinly Overcast"
  Case 6: s = "Less Than 3/8ths Overcast"
  Case Else: s = ""
  End Select
  GetTypeNameStability = s
End Function

Function GetNameHKMat(INTYPE As Integer)
'return the name of the Rotary Nozzle Spray Material
  Dim s As String
  Select Case INTYPE
  Case 0: s = "Water"
  Case 1: s = "Water with 1% w/w Sta-Put®"
  Case 2: s = "Water with 0.25% w/w Hasten®"
  Case 3: s = "Foray 76B neat"
  Case Else: s = ""
  End Select
  GetNameHKMat = s
End Function
Function GetNameHKNoz(INTYPE As Integer)
'return the name of the Rotary Nozzle
  Dim s As String
  Select Case INTYPE
  Case 0: s = "Micronair AU4000"
  Case 1: s = "Micronair AU5000"
  Case Else: s = ""
  End Select
  GetNameHKNoz = s
End Function

Function GetTypeNameDSD(INTYPE As Integer)
'return the name of the Drop Size Distribution type
  ReDim Typelbl(6) As String
  Typelbl(0) = "Reference"
  Typelbl(1) = "DropKick"
  Typelbl(2) = "User-defined"
  Typelbl(3) = "Library" 'SDTF
  Typelbl(4) = "Library" 'FS
  Typelbl(5) = "USDA ARS"
  Typelbl(6) = "FS Rotary Atomizer"
  
  GetTypeNameDSD = Typelbl(INTYPE)
End Function

Function GetTypeNameCN(INTYPE As Integer)
'return the name of the Canopy type
  Select Case INTYPE
  Case 0: GetTypeNameCN = "None"
  Case 1: GetTypeNameCN = "Story"
  Case 2: GetTypeNameCN = "LAI"
  Case 3: GetTypeNameCN = "Height"
  Case Else
    GetTypeNameCN = ""
  End Select
End Function

Function GetTypeNameCanopyElement(INTYPE As Integer)
'return the name of the Canopy Element Type
  Select Case INTYPE
  Case 0: GetTypeNameCanopyElement = "Flat Plate"
  Case 1: GetTypeNameCanopyElement = "Cylinder"
  Case 2: GetTypeNameCanopyElement = "Sphere"
  Case Else
    GetTypeNameCanopyElement = ""
  End Select
End Function

Function GetTypeNameOP(INTYPE As Integer)
'return the name of the Optical Canopy type
  Select Case INTYPE
  Case 1: GetTypeNameOP = "User-Defined"
  Case 2: GetTypeNameOP = "Library"
  Case Else
    GetTypeNameOP = ""
  End Select
End Function

Function GetTypeNameSM(INTYPE As Integer)
'return the name of the Spray Material type
  ReDim Typelbl(2) As String
  Typelbl(0) = "Reference"
  Typelbl(1) = "User-defined"
  Typelbl(2) = "Library"
  
  GetTypeNameSM = Typelbl(INTYPE)
End Function

Sub InitInterface(xUI As InterfaceData)
'Init User Interface data items
  xUI.HasConfidentialData = False
  xUI.FileName = ""
  xUI.LibraryPath = LibGetFileName()
  xUI.MAALibraryPath = App.Path & "\" & "agdmaa.mdb"
  xUI.DataChanged = False
  xUI.CalcsBatchMode = False
  xUI.OkToDoCalcs = True
  xUI.DataNeedsChecking = True
  xUI.CalcsInProgress = False
  xUI.StartCalcsOnLoad = False
  xUI.RevertCalcsAvailable = False
  xUI.PlotVar = 0
  'CALPUFF
  xUI.PuffSprayLineBeginningX = 0
  xUI.PuffSprayLineBeginningY = 0
  xUI.PuffBaseElevationZ = 0
  xUI.PuffSprayLineLength = 0
  xUI.PuffFlightDirection = 0
  xUI.PuffFileName = "Calpuff.txt"
End Sub

Function LineFromString(s, start) As String
'returns the next line, delineated by CRLF, from the
'given string, without the CRLF. on return, start is set
'to a position after the next CRLF
  If start > Len(s) Then
    start = 0
    LineFromString = ""
    Exit Function
  End If
  idx = InStr(start, s, vbCrLf) 'find next crlf after the specified start
  If idx = 0 Then
    slen = Len(s) - start + 1
    LineFromString = Mid$(s, start, slen)
    start = 0
  Else
    slen = idx - start
    LineFromString = Mid$(s, start, slen)
    start = idx + Len(vbCrLf)
  End If
End Function

Function LinesInString(s)
'count lines in given string by counting the number
'of carriage return/linefeeds within the string
  lines = 0
  start = 1
  Do
    idx = InStr(start, s, vbCrLf)
    lines = lines + 1
    start = idx + Len(vbCrLf)
  Loop While idx > 0
  LinesInString = lines
End Function

'---------------------------------------------------------------------------
' Main:
'    Main entry point for application
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2005-05-06  TBC  Added error handling initialization
'
'---------------------------------------------------------------------------
Public Sub Main()
'This is the first routine executed when AGDISP starts up
  Set gobjErrors = New clsErrors 'set up error handling

  App.HelpFile = App.Path & "\" & "agdisp.chm" 'define the help file
  InitInterface UI     'Init User Inteface data
  ReadGeneralPrefs     'Initialize User Preferences
  UnitsSelectSystem UP.Units 'set units system
  UserDataDefault UD   'set user data to default values
  ClearUserCalc UC     'reset calculation data
  PlotSettingsInit PS  'Init Plot setting data
  PlotPrefsRead PS     'Override Plot Settings with User Prefs
  BackupPlotDataSources 'Init other plot data
  frmMain.Show          'Show the main form
  If UP.ShowAboutOnStartup Then frmAbout.Show vbModal
End Sub

Function NewFile() As Integer
'Reset user data to default state

  NewFile = False 'default status
  'save changed data first
  If Not QuerySaveChanged() Then Exit Function
    
  UserDataDefault UD     'load default data into user data
  ClearUserCalc UC        'wipe out existing calcs
  UI.FileName = ""       'wipe out any existing file name
  UpdateDataChangedFlag False 'reset the changed flag
  NewFile = True 'Success
End Function

Function OpenFile() As Integer
'Open a user file and read its contents
  
  OpenFile = False 'default status
  'save changed data first
  If Not QuerySaveChanged() Then Exit Function
    
  'open the file dialog and get a file name
  If FileDialog(FD_OPEN, FD_TYPE_USER, UI.FileName) Then 'if the selection is good
    OpenFile = UserDataRead(UI.FileName, UD, UC, True)
    UpdateDataChangedFlag False
  End If
End Function

Function PerformCalcs(Pause As Integer) As Integer
'load the calcuations form and do the calcs on the current data
  'If Pause is true, the user must press start to continue
  UI.StartCalcsOnLoad = Not Pause
  UI.CalcsBatchMode = False 'set form mode flag
  frmCalc.Show vbModal
  Select Case frmCalc.Tag
  Case "ok"
    PerformCalcs = True
  Case Else
    PerformCalcs = False
  End Select
  Unload frmCalc
End Function

Sub PrintData(ReportText As String, pvw As Integer, pages, Mag)
'print the current UserData
' This routine handles printing to the printer object, and
' to an array of picture boxes (for print=preview).
'
'  ReportText i A String containing the raw text to be
'               printed.
'  pvw        i   the destination of the output
'                   if true, this is a preview
'                   if false, this is a print job
'  pages      o   the number of nonblank pages produced
'  Mag        i   the desired magnification for
'                 the picture box. Ignored if pvw is false.
'
  Dim s As String
  Dim f As Form
  Dim stmp As String
  Dim PVfontname As String
  Dim PVfontsize As Single
  Dim LineHeight As Single
  Dim LinesPerPage As Integer
  Dim lines As Integer
  Dim MarginTop As Single
  Dim MarginLeft As Single
  Dim MarginBottom As Single
  Dim MarginRight As Single

  On Error GoTo ErrHandPrintData
  
  'set the margins
  MarginTop = 1
  MarginBottom = 1
  MarginLeft = 1
  MarginRight = 1

  'Set the output font
  PVfontname = "Courier New"
  PVfontsize = 10
  LineHeight = PVfontsize / 72# 'Set line height in inches

  'Printer object general setup
  Printer.FontName = PVfontname     'select printer font
  Printer.FontSize = PVfontsize     'select printer fontsize
  Printer.FontBold = False
  Printer.FontItalic = False
  Printer.FontStrikethru = False
  Printer.FontTransparent = False
  Printer.FontUnderline = False
  Printer.ScaleMode = 5             'set printer scaling to inches
  PageHeight = Printer.ScaleHeight - MarginTop - MarginBottom
  PageWidth = Printer.ScaleWidth - MarginLeft - MarginRight
  'scale the output page to obtain the top and left margins
  'the Printer scale height and width are already set
  Printer.ScaleLeft = -MarginLeft
  Printer.ScaleTop = -MarginTop
  'Set the starting Position
  Printer.CurrentX = 0
  Printer.CurrentY = 0

  'preview form general setup
  If pvw Then
    pages = 1
    frmPrintPreview.picPage(1).FontName = PVfontname
    frmPrintPreview.picPage(1).FontSize = PVfontsize * Mag
    frmPrintPreview.picPage(1).FontBold = False
    frmPrintPreview.picPage(1).FontItalic = False
    frmPrintPreview.picPage(1).FontStrikethru = False
    frmPrintPreview.picPage(1).FontTransparent = False
    frmPrintPreview.picPage(1).FontUnderline = False
    'set the size of the pictureboxes
    frmPrintPreview.picPage(1).Height = Printer.Height * Mag
    frmPrintPreview.picPage(1).Width = Printer.Width * Mag
    'scale the output page
    frmPrintPreview.picPage(1).ScaleMode = 0       'set form scaling to user
    frmPrintPreview.picPage(1).ScaleLeft = Printer.ScaleLeft
    frmPrintPreview.picPage(1).ScaleWidth = Printer.ScaleWidth
    frmPrintPreview.picPage(1).ScaleTop = Printer.ScaleTop
    frmPrintPreview.picPage(1).ScaleHeight = Printer.ScaleHeight
    frmPrintPreview.picPage(1).CurrentX = 0
    frmPrintPreview.picPage(1).CurrentY = 0
  End If
  
  'Initialize Line counter and limit
  LinesPerPage = PageHeight / LineHeight
  lines = 0

  'print page header
  GoSub printHeader
  
  'Format the report text
  start = 1
  Do
    s = LineFromString(ReportText, start)
    GoSub PrintString
  Loop While start > 0
  
  If Not pvw Then
    Printer.EndDoc
  End If
PrintDataExit:
  Exit Sub

'subroutines -----------------------------------------------------

PrintUnderlineOn:
  If pvw Then
    frmPrintPreview.picPage(pages).FontUnderline = True
  Else
    Printer.FontUnderline = True
  End If
  Return

PrintUnderlineOff:
  If pvw Then
    frmPrintPreview.picPage(pages).FontUnderline = False
  Else
    Printer.FontUnderline = False
  End If
  Return

printHeader:
'subroutine for printing page header
  If pvw Then
    stmp = " " & GetRunID()
    frmPrintPreview.picPage(pages).Line (0, 0)-(PageWidth, 1.5 * LineHeight), , B
    frmPrintPreview.picPage(pages).CurrentX = 0
    frmPrintPreview.picPage(pages).CurrentY = 0.25 * LineHeight
    frmPrintPreview.picPage(pages).Print stmp;
    stmp = "Page " + Format$(pages) + " "
    frmPrintPreview.picPage(pages).CurrentX = PageWidth - Printer.TextWidth(stmp)
    frmPrintPreview.picPage(pages).CurrentY = 0.25 * LineHeight
    frmPrintPreview.picPage(pages).Print stmp;
    lines = 4
  Else
    stmp = " " & GetRunID()
    Printer.Line (0, 0)-(PageWidth, 1.5 * LineHeight), , B
    Printer.CurrentX = 0
    Printer.CurrentY = 0.25 * LineHeight
    Printer.Print stmp;
    stmp = "Page " + Format$(Printer.Page) + " "
    Printer.CurrentX = PageWidth - Printer.TextWidth(stmp)
    Printer.CurrentY = 0.25 * LineHeight
    Printer.Print stmp;
    lines = 4
  End If
  Return

'subroutine for printing line passed by string "s"
PrintString:
  'see if there's room on the current page
  If lines >= LinesPerPage Then
    GoSub PrintNewPage
    GoSub printHeader
  End If
  If pvw Then
    frmPrintPreview.picPage(pages).CurrentX = 0
    frmPrintPreview.picPage(pages).CurrentY = lines * LineHeight
    frmPrintPreview.picPage(pages).Print s;
  Else
    Printer.CurrentX = 0
    Printer.CurrentY = lines * LineHeight
    Printer.Print s;
  End If
  lines = lines + 1
  Return

PrintNewPage:
'prepare a new page for printing
  If pvw Then
    'if this is a preview, load a new picturebox
    pages = pages + 1
    Load frmPrintPreview.picPage(pages)
  Else
    'if we're actually printing, output the current page
    Printer.NewPage
  End If
  lines = 0
  Return

ErrHandPrintData:
'error handler
  MsgBox "Error " + Format$(Err.Number) + " during printing: " + Err.Description
  Resume PrintDataExit
  
End Sub

Function PrintDialog(BeginPage As Integer, EndPage As Integer, NumCopies As Integer) As Integer
'bring up a printing dialog box and return some stats
'
'This procedure does no printing. It only gathers
'user selections for printing

  Dim i As Integer
  Dim c As Control

  Const THISFORM = 0
  Const THEPRINTER = 1

  'Set the control
  Set c = frmMain.CMDialog1

  'Set Cancel to True
  c.CancelError = True
  On Error GoTo ErrHandlerPD

  'Set dialog box flags
  c.Flags = cdlPDNoSelection + cdlPDNoPageNums + cdlPDHidePrintToFile

  'Display the Print Dialog box
  c.ShowPrinter

  'Get user-selected values
  BeginPage = c.FromPage
  EndPage = c.ToPage
  NumCopies = c.Copies
  PrintDialog = True
  Exit Function

ErrHandlerPD:
  'User pressed Cancel button
  BeginPage = 0
  EndPage = 0
  NumCopies = 0
  PrintDialog = False
  Exit Function

End Function

Function QuerySaveChanged()
'check the status of DataChanged and give the user
' a chance to save data.
'
'returns:  true  if sucessful
'          false cancelled
'
  Dim ans As Integer
  Dim Msg As String
  Dim MBType As Integer

  QuerySaveChanged = True 'the default
  'give the user a chance to save the current data, if needed
  If UI.DataChanged Then
    Msg = "The current data has changed." + Chr$(13)
    Msg = Msg + "Do you want to save the changes?"
    MBType = vbQuestion + vbYesNoCancel
    ans = MsgBox(Msg, MBType)
    '6="Yes" 7="No" 2="Cancel"
    Select Case ans
    Case vbYes  'Yes - go to the save routine
      If Not SaveAsFile() Then QuerySaveChanged = False
    Case vbNo  'No - just continue
    Case vbCancel  'Cancel - exit this routine
      QuerySaveChanged = False
    End Select
  End If
End Function

Sub ReadGeneralPrefs()
'read settings in the .ini file
  Dim fn As String      '.ini file name
  Dim an As String      'name for .ini file section
  Dim i As Long
  Dim s As String * 255

  'build the settings file name from the Application
  fn = App.Path & Chr$(92) & App.EXEName & ".ini"
  
  'retrieve preferences
  an = "Preferences"
  UP.ShowAboutOnStartup = GetPrivateProfileInt(an, "ShowAboutOnStartup", True, fn)
  UP.PauseBeforeCalc = GetPrivateProfileInt(an, "PauseBeforeCalc", True, fn)
  UP.Units = GetPrivateProfileInt(an, "Units", UN_IMPERIAL, fn)
  UP.DepUnitsFraction = GetPrivateProfileInt(an, "DepUnitsFraction", 0, fn)
  UP.DepUnitsNum = GetPrivateProfileInt(an, "DepUnitsNum", 1, fn)
  UP.DepUnitsDen = GetPrivateProfileInt(an, "DepUnitsDen", 0, fn)
  UP.FluxUnitsFraction = GetPrivateProfileInt(an, "FluxUnitsFraction", 0, fn)
  UP.FluxUnitsNum = GetPrivateProfileInt(an, "FluxUnitsNum", 1, fn)
  UP.FluxUnitsDen = GetPrivateProfileInt(an, "FluxUnitsDen", 0, fn)
  UP.SuppressCalcWarnings = GetPrivateProfileInt(an, "SuppressCalcWarnings", False, fn)
  UP.SuppressCalcErrors = GetPrivateProfileInt(an, "SuppressCalcErrors", False, fn)
  UP.SuppressModelFeatureWarnings = GetPrivateProfileInt(an, "SuppressModelFeatureWarnings", False, fn)
  i = GetPrivateProfileString(an, "UserLib", App.Path & "\userlib.mdb", s, 255, fn)
  UP.UserLib = Left$(s, i)

End Sub

Function UserDataRead(fn As String, xUD As UserData, xUC As UserCalc, Notify As Integer)
'Read UserData and UserCalc from file
'
'fn     i  the file from which to read
'xUD    o  a UserData structure to receive part of the data
'Notify i Error notification flag:
'           true=notify user of errors through message box
'           false=do not notify
'Return value:
'  true=success
'  false=errors encountered
'
'Currently handles file versions 0 - 1
'Remember: "version" in this routine is an integer
'file format version, not the program version
'
'Perhaps a separate routine for each version would be better?
'That would get cumbersome after a while
'Perhaps separate routines for groups of similar versions
'
  Dim Header As String
  Dim version As Integer
  Dim xNZ As NozzleData
  Dim iDrp As Integer, nDrp As Integer
  Dim dummy As Variant
  Dim idummy As Integer

  'read user data
  On Error GoTo UserDataReadErrHand1
  Open fn For Input As #1
  
  'read header to determine format (e.g. "AGDISP_userdata")
  On Error GoTo UserDataReadErrHand2
  Input #1, Header
  
  'check for a non-AGDISP file
  If Header <> "AGDISP_userdata" Then
    MsgBox "Not an AGDISP input file.", vbOKOnly + vbExclamation, "Read data"
    Close #1
    UserDataRead = False
    Exit Function
  End If
    
  'read file version number (not the AGDISP program version number!)
  Input #1, version
  
  'file version > current version (old program used with new data)
  If version > USERFILEVERSION Then
    MsgBox "Unknown file format", vbOKOnly + vbExclamation, "Read data"
    Close #1
    UserDataRead = False
    Exit Function
  End If
  
  'file format is OK. Read the rest of the file
  
  'But first, default the data and the calcs
  UserDataDefault xUD
  ClearUserCalc xUC
  
  'Userdata
  With xUD
    'General
    If version < 9 Then Input #1, dummy '.Tier
    Input #1, .ApplMethod
    Input #1, .AerialType
    If version < 9 Then Input #1, dummy '.Smokey
    Line Input #1, .Title
    'Get the Notes data in multiline format
    Input #1, lines
    .Notes = "" 'start with a blank
    For i = 1 To lines
      Line Input #1, s                   'get next line
      If i < lines Then s = s & vbCrLf 'append CRLF
      .Notes = .Notes & s          'append to the total string
    Next
    
    'Ground Application
    With .GA
      If version < 9 Then Input #1, dummy '.BasicType
      Input #1, .NumSwaths
      If version >= 5 Then
        Input #1, .NozType
        Input #1, .Pressure
      End If
    End With
    
    'Orchard Airblast
    If version < 9 Then
      Input #1, dummy '.BasicType
      Input #1, dummy '.BegTrow
      Input #1, dummy '.EndTrow
    End If
    
    'Drop Size Distribution(s)
    'Read only the first DSD. The program used to support up to three. Discard
    'any additional DSD definitions.
    With .DSD
      Input #1, .Type
      'Since the atomization tables were combined, 3 and 4 are equivalent
      'but 4 is not supported by the interface, so substitute
      If .Type = 4 Then .Type = 3 'Substitute generic lib key for FS lib key
      Input #1, .BasicType
      Line Input #1, .Name
      Input #1, .LName
      Input #1, .NumDrop
      For i = 0 To .NumDrop - 1
        Input #1, .Diam(i), .MassFrac(i)
      Next
      If version < 9 Then Input #1, dummy '.LibSelFlag
    End With
    'Read and discard any additioinal DSDs
    If version < 9 Then
      For iDSD = 1 To 2
        Input #1, dummy '.Type
        Input #1, dummy '.BasicType
        Line Input #1, dummy '.Name
        Input #1, dummy '.LName
        Input #1, idummy '.NumDrop
        For i = 0 To idummy - 1 '.NumDrop - 1
          Input #1, dummy, dummy '.Diam(i), .MassFrac(i)
        Next
        Input #1, dummy '.LibSelFlag
      Next
    End If
    
    'Dropkirk
    'Read only the first set of values, discard extra values defined in versions
    'earlier than 9
    With .BK
      Input #1, .MaxErrorLevel
      Input #1, .NozType
      Line Input #1, .NameNoz
      Input #1, .LNameNoz
      Input #1, .Orifice
      Input #1, .Speed
      Input #1, .NozAngle
      Input #1, .Pressure
      Input #1, .SprayType
      If version >= 5 And version < 9 Then
        Input #1, dummy '.SpectrumSource
      End If
    End With
    'Read and discard any additioinal DSDs
    If version < 9 Then
      For iDSD = 1 To 2
        Input #1, dummy '.MaxErrorLevel
        Input #1, dummy '.NozType
        Line Input #1, dummy '.NameNoz
        Input #1, dummy '.LNameNoz
        Input #1, dummy '.Orifice
        Input #1, dummy '.Speed
        Input #1, dummy '.NozAngle
        Input #1, dummy '.Pressure
        Input #1, dummy '.SprayType
        If version >= 5 Then
          Input #1, dummy '.SpectrumSource
        End If
      Next
    End If
    
    'USDA FS Rotary Nozzle Models
    If version >= 4 Then
      With .HK
        Input #1, .MaxErrorLevel
        Input #1, .MatType
        If version >= 5 Then
          Input #1, .RotType
        End If
        Input #1, .Speed
        Input #1, .BladeAngle
        If version >= 5 Then
          Input #1, .BladeRPM
        End If
        Input #1, .Flowrate
        Input #1, .SprayType
      End With
      If version < 9 Then
        For iDSD = 1 To 2
          Input #1, dummy '.MaxErrorLevel
          Input #1, dummy '.MatType
          If version >= 5 Then
            Input #1, dummy '.RotType
          End If
          Input #1, dummy '.Speed
          Input #1, dummy '.BladeAngle
          If version >= 5 Then
            Input #1, dummy '.BladeRPM
          End If
          Input #1, dummy '.Flowrate
          Input #1, dummy '.SprayType
        Next
      End If
    End If
    
    'Dropkick
    'This model was removed in version 9
    If version < 9 Then
      For iDSD = 1 To 3
        Input #1, dummy '.MaxErrorLevel
        Input #1, dummy '.NozType
        Line Input #1, dummy '.NameNoz
        Input #1, dummy '.LNameNoz
        Input #1, dummy '.VMD
        Input #1, dummy '.RelSpan
        Input #1, dummy '.EffDiam
        Input #1, dummy '.SprayAngle
        Input #1, dummy '.MatType
        Line Input #1, dummy '.NameMat
        Input #1, dummy '.LNameMat
        Input #1, dummy '.DynSurfTens
        Input #1, dummy '.ShearVisc
        Input #1, dummy '.Density
        Input #1, dummy '.ElongVisc
        Input #1, dummy '.Speed
        Input #1, dummy '.NozAngle
        Input #1, dummy '.Pressure
        Input #1, dummy '.FlowType
        Input #1, dummy '.FLOW
        Input #1, dummy '.SprayType
      Next
    End If
    
    'Spray Material
    With .SM
      Input #1, .Type
      Input #1, .BasicType
      Line Input #1, .Name
      Input #1, .LName
      If version >= 6 Then
        Input #1, .CalcInputSelect
      End If
      Input #1, .NVfrac
      Input #1, .ACfrac
      If version >= 6 Then
        Input #1, .ActSolFrac
        Input #1, .AddSolFrac
        Input #1, .ActNVFrac
        Input #1, .AddNVFrac
      End If
      Input #1, .Flowrate
      If version >= 6 Then
        Input #1, .FlowrateUnits
      End If
      Input #1, .SpecGrav
      Input #1, .NonVGrav
      Input #1, .EvapRate
    End With
    
    'Aircraft
    With .AC
      Input #1, .Type
      If version < 9 Then Input #1, dummy '.BasicType
      Line Input #1, .Name
      Input #1, .LName
      Input #1, .WingType
      Input #1, .SemiSpan
      Input #1, .TypSpeed
      Input #1, .BiplSep
      Input #1, .Weight
      Input #1, .PlanArea
      Input #1, .PropRPM
      Input #1, .PropRad
      Input #1, .PropEff
      Input #1, .EngVert
      Input #1, .EngFwd
      Input #1, .NumEng
      If version >= 1 Then
        For i = 0 To MAX_ENGINES - 1
          Input #1, .EngHoriz(i)
        Next
      Else
       'prior to 8.03 the EngHoriz array was read in a loop
       'on NumEng, which was incorrect. The end result was satisfactory
       'for 1- or 2-engine aircraft, so keep reading the old files
       'this way.
        For i = 0 To .NumEng - 1
          Input #1, .EngHoriz(i)
        Next
      End If
      Input #1, .WingVert
      Input #1, .BoomVert
      Input #1, .BoomFwd
      Input #1, .DragCoeff
    End With
    
    'Nozzles
    With .NZ
      Input #1, .Type
      If version < 9 Then Input #1, dummy '.BasicType
      Line Input #1, .Name
      Input #1, .LName
      Input #1, .NumNoz
      If version < 9 Then
        For i = 0 To .NumNoz - 1
          Input #1, dummy, .PosHoriz(i), .PosVert(i), .PosFwd(i)
        Next
      Else
        For i = 0 To .NumNoz - 1
          Input #1, .PosHoriz(i), .PosVert(i), .PosFwd(i)
        Next
      End If
      Input #1, .PosHorizLimit
      Input #1, .BoomWidth
    End With
    
    'Dry Delivery
    With xUD.DRY
      Input #1, .Type
      Input #1, .Sphericity
      Input #1, .Width
      Input #1, .angle
      Input #1, .Velocity
      Input #1, .Hub
      Input #1, .RPM
      Input #1, .Length
    End With
    
    'Meteorology
    With .MET
      If version >= 7 Then
        Input #1, .WindType
        Input #1, .NumWinds
        For i = 0 To .NumWinds - 1
          Input #1, .WindHeight(i), .WindSpeed(i)
        Next i
      End If
      Input #1, .WS
      Input #1, .WD
      Input #1, .WH
      Input #1, .temp
      Input #1, .Humidity
      Input #1, .Pressure
      If version >= 2 Then
        Input #1, .VortexDecayIGE
        Input #1, .VortexDecayOGE
      Else
        Input #1, .VortexDecayIGE
      End If
      Input #1, .SurfRough
      Input #1, .Insolation
    End With
    
    'Canopy
    With .CAN
      Input #1, .Type
      Line Input #1, .Name
      Input #1, .LName
      Input #1, .EleSiz
      If version >= 4 Then
        Input #1, .EleTyp
      End If
      Input #1, .StanDen
      Input #1, .NumEnv
      For i = 0 To .NumEnv - 1
        Input #1, .EnvHgt(i), .EnvDiam(i), .EnvPop(i)
      Next
      Input #1, .optType
      Input #1, .LibHgt
      Input #1, .LibLAI
      Input #1, .LibB
      Input #1, .LibC
      If version >= 5 Then
        Input #1, .LibDataQuality
      End If
      Input #1, .NumLAI
      For i = 0 To .NumLAI - 1
        Input #1, .LAIHgt(i), .LAICum(i)
      Next
      Input #1, .temp
      Input #1, .Humidity
      Input #1, .NDRuff
      Input #1, .NDDisp
      Input #1, .Height
    End With
    
    'Terrain
    With .TRN
      Input #1, .Zref
      Input #1, .Upslope
      Input #1, .Sideslope
    End With
    
    'Riparian Barrier
    If version <= 3 Then
'    With .RIP
      Input #1, dummy '.Dist
      Input #1, dummy '.Height
      Input #1, dummy '.Porosity
      If version >= 3 Then
        Input #1, dummy '.EleSiz
      End If
 '   End With
    End If
    
    'Control
    With .CTL
      Input #1, .Height
      Input #1, .NumLines
      If version >= 2 Then
        For i = 0 To .NumLines - 1
          Input #1, .LineReps(i)
        Next
        Input #1, .LineOptimize
      End If
      If version < 9 Then Input #1, dummy '.SwathWidthType
      Input #1, .SwathWidth
      If version < 9 Then Input #1, dummy '.SwathDispType
      Input #1, .SwathDisp
      If version >= 5 Then
        Input #1, .SwathOffset
      End If
      Input #1, .FluxPlane
      If version < 7 Then
        Input #1, dummy '.TransHgtMin
        Input #1, dummy '.TransHgtMax
      ElseIf version < 9 Then
        Input #1, dummy '.TransportHeight
      End If
      Input #1, .MaxComputeTime
      Input #1, .MaxDownwindDist
      Input #1, .HalfBoom
      If version >= 2 Then
        Input #1, .SaveTraj
      End If
    End With
  End With
  
  'Always force recalcs when reading old files
  If version < USERFILEVERSION Then
    ClearUserCalc xUC
  Else
    'Read in the current calcs
    With xUC
      Input #1, .Valid
      If .Valid Then
        'calc header data
        Input #1, .CodeVersion
        Line Input #1, .StartDate
        Line Input #1, .StartTime
        Input #1, .MaxErrorLevel
        'Get the Message Log in multiline format
        Input #1, lines
        .MessageLog = "" 'start with a blank
        For i = 1 To lines
          Line Input #1, s                   'get next line
          If i < lines Then s = s & vbCrLf 'append CRLF
          .MessageLog = .MessageLog & s 'append to the total string
        Next
      
        'deposition
        Input #1, .NumDep
        ReDim .DepDist(.NumDep)
        ReDim .DepVal(.NumDep)
        For i = 0 To .NumDep - 1
          Input #1, .DepDist(i), .DepVal(i)
        Next
        Input #1, .DepExtrap
        
        'number deposition
        If version >= 8 Then
          Input #1, .NumDrp
          ReDim .DrpDist(.NumDrp)
          ReDim .DrpVal(.NumDrp)
          For i = 0 To .NumDrp - 1
            Input #1, .DrpDist(i), .DrpVal(i)
          Next
        Else
          .NumDrp = 0
        End If
        
        'Vertical Profile
        Input #1, .NumFlux
        ReDim .FluxDist(.NumFlux)
        ReDim .FluxVal(.NumFlux)
        For i = 0 To .NumFlux - 1
          Input #1, .FluxVal(i), .FluxDist(i)
        Next
      
        'average deposition
        Input #1, .NumPID
        ReDim .PIDDist(.NumPID)
        ReDim .PIDVal(.NumPID)
        For i = 0 To .NumPID - 1
          Input #1, .PIDDist(i), .PIDVal(i)
        Next
        Input #1, .PIDExtrap
      
        'Coeffient of Variation
        Input #1, .NumCOV
        ReDim .COVVal(.NumCOV)
        ReDim .COVESW(.NumCOV)
        For i = 0 To .NumCOV - 1
          Input #1, .COVVal(i), .COVESW(i)
        Next
      
        'Concentration
        Input #1, .NumConc
        ReDim .ConcDist(.NumConc)
        ReDim .ConcVal(.NumConc)
        For i = 0 To .NumConc - 1
          Input #1, .ConcVal(i), .ConcDist(i)
        Next
      
        'numerics
        Input #1, .SwathDisp
        Input #1, .SBCOV
        Input #1, .SBMeanDep
        Input #1, .SWATH
        Input #1, .AirborneDrift
        Input #1, .EvapFrac
        Input #1, .AppEff
        Input #1, .DownwindDep
        Input #1, .CanopyDep
      
        'Single-swath deposition
        Input #1, .NumSgl
        ReDim .SglDist(.NumSgl)
        ReDim .SglVal(.NumSgl)
        For i = 0 To .NumSgl - 1
          Input #1, .SglDist(i), .SglVal(i)
        Next
      
        'Fraction Aloft
        .NumFA = 0
        Input #1, .NumFA
        ReDim .FADist(.NumFA)
        ReDim .FAVal(.NumFA)
        For i = 0 To .NumFA - 1
          Input #1, .FADist(i), .FAVal(i)
        Next
      
        'COV Mean Deposition
        .NumCOVM = 0
        Input #1, .NumCOVM
        ReDim .COVMDist(.NumCOVM)
        ReDim .COVMVal(.NumCOVM)
        For i = 0 To .NumCOVM - 1
          Input #1, .COVMDist(i), .COVMVal(i)
        Next
      
        'Single Swath Deposition
        .NumHalf = 0
        Input #1, .NumHalf
        ReDim .HalfDist(.NumHalf)
        ReDim .HalfVal(.NumHalf)
        For i = 0 To .NumHalf - 1
          Input #1, .HalfDist(i), .HalfVal(i)
        Next
      
        'Spray Block Deposition
        .NumSBD = 0
        Input #1, .NumSBD
        ReDim .SBDDist(.NumSBD)
        ReDim .SBDVal(.NumSBD)
        For i = 0 To .NumSBD - 1
          Input #1, .SBDDist(i), .SBDVal(i)
        Next
      
        'Canopy Deposition
        .NumCAN = 0
        Input #1, .NumCAN
        ReDim .CANDist(.NumCAN)
        ReDim .CANVal(.NumCAN)
        For i = 0 To .NumCAN - 1
          Input #1, .CANDist(i), .CANVal(i)
        Next
      
        'Time Accountancy Aloft
        .NumTAA = 0
        Input #1, .NumTAA
        ReDim .TAATime(.NumTAA)
        ReDim .TAAVal(.NumTAA)
        For i = 0 To .NumTAA - 1
          Input #1, .TAATime(i), .TAAVal(i)
        Next
      
        'Time Accountancy Vapor
        .NumTAV = 0
        Input #1, .NumTAV
        ReDim .TAVTime(.NumTAV)
        ReDim .TAVVal(.NumTAV)
        For i = 0 To .NumTAV - 1
          Input #1, .TAVTime(i), .TAVVal(i)
        Next
      
        'Time Accountancy Canopy
        .NumTAC = 0
        Input #1, .NumTAC
        ReDim .TACTime(.NumTAC)
        ReDim .TACVal(.NumTAC)
        For i = 0 To .NumTAC - 1
          Input #1, .TACTime(i), .TACVal(i)
        Next
      
        'Time Accountancy Ground
        .NumTAG = 0
        Input #1, .NumTAG
        ReDim .TAGTime(.NumTAG)
        ReDim .TAGVal(.NumTAG)
        For i = 0 To .NumTAG - 1
          Input #1, .TAGTime(i), .TAGVal(i)
        Next
      
        'Distance Accountancy Aloft
        .NumDAA = 0
        Input #1, .NumDAA
        ReDim .DAADist(.NumDAA)
        ReDim .DAAVal(.NumDAA)
        For i = 0 To .NumDAA - 1
          Input #1, .DAADist(i), .DAAVal(i)
        Next
      
        'Distance Accountancy Vapor
        .NumDAV = 0
        Input #1, .NumDAV
        ReDim .DAVDist(.NumDAV)
        ReDim .DAVVal(.NumDAV)
        For i = 0 To .NumDAV - 1
          Input #1, .DAVDist(i), .DAVVal(i)
        Next
      
        'Distance Accountancy Canopy
        .NumDAC = 0
        Input #1, .NumDAC
        ReDim .DACDist(.NumDAC)
        ReDim .DACVal(.NumDAC)
        For i = 0 To .NumDAC - 1
          Input #1, .DACDist(i), .DACVal(i)
        Next
      
        'Distance Accountancy Ground
        .NumDAG = 0
        Input #1, .NumDAG
        ReDim .DAGDist(.NumDAG)
        ReDim .DAGVal(.NumDAG)
        For i = 0 To .NumDAG - 1
          Input #1, .DAGDist(i), .DAGVal(i)
        Next
      
        'Height Accountancy Aloft
        .NumHAA = 0
        Input #1, .NumHAA
        ReDim .HAAHgt(.NumHAA)
        ReDim .HAAVal(.NumHAA)
        For i = 0 To .NumHAA - 1
          Input #1, .HAAHgt(i), .HAAVal(i)
        Next
      
        'Height Accountancy Vapor
        .NumHAV = 0
        Input #1, .NumHAV
        ReDim .HAVHgt(.NumHAV)
        ReDim .HAVVal(.NumHAV)
        For i = 0 To .NumHAV - 1
          Input #1, .HAVHgt(i), .HAVVal(i)
        Next
      
        'Height Accountancy Canopy
        .NumHAC = 0
        Input #1, .NumHAC
        ReDim .HACHgt(.NumHAC)
        ReDim .HACVal(.NumHAC)
        For i = 0 To .NumHAC - 1
          Input #1, .HACHgt(i), .HACVal(i)
        Next
      
        'Spray Block DSD
        .NumSBDSD = 0
        Input #1, .NumSBDSD
        ReDim .SBDSDDiam(.NumSBDSD)
        ReDim .SBDSDFrac(.NumSBDSD)
        For i = 0 To .NumSBDSD - 1
          Input #1, .SBDSDDiam(i), .SBDSDFrac(i)
        Next
      
        'Downwind DSD
        .NumDWDSD = 0
        Input #1, .NumDWDSD
        ReDim .DWDSDDiam(.NumDWDSD)
        ReDim .DWDSDFrac(.NumDWDSD)
        For i = 0 To .NumDWDSD - 1
          Input #1, .DWDSDDiam(i), .DWDSDFrac(i)
        Next
      
        'Vertical Flux DSD
        .NumFXDSD = 0
        Input #1, .NumFXDSD
        ReDim .FXDSDDiam(.NumFXDSD)
        ReDim .FXDSDFrac(.NumFXDSD)
        For i = 0 To .NumFXDSD - 1
          Input #1, .FXDSDDiam(i), .FXDSDFrac(i)
        Next
      
        'Canopy DSD
        .NumCNDSD = 0
        Input #1, .NumCNDSD
        ReDim .CNDSDDiam(.NumCNDSD)
        ReDim .CNDSDFrac(.NumCNDSD)
        For i = 0 To .NumCNDSD - 1
          Input #1, .CNDSDDiam(i), .CNDSDFrac(i)
        Next
      
        'Point DSD
        .NumPTDSD = 0
        If version >= 7 Then
          Input #1, .NumPTDSD
          ReDim .PTDSDDiam(.NumPTDSD)
          ReDim .PTDSDFrac(.NumPTDSD)
          For i = 0 To .NumPTDSD - 1
            Input #1, .PTDSDDiam(i), .PTDSDFrac(i)
          Next
        End If
      
        'Spray Block Area Coverage
        .NumSBAC = 0
        Input #1, .NumSBAC
        ReDim .SBACRate(.NumSBAC)
        ReDim .SBACFrac(.NumSBAC)
        For i = 0 To .NumSBAC - 1
          Input #1, .SBACRate(i), .SBACFrac(i)
        Next
      
        'Application Layout
        .NumLAY = 0
        Input #1, .NumLAY
        ReDim .LAYDist(.NumLAY)
        ReDim .LAYFrac(.NumLAY)
        For i = 0 To .NumLAY - 1
          Input #1, .LAYDist(i), .LAYFrac(i)
        Next
      End If
    End With
  End If
  
  Close #1
  UserDataRead = True
  Exit Function

UserDataReadErrHand1:
  If Notify Then
    s = "Error opening file: " + fn + Chr$(13) + Error$(Err)
    MsgBox s, vbCritical + vbOKOnly
  End If
  UserDataRead = False
  Exit Function

UserDataReadErrHand2:
  If Notify Then
    s = "Error reading file: " + fn + Chr$(13) + Error$(Err)
    MsgBox s, vbCritical + vbOKOnly
  End If
  Close #1
  UserDataRead = False
  Exit Function
End Function

Function SaveAsFile() As Integer
'Save the current user data to a file,
'always prompting the user for a file name
'
'returns true if sucessful, false if cancelled
'
  SaveAsFile = False 'default
  If Not FileDialog(FD_SAVEAS, FD_TYPE_USER, UI.FileName) Then
    Exit Function
  End If
  If UserDataWrite(UI.FileName, UD, UC, True) Then
    UpdateDataChangedFlag False
    SaveAsFile = True
  End If
End Function

Function SaveFile() As Integer
'Save the current user data to a file, prompting the user
'for a file name only if necessary
'
'returns true if sucessful, false if cancelled
'
  SaveFile = False 'default

  If UI.FileName = "" Then       'if no file name prompt for one
    If Not FileDialog(FD_SAVEAS, FD_TYPE_USER, UI.FileName) Then
      Exit Function
    End If
  End If
  If UserDataWrite(UI.FileName, UD, UC, True) Then
    UpdateDataChangedFlag False
    SaveFile = True
  End If
End Function

Function UnexpectedError(Where As String) As Integer
'General error handling routine
'Just ask the user how to continue with a general message
  Dim Msg As String
  Dim NL As String
  NL = Chr$(13) + Chr$(10)
  Msg = "Unexpected AGDISP Error!" + NL + NL
  Msg = Msg + Chr$(34) + Error$(Err) + Chr$(34) + NL + NL
  Msg = Msg + "Where: " + Where
  UnexpectedError = MsgBox(Msg, vbAbortRetryIgnore)
End Function

Sub UpdateDataChangedFlag(NewValue)
'Update the value of UI.DataChanged
'If it has changed, also update the main form caption
'
' NewValue: the new value for UI.DataChanged
'           True=data has changed since last save
'           False=data has not changed since last save
'
  On Error Resume Next 'In case there is no active form
  If NewValue <> UI.DataChanged Then
    UI.DataChanged = NewValue
    frmMain.Caption = FormCaption
  End If
End Sub

Function FormCaption() As String
'Build a string to act as a form caption.
'Include the current
'file name and an asterisk, if the data has changed
'
  FormCaption = App.Title & _
                IIf(UI.DataChanged Or (UI.FileName <> ""), " ", "") & _
                IIf(UI.DataChanged, "*", "") & _
                UI.FileName
End Function

Sub WriteGeneralPrefs()
'write settings to the .ini file
  Dim fn As String      '.ini file name
  Dim an As String      'name for .ini file section
  Dim stat As Integer   'return value for functions

  'build the settings file name from the Application
  fn = App.Path & Chr$(92) & App.EXEName & ".ini"
  
  'save preferences
  an = "Preferences"
  stat = WritePrivateProfileString(an, "ShowAboutOnStartup", CStr(UP.ShowAboutOnStartup), fn)
  stat = WritePrivateProfileString(an, "PauseBeforeCalc", CStr(UP.PauseBeforeCalc), fn)
  stat = WritePrivateProfileString(an, "Units", CStr(UP.Units), fn)
  stat = WritePrivateProfileString(an, "DepUnitsFraction", CStr(UP.DepUnitsFraction), fn)
  stat = WritePrivateProfileString(an, "DepUnitsNum", CStr(UP.DepUnitsNum), fn)
  stat = WritePrivateProfileString(an, "DepUnitsDen", CStr(UP.DepUnitsDen), fn)
  stat = WritePrivateProfileString(an, "FluxUnitsFraction", CStr(UP.FluxUnitsFraction), fn)
  stat = WritePrivateProfileString(an, "FluxUnitsNum", CStr(UP.FluxUnitsNum), fn)
  stat = WritePrivateProfileString(an, "FluxUnitsDen", CStr(UP.FluxUnitsDen), fn)
  stat = WritePrivateProfileString(an, "SuppressCalcWarnings", CStr(UP.SuppressCalcWarnings), fn)
  stat = WritePrivateProfileString(an, "SuppressCalcErrors", CStr(UP.SuppressCalcErrors), fn)
  stat = WritePrivateProfileString(an, "SuppressModelFeatureWarnings", CStr(UP.SuppressModelFeatureWarnings), fn)
  stat = WritePrivateProfileString(an, "UserLib", UP.UserLib, fn)
End Sub

Function UserDataWrite(fn As String, xUD As UserData, xUC As UserCalc, Notify As Integer)
'Write user data to a file
'fn    i  the file to which to write
'xUD   i  a UserData structure to supply the data
'Notify i Error notification flag:
'           true=notify user of errors through message box
'           false=do not notify
'Return value:
'  true=success
'  false=errors encountered
'
'File format history:
'0: original format
'1: corrected aircraft EngHoriz writing/reading. Before, this array
'   was incorrectly written in a loop on NumEng. Now the entire
'   2-element array is written, no matter what value NumEng has.
'2: added UD.MET.VortexDecayIGE/OGE (was just VortexDecay), added UD.CTL.LineReps(),
'   LineOptimize, and SaveTraj
'3: added UD.RIP.EleSiz
'4: added HK.*, CAN.EleTyp
'5: added UD.HK.RotType, UD.HK.BladeRPM, UD.CTL.SwathOffset
'   UD.CAN.LibDataQuality, UD.DK.SpectrumSource,
'   UD.GA.NozType, UD.GA.Pressure
'6: UD.SM: added CalcInputSelect, ActSolFrac, AddSolFrac, ActNVFrac, AddNVFrac,
'   FlowrateUnits
'7: Starting with 8.18:
'   Added UD.Met.WindType, .NumWinds, .WindHeight(), .WindSpeed(),
'   Replaced UD.CTL.MinTransHgt and .MaxTransHgt with .Transportheight
'   Added Point DSD (UC.NumPTDSD, etc.) after Canopy DSD
'8: Starting with 8.20:
'   Added UC.NumDrp, UC.DrpDist(), UC.DrpVal()
'9: Starting with 8.29:
'   Remove UD.Tier, UD.Smokey, UD.DSD.LibSelFlag
'   Change UD.DSD,UD.BK,UD.HK from arrays to single elements
'   Remove UD.DK, UD.OA
'
  On Error GoTo UserDataWriteErrHand1
  Open fn For Output As #1
  'write header info
  On Error GoTo UserDataWriteErrHand2
  Print #1, "AGDISP_userdata"
  Print #1, USERFILEVERSION  'save the format version
  
  'Userdata
  With xUD
    'General
    Print #1, .ApplMethod
    Print #1, .AerialType
    Print #1, .Title
    'write Notes
    lines = LinesInString(.Notes) 'count # of text lines in Notes
    Print #1, lines
    start = 1
    Do
      Print #1, LineFromString(.Notes, start)
    Loop While start > 0
    
    'Ground Application
    With .GA
      Print #1, .NumSwaths
      Print #1, .NozType
      Print #1, .Pressure
    End With
    
    'Drop Size Distribution(s)
    With .DSD
      Print #1, .Type
      Print #1, .BasicType
      Print #1, .Name
      Print #1, .LName
      Print #1, .NumDrop
      For i = 0 To .NumDrop - 1
        Print #1, .Diam(i); .MassFrac(i)
      Next
    End With
    
    'Dropkirk
    With .BK
      Print #1, .MaxErrorLevel
      Print #1, .NozType
      Print #1, .NameNoz
      Print #1, .LNameNoz
      Print #1, .Orifice
      Print #1, .Speed
      Print #1, .NozAngle
      Print #1, .Pressure
      Print #1, .SprayType
    End With
    
    'USDA FS Rotary Nozzle Models
    With .HK
      Print #1, .MaxErrorLevel
      Print #1, .MatType
      Print #1, .RotType
      Print #1, .Speed
      Print #1, .BladeAngle
      Print #1, .BladeRPM
      Print #1, .Flowrate
      Print #1, .SprayType
    End With
    
    'Spray Material
    With .SM
      Print #1, .Type
      Print #1, .BasicType
      Print #1, .Name
      Print #1, .LName
      Print #1, .CalcInputSelect
      Print #1, .NVfrac
      Print #1, .ACfrac
      Print #1, .ActSolFrac
      Print #1, .AddSolFrac
      Print #1, .ActNVFrac
      Print #1, .AddNVFrac
      Print #1, .Flowrate
      Print #1, .FlowrateUnits
      Print #1, .SpecGrav
      Print #1, .NonVGrav
      Print #1, .EvapRate
    End With
    
    'Aircraft
    With .AC
      Print #1, .Type
      Print #1, .Name
      Print #1, .LName
      Print #1, .WingType
      Print #1, .SemiSpan
      Print #1, .TypSpeed
      Print #1, .BiplSep
      Print #1, .Weight
      Print #1, .PlanArea
      Print #1, .PropRPM
      Print #1, .PropRad
      Print #1, .PropEff
      Print #1, .EngVert
      Print #1, .EngFwd
      Print #1, .NumEng
      For i = 0 To MAX_ENGINES - 1
        Print #1, .EngHoriz(i)
      Next
      Print #1, .WingVert
      Print #1, .BoomVert
      Print #1, .BoomFwd
      Print #1, .DragCoeff
    End With
    
    'Nozzles
    With .NZ
      Print #1, .Type
      Print #1, .Name
      Print #1, .LName
      Print #1, .NumNoz
      For i = 0 To .NumNoz - 1
        Print #1, .PosHoriz(i); .PosVert(i); .PosFwd(i)
      Next
      Print #1, .PosHorizLimit
      Print #1, .BoomWidth
    End With
    
    'Dry Delivery
    With xUD.DRY
      Print #1, .Type
      Print #1, .Sphericity
      Print #1, .Width
      Print #1, .angle
      Print #1, .Velocity
      Print #1, .Hub
      Print #1, .RPM
      Print #1, .Length
    End With
    
    'Meteorology
    With .MET
      Print #1, .WindType
      Print #1, .NumWinds
      For i = 0 To .NumWinds - 1
        Print #1, .WindHeight(i); .WindSpeed(i)
      Next i
      Print #1, .WS
      Print #1, .WD
      Print #1, .WH
      Print #1, .temp
      Print #1, .Humidity
      Print #1, .Pressure
      Print #1, .VortexDecayIGE
      Print #1, .VortexDecayOGE
      Print #1, .SurfRough
      Print #1, .Insolation
    End With
    
    'Canopy
    With .CAN
      Print #1, .Type
      Print #1, .Name
      Print #1, .LName
      Print #1, .EleSiz
      Print #1, .EleTyp
      Print #1, .StanDen
      Print #1, .NumEnv
      For i = 0 To .NumEnv - 1
        Print #1, .EnvHgt(i); .EnvDiam(i); .EnvPop(i)
      Next
      Print #1, .optType
      Print #1, .LibHgt
      Print #1, .LibLAI
      Print #1, .LibB
      Print #1, .LibC
      Print #1, .LibDataQuality
      Print #1, .NumLAI
      For i = 0 To .NumLAI - 1
        Print #1, .LAIHgt(i); .LAICum(i)
      Next
      Print #1, .temp
      Print #1, .Humidity
      Print #1, .NDRuff
      Print #1, .NDDisp
      Print #1, .Height
    End With
    
    'Terrain
    With .TRN
      Print #1, .Zref
      Print #1, .Upslope
      Print #1, .Sideslope
    End With
    
'    'Riparian Barrier
'    With .RIP
'      Print #1, .Dist
'      Print #1, .Height
'      Print #1, .Porosity
'      Print #1, .EleSiz
'    End With
    
    'Control
    With .CTL
      Print #1, .Height
      Print #1, .NumLines
      For i = 0 To .NumLines - 1
        Print #1, .LineReps(i)
      Next
      Print #1, .LineOptimize
      Print #1, .SwathWidth
      Print #1, .SwathDisp
      Print #1, .SwathOffset
      Print #1, .FluxPlane
      Print #1, .MaxComputeTime
      Print #1, .MaxDownwindDist
      Print #1, .HalfBoom
      Print #1, .SaveTraj
    End With
  End With
  
  'Read user calcs if there are any
  With xUC
    Print #1, .Valid
    If .Valid Then
      'calc header data
      Print #1, .CodeVersion
      Print #1, .StartDate
      Print #1, .StartTime
      Print #1, .MaxErrorLevel
      'write MessageLog
      lines = LinesInString(.MessageLog) 'count # of text lines
      Print #1, lines
      start = 1
      Do
        Print #1, LineFromString(.MessageLog, start)
      Loop While start > 0
      
      'deposition
      Print #1, .NumDep
      For i = 0 To .NumDep - 1
        Print #1, .DepDist(i); .DepVal(i)
      Next
      Print #1, .DepExtrap
      
      'number deposition
      Print #1, .NumDrp
      For i = 0 To .NumDrp - 1
        Print #1, .DrpDist(i); .DrpVal(i)
      Next
      
      'Vertical Profile
      Print #1, .NumFlux
      For i = 0 To .NumFlux - 1
        Print #1, .FluxVal(i); .FluxDist(i)
      Next
      
      'average deposition
      Print #1, .NumPID
      For i = 0 To .NumPID - 1
        Print #1, .PIDDist(i); .PIDVal(i)
      Next
      Print #1, .PIDExtrap
      
      'Coeffient of Variation
      Print #1, .NumCOV
      For i = 0 To .NumCOV - 1
        Print #1, .COVVal(i); .COVESW(i)
      Next
      
      'Concentration
      Print #1, .NumConc
      For i = 0 To .NumConc - 1
        Print #1, .ConcVal(i); .ConcDist(i)
      Next
      
      'numerics
      Print #1, .SwathDisp
      Print #1, .SBCOV
      Print #1, .SBMeanDep
      Print #1, .SWATH
      Print #1, .AirborneDrift
      Print #1, .EvapFrac
      Print #1, .AppEff
      Print #1, .DownwindDep
      Print #1, .CanopyDep
      
      'Single-swath deposition
      Print #1, .NumSgl
      For i = 0 To .NumSgl - 1
        Print #1, .SglDist(i); .SglVal(i)
      Next
      
      'Fraction Aloft
      Print #1, .NumFA
      For i = 0 To .NumFA - 1
        Print #1, .FADist(i); .FAVal(i)
      Next
      
      'COV Mean Deposition
      Print #1, .NumCOVM
      For i = 0 To .NumCOVM - 1
        Print #1, .COVMDist(i); .COVMVal(i)
      Next
      
      'Single Swath Deposition
      Print #1, .NumHalf
      For i = 0 To .NumHalf - 1
        Print #1, .HalfDist(i); .HalfVal(i)
      Next
      
      'Spray Block Deposition
      Print #1, .NumSBD
      For i = 0 To .NumSBD - 1
        Print #1, .SBDDist(i); .SBDVal(i)
      Next
      
      'Canopy Deposition
      Print #1, .NumCAN
      For i = 0 To .NumCAN - 1
        Print #1, .CANDist(i); .CANVal(i)
      Next
      
      'Time Accountancy Aloft
      Print #1, .NumTAA
      For i = 0 To .NumTAA - 1
        Print #1, .TAATime(i); .TAAVal(i)
      Next
      
      'Time Accountancy Vapor
      Print #1, .NumTAV
      For i = 0 To .NumTAV - 1
        Print #1, .TAVTime(i); .TAVVal(i)
      Next
      
      'Time Accountancy Canopy
      Print #1, .NumTAC
      For i = 0 To .NumTAC - 1
        Print #1, .TACTime(i); .TACVal(i)
      Next
      
      'Time Accountancy Ground
      Print #1, .NumTAG
      For i = 0 To .NumTAG - 1
        Print #1, .TAGTime(i); .TAGVal(i)
      Next
      
      'Distance Accountancy Aloft
      Print #1, .NumDAA
      For i = 0 To .NumDAA - 1
        Print #1, .DAADist(i); .DAAVal(i)
      Next
      
      'Distance Accountancy Vapor
      Print #1, .NumDAV
      For i = 0 To .NumDAV - 1
        Print #1, .DAVDist(i); .DAVVal(i)
      Next
      
      'Distance Accountancy Canopy
      Print #1, .NumDAC
      For i = 0 To .NumDAC - 1
        Print #1, .DACDist(i); .DACVal(i)
      Next
      
      'Distance Accountancy Ground
      Print #1, .NumDAG
      For i = 0 To .NumDAG - 1
        Print #1, .DAGDist(i); .DAGVal(i)
      Next
      
      'Height Accountancy Aloft
      Print #1, .NumHAA
      For i = 0 To .NumHAA - 1
        Print #1, .HAAHgt(i); .HAAVal(i)
      Next
      
      'Height Accountancy Vapor
      Print #1, .NumHAV
      For i = 0 To .NumHAV - 1
        Print #1, .HAVHgt(i); .HAVVal(i)
      Next
      
      'Height Accountancy Canopy
      Print #1, .NumHAC
      For i = 0 To .NumHAC - 1
        Print #1, .HACHgt(i); .HACVal(i)
      Next
      
      'Spray Block DSD
      Print #1, .NumSBDSD
      For i = 0 To .NumSBDSD - 1
        Print #1, .SBDSDDiam(i); .SBDSDFrac(i)
      Next
      
      'Downwind DSD
      Print #1, .NumDWDSD
      For i = 0 To .NumDWDSD - 1
        Print #1, .DWDSDDiam(i); .DWDSDFrac(i)
      Next
      
      'Vertical Flux DSD
      Print #1, .NumFXDSD
      For i = 0 To .NumFXDSD - 1
        Print #1, .FXDSDDiam(i); .FXDSDFrac(i)
      Next
      
      'Canopy DSD
      Print #1, .NumCNDSD
      For i = 0 To .NumCNDSD - 1
        Print #1, .CNDSDDiam(i); .CNDSDFrac(i)
      Next
      
      'Point DSD
      Print #1, .NumPTDSD
      For i = 0 To .NumPTDSD - 1
        Print #1, .PTDSDDiam(i); .PTDSDFrac(i)
      Next
      
      'Spray Block Area Coverage
      Print #1, .NumSBAC
      For i = 0 To .NumSBAC - 1
        Print #1, .SBACRate(i); .SBACFrac(i)
      Next
    
      'Application Layout
      Print #1, .NumLAY
      For i = 0 To .NumLAY - 1
        Print #1, .LAYDist(i); .LAYFrac(i)
      Next
    End If
  End With
    
  Close #1
  UserDataWrite = True
  Exit Function

UserDataWriteErrHand1:
  If Notify Then
    s$ = "Error opening file: " + fn + Chr$(13) + Error$(Err)
    MsgBox s$, vbCritical + vbOKOnly
  End If
  UserDataWrite = False
  Exit Function

UserDataWriteErrHand2:
  If Notify Then
    s$ = "Error writing file: " + fn + Chr$(13) + Error$(Err)
    MsgBox s$, vbCritical + vbOKOnly
  End If
  Close #1
  UserDataWrite = False
  Exit Function
End Function

Public Function ClipStr$(s, wid As Integer)
'Clips a string to the specified width. If the string is
'less than or equal to wid, it is returned unchanged.
'Otherwise, the first part of the string is returned with
'"..." appended to it. The length of the string returned
'in the latter case is wid.
  Dim tmpstr As String
  
  tmpstr = Trim$(s)
  If Len(tmpstr) <= wid Then
    ClipStr$ = tmpstr
  Else
    If wid > 3 Then
      ClipStr$ = Left$(tmpstr, wid - 3) & "..."
    Else
      ClipStr$ = Left$(tmpstr, wid)
    End If
  End If
End Function

Public Function LibOpenDB(DB As Database) As Integer
'Open AGDISP Library Database
  Dim mbtitle As String
  Dim mbflags As Integer
  Dim mbmsg As String
  
  On Error GoTo ErrHandlerLODB
  LibOpenDB = True 'default return value
  Set DB = Workspaces(0).OpenDatabase(UI.LibraryPath, False, True)
  
ExitLibOpenDB:
  Exit Function

ErrHandlerLODB:
  Select Case Err.Number
  Case 3024  'Database file not found
    mbtitle = "AGDISP Library Error"
    mbmsg = "The AGDISP Library file was not found." + vbCrLf + vbCrLf
    mbmsg = mbmsg + "AGDISP cannot continue without this library and will terminate."
    mbflags = vbExclamation + vbOKOnly
    MsgBox mbmsg, mbflags, mbtitle
    End
  End Select
  LibOpenDB = False
  Resume ExitLibOpenDB
End Function

Public Function LibOpenMAADB(DB As Database) As Integer
'Open AGDISP MAA Library Database
  Dim mbtitle As String
  Dim mbflags As Integer
  Dim mbmsg As String
  Dim RS As Recordset
  
  On Error GoTo ErrHandlerLOMAADB
  LibOpenMAADB = True 'default return value
  Set DB = Workspaces(0).OpenDatabase(UI.MAALibraryPath, False, True)

  'Database is open. Check version.
  'First look for the Info table. If it is not there, the library
  'is very old or very wrong. Then match the Version field in the
  'database to the version wired in this program.
  If LibOpenRS(DB, "Info", RS) Then
    If RS.Fields("Version") = LIBRARYVERSION Then
      RS.Close
      Exit Function  'Success!
    Else
      'Version mismatch
      RS.Close
      Error 10000 'Raise a fake error for Version Mismatch
    End If
  Else
    'No Info table, must be an old library
    Error 10000 'Raise a fake error for Version Mismatch
  End If
  
ExitLibOpenMAADB:
  Exit Function

ErrVersionLOMAADB:
  'Version mismatch error
  mbtitle = "AGDISP Library Error"
  mbmsg = "The AGDISP MAA Library file is the wrong version." + vbCrLf + vbCrLf
  mbmsg = mbmsg + "AGDISP cannot open this toolbox without the correct library."
  mbflags = vbExclamation + vbOKOnly
  MsgBox mbmsg, mbflags, mbtitle
  LibOpenMAADB = False
  GoTo ExitLibOpenMAADB
  
ErrHandlerLOMAADB:
  mbtitle = "AGDISP Library Error"
  mbflags = vbExclamation + vbOKOnly
  Select Case Err.Number
  Case 3024  'Database file not found
    mbmsg = "The AGDISP MAA Library file was not found." + vbCrLf + vbCrLf
    mbmsg = mbmsg + "AGDISP cannot open this toolbox without the correct library."
  Case 10000  '(user-defined) Library Version mismatch
    mbmsg = "The AGDISP MAA Library file is the wrong version." + vbCrLf + vbCrLf
    mbmsg = mbmsg + "AGDISP cannot open this toolbox without the correct library."
  End Select
  MsgBox mbmsg, mbflags, mbtitle
  LibOpenMAADB = False
  Resume ExitLibOpenMAADB
End Function

Public Function LibOpenRS(DB As Database, TableName As String, RS As Recordset) As Integer
'Open a Recordset within an open Database
  
  On Error GoTo ErrHandlerLORS
  LibOpenRS = True 'default return value
  Set RS = DB.OpenRecordset(TableName, dbOpenDynaset)
  Exit Function
  
ErrHandlerLORS:
  Select Case Err.Number
  Case 3011  '
    s$ = Err.Description
    'no message, just return false
  End Select
  LibOpenRS = False
  Resume Next
End Function

Public Sub ClearUserCalc(xUC As UserCalc)
'Reset UserCalc structure to clear calc data
'Set the arrays to 1 element, rather than erasing them
'in case the empty array is referenced. This avoids
'"Subscript out of range" errors for calling a routine
'by referencing the zero-th element
  xUC.Valid = False
  xUC.CodeVersion = 0
  xUC.StartDate = ""
  xUC.StartTime = ""
  xUC.MaxErrorLevel = 0
  xUC.MessageLog = ""
  xUC.NumDep = 0
  ReDim xUC.DepDist(0)
  ReDim xUC.DepVal(0)
  xUC.DepExtrap = 0
  xUC.NumDrp = 0
  ReDim xUC.DrpDist(0)
  ReDim xUC.DrpVal(0)
  xUC.NumPID = 0
  ReDim xUC.PIDDist(0)
  ReDim xUC.PIDVal(0)
  xUC.PIDExtrap = 0
  xUC.NumFlux = 0
  ReDim xUC.FluxDist(0)
  ReDim xUC.FluxVal(0)
  xUC.NumCOV = 0
  ReDim xUC.COVVal(0)
  ReDim xUC.COVESW(0)
  xUC.NumConc = 0
  ReDim xUC.ConcDist(0)
  ReDim xUC.ConcVal(0)
  xUC.SwathDisp = 0
  xUC.SBCOV = 0
  xUC.SBMeanDep = 0
  xUC.SWATH = 0
  xUC.AirborneDrift = 0
  xUC.EvapFrac = 0
  xUC.AppEff = 0
  xUC.DownwindDep = 0
  xUC.CanopyDep = 0
  xUC.NumSgl = 0
  ReDim xUC.SglDist(0)
  ReDim xUC.SglVal(0)
  xUC.NumFA = 0
  ReDim xUC.FADist(0)
  ReDim xUC.FAVal(0)
  xUC.NumCOVM = 0
  ReDim xUC.COVMDist(0)
  ReDim xUC.COVMVal(0)
  xUC.NumHalf = 0
  ReDim xUC.HalfDist(0)
  ReDim xUC.HalfVal(0)
  xUC.NumSBD = 0
  ReDim xUC.SBDDist(0)
  ReDim xUC.SBDVal(0)
  xUC.NumCAN = 0
  ReDim xUC.CANDist(0)
  ReDim xUC.CANVal(0)
  xUC.NumTAA = 0
  ReDim xUC.TAATime(0)
  ReDim xUC.TAAVal(0)
  xUC.NumTAV = 0
  ReDim xUC.TAVTime(0)
  ReDim xUC.TAVVal(0)
  xUC.NumTAC = 0
  ReDim xUC.TACTime(0)
  ReDim xUC.TACVal(0)
  xUC.NumTAG = 0
  ReDim xUC.TAGTime(0)
  ReDim xUC.TAGVal(0)
  xUC.NumDAA = 0
  ReDim xUC.DAADist(0)
  ReDim xUC.DAAVal(0)
  xUC.NumDAV = 0
  ReDim xUC.DAVDist(0)
  ReDim xUC.DAVVal(0)
  xUC.NumDAC = 0
  ReDim xUC.DACDist(0)
  ReDim xUC.DACVal(0)
  xUC.NumDAG = 0
  ReDim xUC.DAGDist(0)
  ReDim xUC.DAGVal(0)
  xUC.NumHAA = 0
  ReDim xUC.HAAHgt(0)
  ReDim xUC.HAAVal(0)
  xUC.NumHAV = 0
  ReDim xUC.HAVHgt(0)
  ReDim xUC.HAVVal(0)
  xUC.NumHAC = 0
  ReDim xUC.HACHgt(0)
  ReDim xUC.HACVal(0)
  xUC.NumSBDSD = 0
  ReDim xUC.SBDSDDiam(0)
  ReDim xUC.SBDSDFrac(0)
  xUC.NumDWDSD = 0
  ReDim xUC.DWDSDDiam(0)
  ReDim xUC.DWDSDFrac(0)
  xUC.NumFXDSD = 0
  ReDim xUC.FXDSDDiam(0)
  ReDim xUC.FXDSDFrac(0)
  xUC.NumCNDSD = 0
  ReDim xUC.CNDSDDiam(0)
  ReDim xUC.CNDSDFrac(0)
  xUC.NumPTDSD = 0
  ReDim xUC.PTDSDDiam(0)
  ReDim xUC.PTDSDFrac(0)
  xUC.NumSBAC = 0
  ReDim xUC.SBACRate(0)
  ReDim xUC.SBACFrac(0)
  xUC.NumLAY = 0
  ReDim xUC.LAYDist(0)
  ReDim xUC.LAYFrac(0)
  xUC.CalpuffCalcsAvailable = False 'NOT SAVED: CALPUFF Calcs flag.
End Sub

Public Function PrinterExists()
'Test see if there is a default printer configured
  Dim Msg As String
  If Printers.Count > 0 Then
    PrinterExists = True
  Else
    Msg = "There is no printer configured."
    MsgBox Msg, vbOKOnly
    PrinterExists = False
  End If
End Function

Public Function LibGetFileName() As String
'Get the file name of the Library Database
  
  Dim STDlib As String
  Dim STDExists As Boolean
  Dim STDVersionOK As Boolean
  
  Dim Msg As String
  Dim Mflags As Long
  Dim Mtitle As String
  
  'Set up candidate library paths
  STDlib = App.Path & "\" & "agdisp.mdb"
  
  'Check for file existance
  STDExists = FileExists(STDlib)
  
  'Check that the library version is okay
  STDVersionOK = LibVersionMatches(STDlib)
    
  'Now for the big decision tree
  Mtitle = "AGDISP Library Error"
  Mflags = vbOKOnly
  Msg = ""
    
  If STDExists Then
    If STDVersionOK Then
      LibGetFileName = STDlib
    Else
      'STD bad
      Msg = Msg + "The AGDISP Library file is the wrong version." + vbCrLf + vbCrLf
      Msg = Msg + "AGDISP cannot "
      Msg = Msg + "continue without the correct library and will terminate."
      Mflags = Mflags + vbCritical
      MsgBox Msg, Mflags, Mtitle
      End
    End If
  Else
    'STD missing
    Msg = Msg + "The AGDISP Library file was not found." + vbCrLf + vbCrLf
    Msg = Msg + "AGDISP cannot "
    Msg = Msg + "continue without the library and will terminate."
    Mflags = Mflags + vbCritical
    MsgBox Msg, Mflags, Mtitle
    End
  End If
End Function

Public Function LibVersionMatches(LibraryPath As String) As Integer
'Open the given library file and check its version against the program's
'Return True if all is well, False is there is any kind of problem.
  Dim DB As Database
  Dim RS As Recordset

  'Try to open the database
  On Error GoTo LibVersionMatchesErrHand
  Set DB = Workspaces(0).OpenDatabase(LibraryPath, False, True)
  On Error GoTo 0
  
  'Database is open. Check version.
  'First look for the Info table. If it is not there, the library
  'is very old or very wrong. Then match the Version field in the
  'database to the version wired in this program.
  If LibOpenRS(DB, "Info", RS) Then
    If RS.Fields("Version") = LIBRARYVERSION Then
      'Success!
      LibVersionMatches = True
    Else
      'Version mismatch
      LibVersionMatches = False
    End If
    RS.Close
  Else
    'No Info table
    LibVersionMatches = False
  End If
  DB.Close

ExitLibVersionMatches:
  Exit Function
  
LibVersionMatchesErrHand:
  LibVersionMatches = False
  Resume ExitLibVersionMatches
End Function

Public Function QueryPerformCalcs() As Integer
'Ask the user about do calcs, then proceed
' Returns: True if calcs were performed
'          False if not
'
  Dim Msg As String
  
  QueryPerformCalcs = False 'default return value
  
  Msg = "Calculations must be performed before "
  Msg = Msg + "continuing. Perform them now?"
  MBType = vbQuestion + vbOKCancel
  If MsgBox(Msg, MBType) = vbOK Then
    'try to do the calcs
    QueryPerformCalcs = PerformCalcs(UP.PauseBeforeCalc)
  End If
End Function

Public Sub AddToArray(newval As Single, nary As Integer, ary() As Single)
'Add a new value to an array so that the array
'maintains an ascending sort. It is assumed that
'the existing array is already sorting in an
'ascending direction
'
' newval  - the value to be added to the array
' nary    - the number of elements currently in ary
' ary     - the existing array

  'array is empty
  If nary = 0 Then
    nary = 1
    ary(0) = newval
  'value is greater than last element
  ElseIf newval >= ary(nary - 1) Then
    nary = nary + 1
    ary(nary - 1) = newval
  Else
    For i = 0 To nary - 1
      If newval < ary(i) Then
        For j = nary - 1 To i Step -1
          ary(j + 1) = ary(j)
        Next
        ary(i) = newval
        nary = nary + 1
        Exit For
      End If
    Next
  End If
End Sub

Public Sub ArrayToField(fld As Field, X() As Single, Optional NumX)
'Store an array in a Longbinary database field
'
' fld must be a longbinary field of an open recordset
' X() contains the data to be packed into the field
' if NumX is provided, that number of array elements will
'   be transferred.
' If NumX is not provided, the entire X() array will be transferred
'
  Dim bb() As Byte
  Dim nb As Integer
  
  If IsMissing(NumX) Then
    nb = (UBound(X) + 1) * Len(X(0))
  Else
    nb = NumX * Len(X(0))
  End If
  ReDim bb(nb)                'make room for data
  CopyMemory bb(0), X(0), nb  'transfer data to byte array
  fld.AppendChunk bb()        'store in DB field
End Sub

Public Function FieldToArray(fld As Field, X() As Single) As Long
'Recover an array from a Longbinary database field
'
' fld must be a longbinary field of an open recordset
' X() must be dimensioned large enough to hold the array
'
' returns: number of array elements recovered
'
  Dim bb() As Byte
  Dim nb As Long
  
  FieldToArray = 0 'default return value
  
  'Longbinary fields seem to end with a null, which is included in the fieldsize.
  'Subtract it from the byte count
  nb = fld.FieldSize - 1
  
  If nb <= 0 Then Exit Function
  
  bb = fld.GetChunk(0, nb) 'retrieve raw data
  CopyMemory X(0), bb(0), nb 'transfer it to the array
  FieldToArray = nb / Len(X(0)) 'elements = bytes / bytes/element
End Function

Public Sub FormatAgreadMessage(istat As Long, _
                               itype As Long, _
                               adat() As Single, _
                               cdat As String, _
                               ShortMsg As String, _
                               LongMsg As String)
'Format short and long messages based on values returned by agread
'The short messages are to be added to the calc log
'The long messages are to be displayed in a MsgBox
'
  ' agread info: istat 0=OK, keep reading
  '                    1=warning, supressible: keep reading
  '                    2=error, suppressible: keep reading
  '                    3=error, non-suppressible: STOP
  '                    4=End of data, STOP
  '              itype 0=char data only
  '                    1=int and char data
  '                    2=real and char data
  '          adat/idat Limits in arrays: (0)=value (1)=min (2)=max
  Dim Header As String
  
  ShortMsg = ""
  LongMsg = ""
  Select Case istat  'error level
  Case 0: 'ok
    Select Case itype
    Case 0: 'no message or data
    Case 1: 'message
      ShortMsg = Trim$(cdat)
    Case 2, 3: 'message and data
      ShortMsg = cdat & " " & AGFormat$(adat(0))
    End Select
  Case 1, 2: 'warning/error
    Select Case istat
    Case 1:
      Header = "Warning!"
    Case 2:
      Header = "Error!"
    End Select
    Select Case itype
    Case 0: 'no message or data
    Case 1: 'message
      ShortMsg = Trim$(cdat)
      AppendStr LongMsg, Header, True
      AppendStr LongMsg, Trim$(cdat), True
    Case 2, 3: 'message and float/int data
      ShortMsg = Trim$(cdat)
      Select Case istat
      Case 1: 'Warning
        AppendStr LongMsg, Header, True
        AppendStr LongMsg, "Although calculations can continue,", True
        AppendStr LongMsg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr LongMsg, "is beyond recommended limits. The limits are:", True
      Case 2: 'Error
        AppendStr LongMsg, Header, True
        AppendStr LongMsg, "Calculations cannot continue,", True
        AppendStr LongMsg, Chr$(34) & Trim$(cdat) & Chr$(34), True
        AppendStr LongMsg, "is out of range. The limits are:", True
      End Select
      AppendStr LongMsg, "", True
      AppendStr LongMsg, "Min: " & AGFormat$(adat(1)), True
      AppendStr LongMsg, "Val: " & AGFormat$(adat(0)), True
      AppendStr LongMsg, "Max: " & AGFormat$(adat(2)), True
    End Select
  Case 3: 'normal end of data
  Case 4, 5: 'Special DropKick message
    AppendStr LongMsg, "Warning!", True
    AppendStr LongMsg, Trim$(cdat), True
    AppendStr LongMsg, "", True
    AppendStr LongMsg, "    AGDISP:  " & AGFormat$(adat(1)), True
    AppendStr LongMsg, "    DropKick: " & AGFormat$(adat(2)), True
    AppendStr LongMsg, "", True
  End Select
End Sub

Public Sub OpenFileAndSkipComments(fn As String, funit As Integer)
'Open file fn on unit funit and position it past any leading comment
'lines. Comment lines begin with a pound sign (#). Blank lines are
'also skipped.
  Dim icmnt As Integer
  Dim buf As String
  Dim i As Integer
  
  Open fn For Input As #funit
    
  'Read past (and count) any comment lines
  icmnt = -1
  Do
    icmnt = icmnt + 1
    Line Input #funit, buf
  Loop While (Left(buf, 1) = "#") Or (Trim(buf) = "")
    
  'Reopen the file to rewind it to the beginning
  Close #funit
  Open fn For Input As #funit
    
  'Read past comment lines
  For i = 1 To icmnt
    Line Input #funit, buf
  Next

End Sub

'---------------------------------------------------------------------------
' GetNameCanDataQuality:
'    Return the text name corresponding to the DataQuality descriptor in the Canopy LAI Library
'
' Arguments:
'     intDataQuality: DataQuality descriptor
'
' Return: The text name of the Data Quality descriptor
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2004-05-27  TBC  Added error handling
'
'---------------------------------------------------------------------------
Public Function GetNameCanDataQuality(intDataQuality As Integer) As String
  Select Case intDataQuality
  Case 1
    GetNameCanDataQuality = "High"
  Case 2
    GetNameCanDataQuality = "Medium"
  Case 3
    GetNameCanDataQuality = "Low"
  Case Else
    GetNameCanDataQuality = ""
  End Select
End Function
