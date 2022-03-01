Attribute VB_Name = "basAGPLOT"
'$Id: basAgplot.bas,v 1.14 2016/12/05 15:36:29 tom Exp $
'basAGPLOT - AGDISP plotting utilities
Option Explicit

'This is where all the work is done to define and produce
'the plots associated with AGDISP. Each plot is assigned
'as an identifier a constant beginning with PV_
'
'How to define a new plot:
'- Create a new PV_ constant and place it in the appropriate
'  location in the list (see below).
'- Add Titles in GenPlotTitles
'-----------------------------------------------------------
'
'*** Public Data Definitions ***
'
'define a type to hold Transient Toolbox Data
'Because this data does not survive between invokations
'of a toolbox, the titles, units, and units-converted data
'may all be defined at creation time
'
'if X1D is true, both np and X are defined to be
'one-dimensional and all of the Y columns are
'assumed to have the same number of points and
'correspond to X.
'if X1D is false, then np is has nc elements and
'X and Y are dimensioned the same. Each column in
'X corresponds to the matching column in Y
Public Type ToolboxPlotData
  X1D As Integer  'True=X is 1D, False=X matches Y
  NC As Integer   'number of data sets
  np() As Integer 'number of points in each set
  X() As Single   'X values for data sets
  Y() As Single   'Y values for data sets
End Type
Public TPD As ToolboxPlotData

Public DataSourceBackup(4) As String
Public DataTitleBackup(4) As String

'*** Constants ***

'PlotVar defines each individual AGDISP plot
'They also provide attributes of the plot
'
'MSB                             LSB
'1------- --SSYYYY MMAAATTT PPPPPPPP
'
'-=Unused   Set first bit to 1 as placeholder
'           (otherwise things get messed up)
'S=Source     00 = UD
'             01 = UC
'             10 = Toolbox
'Y=PlotType 0000 = Line
'           0001 = Multi Line
'           0010 = Contour
'           0011 = Colormap
'           0100 = Bar
'           0101 = Pie
'M=Mode       x1 = Available in Ag
'             1x = Available in FS
'A=AM        xx1 = Available in Aerial
'            x1x = Available in Ground
'            1xx = Available on Orchard
'T=Tier      xx1 = Available in Tier I
'            x1x = Available in Tier II
'            1xx = Available in Tier III
'P=Plot xxxxxxxx
'

'PlotVar Attributes
'
'
Public Const PVA_ZERO = &H80000000
'Tier (combine)
Public Const PVA_T1 = &H80000100
Public Const PVA_T2 = &H80000200
Public Const PVA_T3 = &H80000400
Public Const PVA_T23 = &H80000600 'T2 + T3
Public Const PVA_T123 = &H80000700 'T1 + T2 + T3
'Application Method (combine)
Public Const PVA_AIR = &H80000800
Public Const PVA_GND = &H80001000
Public Const PVA_OCH = &H80002000
Public Const PVA_AGX = &H80001800 'Air + Ground
Public Const PVA_AGO = &H80003800 'Air + Ground + Orchard
'Mode (combine)
Public Const PVA_AG = &H80004000
Public Const PVA_FS = &H80008000
Public Const PVA_AF = &H8000C000 'both
'Plot Type (pick one)
Public Const PVA_TYPE_MASK = &H800F0000
Public Const PVA_LINE = &H80010000
Public Const PVA_MULT = &H80020000
Public Const PVA_CNTR = &H80030000
Public Const PVA_CMAP = &H80040000
Public Const PVA_BAR = &H80050000
Public Const PVA_PIE = &H80060000
'Source (pick one)
Public Const PVA_SOURCE_MASK = &H80300000
Public Const PVA_UD = &H80100000
Public Const PVA_UC = &H80200000
Public Const PVA_TB = &H80300000
'
'Common Combinations of Plot Attributes
'Many plots share the same combination of Plot Attributes.
Public Const PVC_T1AGOAF = PVA_T123 Or PVA_AGO Or PVA_AF
Public Const PVC_T1AIRAF = PVA_T123 Or PVA_AGX Or PVA_AF
Public Const PVC_T2AGXAF = PVA_T23 Or PVA_AGX Or PVA_AF
Public Const PVC_T2AGXFS = PVA_T23 Or PVA_AGX Or PVA_FS
Public Const PVC_T3AGXAF = PVA_T3 Or PVA_AGX Or PVA_AF
Public Const PVC_T3AGXFS = PVA_T3 Or PVA_AGX Or PVA_FS
'
'PlotVars - combination of plot number and attributes
'
'(Tier I+)
Public Const PV_VFINC = 0 Or PVC_T3AGXAF Or PVA_UD Or PVA_LINE 'PVA_MULT   'IVF all DSD's
'Public Const PV_VFINC0 = 1 Or PVC_T1AIRAF Or PVA_UD Or PVA_LINE  'IVF for DSD 0 only
'Public Const PV_VFINC1 = 2 Or PVC_T3AGXAF Or PVA_UD Or PVA_LINE  'IVF for DSD 1 only
'Public Const PV_VFINC2 = 3 Or PVC_T3AGXAF Or PVA_UD Or PVA_LINE  'IVF for DSD 2 only
Public Const PV_VFCUM = 4 Or PVC_T3AGXAF Or PVA_UD Or PVA_LINE 'PVA_MULT   'CVF all DSD's
'Public Const PV_VFCUM0 = 5 Or PVC_T1AIRAF Or PVA_UD Or PVA_LINE  'CVF for DSD 0 only
'Public Const PV_VFCUM1 = 6 Or PVC_T3AGXAF Or PVA_UD Or PVA_LINE  'CVF for DSD 1 only
'Public Const PV_VFCUM2 = 7 Or PVC_T3AGXAF Or PVA_UD Or PVA_LINE  'CVF for DSD 2 only
'(Tier I+)
Public Const PV_DEP = 8 Or PVC_T1AGOAF Or PVA_UC Or PVA_LINE     'deposition
Public Const PV_PID = 9 Or PVC_T1AGOAF Or PVA_UC Or PVA_LINE 'NOT USED!    'pond-integrated deposition
'(Tier II+ Aerial)
Public Const PV_VERT = 10 Or PVC_T2AGXAF Or PVA_UC Or PVA_LINE     'vertical profile
Public Const PV_CONC = 11 Or PVC_T2AGXAF Or PVA_UC Or PVA_LINE     '1-hour average concentration
'(Tier III Aerial)
Public Const PV_COV = 12 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE      'cov/effective swath width
Public Const PV_FA = 13 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE       'Fraction Aloft
Public Const PV_MEAN = 14 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE     'Mean Deposition
Public Const PV_DWDSDINC = 15 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE 'Downwind DSD
Public Const PV_DWDSDCUM = 16 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE 'Downwind DSD
Public Const PV_FXDSDINC = 17 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE 'Vertical Flux DSD
Public Const PV_FXDSDCUM = 18 Or PVC_T3AGXAF Or PVA_UC Or PVA_LINE 'Vertical Flux DSD
'tbc reorder the plotvars
'(Tier II+ Aerial FS)
Public Const PV_TA = 20 Or PVC_T2AGXFS Or PVA_UC Or PVA_MULT       'Time Accountancy (all curves)
Public Const PV_TAALOFT = 21 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE  'Time Acc (aloft)
Public Const PV_TAVAPOR = 22 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE  'Time Acc (vapor)
Public Const PV_TACANOPY = 23 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE 'Time Acc (canopy)
Public Const PV_TAGROUND = 24 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE 'Time Acc (ground)
Public Const PV_HA = 25 Or PVC_T2AGXFS Or PVA_UC Or PVA_MULT       'Height Accountancy (all curves)
Public Const PV_HAALOFT = 26 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE  'Height Acc (aloft)
Public Const PV_HAVAPOR = 27 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE  'Height Acc (vapor)
Public Const PV_HACANOPY = 28 Or PVC_T2AGXFS Or PVA_UC Or PVA_LINE 'Height Acc (canopy)
Public Const PV_TAB = 29 Or PVC_T2AGXFS Or PVA_UC Or PVA_BAR       'Total Accountancy Bar Chart
Public Const PV_TAP = 30 Or PVC_T2AGXFS Or PVA_UC Or PVA_PIE       'Total Accountancy Pie Chart
'(Tier III Aerial FS)
Public Const PV_LAYOUT = 31 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE   'application layout
Public Const PV_DA = 32 Or PVC_T3AGXFS Or PVA_UC Or PVA_MULT       'Distance Acc
Public Const PV_DAALOFT = 33 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE  'Time Acc (aloft)
Public Const PV_DAVAPOR = 34 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE  'Time Acc (vapor)
Public Const PV_DACANOPY = 35 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Time Acc (canopy)
Public Const PV_DAGROUND = 36 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Time Acc (ground)
Public Const PV_SBDSDINC = 37 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Spray Block DSD
Public Const PV_SBDSDCUM = 38 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Spray Block DSD
Public Const PV_CNDSDINC = 39 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Canopy DSD
Public Const PV_CNDSDCUM = 40 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Canopy DSD
Public Const PV_PTDSDINC = 59 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Point DSD
Public Const PV_PTDSDCUM = 60 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE 'Point DSD
Public Const PV_SBDEP = 41 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE    'Spray Block Deposition
Public Const PV_SBCOVER = 42 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE  'Spray Block Area Coverage
Public Const PV_CANDEP = 43 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE   'Canopy Deposition
Public Const PV_SV = 19 Or PVC_T3AGXFS Or PVA_UD Or PVA_MULT       'Settling Velocity
Public Const PV_SVTANK = 56 Or PVC_T3AGXFS Or PVA_UD Or PVA_LINE   'Settling Velocity (Carrier)
Public Const PV_SVNONV = 57 Or PVC_T3AGXFS Or PVA_UD Or PVA_LINE   'Settling Velocity (Nonvol)
Public Const PV_DRP = 61 Or PVC_T3AGXFS Or PVA_UC Or PVA_LINE      'number deposition (drops/cm2)
'Temporary data -based plots
'(since temporary data is populated separately, these plots are
' set to be available in all tiers, AM's, and Modes. Access to the
' plots are restricted elsewhere)
Public Const PV_SATM = 44 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE  'Stream Assessment TB (Time)
Public Const PV_SADI = 45 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE  'Stream Assessment TB (Distance)
Public Const PV_MAA = 46 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE   'Multiple Application Assessment TB
Public Const PV_MAAT = 47 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE  'MAA Library Median Temperature TB
Public Const PV_MAARH = 48 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE 'MAA Library Median Relative Humidity TB
Public Const PV_MAAWS = 49 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE 'MAA Library Median Wind Speed TB
Public Const PV_MAAWD = 50 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE 'MAA Library Median Wind Direction TB
Public Const PV_MAAROSE = 51 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE 'MAA Library Median Wind Direction TB
Public Const PV_AQUA = 52 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE  'Aquatic Assessment TB user-def pond dep
Public Const PV_TERR = 53 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE 'NOT USED 'Terrestrial Assessment TB user-def pond dep
Public Const PV_SBLK = 54 Or PVC_T1AGOAF Or PVA_TB Or PVA_LINE 'NOT USED 'Spray Block Assessment TB
Public Const PV_SBDET = 55 Or PVC_T3AGXFS Or PVA_TB Or PVA_LINE 'Spray Block Details
Public Const PV_GAUS = 58 Or PVC_T3AGXFS Or PVA_TB Or PVA_LINE 'Gaussian Far-Field
Public Const PV_CURV = 62 Or PVC_T3AGXFS Or PVA_TB Or PVA_LINE 'Curve-fit deposition

'SourceID's plotting source ID numbers
Public Const SID_NONE = -1    'No data
Public Const SID_CURRENT = 0  'Current Data (in UD/UC)
Public Const SID_FILE = 1     'Saved Data
Public Const SID_T1LIB = 2    'Tier 1 Library
Public Const SID_LIB = 3      'Drop Library
Public Const SID_FTPRED = 4   'Field Trial Prediction
Public Const SID_FTMEAS = 5   'Field Trial Measurement
Public Const SID_TPD = 6      'Toolbox Plot Data

Public Sub ShowPlot(PlotVar As Long)
'Setup and display a plot
  
  On Error GoTo ShowPlotErrHand
  
  If SetupPlot(PlotVar) Then frmPlot.Show vbModal
  
  Exit Sub

ShowPlotErrHand:
  Select Case UnexpectedError("ShowPlot")
  Case vbAbort  'Abort - Stop the whole program
    End
  Case vbRetry  'Retry - Resume at the same line
    Resume
  Case vbIgnore 'Ignore - Resume at the next line
    Resume Next
  End Select
End Sub

Public Function PlotIsAvailable(PlotVar As Long) As Boolean
'Determine whether a particular plot is available based
'on the current Tier, Application Method, and Mode, and one or two others
'This routine is used to set up:
'  the View menu on the main form
'  the checkboxes on the Export form
'  the PlotVar combo box on the plot form
'Note the special case for DSD plots. In addition to everything else, the
'availability of these plots also depends upon whether or not a nozzle
'is associated with the DSD.

  PlotIsAvailable = False 'Default return value
  
  'Check Application Method
  'current AM must be selected in PlotVar
  Select Case UD.ApplMethod
  Case AM_AERIAL
    If (PlotVar And PVA_AIR) <> PVA_AIR Then Exit Function
  Case AM_GROUND
    If (PlotVar And PVA_GND) <> PVA_GND Then Exit Function
  Case AM_ORCHARD
    If (PlotVar And PVA_OCH) <> PVA_OCH Then Exit Function
  End Select
  
  PlotIsAvailable = True
End Function

Public Function PlotIsAvailableExtended(PlotVar As Long) As Boolean
'Determine if a particular plot is available not only by tier,mode,and AM,
'but also if calcs are required and have been done.
'This routine is called by the Numerics form and the Export form
  Dim i As Integer
  Dim found As Boolean
  
  'Start with the basic test
  PlotIsAvailableExtended = PlotIsAvailable(PlotVar)
  If Not PlotIsAvailableExtended Then Exit Function
  
  'For UC Plots, see if the Calcs are valid
  If ((PlotVar And PVA_SOURCE_MASK) = PVA_UC) And _
     (Not UC.Valid) Then
    PlotIsAvailableExtended = False
    Exit Function
  End If
End Function

Public Sub BackupPlotDataSources()
  Dim i As Integer
  For i = 0 To 4: DataSourceBackup(i) = PlotGetDataSource(i): Next 'save existing
  For i = 0 To 4: DataTitleBackup(i) = PlotGetDataTitle(i): Next 'save existing
End Sub

Public Sub RestorePlotDataSources()
  Dim i As Integer
    For i = 0 To 4: PlotSetDataSource i, DataSourceBackup(i): Next
    For i = 0 To 4: PlotSetDataTitle i, DataTitleBackup(i): Next
End Sub

Public Function SetupPlot(PlotVar As Long)
'Setup plot titles and data for all slots
'
' PlotVar  i  PV_*
'
  Dim slot As Integer
  Dim Param1 As String
  Dim Param2 As String
  Dim Param3 As String
  Dim Param4 As String
  Dim SourceID As Integer
  Dim i As Integer
  
  SetupPlot = False 'default return value
  On Error GoTo SetupPlotErrHand

  'Generate Title data
  If Not GenPlotTitles(PlotVar, True) Then Exit Function
  
  'Save/Restore DataSources and DataTitles
  'Multi-curve plots mess up the data sources and data titles, as can
  'other non-line plots. We save this information from the last line plot
  'and restore it after a non-line plot is produced. All bets are off if
  'the plot is a toolbox plot, since the toolboxes save and restore the
  'datasources themselves
  'PlotVar contains the new plot variable
  'UI.PlotVar contains the previous plot variable
  'if UI.PlotVar is 0, this is the first plot
  If (UI.PlotVar <> 0) And ((PlotVar And PVA_SOURCE_MASK) <> PVA_TB) Then 'not the first plot produced
    If ((PlotVar And PVA_TYPE_MASK) = PVA_MULT) And _
       ((UI.PlotVar And PVA_TYPE_MASK) <> PVA_MULT) And _
       ((UI.PlotVar And PVA_SOURCE_MASK) <> PVA_TB) Then 'new=multi, prev=non-multi,non-tb
      BackupPlotDataSources
    End If
    If ((PlotVar And PVA_TYPE_MASK) <> PVA_MULT) And _
       ((PlotVar And PVA_SOURCE_MASK) <> PVA_TB) And _
       ((UI.PlotVar And PVA_TYPE_MASK) = PVA_MULT) Then 'new=non-multi,non-tb, prev=multi
      RestorePlotDataSources
    End If
  End If
  
  'Clean out the DataSources if this is a new kind of plot
  'and it is not a toolbox plot. For toolbox plots the
  'data sources have already been set up
  If (UI.PlotVar <> PlotVar) And _
     ((PlotVar And PVA_SOURCE_MASK) <> PVA_TB) Then
    For slot = 0 To 4
      'recover data source ID and parameters for the current slot
      SourceID = SourceToSourceID(PlotGetDataSource(slot), Param1, Param2, Param3, Param4)
      'If this source is not valid for the new PlotVar, clear it.
      If Not SourceIsValid(PlotVar, SourceID) Then
        PlotSetDataSource slot, ""
        PlotSetDataTitle slot, ""
        PlotXYDataResetSlot slot
      End If
    Next
  End If
  
  'Set the plot type
  Select Case (PlotVar And PVA_TYPE_MASK)
  Case PVA_LINE, PVA_MULT
    PlotSetType GQ_XYPLOT
  Case PVA_CNTR
    PlotSetType GQ_XYPLOT
  Case PVA_CMAP
    PlotSetType GQ_XYPLOT
  Case PVA_BAR, PVA_PIE
    PlotSetType GQ_BAR
  Case Else 'default
    PlotSetType GQ_XYPLOT
  End Select
  
  'Generate plot data based on "DataSources"
  If Not GenPlotData(PlotVar) Then Exit Function

  'Reset some plot options if the plot var has changed
  If UI.PlotVar <> PlotVar Then
    PlotSetXauto True 'reset x autoscaling
    PlotSetYauto True 'reset y autoscaling
    If PS.PlotType = GQ_BAR Then
      PlotSetXlog False 'Force linear scale
      PlotSetYlog False 'Force linear scale
    End If
  End If
  
  'update the plot variable in the interface
  UI.PlotVar = PlotVar
  
  'Successful exit
  SetupPlot = True
  Exit Function
  
SetupPlotErrHand:
  Select Case UnexpectedError("SetupPlot")
  Case vbAbort  'Abort - Stop the whole program
    End
  Case vbRetry  'Retry - Resume at the same line
    Resume
  Case vbIgnore 'Ignore - Resume at the next line
    Resume Next
  End Select
End Function

Public Function GenPlotTitleStrings(PlotVar As Long, FormatForPlots As Integer, _
                                    PlotTitle As String, _
                                    XTitle As String, YTitle As String, HelpID As Long)
'Generate just the strings used to title plots
'
' FormatForPlots i True =insert CR between Y title words
'                        for a snazzy plot appearance
'                  False=insert a space between Y title
'                        words for neater column headings
  Dim dlm As String
  Dim s As String

  'Default return value
  GenPlotTitleStrings = True
  
  'select an appropriate word delimiter
  If FormatForPlots Then
    dlm = vbCrLf
  Else
    dlm = " "
  End If

  Select Case PlotVar
  Case PV_VFINC, PV_VFCUM 'incremental/cumulative volume fraction
    Select Case PlotVar
    Case PV_VFINC
      PlotTitle = "Incremental Volume Fraction"
    Case PV_VFCUM
      PlotTitle = "Cumulative Volume Fraction"
    End Select
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_DEP 'deposition
    PlotTitle = "Deposition"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 1075
  Case PV_DRP 'number deposition
    PlotTitle = "Number Deposition"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm + "(drops/cm2)"
    HelpID = 1571
  Case PV_VERT 'Vertical Profile
    PlotTitle = "Transport Aloft"
    XTitle = "Transport"
    YTitle = "Vertical" + dlm + "Distance" + dlm
    HelpID = 1165
  Case PV_PID 'pond-integrated deposition
    PlotTitle = "Pond-Integrated Deposition"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Application" + dlm + "Rate"
    HelpID = 1225
  Case PV_COV 'cov/effective swath width
    PlotTitle = "Coefficient of Variation"
    XTitle = "Effective Swath Width"
    YTitle = "COV"
    HelpID = 1072
  Case PV_LAYOUT  'application Layout
    PlotTitle = "Application Layout"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 1463
  Case PV_CONC 'concentration
    PlotTitle = "1 Hour Average Concentration"
    XTitle = "Concentration"
    YTitle = "Vertical" + dlm + "Distance" + dlm
    HelpID = 1074
  Case PV_SBLK 'Spray Block Assessment Toolbax
    PlotTitle = "Spray Block Assessment"
    XTitle = "Block Width"
    YTitle = "Buffer" + dlm + "Distance"
    HelpID = 1437
  Case PV_FA 'Fraction Aloft
    PlotTitle = "Fraction Aloft"
    XTitle = "Distance"
    YTitle = "Fraction" + dlm + "of Active" + dlm + "Aloft"
    HelpID = 1445
  Case PV_SATM
    PlotTitle = "Stream Assessment"
    XTitle = "Distance"
    YTitle = "Concentration" + dlm
    HelpID = 1448
  Case PV_SADI
    PlotTitle = "Stream Assessment"
    XTitle = "Time"
    YTitle = "Concentration" + dlm
    HelpID = 1448
  Case PV_MAA
    PlotTitle = "Multiple Application Assessment"
    If frmTBMultiApp.GetLocationName() <> "" Then
      PlotTitle = PlotTitle + " for " + frmTBMultiApp.GetLocationName()
    End If
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 1450
  Case PV_MAAT
    PlotTitle = "MAA Library Median Temperature"
    If frmTBMultiApp.GetLocationName() <> "" Then
      PlotTitle = PlotTitle + " for " + frmTBMultiApp.GetLocationName()
    End If
    XTitle = "Month"
    YTitle = "Temperature" + dlm
    HelpID = 1464
  Case PV_MAARH
    PlotTitle = "MAA Library Median Relative Humidity"
    If frmTBMultiApp.GetLocationName() <> "" Then
      PlotTitle = PlotTitle + " for " + frmTBMultiApp.GetLocationName()
    End If
    XTitle = "Month"
    YTitle = "Relative" + dlm + "Humidity" + dlm
    HelpID = 1464
  Case PV_MAAWS
    PlotTitle = "MAA Library Median Wind Speed"
    If frmTBMultiApp.GetLocationName() <> "" Then
      PlotTitle = PlotTitle + " for " + frmTBMultiApp.GetLocationName()
    End If
    XTitle = "Month"
    YTitle = "Wind Speed" + dlm
    HelpID = 1464
  Case PV_MAAWD
    PlotTitle = "MAA Library Median Wind Direction"
    If frmTBMultiApp.GetLocationName() <> "" Then
      PlotTitle = PlotTitle + " for " + frmTBMultiApp.GetLocationName()
    End If
    XTitle = "Month"
    YTitle = "Wind" + dlm + "Direction" + dlm
    HelpID = 1464
  Case PV_MAAROSE
    PlotTitle = "MAA Wind Rose"
    If frmTBMultiApp.GetLocationName() <> "" Then
      PlotTitle = PlotTitle + " for " + frmTBMultiApp.GetLocationName()
    End If
    XTitle = "Wind Direction"
    YTitle = "Wind Speed" + dlm
    HelpID = 1465
  Case PV_MEAN   'Mean Deposition
    PlotTitle = "Mean Deposition"
    XTitle = "Effective Swath Width"
    YTitle = "Deposition" + dlm
    HelpID = 1466
  Case PV_DWDSDINC  'Downwind DSD
    PlotTitle = "Downwind Incremental DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_DWDSDCUM  'Downwind DSD
    PlotTitle = "Downwind Cumulative DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_FXDSDINC  'Vertical Flux DSD
    PlotTitle = "Transport Aloft Incremental DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_FXDSDCUM  'Vertical Flux DSD
    PlotTitle = "Transport Aloft Cumulative DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_SV 'Settling Velocity
    PlotTitle = "Settling Velocity"
    XTitle = "Drop Size"
    YTitle = "Settling" + dlm + "Velocity" + dlm
    HelpID = 1467
  Case PV_SVTANK 'Settling Velocity
    PlotTitle = "Settling Velocity - Carrier"
    XTitle = "Drop Size"
    YTitle = "Settling" + dlm + "Velocity" + dlm
    HelpID = 1467
  Case PV_SVNONV 'Settling Velocity
    PlotTitle = "Settling Velocity - Active/Additive"
    XTitle = "Drop Size"
    YTitle = "Settling" + dlm + "Velocity" + dlm
    HelpID = 1467
  Case PV_SBDSDINC  'Spray Block DSD
    PlotTitle = "Spray Block Incremental DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_SBDSDCUM  'Spray Block DSD
    PlotTitle = "Spray Block Cumulative DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_CNDSDINC  'Canopy DSD
    PlotTitle = "Canopy Incremental DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_CNDSDCUM  'Canopy DSD
    PlotTitle = "Canopy Cumulative DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_PTDSDINC  'Point DSD
    PlotTitle = "Point Incremental DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_PTDSDCUM  'Point DSD
    PlotTitle = "Point Cumulative DSD"
    XTitle = "Drop Size"
    YTitle = "Volume" + dlm + "Fraction"
    HelpID = 1110
  Case PV_SBDEP  'Spray Block Deposition
    PlotTitle = "Spray Block Deposition"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 1468
  Case PV_SBCOVER  'Spray Block Area Coverage
    PlotTitle = "Spray Block Area Coverage"
    XTitle = "Fraction of Application Rate"
    YTitle = "Percent" + dlm + "of Area" + dlm + "Covered"
    HelpID = 1469
  Case PV_CANDEP 'Canopy Deposition
    PlotTitle = "Canopy Deposition"
    XTitle = "Volume Fraction Remaining"
    YTitle = "Vertical" + dlm + "Distance"
    HelpID = 1470
  Case PV_TA    'Time Accountancy
    PlotTitle = "Time Accountancy"
    XTitle = "Time"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1471
  Case PV_TAALOFT    'Time Accountancy
    PlotTitle = "Time Accountancy - Aloft"
    XTitle = "Time"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1471
  Case PV_TA, PV_TAVAPOR    'Time Accountancy
    PlotTitle = "Time Accountancy - Vapor"
    XTitle = "Time"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1471
  Case PV_TACANOPY    'Time Accountancy
    PlotTitle = "Time Accountancy - Canopy"
    XTitle = "Time"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1471
  Case PV_TAGROUND    'Time Accountancy
    PlotTitle = "Time Accountancy - Ground"
    XTitle = "Time"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1471
  Case PV_HA    'Height Accountancy
    PlotTitle = "Height Accountancy"
    XTitle = "Fraction of Tank Mix"
    YTitle = "Vertical" + dlm + "Distance" + dlm
    HelpID = 1472
  Case PV_HAALOFT    'Height Accountancy
    PlotTitle = "Height Accountancy - Aloft"
    XTitle = "Fraction of Tank Mix"
    YTitle = "Vertical" + dlm + "Distance" + dlm
    HelpID = 1472
  Case PV_HAVAPOR    'Height Accountancy
    PlotTitle = "Height Accountancy - Vapor"
    XTitle = "Fraction of Tank Mix"
    YTitle = "Vertical" + dlm + "Distance" + dlm
    HelpID = 1472
  Case PV_HACANOPY    'Height Accountancy
    PlotTitle = "Height Accountancy - Canopy"
    XTitle = "Fraction of Tank Mix"
    YTitle = "Vertical" + dlm + "Distance" + dlm
    HelpID = 1472
  Case PV_DA    'Distance Accountancy
    PlotTitle = "Distance Accountancy"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1473
  Case PV_DAALOFT    'Distance Accountancy
    PlotTitle = "Distance Accountancy - Aloft"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1473
  Case PV_DAVAPOR    'Distance Accountancy
    PlotTitle = "Distance Accountancy - Vapor"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1473
  Case PV_DACANOPY    'Distance Accountancy
    PlotTitle = "Distance Accountancy - Canopy"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1473
  Case PV_DAGROUND    'Distance Accountancy
    PlotTitle = "Distance Accountancy - Ground"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1473
  Case PV_TAB    'Total Accountancy Bar Chart
    PlotTitle = "Total Accountancy"
    XTitle = ""
    YTitle = "Fraction of" + dlm + "Tank Mix"
    HelpID = 1474
  Case PV_TAP    'Total Accountancy Pie Chart
    PlotTitle = "Total Accountancy"
    XTitle = ""
    YTitle = ""
    HelpID = 1474
  Case PV_AQUA  'Deposition Assessment Toolbox
    PlotTitle = "Deposition Assessment"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 1512
  Case PV_TERR  'Terrestrial Assessment Toolbox User-def pond-int depos
    PlotTitle = "Area Average Deposition"
    XTitle = "Distance"
    YTitle = "Fraction of" + dlm + "Application" + dlm + "Rate"
    HelpID = 1512
  Case PV_SBDET
    PlotTitle = "Spray Block Details"
    XTitle = "X Distance"
    YTitle = "Y" + dlm + "Distance"
    HelpID = 1502
  Case PV_GAUS
    PlotTitle = "Gaussian Far-Field Extension"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 1559
  Case PV_CURV 'Curve Fit Toolbox depos
    PlotTitle = "Curve-Fit"
    XTitle = "Distance"
    YTitle = "Deposition" + dlm
    HelpID = 0
  Case Else
    GenPlotTitleStrings = False
  End Select
End Function

Public Function GenPlotTitles(PlotVar As Long, FormatForPlots As Integer)
'Generate Plot Title data
'
'Return true is successful
'
' PlotVar  i  PV_*
'
' FormatForPlots i True =insert CR between Y title words
'                        for a snazzy plot appearance
'                  False=insert a space between Y title
'                        words for neater column headings
'
  Dim PlotTitle As String
  Dim DataTitle As String
  Dim XTitle As String
  Dim YTitle As String
  Dim HelpID As Long
  Dim s As String
  
  GenPlotTitles = False
  If GenPlotTitleStrings(PlotVar, FormatForPlots, _
                         PlotTitle, XTitle, YTitle, HelpID) Then
    s = PlotTitle
    PlotSetCaption s
    PlotSetPlotTitle s
    PlotSetXtitle XTitle
    PlotSetYtitle YTitle
    PlotSetRunTitle UD.Title
    PlotSetHelpID HelpID
    GenPlotTitles = True
  End If
End Function

Public Function GenPlotData(PlotVar As Long) As Integer
'generate plot data based on SourceID
'
' PlotVar  i  PV_*
'
' returns: True if data was generated for all slots
'          False if there was a promlem generating data
'          in one or more slots, or if the PlotVar
'          was not valid
'
'The "data source" of each plot data slot is a string that
'is encoded with lookup information. It can have the following
'formats:
' "Current Data:"  - use the current calculations in UserData
' "File: filename" - load data from a user file. "filename"
'                    is a path to the file.
' "Tier1Lib: libkey" - load data from the Tier 1 library. "libkey"
'                 is a unique string identifying the library
'                 record.
' "Lib: libkey" - load data from the Dropsize library. "libkey"
'                 is a unique string identifying the library
'                 record.
' "TrialPred: libkey" - load data from the Field Trial Prediction library. "libkey"
'                   is a unique string identifying the library
'                   record.
' "TrialMeas: libkey" - load data from the Field Trial Measurement library. "libkey"
'                   is a unique string identifying the library
'                   record.
' "ToolboxData: col" - load data from the Toolbox Plot Data, selecting
'                   the indicated column.
'
  Dim Source As String
  Dim SourceID As Integer
  Dim Param1 As String
  Dim Param2 As String
  Dim Param3 As String
  Dim Param4 As String
  Dim slot As Integer
  Dim CMF As Single
  Dim ApplMethod As Integer
  Dim BasicType As Integer
  Dim NumSwaths As Integer
  Dim StartSwath As Integer
  Dim EndSwath As Integer
  Dim Col As Integer
  Dim i As Integer
  Dim np As Integer
  Dim X(MAX_CALCDATA) As Single
  Dim Y(MAX_CALCDATA) As Single
  Dim Lbl(MAX_CALCDATA) As String
  Dim dum As Integer
  Dim PVtmp(3) As Long
  Dim iDSD As Integer
  Dim nDSD As Integer
  Dim DataTitle As String
  Dim DSDflag(2) As Boolean
  Dim key As String
  
  'Default return value
  GenPlotData = True
  
  'Set up the plot units
  GenPlotUnits PlotVar
  
  Select Case PlotVar
  
  'Contour plots
  'These plots get their data from a special location
  Case PV_SBDET
'tbc    InitContourData 'finds min/max etc
  
  'Milti-curve plots
  'Plots that produce more than one curve override the current
  'data sources and titles
  Case PV_SV
    PVtmp(0) = PV_SVTANK
    PVtmp(1) = PV_SVNONV
    PlotXYDataReset 'clear all slots
    'Clear all data sources and titles
    For i = 0 To 4: PlotSetDataSource i, "": PlotSetDataTitle i, "": Next
    slot = 0
    For i = 0 To 1
      If Not GenPlotDataUDUC(UD, UC, PVtmp(i), True, np, X(), Y(), Lbl()) Then
        GenPlotData = False
        Exit For
      End If
      If np > 0 Then
        PlotDataAddPoints slot, np, X(), Y(), Lbl()
        PlotSetDataSource slot, "Current Data:"
        GenPlotDataTitleString UD, PVtmp(i), DataTitle
        PlotSetDataTitle slot, DataTitle
        slot = slot + 1
      End If
    Next
  Case PV_TA
    PVtmp(0) = PV_TAALOFT
    PVtmp(1) = PV_TAVAPOR
    PVtmp(2) = PV_TACANOPY
    PVtmp(3) = PV_TAGROUND
    PlotXYDataReset 'clear all slots
    'Clear all data sources and titles
    For i = 0 To 4: PlotSetDataSource i, "": PlotSetDataTitle i, "": Next
    slot = 0
    For i = 0 To 3
      If Not GenPlotDataUDUC(UD, UC, PVtmp(i), True, np, X(), Y(), Lbl()) Then
        GenPlotData = False
        Exit For
      End If
      If np > 0 Then
        PlotDataAddPoints slot, np, X(), Y(), Lbl()
        PlotSetDataSource slot, "Current Data:"
        GenPlotDataTitleString UD, PVtmp(i), DataTitle
        PlotSetDataTitle slot, DataTitle
        slot = slot + 1
      End If
    Next
  Case PV_DA
    PVtmp(0) = PV_DAALOFT
    PVtmp(1) = PV_DAVAPOR
    PVtmp(2) = PV_DACANOPY
    PVtmp(3) = PV_DAGROUND
    PlotXYDataReset 'clear all slots
    'Clear all data sources and titles
    For i = 0 To 4: PlotSetDataSource i, "": PlotSetDataTitle i, "": Next
    slot = 0
    For i = 0 To 3
      If Not GenPlotDataUDUC(UD, UC, PVtmp(i), True, np, X(), Y(), Lbl()) Then
        GenPlotData = False
        Exit For
      End If
      If np > 0 Then
        PlotDataAddPoints slot, np, X(), Y(), Lbl()
        PlotSetDataSource slot, "Current Data:"
        GenPlotDataTitleString UD, PVtmp(i), DataTitle
        PlotSetDataTitle slot, DataTitle
        slot = slot + 1
      End If
    Next
  Case PV_HA
    PVtmp(0) = PV_HAALOFT
    PVtmp(1) = PV_HAVAPOR
    PVtmp(2) = PV_HACANOPY
    PlotXYDataReset 'clear all slots
    'Clear all data sources and titles
    For i = 0 To 4: PlotSetDataSource i, "": PlotSetDataTitle i, "": Next
    slot = 0
    For i = 0 To 2
      If Not GenPlotDataUDUC(UD, UC, PVtmp(i), True, np, X(), Y(), Lbl()) Then
        GenPlotData = False
        Exit For
      End If
      If np > 0 Then
        PlotDataAddPoints slot, np, X(), Y(), Lbl()
        PlotSetDataSource slot, "Current Data:"
        GenPlotDataTitleString UD, PVtmp(i), DataTitle
        PlotSetDataTitle slot, DataTitle
        slot = slot + 1
      End If
    Next
  
  'Normal, slot-related plots
  Case Else
    'Generate plot data for all "slots"
    
    PlotXYDataReset 'clear all slots
    'Loop through the plot data slots and generate data for each of them
    'based on the DataSource value
    For slot = 0 To 4
      'recover data source ID and parameters for the current slot
      SourceID = SourceToSourceID(PlotGetDataSource(slot), Param1, Param2, Param3, Param4)
      np = 0
      If SourceIsValid(PlotVar, SourceID) Then
        Select Case SourceID
        
        Case SID_NONE 'No data
          'nothing to do
          
        Case SID_CURRENT 'Current Data
          If GenPlotDataUDUC(UD, UC, PlotVar, True, np, X(), Y(), Lbl()) Then
            PlotDataAddPoints slot, np, X(), Y(), Lbl()
            If UI.PlotVar <> PlotVar Then
              GenPlotDataTitleString UD, PlotVar, DataTitle
              PlotSetDataTitle slot, DataTitle
            End If
          Else
           'if we can't generate "current data", the whole operation is bad
            GenPlotData = False
          End If
        
        Case SID_FILE 'Saved Data
          Dim xUD As UserData
          Dim xUC As UserCalc
          If UserDataRead(Param1, xUD, xUC, True) And _
             GenPlotDataUDUC(xUD, xUC, PlotVar, False, np, X(), Y(), Lbl()) Then
            PlotDataAddPoints slot, np, X(), Y(), Lbl()
          Else
            'If the file read failed, don't mark the whole operation
            'as bad, just don't return any data for the slot
            PlotXYDataResetSlot slot
          End If
        
        Case SID_LIB 'Dropsize Library Entry
          Select Case PlotVar
          Case PV_VFCUM 'Cumul Mass Frac
            'The library key is a multi-element comma-separated list
            'distributed throughout the ParamX's. We need to stitch
            'them all together.
            key = Param1
            If Param2 <> "" Then key = key + "," + Param2
            If Param3 <> "" Then key = key + "," + Param3
            If Param4 <> "" Then key = key + "," + Param4
            GetLibDataDSD key, np, X(), Y()
            CMF = 0
            For i = 0 To np - 1
              CMF = CMF + Y(i) 'generate cumulative mass fractions
              Y(i) = CMF
            Next
            PlotDataAddPoints slot, np, X(), Y(), Lbl()
          Case PV_VFINC 'Incr Mass Frac
            'The library key is a multi-element comma-separated list
            'distributed throughout the ParamX's. We need to stitch
            'them all together.
            key = Param1
            If Param2 <> "" Then key = key + "," + Param2
            If Param3 <> "" Then key = key + "," + Param3
            If Param4 <> "" Then key = key + "," + Param4
            GetLibDataDSD key, np, X(), Y()
            PlotDataAddPoints slot, np, X(), Y(), Lbl()
          End Select
        
        Case SID_TPD 'Toolbox Plot Data
          If (PlotVar And PVA_SOURCE_MASK) = PVA_TB Then
            Col = Val(Param1)
            GenPlotDataTB Col, np, X(), Y()
            PlotDataAddPoints slot, np, X(), Y(), Lbl()
            'Toolbox plots get their data titles set elsewhere
          End If
        
        Case Else 'Unknown PlotVar
          GenPlotData = False
        End Select
      End If
    Next
  End Select
End Function

Public Function GenPlotDataUDUC(xUD As UserData, xUC As UserCalc, _
                                PlotVar As Long, QueryUser As Boolean, _
                                np As Integer, X() As Single, Y() As Single, _
                                Lbl() As String) As Integer
'Generate plot data from UD data structure
'
' PlotVar   i  PV_* (but only those based on calculated data)
' QueryUser i  True=give user a chance to recalc (only works if xUD is UD)
'              False=do not query user for recalc
'
'returns - true if successful, false if not
'
'np  - returned number of data points
'X() - returned array of X values
'Y() - returned array of Y values
'Lbl() - returned array of point labels
'
  Dim Msg As String
  Dim MBType As Integer
  Dim NXtmp As Long
  Dim CMF As Single
  Dim i As Integer
  Dim npl As Long
  Dim DepUnitsFact As Single 'Deposition units conversion factor
  Dim FluxUnitsFact As Single 'Flux units conversion factor

  Dim VMD As Single
  Dim Span As Single
  Dim D10 As Single
  Dim D90 As Single
  Dim F141 As Single
  Dim DriftPotential As Single
'tbc
  Dim Xtbc(1000) As Single, Ytbc(1000) As Single

  GenPlotDataUDUC = False  'set to true when done

  'Tests for recalculating.
  'This operation is only meaningful if UD was passed to xUD
  If QueryUser Then
    'For UC-based plots:
    'See if the calcs have been done and give
    'the user a chance to do them if necessary
    If (PlotVar And PVA_SOURCE_MASK) = PVA_UC Then
      If Not xUC.Valid Then
        If Not QueryPerformCalcs() Then Exit Function
      End If
    End If
    
'tbc - does this need to be updated or removed?
    'Special behavior because of Field Trial Data:
    'If the calcs are valid but no data is present
    'for these values (because calcs were loaded
    'from Field Trial Library which doesn't include
    'those calcs), give the user a chance to do the calcs.
    If (PlotVar = PV_VERT And xUC.NumFlux = 0) Or _
       (PlotVar = PV_COV And xUC.NumCOV = 0) Or _
       (PlotVar = PV_CONC And xUC.NumConc = 0) Or _
       (PlotVar = PV_FA And xUC.NumFA = 0) Then
      If Not QueryPerformCalcs() Then Exit Function
    End If
  End If
  
  'load up return arrays
  Select Case PlotVar
  'UD-based plots follow
  Case PV_VFCUM
    np = xUD.DSD.NumDrop
    CMF = 0
    For i = 0 To xUD.DSD.NumDrop - 1
      CMF = CMF + xUD.DSD.MassFrac(i) 'generate cumulative mass fractions
      X(i) = xUD.DSD.Diam(i)
      Y(i) = CMF
      Lbl(i) = ""
    Next
  Case PV_VFINC
    np = xUD.DSD.NumDrop
    For i = 0 To xUD.DSD.NumDrop - 1
      X(i) = xUD.DSD.Diam(i)
      Y(i) = xUD.DSD.MassFrac(i)
      Lbl(i) = ""
    Next
  Case PV_SVTANK
    With xUD.DSD 'AGDISP only uses the first DSD
      Call agdsrn(0, CLng(.NumDrop), .Diam(0), .MassFrac(0), _
                  VMD, Span, D10, D90, F141, DriftPotential)
    End With
    agsetl xUD.SM, VMD, npl, X(0), Y(0), Xtbc(0), Ytbc(0)
    np = npl
    For i = 0 To np - 1
      Y(i) = UnitsDisplay(Y(i), UN_SPEED)
      Lbl(i) = ""
    Next
  Case PV_SVNONV
    With xUD.DSD 'AGDISP only uses the first DSD
      Call agdsrn(0, CLng(.NumDrop), .Diam(0), .MassFrac(0), _
                  VMD, Span, D10, D90, F141, DriftPotential)
    End With
    agsetl xUD.SM, VMD, npl, Xtbc(0), Ytbc(0), X(0), Y(0)
    np = npl
    For i = 0 To np - 1
      Y(i) = UnitsDisplay(Y(i), UN_SPEED)
      Lbl(i) = ""
    Next
  'UC-based plots follow
  Case PV_DEP
    'Set up deposition units conversion factor
    DepUnitsFact = UnitsFactorDep()
    np = xUC.NumDep
    For i = 0 To xUC.NumDep - 1
      X(i) = UnitsDisplay(xUC.DepDist(i), UN_LENGTH)
      Y(i) = xUC.DepVal(i) * DepUnitsFact
      Lbl(i) = ""
    Next
  Case PV_DRP
    np = xUC.NumDrp
    For i = 0 To xUC.NumDrp - 1
      X(i) = UnitsDisplay(xUC.DrpDist(i), UN_LENGTH)
      Y(i) = xUC.DrpVal(i)
      Lbl(i) = ""
    Next
  Case PV_VERT
    'Set up flux units conversion factor
    FluxUnitsFact = UnitsFactorFlux()
    np = xUC.NumFlux
    For i = 0 To xUC.NumFlux - 1
      X(i) = xUC.FluxVal(i) * FluxUnitsFact
      Y(i) = UnitsDisplay(xUC.FluxDist(i), UN_LENGTH)
      Lbl(i) = ""
    Next
  Case PV_PID
    np = xUC.NumPID
    For i = 0 To xUC.NumPID - 1
      X(i) = UnitsDisplay(xUC.PIDDist(i), UN_LENGTH)
      Y(i) = xUC.PIDVal(i)
      Lbl(i) = ""
    Next
  Case PV_COV
    np = xUC.NumCOV
    For i = 0 To xUC.NumCOV - 1
      X(i) = UnitsDisplay(xUC.COVESW(i), UN_LENGTH)
      Y(i) = xUC.COVVal(i)
      Lbl(i) = ""
    Next
  Case PV_CONC
    np = xUC.NumConc
    For i = 0 To xUC.NumConc - 1
      X(i) = xUC.ConcVal(i)
      Y(i) = UnitsDisplay(xUC.ConcDist(i), UN_LENGTH)
      Lbl(i) = ""
    Next
  Case PV_LAYOUT         'application layout
    'Set up deposition units conversion factor
    DepUnitsFact = UnitsFactorDep()
    np = xUC.NumLAY
    For i = 0 To xUC.NumLAY - 1
      X(i) = UnitsDisplay(xUC.LAYDist(i), UN_LENGTH)
      Y(i) = xUC.LAYFrac(i) * DepUnitsFact
      Lbl(i) = ""
    Next
  Case PV_FA
    np = xUC.NumFA
    For i = 0 To xUC.NumFA - 1
      X(i) = UnitsDisplay(xUC.FADist(i), UN_LENGTH)
      Y(i) = xUC.FAVal(i)
      Lbl(i) = ""
    Next
  Case PV_MEAN   'Mean Deposition
    'Set up deposition units conversion factor
    DepUnitsFact = UnitsFactorDep()
    np = xUC.NumCOVM
    For i = 0 To xUC.NumCOVM - 1
      X(i) = UnitsDisplay(xUC.COVMDist(i), UN_LENGTH)
      Y(i) = xUC.COVMVal(i) * DepUnitsFact
      Lbl(i) = ""
    Next
  Case PV_DWDSDINC  'Downwind DSD incremental
    np = xUC.NumDWDSD
    For i = 0 To xUC.NumDWDSD - 1
      X(i) = xUC.DWDSDDiam(i)
      Y(i) = xUC.DWDSDFrac(i)
      Lbl(i) = ""
    Next
  Case PV_DWDSDCUM  'Downwind DSD cumulative
    np = xUC.NumDWDSD
    CMF = 0
    For i = 0 To xUC.NumDWDSD - 1
      X(i) = xUC.DWDSDDiam(i)
      CMF = CMF + xUC.DWDSDFrac(i)
      Y(i) = CMF
      Lbl(i) = ""
    Next
  Case PV_FXDSDINC  'Vertical Flux DSD incremental
    np = xUC.NumFXDSD
    For i = 0 To xUC.NumFXDSD - 1
      X(i) = xUC.FXDSDDiam(i)
      Y(i) = xUC.FXDSDFrac(i)
      Lbl(i) = ""
    Next
  Case PV_FXDSDCUM  'Vertical Flux DSD cumulative
    np = xUC.NumFXDSD
    CMF = 0
    For i = 0 To xUC.NumFXDSD - 1
      X(i) = xUC.FXDSDDiam(i)
      CMF = CMF + xUC.FXDSDFrac(i)
      Y(i) = CMF
      Lbl(i) = ""
    Next
  Case PV_SBDSDINC  'Spray Block DSD incremental
    np = xUC.NumSBDSD
    For i = 0 To xUC.NumSBDSD - 1
      X(i) = xUC.SBDSDDiam(i)
      Y(i) = xUC.SBDSDFrac(i)
      Lbl(i) = ""
    Next
  Case PV_SBDSDCUM  'Spray Block DSD cumulative
    np = xUC.NumSBDSD
    CMF = 0
    For i = 0 To xUC.NumSBDSD - 1
      X(i) = xUC.SBDSDDiam(i)
      CMF = CMF + xUC.SBDSDFrac(i)
      Y(i) = CMF
      Lbl(i) = ""
    Next
  Case PV_CNDSDINC  'Canopy DSD incremental
    np = xUC.NumCNDSD
    For i = 0 To xUC.NumCNDSD - 1
      X(i) = xUC.CNDSDDiam(i)
      Y(i) = xUC.CNDSDFrac(i)
      Lbl(i) = ""
    Next
  Case PV_CNDSDCUM  'Canopy DSD cumulative
    np = xUC.NumCNDSD
    CMF = 0
    For i = 0 To xUC.NumCNDSD - 1
      X(i) = xUC.CNDSDDiam(i)
      CMF = CMF + xUC.CNDSDFrac(i)
      Y(i) = CMF
      Lbl(i) = ""
    Next
  Case PV_PTDSDINC  'Point DSD incremental
    np = xUC.NumPTDSD
    For i = 0 To xUC.NumPTDSD - 1
      X(i) = xUC.PTDSDDiam(i)
      Y(i) = xUC.PTDSDFrac(i)
      Lbl(i) = ""
    Next
  Case PV_PTDSDCUM  'Point DSD cumulative
    np = xUC.NumPTDSD
    CMF = 0
    For i = 0 To xUC.NumPTDSD - 1
      X(i) = xUC.PTDSDDiam(i)
      CMF = CMF + xUC.PTDSDFrac(i)
      Y(i) = CMF
      Lbl(i) = ""
    Next
  Case PV_SBDEP  'Spray Block Deposition
    'Set up deposition units conversion factor
    DepUnitsFact = UnitsFactorDep()
    np = xUC.NumSBD
    For i = 0 To xUC.NumSBD - 1
      X(i) = UnitsDisplay(xUC.SBDDist(i), UN_LENGTH)
      Y(i) = xUC.SBDVal(i) * DepUnitsFact
      Lbl(i) = ""
    Next
  Case PV_SBCOVER  'Spray Block Area Coverage
    np = xUC.NumSBAC
    For i = 0 To xUC.NumSBAC - 1
      X(i) = xUC.SBACRate(i)
      Y(i) = xUC.SBACFrac(i)
      Lbl(i) = ""
    Next
  Case PV_CANDEP 'Canopy Deposition
    np = xUC.NumCAN
    For i = 0 To xUC.NumCAN - 1
      X(i) = xUC.CANVal(i)
      Y(i) = UnitsDisplay(xUC.CANDist(i), UN_LENGTH)
      Lbl(i) = ""
    Next
  Case PV_TAALOFT    'Time Accountancy aloft
    np = xUC.NumTAA
    For i = 0 To xUC.NumTAA - 1
      X(i) = xUC.TAATime(i)
      Y(i) = xUC.TAAVal(i)
      Lbl(i) = ""
    Next
  Case PV_TAVAPOR    'Time Accountancy vapor
    np = xUC.NumTAV
    For i = 0 To xUC.NumTAV - 1
      X(i) = xUC.TAVTime(i)
      Y(i) = xUC.TAVVal(i)
      Lbl(i) = ""
    Next
  Case PV_TACANOPY    'Time Accountancy canopy
    np = xUC.NumTAC
    For i = 0 To xUC.NumTAC - 1
      X(i) = xUC.TACTime(i)
      Y(i) = xUC.TACVal(i)
      Lbl(i) = ""
    Next
  Case PV_TAGROUND    'Time Accountancy ground
    np = xUC.NumTAG
    For i = 0 To xUC.NumTAG - 1
      X(i) = xUC.TAGTime(i)
      Y(i) = xUC.TAGVal(i)
      Lbl(i) = ""
    Next
  
  Case PV_DAALOFT    'Dist Accountancy aloft
    np = xUC.NumDAA
    For i = 0 To xUC.NumDAA - 1
      X(i) = UnitsDisplay(xUC.DAADist(i), UN_LENGTH)
      Y(i) = xUC.DAAVal(i)
      Lbl(i) = ""
    Next
  Case PV_DAVAPOR    'Dist Accountancy vapor
    np = xUC.NumDAV
    For i = 0 To xUC.NumDAV - 1
      X(i) = UnitsDisplay(xUC.DAVDist(i), UN_LENGTH)
      Y(i) = xUC.DAVVal(i)
      Lbl(i) = ""
    Next
  Case PV_DACANOPY    'Dist Accountancy canopy
    np = xUC.NumDAC
    For i = 0 To xUC.NumDAC - 1
      X(i) = UnitsDisplay(xUC.DACDist(i), UN_LENGTH)
      Y(i) = xUC.DACVal(i)
      Lbl(i) = ""
    Next
  Case PV_DAGROUND    'Dist Accountancy ground
    np = xUC.NumDAG
    For i = 0 To xUC.NumDAG - 1
      X(i) = UnitsDisplay(xUC.DAGDist(i), UN_LENGTH)
      Y(i) = xUC.DAGVal(i)
      Lbl(i) = ""
    Next
  
  Case PV_HAALOFT    'Height Accountancy
    np = xUC.NumHAA
    For i = 0 To xUC.NumHAA - 1
      X(i) = xUC.HAAVal(i)
      Y(i) = UnitsDisplay(xUC.HAAHgt(i), UN_LENGTH)
      Lbl(i) = ""
    Next
  Case PV_HAVAPOR    'Height Accountancy
    np = xUC.NumHAV
    For i = 0 To xUC.NumHAV - 1
      X(i) = xUC.HAVVal(i)
      Y(i) = UnitsDisplay(xUC.HAVHgt(i), UN_LENGTH)
      Lbl(i) = ""
    Next
  Case PV_HACANOPY    'Height Accountancy
    np = xUC.NumHAC
    For i = 0 To xUC.NumHAC - 1
      X(i) = xUC.HACVal(i)
      Y(i) = UnitsDisplay(xUC.HACHgt(i), UN_LENGTH)
      Lbl(i) = ""
    Next
  Case PV_TAB, PV_TAP   'Total Accountancy Bar/Pie Chart
'tbc need a pie chart
    i = 0
    X(i) = i: Y(i) = 0
    If xUC.NumTAA > 0 Then Y(i) = xUC.TAAVal(xUC.NumTAA - 1)
    Lbl(i) = "Aloft"
    i = i + 1
    
    X(i) = i: Y(i) = 0
    If xUC.NumTAV > 0 Then Y(i) = xUC.TAVVal(xUC.NumTAV - 1)
    Lbl(i) = "Vapor"
    i = i + 1
    
    'canopy is different
    If xUD.CAN.Type > 0 Then 'if there is a canopy
      X(i) = i: Y(i) = 0
      If xUC.NumTAC > 0 Then Y(i) = xUC.TACVal(xUC.NumTAC - 1)
      Lbl(i) = "Canopy"
      i = i + 1
    End If
    
    X(i) = i: Y(i) = 0
    If xUC.NumTAG > 0 Then Y(i) = xUC.TAGVal(xUC.NumTAG - 1)
    Lbl(i) = "Ground"
    i = i + 1
    
    np = i
  End Select
  
  'Made it to the end, success!
  GenPlotDataUDUC = True
End Function

Public Function GenPlotUnits(PlotVar As Long) As Integer
'generate units strings based on plot variable
'These strings will be appended to the X and Y titles at plot time
'
' PlotVar  i  PV_*
'
' Returns: True if successful
'          False if PlotVar is unsupported
'
  GenPlotUnits = True 'default return value
  Select Case PlotVar
  Case PV_VFCUM, PV_VFINC, _
       PV_DWDSDINC, PV_DWDSDCUM, _
       PV_FXDSDINC, PV_FXDSDCUM, _
       PV_SBDSDINC, PV_SBDSDCUM, _
       PV_CNDSDINC, PV_CNDSDCUM, _
       PV_PTDSDINC, PV_PTDSDCUM
    PlotSetYunits ""
    PlotSetXunits " (µm)"
  Case PV_SV, PV_SVTANK, PV_SVNONV
    PlotSetYunits " (" & UnitsName(UN_SPEED) & ")"
    PlotSetXunits " (µm)"
  Case PV_DEP, PV_MEAN, PV_SBDEP, PV_MAA, PV_AQUA, PV_LAYOUT, PV_GAUS, PV_CURV
    PlotSetYunits "(" & UnitsNameDep() & ")"
    PlotSetXunits " (" & UnitsName(UN_LENGTH) & ")"
  Case PV_DRP, PV_PID, PV_COV, PV_FA, PV_TERR, PV_DA
    PlotSetYunits ""
    PlotSetXunits " (" & UnitsName(UN_LENGTH) & ")"
  Case PV_CANDEP, PV_HA, PV_HAALOFT, PV_HAVAPOR, PV_HACANOPY
    PlotSetYunits " (" & UnitsName(UN_LENGTH) & ")"
    PlotSetXunits ""
  Case PV_SBLK, PV_SBDET
    PlotSetYunits " (" & UnitsName(UN_LENGTH) & ")"
    PlotSetXunits " (" & UnitsName(UN_LENGTH) & ")"
  Case PV_VERT
    PlotSetYunits " (" & UnitsName(UN_LENGTH) & ")"
    PlotSetXunits " (" & UnitsNameFlux() & ")"
  Case PV_CONC
    PlotSetYunits " (" & UnitsName(UN_LENGTH) & ")"
    PlotSetXunits " (ng/l)"
  Case PV_SATM
    PlotSetYunits " (ng/l) (ppt)"
    PlotSetXunits " (" & UnitsName(UN_LENGTH) & ")"
  Case PV_SADI
    PlotSetYunits " (ng/l) (ppt)"
    PlotSetXunits " (sec)"
  Case PV_TA, PV_TAALOFT, PV_TAVAPOR, PV_TACANOPY, PV_TAGROUND
    PlotSetYunits ""
    PlotSetXunits " (sec)"
  Case PV_MAAT
    PlotSetYunits " (" & UnitsName(UN_TEMP) & ")"
    PlotSetXunits ""
  Case PV_MAARH
    PlotSetYunits " (" & UnitsName(UN_PERCENT) & ")"
    PlotSetXunits ""
  Case PV_MAAWS
    PlotSetYunits " (" & UnitsName(UN_SPEED) & ")"
    PlotSetXunits ""
  Case PV_MAAWD
    PlotSetYunits " (deg)"
    PlotSetXunits ""
  Case PV_MAAROSE
    PlotSetYunits " (" & UnitsName(UN_SPEED) & ")"
    PlotSetXunits " (deg from)"
  Case Else
    PlotSetYunits ""
    PlotSetXunits ""
  End Select
End Function

Public Function GenPlotDataTitleString(xUD As UserData, PlotVar As Long, DataTitle As String)
  
  GenPlotDataTitleString = True 'Default
  
  Select Case PlotVar
  Case PV_VFINC, PV_VFCUM
    DataTitle = "Initial DSD"
  Case PV_DEP 'deposition
    DataTitle = "Deposition"
  Case PV_DRP 'number deposition
    DataTitle = "Number Deposition"
  Case PV_PID 'pond-integrated deposition
    DataTitle = "Pond-Integrated Deposition"
  Case PV_VERT 'Transport Aloft
    DataTitle = "Transport Aloft at " & _
                AGFormat$(UnitsDisplay(xUD.CTL.FluxPlane, UN_LENGTH)) & _
                " " & UnitsName(UN_LENGTH)
  Case PV_CONC 'concentration
    DataTitle = "Concentration at " & _
                AGFormat$(UnitsDisplay(xUD.CTL.FluxPlane, UN_LENGTH)) & _
                " " & UnitsName(UN_LENGTH)
  Case PV_COV 'cov/effective swath width
    DataTitle = "COV"
  Case PV_FA 'Fraction Aloft
    DataTitle = "Fraction Aloft"
  Case PV_MEAN   'Mean Deposition
    DataTitle = "Mean Deposition"
  Case PV_DWDSDINC  'Downwind DSD
    DataTitle = "Downwind Incremental DSD"
  Case PV_DWDSDCUM  'Downwind DSD
    DataTitle = "Downwind Cumulative DSD"
  Case PV_FXDSDINC  'Vertical Flux DSD
    DataTitle = "Transport Aloft Incremental DSD at " & _
                AGFormat$(UnitsDisplay(xUD.CTL.FluxPlane, UN_LENGTH)) & _
                " " & UnitsName(UN_LENGTH)
  Case PV_FXDSDCUM  'Vertical Flux DSD
    DataTitle = "Transport Aloft Cumulative DSD at " & _
                AGFormat$(UnitsDisplay(xUD.CTL.FluxPlane, UN_LENGTH)) & _
                " " & UnitsName(UN_LENGTH)
  Case PV_SV
    DataTitle = "Settling Velocity"
  Case PV_SVTANK
    DataTitle = "Carrier"
  Case PV_SVNONV
    DataTitle = "Active/Additive"
  Case PV_LAYOUT  'application Layout
    DataTitle = "Application Layout"
  Case PV_SATM
    DataTitle = "Time="
  Case PV_SADI
    DataTitle = "Distance="
  Case PV_MAA
    DataTitle = "Multiple Application Assessment"
  Case PV_MAAT
    DataTitle = "Temperature"
  Case PV_MAARH
    DataTitle = "Relative Humidity"
  Case PV_MAAWS
    DataTitle = "Wind Speed"
  Case PV_MAAWD
    DataTitle = "Wind Direction"
  Case PV_SBDSDINC  'Spray Block DSD
    DataTitle = "Spray Block Incremental DSD"
  Case PV_SBDSDCUM  'Spray Block DSD
    DataTitle = "Spray Block Cumulative DSD"
  Case PV_CNDSDINC  'Canopy DSD
    DataTitle = "Canopy Incremental DSD"
  Case PV_CNDSDCUM  'Canopy DSD
    DataTitle = "Canopy Cumulative DSD"
  Case PV_PTDSDINC  'Point DSD
    DataTitle = "Point Incremental DSD"
  Case PV_PTDSDCUM  'Point DSD
    DataTitle = "Point Cumulative DSD"
  Case PV_SBDEP  'Spray Block Deposition
    DataTitle = "Spray Block Deposition"
  Case PV_SBCOVER  'Spray Block Area Coverage
    DataTitle = "Area Coverage"
  Case PV_CANDEP 'Canopy Deposition
    DataTitle = "Canopy Deposition"
  Case PV_TA    'Time Accountancy
    DataTitle = "Time Accountancy"
  Case PV_DA   'Distance Accountancy
    DataTitle = "Distance Accountancy"
  Case PV_HA   'Height Accountancy
    DataTitle = "Height Accountancy"
  Case PV_TAALOFT, PV_DAALOFT, PV_HAALOFT
    DataTitle = "Aloft"
  Case PV_TAVAPOR, PV_DAVAPOR, PV_HAVAPOR
    DataTitle = "Vapor"
  Case PV_TACANOPY, PV_DACANOPY, PV_HACANOPY
    DataTitle = "Canopy"
  Case PV_TAGROUND, PV_DAGROUND
    DataTitle = "Ground"
  Case PV_TAB    'Total Accountancy Bar Chart
    DataTitle = "Total Accountancy"
  Case PV_TAP    'Total Accountancy Pie Chart
    DataTitle = "Total Accountancy"
  Case PV_AQUA  'Deposition Assessment
    DataTitle = "Deposition Assessment"
  Case PV_TERR  'Terrestrial Assessment Toolbox User-def pond-int depos
    DataTitle = "Area Average Deposition"
  Case PV_MAA
    DataTitle = "Multiple Application Assessment"
  Case PV_SBDET
    DataTitle = "Spray Block Details"
  Case PV_GAUS
    DataTitle = "Gaussian Far-Field Extension"
  Case PV_CURV
    DataTitle = "Curve Fit"
  Case Else
    DataTitle = ""
    GenPlotDataTitleString = False
  End Select
End Function

Public Function GenPlotDataTB(Col As Integer, np As Integer, X() As Single, Y() As Single)
'recover plot data from the Toolbox Plot data area
'
  If TPD.X1D Then
    np = TPD.np(0)
    CopyMemory X(0), TPD.X(0), TPD.np(0) * Len(X(0))
    CopyMemory Y(0), TPD.Y(0, Col), TPD.np(0) * Len(Y(0))
  Else
    np = TPD.np(Col)
    CopyMemory X(0), TPD.X(0, Col), TPD.np(Col) * Len(X(0))
    CopyMemory Y(0), TPD.Y(0, Col), TPD.np(Col) * Len(Y(0))
  End If
End Function

Public Function SourceIsValid(PlotVar As Long, SourceID As Integer) As Integer
'Determine whether a DataSource is valid for a given PlotVar
'
' PlotVar  i  PV_*
' SourceID i  SID_*
'
  SourceIsValid = False 'default return value
  
  'First of all, no source can be vaild for a PlotVar
  'that is not available for the current Tier, etc.
  'then impose further restrictions based on source
  If PlotIsAvailable(PlotVar) Then
    'Toolbox plots can only come from toolbox data
    If (PlotVar And PVA_SOURCE_MASK) = PVA_TB Then
      Select Case SourceID
      Case SID_TPD
        SourceIsValid = True
      End Select
    'UD/UC plots are more complicated
    Else
      'select valid sources
      Select Case SourceID
      Case SID_NONE, SID_CURRENT, SID_FILE
        'these sources are available for all Plots
        SourceIsValid = True
      Case SID_LIB      'Drop Library
        'The Drop library provides DSD's for Tier II and III
        Select Case PlotVar
        Case PV_VFINC, PV_VFCUM
          SourceIsValid = True
        End Select
      End Select
    End If
  End If
End Function

Public Function SourceToSourceID(Source As String, _
                                 Param1 As String, _
                                 Param2 As String, _
                                 Param3 As String, _
                                 Param4 As String) _
                                 As Integer
'Decode a Plot Data Dource string and return its key
'
' Source        i  The contents of a DataSource slot
'
' Param1-4      o  extra data from the input line. These may be blank,
'                  as for Current Data, or may contain a filename or key
'                  for the other IDs
'
' returns:    SID_NONE    -1=No Data                 ("")
'             SID_CURRENT  0=Current Data            ("Current Data:")
'             SID_FILE     1=Saved Results           ("File: filename")
'             SID_T1LIB    2=Tier I Library          ("Tier1Lib: am, type, swaths/start, end")
'             SID_LIB      3=Dropsize Library        ("Lib: key")
'             SID_FTPRED   4=Field Trial Prediction  ("TrialPred: key")
'             SID_FTMEAS   5=Field Trial Measurement ("TrialMeas: key")
'             SID_TPD      6=Toolbox Plot Data       ("ToolboxData: col")
'
  Dim key As String
  Dim pColon As Integer
  Dim pComma1 As Integer
  Dim pComma2 As Integer
  Dim pComma3 As Integer
  
  'default return values
  Param1 = ""
  Param2 = ""
  Param3 = ""
  Param4 = ""
  
  'find the delimiters
  pColon = InStr(Source, ":")  'position of colon
  pComma1 = InStr(pColon + 1, Source, ",") 'position of first comma
  pComma2 = InStr(pComma1 + 1, Source, ",") 'position of second comma
  pComma3 = InStr(pComma2 + 1, Source, ",") 'position of third comma
  
  'extract key and params
  If pColon > 0 Then 'There is a colon
    key = Trim$(Left$(Source, pColon - 1)) 'up to colon
    If pComma1 > 0 Then 'there is a comma
      'extract Param1 from colon to first comma
      Param1 = Trim$(Mid$(Source, pColon + 1, pComma1 - pColon - 1))
      If pComma2 > 0 Then 'there is a second comma
        'extract Param2 from first comma to second comma
        Param2 = Trim$(Mid$(Source, pComma1 + 1, pComma2 - pComma1 - 1))
        If pComma3 > 0 Then 'there is a third comma
          'extract Param3 from second comma to third
          Param3 = Trim$(Mid$(Source, pComma2 + 1, pComma3 - pComma2 - 1))
          'extract Param4 from third comma to end
          Param4 = Trim$(Mid$(Source, pComma3 + 1))
        Else 'no third comma
          'extract Param3 from second comma to end
          Param3 = Trim$(Mid$(Source, pComma2 + 1))
        End If
      Else 'no second comma
        'extract Param2 from first comma to end
        Param2 = Trim$(Mid$(Source, pComma1 + 1))
      End If
    Else 'no first comma
      'extract Param1 from colon to end
      Param1 = Trim$(Mid$(Source, pColon + 1))
    End If
  Else  'no colon
    key = Trim$(Source) 'no colon, use whole string
  End If
  
  'Decode the key and params
  Select Case key
  Case ""  'no data
    SourceToSourceID = SID_NONE
  Case "Current Data"     'use current calculations
    SourceToSourceID = SID_CURRENT
  Case "File"
    SourceToSourceID = SID_FILE    'Saved Results
  Case "Tier1Lib"
    SourceToSourceID = SID_T1LIB  'Tier I Library
  Case "Lib"
    SourceToSourceID = SID_LIB     'Dropsize Library Entry
  Case "TrialPred"
    SourceToSourceID = SID_FTPRED  'Field Trial Prediction
  Case "TrialMeas"
    SourceToSourceID = SID_FTMEAS  'Field Trial Measurement
  Case "ToolboxData"
    SourceToSourceID = SID_TPD     'Toolbox Plot Data
  Case Else
    SourceToSourceID = SID_NONE
  End Select
End Function

