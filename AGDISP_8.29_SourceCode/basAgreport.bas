Attribute VB_Name = "basAGREPORT"
'agreport.bas - report-generation formatting routines
'$Id: basAgreport.bas,v 1.16 2016/12/05 15:36:29 tom Exp $
Option Explicit

'The general form of the report output consists of three columns;
'the first generally contains a descriptive name, the second contains
'the current value of the quantity displayed, while the third contains
'the default value for reference if the current value differs from it.
'For tables, where more than one quantity is to be displayed on a line,
'"subcolumns" are defined to contain multiple values in a single main
'column.
Const c1wid = 50 '26 'number of columns to allot for column 1
Const c2wid = 28 'number of columns to allot for column 2
Const c3wid = 28 'number of columns to allot for column 3
Private c1fmt As String  'format string for column 1
Private c2fmt As String  'format string for column 2
Private c3fmt As String  'format string for column 3

Public Function GenReportText() As String
'Generate the text of printed reports for:
'  Input Summary
'  Print Preview
'  Print Report
'The report lists the user-defined inputs in a three-column format.
'Column 1 describes the value and its units, if applicable.
'Column 2 lists the user-entered value
'Column 3 lists the default value if the user-entered value is different
  
  Dim grt As String    'temporary storage for report text
  Dim s As String      'workspace string
  Dim s1 As String, s2 As String
  Dim xUD As UserData  'default user data values
  Dim nloop As Integer 'local loop counter
  Dim start As Integer
  Dim i As Integer
  Dim iDSD As Integer  'DSD index
  Dim Col2Hdr As String, Col3Hdr As String

  'get a set of default data for comparisons
  UserDataDefault xUD

  'Set up the formats for the three columns and subcolumns
  'Try to keep the total under 80 columns for printing
  c1fmt = "!" & String$(c1wid, "@") 'left-justified
  c2fmt = " " & String$(c2wid, "@") '1 space, right-justified
  c3fmt = " " & String$(c3wid, "@") '1 space, right-justified
  'the following produces this: ---Current--- ---Default---
  s = "": i = (c2wid - Len(s)) / 2
  Col2Hdr = String(i, "-") + s + String(c2wid - i - Len(s), "-")
  s = "Default": i = (c2wid - Len(s)) / 2
  Col3Hdr = String(i, "-") + s + String(c2wid - i - Len(s), "-")
  
  grt = "" 'start with a blank string
    
  'Report title
  AppendStr grt, "AGDISP Input Data Summary", True
  AppendStr grt, "", True
  
  'General data for all tiers
  AppendStr grt, "Title: " & UD.Title, True
  AppendStr grt, "Notes: ", True
  If Len(UD.Notes) > 0 Then
    start = 1
    Do
      AppendStr grt, " " & LineFromString(UD.Notes, start), True
    Loop While start > 0
  End If
  AppendStr grt, "", True
  
  'Calculation status
  AppendStr grt, "Calculations Done: " & Format$(UC.Valid, "YES/NO"), True
  AppendStr grt, "Run ID: " & GetRunID(), True
  AppendStr grt, "", True

  'Application Method =============================================================
  AppendReportLineS grt, "--APPLICATION METHOD--", Col2Hdr ', Col3Hdr
  Select Case UD.ApplMethod
  Case AM_AERIAL
    AppendReportLineS grt, "Method", "Aerial" ', xUD.AC.Name
    AppendReportLineS grt, "--Aircraft--", Col2Hdr ', Col3Hdr
    AppendReportLineS grt, "Name", UD.AC.Name ', xUD.AC.Name
    AppendReportLineS grt, "Type", GetTypeNameAC(UD.AC.Type) ', GetTypeNameAC(xUD.AC.Type)
    AppendReportLineS grt, "Wing Type", GetTypeNameACWing(UD.AC.WingType) ', GetTypeNameACWing(xUD.AC.WingType)
    Select Case UD.AC.WingType
    Case 3  'fixed-wing
      AppendReportLineN grt, "Semispan", UN_LENGTH, UD.AC.SemiSpan ', xUD.AC.SemiSpan
      AppendReportLineN grt, "Weight", UN_MASS, UD.AC.Weight ', xUD.AC.Weight
      AppendReportLineN grt, "Typical Speed", UN_SPEED, UD.AC.TypSpeed ', xUD.AC.TypSpeed
      AppendReportLineN grt, "Propeller RPM", UN_NONE, UD.AC.PropRPM ', xUD.AC.PropRPM
      AppendReportLineN grt, "Propeller Radius", UN_LENGTH, UD.AC.PropRad ', xUD.AC.PropRad
      AppendReportLineN grt, "Biplane Separation", UN_LENGTH, UD.AC.BiplSep ', xUD.AC.BiplSep
      AppendReportLineN grt, "Planform Area", UN_AREA, UD.AC.PlanArea ', xUD.AC.PlanArea
      AppendReportLineN grt, "Engines", UN_NONE, UD.AC.NumEng '
      AppendReportLineN grt, "Engine Vert Distance", UN_LENGTH, UD.AC.EngVert ', xUD.AC.EngVert
      AppendReportLineN grt, "Engine Fwd Distance", UN_LENGTH, UD.AC.EngFwd ', xUD.AC.EngFwd
      AppendReportLineN grt, "Engine Horiz Distance 1", UN_LENGTH, UD.AC.EngHoriz(0)
      AppendReportLineN grt, "Engine Horiz Distance 2", UN_LENGTH, UD.AC.EngHoriz(1)
      AppendReportLineN grt, "Wing Vertical Distance", UN_LENGTH, UD.AC.WingVert
      AppendReportLineN grt, "Boom Vertical Distance", UN_LENGTH, UD.AC.BoomVert
      AppendReportLineN grt, "Boom Forward Distance", UN_LENGTH, UD.AC.BoomFwd
    Case 4  'helicopter
      AppendReportLineN grt, "Rotor Radius", UN_LENGTH, UD.AC.SemiSpan ', xUD.AC.SemiSpan
      AppendReportLineN grt, "Weight", UN_MASS, UD.AC.Weight ', xUD.AC.Weight
      AppendReportLineN grt, "Typical Speed", UN_SPEED, UD.AC.TypSpeed ', xUD.AC.TypSpeed
      AppendReportLineN grt, "Rotor RPM", UN_NONE, UD.AC.PropRPM ', xUD.AC.PropRPM
    End Select
    
  Case AM_GROUND
    AppendReportLineS grt, "Method", "Ground" ', xUD.AC.Name
    AppendReportLineS grt, "--Ground Sprayer--", Col2Hdr ', Col3Hdr
    AppendReportLineS grt, "Nozzle Type", GetTypeNameGANoz(UD.GA.NozType)
    AppendReportLineN grt, "Boom Pressure", UN_PRESSURE, UD.GA.Pressure ', xUD.GA.Pressure
  End Select
  
  'Release height and spray lines
  AppendReportLineS grt, "--Spray Lines--", Col2Hdr ', Col3Hdr
  AppendReportLineN grt, "Release Height", UN_LENGTH, UD.CTL.Height ', xUD.CTL.Height
  AppendReportLineN grt, "Spray Lines", UN_NONE, UD.CTL.NumLines ', xUD.CTL.NumLines
  AppendReportLineS grt, "Optimize Spray Reps", Format(UD.CTL.LineOptimize, "YES/NO")
  AppendReportLineCH grt, "Spray Line Reps", UN_NONE, "Reps"
  For i = 0 To UD.CTL.NumLines - 1
    AppendReportLineC grt, i + 1, UN_NONE, , UD.CTL.LineReps(i)
  Next
  AppendStr grt, "", True

  'Application Technique ===========================================================
  'Application Technique
  AppendReportLineS grt, "--APPLICATION TECHNIQUE--", Col2Hdr  ', Col3Hdr
  AppendReportLineS grt, "Application Technique", GetTypeNameAerial(UD.AerialType)
  
  'Nozzles (for liquid applications)
  If UD.AerialType = AD_LIQUID Then 'liquid
    AppendReportLineS grt, "--Nozzles--", Col2Hdr ', Col3Hdr
    AppendReportLineN grt, "Boom Length (%)", UN_NONE, UD.NZ.BoomWidth ', xUD.NZ.BoomWidth
    AppendReportLineCH grt, "Nozzle Locations", _
                            UN_LENGTH, "Hor", UN_LENGTH, "Ver", UN_LENGTH, "Fwd"
    For i = 0 To UD.NZ.NumNoz - 1
      AppendReportLineC grt, i + 1, UN_LENGTH, , UD.NZ.PosHoriz(i), , _
                                    UN_LENGTH, , UD.NZ.PosVert(i), , _
                                    UN_LENGTH, , UD.NZ.PosFwd(i)
    Next
    AppendStr grt, "", True
  End If
  
  Select Case UD.AerialType
  Case AD_LIQUID 'Liquid application
    'Drop Size Distributions (liquid)
    AppendReportLineS grt, "--Drop Size Distribution--", Col2Hdr  ', Col3Hdr
    AppendReportLineS grt, "Name", UD.DSD.Name ', xUD.DSD.Name
    AppendReportLineS grt, "Type", GetTypeNameDSD(UD.DSD.Type) ', GetTypeNameDSD(xUD.DSD.Type)
       
    Select Case UD.DSD.Type
    Case 5 'ARS (DropKirk)
      AppendReportLineS grt, "Nozzle Name", GetARSNozName(UD.BK.NozType) ', GetTypeNameBKNoz(xUD.BK.NozType)
      AppendReportLineN grt, "Orifice (in or #)", UN_NONE, UD.BK.Orifice ', xUD.BK.Orifice
      AppendReportLineN grt, "Air Speed", UN_SPEED, UD.BK.Speed ', xUD.BK.Speed
      AppendReportLineN grt, "Nozzle Angle (deg)", UN_NONE, UD.BK.NozAngle ', xUD.BK.NozAngle
      AppendReportLineN grt, "Pressure", UN_PRESSURE, UD.BK.Pressure ', xUD.BK.Pressure
      AppendReportLineN grt, "Spray Type", UN_NONE, UD.BK.SprayType ', xUD.BK.SprayType
    Case 6 'USDA FS Rotary Nozzle
      AppendReportLineS grt, "Nozzle Name", GetNameHKNoz(UD.HK.RotType) ', GetNameHKNoz(xUD.HK.RotType)
      AppendReportLineS grt, "Material Type", GetNameHKMat(UD.HK.MatType) ', GetTypeNameBKNoz(xUD.HK.NozType)
      AppendReportLineN grt, "Tunnel Speed", UN_SPEED, UD.HK.Speed ', xUD.HK.Speed
      If UD.HK.RotType > 0 Then AppendReportLineN grt, "Blade Angle (deg)", UN_NONE, UD.HK.BladeAngle ', xUD.HK.NozAngle
      AppendReportLineN grt, "Blade RPM (RPM)", UN_NONE, UD.HK.BladeRPM ', xUD.HK.NozAngle
      AppendReportLineN grt, "Flow Rate", UN_FLOWRATE, UD.HK.Flowrate ', xUD.HK.Pressure
      AppendReportLineN grt, "Spray Type", UN_NONE, UD.HK.SprayType ', xUD.HK.SprayType
    End Select
    AppendReportLineCH grt, "Drop Categories", UN_NONE, "Diam (um)", UN_NONE, "Frac"
    For i = 0 To UD.DSD.NumDrop - 1
      AppendReportLineC grt, i + 1, UN_NONE, "0.00", UD.DSD.Diam(i), , _
                                        UN_NONE, "0.0000", UD.DSD.MassFrac(i)
    Next
    AppendStr grt, "", True
  
  Case AD_DRY 'Dry Delivery
    'Particle Size Distributions (dry)
    AppendReportLineS grt, "--Particle Size Distribution--", Col2Hdr  ', Col3Hdr
    AppendReportLineS grt, "Name", UD.DSD.Name ', xUD.DSD.Name
    AppendReportLineS grt, "Type", GetTypeNameDSD(UD.DSD.Type) ', GetTypeNameDSD(xUD.DSD.Type)
    AppendReportLineN grt, "Particle Sphericity", UN_NONE, UD.DRY.Sphericity
    AppendReportLineS grt, "Spreader Type", GetTypeNameSpreader(UD.DRY.Type)
        
    Select Case UD.DRY.Type
    Case 0 'Venturi
      AppendReportLineN grt, "Exit Width", UN_LENGTH, UD.DRY.Length
      AppendReportLineN grt, "Exit Angle (deg)", UN_NONE, UD.DRY.angle
      AppendReportLineN grt, "Exit Speed", UN_SPEED, UD.DRY.Velocity
    Case 1 'Radial
      AppendReportLineN grt, "Disc Radius", UN_LENGTH, UD.DRY.Hub
      AppendReportLineN grt, "Rotation Rate (RPM)", UN_NONE, UD.DRY.RPM
    Case 2 'Bucket
      AppendReportLineN grt, "Swing Distance", UN_LENGTH, UD.DRY.Length
    End Select
        
    AppendReportLineCH grt, "Particle Size Categories", UN_NONE, "Diam (um)", UN_NONE, "Frac"
    For i = 0 To UD.DSD.NumDrop - 1
      AppendReportLineC grt, i + 1, UN_NONE, "0.00", UD.DSD.Diam(i), , _
                                    UN_NONE, "0.0000", UD.DSD.MassFrac(i)
    Next
    AppendStr grt, "", True
  End Select

  'Swath ===========================================================================
  AppendReportLineS grt, "--SWATH--", Col2Hdr ', Col3Hdr
  s1 = "": s2 = ""
  Select Case 0 'UD.CTL.SwathWidthType
  Case 0:    s1 = AGFormat$(UnitsDisplay(UD.CTL.SwathWidth, UN_LENGTH)) + " " & UnitsName(UN_LENGTH)
  Case 1, 2: s1 = AGFormat$(UD.CTL.SwathWidth) + " x Wingspan"
  End Select
  Select Case 0 'xUD.CTL.SwathWidthType
  Case 0:    s2 = AGFormat$(UnitsDisplay(xUD.CTL.SwathWidth, UN_LENGTH)) + " " & UnitsName(UN_LENGTH)
  Case 1, 2: s2 = AGFormat$(xUD.CTL.SwathWidth) + " x Wingspan"
  End Select
  AppendReportLineS grt, "Swath Width", s1 ', s2
  
  s1 = "": s2 = ""
  Select Case 2 'UD.CTL.SwathDispType
  Case 0: s1 = AGFormat$(UD.CTL.SwathDisp) + " x Swath Width"
  Case 1: s1 = AGFormat$(UD.CTL.SwathDisp) + " x Application Rate"
  Case 2: s1 = AGFormat$(UnitsDisplay(UD.CTL.SwathDisp, UN_LENGTH)) + " " & UnitsName(UN_LENGTH)
  Case 3: s1 = "Aircraft Centerline"
  End Select
  Select Case 2 'xUD.CTL.SwathDispType
  Case 0: s2 = AGFormat$(xUD.CTL.SwathDisp) + " x Swath Width"
  Case 1: s2 = AGFormat$(xUD.CTL.SwathDisp) + " x Application Rate"
  Case 2: s2 = AGFormat$(UnitsDisplay(xUD.CTL.SwathDisp, UN_LENGTH)) + " " & UnitsName(UN_LENGTH)
  Case 3: s2 = "Aircraft Centerline"
  End Select
  AppendReportLineS grt, "Swath Displacement", s1 ', s2
  AppendStr grt, "", True
  
  'Meteorology =====================================================================
  AppendReportLineS grt, "--METEOROLOGY--", Col2Hdr ', Col3Hdr
  Select Case UD.MET.WindType
  Case WIND_TYPE_SINGLE
    AppendReportLineN grt, "Wind Speed", UN_SPEED, UD.MET.WS ', xUD.MET.WS
  Case WIND_TYPE_TABLE
    AppendReportLineCH grt, "Wind Speed Table", UN_LENGTH, "Height", UN_SPEED, "Speed"
    For i = 0 To UD.MET.NumWinds - 1
      AppendReportLineC grt, i + 1, UN_LENGTH, "0.00", UD.MET.WindHeight(i), , _
                                    UN_SPEED, "0.0000", UD.MET.WindHeight(i)
    Next
  End Select
  AppendReportLineN grt, "Wind Direction (deg)", UN_NONE, UD.MET.WD ', xUD.MET.WD
  AppendReportLineN grt, "Temperature", UN_TEMP, UD.MET.temp ', xUD.MET.temp
  AppendReportLineN grt, "Relative Humidity (%)", UN_NONE, UD.MET.Humidity ', xUD.MET.Humidity
  AppendStr grt, "", True
  
  'Spray Material ==================================================================
  AppendReportLineS grt, "--SPRAY MATERIAL--", Col2Hdr ', Col3Hdr
  'Properties
  AppendReportLineS grt, "Name", UD.SM.Name ', xUD.SM.Name
  AppendReportLineS grt, "Spray Material Evaporates", Format(UD.SM.BasicType, "YES/NO")
  If UD.AerialType = AD_LIQUID Then 'liquid
    AppendReportLineN grt, "Spray Volume Rate", UN_RATEVOL, UD.SM.Flowrate
  Else
    AppendReportLineN grt, "Spray Volume Rate", UN_RATEMASS, UD.SM.Flowrate
  End If
  
  AppendReportLineN grt, "Active Fraction", UN_NONE, UD.SM.ACfrac ', xUD.SM.ACfrac
  AppendReportLineN grt, "Nonvolatile Fraction", UN_NONE, UD.SM.NVfrac ', xUD.SM.NVfrac
  
  'Tank Mix
  AppendReportLineN grt, "Active Fraction of Tank Mix", UN_NONE, UD.SM.ActSolFrac
  AppendReportLineN grt, "Fraction of Active Solution that is Nonvolatile", UN_NONE, UD.SM.ActNVFrac
  AppendReportLineN grt, "Additive Fraction of Tank Mix", UN_NONE, UD.SM.AddSolFrac
  AppendReportLineN grt, "Fraction of Additive Solution that is Nonvolatile", UN_NONE, UD.SM.AddNVFrac
  AppendStr grt, "", True
  
  'Atmospheric Stability ===========================================================================
  AppendReportLineS grt, "--ATMOSPHERIC STABILITY--", Col2Hdr ', Col3Hdr
  AppendReportLineS grt, "Atmospheric Stability", GetTypeNameStability(UD.MET.Insolation)
  AppendStr grt, "", True
  
  'Surface =========================================================================================
  AppendReportLineS grt, "--SURFACE--", Col2Hdr ', Col3Hdr
  AppendReportLineN grt, "Upslope Angle (deg)", UN_NONE, UD.TRN.Upslope ', xUD.TRN.Upslope
  AppendReportLineN grt, "Sideslope Angle (deg)", UN_NONE, UD.TRN.Sideslope ', xUD.TRN.Sideslope
  AppendReportLineS grt, "--Canopy--", Col2Hdr ', Col3Hdr
  AppendReportLineS grt, "Type", GetTypeNameCN(UD.CAN.Type) ', GetTypeNameCN(xUD.CAN.Type)
  If UD.CAN.Type <> 0 Then
    AppendReportLineS grt, "Name", UD.CAN.Name ', xUD.CAN.Name
  End If
  Select Case UD.CAN.Type
  Case 0 'No Canopy
    AppendReportLineN grt, "Surface Roughness", UN_LENGTH, UD.MET.SurfRough ', xUD.MET.SurfRough
  Case 1 'Story canopy
    AppendReportLineN grt, "Element Size", UN_SMLENGTH, UD.CAN.EleSiz ', xUD.CAN.EleSiz
    AppendReportLineS grt, "Element Type", GetTypeNameCanopyElement(UD.CAN.EleTyp) ', xUD.CAN.EleSiz
    AppendReportLineN grt, "Temperature", UN_TEMP, UD.CAN.temp ', xUD.CAN.temp
    AppendReportLineN grt, "Humidity", UN_PERCENT, UD.CAN.Humidity ', xUD.CAN.Humidity
    AppendReportLineN grt, "Stand Density", UN_STANDDENSITY, UD.CAN.StanDen ', xUD.CAN.StanDen
    AppendReportLineCH grt, "Tree Envelope", UN_LENGTH, "Hgt", UN_LENGTH, "Dia", UN_NONE, "PoP"
    For i = 0 To UD.CAN.NumEnv - 1
      AppendReportLineC grt, i + 1, UN_LENGTH, , UD.CAN.EnvHgt(i), , _
                                    UN_LENGTH, , UD.CAN.EnvDiam(i), , _
                                    UN_NONE, , UD.CAN.EnvPop(i)
    Next
  Case 2 'Optical
    AppendReportLineN grt, "Element Size", UN_SMLENGTH, UD.CAN.EleSiz ', xUD.CAN.EleSiz
    AppendReportLineS grt, "Element Type", GetTypeNameCanopyElement(UD.CAN.EleTyp) ', xUD.CAN.EleSiz
    AppendReportLineN grt, "Temperature", UN_TEMP, UD.CAN.temp ', xUD.CAN.temp
    AppendReportLineN grt, "Humidity", UN_PERCENT, UD.CAN.Humidity ', xUD.CAN.Humidity
    AppendReportLineS grt, "LAI Canopy Type", GetTypeNameOP(UD.CAN.optType) ', GetTypeNameOP(xUD.CAN.optType)
    Select Case UD.CAN.optType
    Case 1 'User-Defined
      AppendReportLineCH grt, "LAI Envelope", UN_LENGTH, "Hgt", UN_NONE, "LAI"
      For i = 0 To UD.CAN.NumLAI - 1
        AppendReportLineC grt, i + 1, UN_LENGTH, , UD.CAN.LAIHgt(i), , _
                                      UN_NONE, , UD.CAN.LAICum(i)
      Next
    Case 2 'Library
      AppendReportLineS grt, "Data Quality", GetNameCanDataQuality(UD.CAN.LibDataQuality) ', xUD.CAN.LibDataQuality
      AppendReportLineN grt, "Height", UN_LENGTH, UD.CAN.LibHgt ', xUD.CAN.LibHgt
      AppendReportLineN grt, "LAI", UN_NONE, UD.CAN.LibLAI ', xUD.CAN.LibLAI
    End Select
  Case 3 'Height-only
    AppendReportLineN grt, "Height", UN_LENGTH, UD.CAN.Height ', xUD.CAN.Height
  End Select
  AppendStr grt, "", True
  
  'Transport =======================================================================================
  AppendReportLineS grt, "--TRANSPORT--", Col2Hdr ', Col3Hdr
  AppendReportLineN grt, "Flux Plane Distance", UN_LENGTH, UD.CTL.FluxPlane ', xUD.CTL.FluxPlane
  AppendStr grt, "", True
  
  'Advanced Settings ===============================================================================
  AppendReportLineS grt, "--ADVANCED SETTINGS--", Col2Hdr ', Col3Hdr
  AppendReportLineN grt, "Wind Speed Height", UN_LENGTH, UD.MET.WH ', xUD.MET.WH
  AppendReportLineN grt, "Max Compute Time (sec)", UN_NONE, UD.CTL.MaxComputeTime ', xUD.CTL.MaxComputeTime
  AppendReportLineN grt, "Max Downwind Dist", UN_LENGTH, UD.CTL.MaxDownwindDist ', xUD.CTL.MaxDownwindDist
  AppendReportLineN grt, "Vortex Decay Rate (OGE)", UN_SPEED, UD.MET.VortexDecayOGE ', xUD.MET.VortexDecay
  AppendReportLineN grt, "Vortex Decay Rate (IGE)", UN_SPEED, UD.MET.VortexDecayIGE ', xUD.MET.VortexDecay
  AppendReportLineN grt, "Aircraft Drag Coeff", UN_NONE, UD.AC.DragCoeff ', xUD.AC.DragCoeff
  AppendReportLineN grt, "Propeller Efficiency", UN_NONE, UD.AC.PropEff ', xUD.AC.PropEff
  AppendReportLineN grt, "Ambient Pressure", UN_AIRPRESSURE, UD.MET.Pressure ', xUD.MET.Pressure
  AppendReportLineN grt, "Ground Reference", UN_LENGTH, UD.TRN.Zref
  AppendReportLineS grt, "Save Trajectory Files", Format(UD.CTL.SaveTraj, "YES/NO")
  AppendReportLineS grt, "Save CALPUFF Data", Format(UD.CALPUFFFLAG, "YES/NO")
  If UD.AerialType = AD_LIQUID Then
    AppendReportLineS grt, "Half Boom", Format(UD.CTL.HalfBoom, "YES/NO") ', Format(xUD.CTL.HalfBoom, "YES/NO")
  End If
  Select Case UD.CTL.SwathOffset
  Case 0
    AppendReportLineS grt, "Default Swath Offset", "1/2 Swath"
  Case 1
    AppendReportLineS grt, "Default Swath Offset", "0 Swath"
  End Select
  AppendReportLineN grt, "Specific Gravity (Carrier)", UN_NONE, UD.SM.SpecGrav ', xUD.SM.SpecGrav
  AppendReportLineN grt, "Specific Gravity (Active/Additive)", UN_NONE, UD.SM.NonVGrav ', xUD.SM.NonVGrav
  If UD.AerialType = AD_LIQUID Then 'liquid
    AppendReportLineN grt, "Evaporation Rate (µm²/deg C/sec)", UN_NONE, UD.SM.EvapRate ', xUD.SM.EvapRate
  End If
  AppendStr grt, "", True

  GenReportText = grt
End Function

Public Sub AppendReportLineN(s As String, Name As String, _
                            Units As Integer, Optional CurVal, Optional DefVal)
'Standard report line: single numbers (floating or integer)
  
  'Column 1 - the name
  If Units = UN_NONE Then
    AppendStr s, Format$(Name, c1fmt), False
  Else
    AppendStr s, Format$(Name & " (" & UnitsName(Units) & ")", c1fmt), False
  End If
  
  'Column 2 - the current value
  If Not IsMissing(CurVal) Then
    AppendStr s, Format$(AGFormat$(UnitsDisplay(CurVal, Units)), c2fmt), False
  End If
  
  'Column 3 - the default value
  If Not IsMissing(DefVal) Then
    If (CurVal <> DefVal) Then
      AppendStr s, Format$(AGFormat$(UnitsDisplay(DefVal, Units)), c3fmt), False
    End If
  End If
  
  'end with a CR
  AppendStr s, "", True
End Sub

Public Sub AppendReportLineS(s As String, _
                             Name As String, Optional CurVal, Optional DefVal)
'Standard report line: strings
  
  'Column 1 - the name
  AppendStr s, Format$(Name, c1fmt), False
  
  'Column 2 - the current value
  If Not IsMissing(CurVal) Then
    AppendStr s, Format$(ClipStr$(CurVal, c2wid), c2fmt), False
  End If
  
  'Column 3 - the default value
  If Not IsMissing(DefVal) Then
    If (CurVal <> DefVal) Then
      AppendStr s, Format$(ClipStr$(DefVal, c3wid), c3fmt), False
    End If
  End If
  
  'end with a CR
  AppendStr s, "", True
End Sub

Public Sub AppendReportLineC(s As String, Index As Integer, _
                             Optional Units1, Optional Fmt1, Optional CurVal1, Optional DefVal1, _
                             Optional Units2, Optional Fmt2, Optional CurVal2, Optional DefVal2, _
                             Optional Units3, Optional Fmt3, Optional CurVal3, Optional DefVal3, _
                             Optional Units4, Optional Fmt4, Optional CurVal4, Optional DefVal4)
'Standard report line: 1- to 4-column table entries
'If Fmtx is missing, use AGFormat

  Dim s0 As String, s1 As String
  Dim subwid As Integer
  Dim subfmt As String
  Dim ncol As Integer
  Dim ShowDefaults As Boolean
  
  'Compute subcolumn width and define format
  ncol = 0
  If Not IsMissing(CurVal1) Then ncol = 1
  If Not IsMissing(CurVal2) Then ncol = 2
  If Not IsMissing(CurVal3) Then ncol = 3
  If Not IsMissing(CurVal4) Then ncol = 4
  subwid = (c2wid - ncol - 1) / ncol
  subfmt = String$(subwid, "@")
  
  'Column 1 - the Index, right justified
  AppendStr s, Format$(CStr(Index), String$(c1wid, "@")), False
  
  'Column 2 - the current values
  s0 = "": s1 = ""
  If Not IsMissing(CurVal1) Then
    If Not IsMissing(Fmt1) Then
      s1 = Format(UnitsDisplay(CurVal1, Units1), Fmt1)
    Else
      s1 = AGFormat$(UnitsDisplay(CurVal1, Units1))
    End If
    AppendStr s0, Format(s1, subfmt), False
  End If
  If Not IsMissing(CurVal2) Then
    If Not IsMissing(Fmt2) Then
      s1 = Format(UnitsDisplay(CurVal2, Units2), Fmt2)
    Else
      s1 = AGFormat$(UnitsDisplay(CurVal2, Units2))
    End If
    AppendStr s0, " " + Format(s1, subfmt), False
  End If
  If Not IsMissing(CurVal3) Then
    If Not IsMissing(Fmt3) Then
      s1 = Format(UnitsDisplay(CurVal3, Units3), Fmt3)
    Else
      s1 = AGFormat$(UnitsDisplay(CurVal3, Units3))
    End If
    AppendStr s0, " " + Format(s1, subfmt), False
  End If
  If Not IsMissing(CurVal4) Then
    If Not IsMissing(Fmt4) Then
      s1 = Format(UnitsDisplay(CurVal4, Units4), Fmt4)
    Else
      s1 = AGFormat$(UnitsDisplay(CurVal4, Units4))
    End If
    AppendStr s0, " " + Format(s1, subfmt), False
  End If
  AppendStr s, Format$(s0, c2fmt), False
  
  'Column 3 - the default values
  ShowDefaults = False
  If Not IsMissing(CurVal1) And Not IsMissing(DefVal1) Then
    If (CurVal1 <> DefVal1) Then ShowDefaults = True
  End If
  If Not IsMissing(CurVal2) And Not IsMissing(DefVal2) Then
    If (CurVal2 <> DefVal2) Then ShowDefaults = True
  End If
  If Not IsMissing(CurVal3) And Not IsMissing(DefVal3) Then
    If (CurVal3 <> DefVal3) Then ShowDefaults = True
  End If
  If Not IsMissing(CurVal4) And Not IsMissing(DefVal4) Then
    If (CurVal4 <> DefVal4) Then ShowDefaults = True
  End If
  If ShowDefaults Then
    s0 = "": s1 = ""
    If Not IsMissing(DefVal1) Then
      If Not IsMissing(Fmt1) Then
        s1 = Format(UnitsDisplay(DefVal1, Units1), Fmt1)
      Else
        s1 = AGFormat$(UnitsDisplay(DefVal1, Units1))
      End If
      AppendStr s0, Format(s1, subfmt), False
    End If
    If Not IsMissing(DefVal2) Then
      If Not IsMissing(Fmt2) Then
        s1 = Format(UnitsDisplay(DefVal2, Units2), Fmt2)
      Else
        s1 = AGFormat$(UnitsDisplay(DefVal2, Units2))
      End If
      AppendStr s0, " " + Format(s1, subfmt), False
    End If
    If Not IsMissing(DefVal3) Then
      If Not IsMissing(Fmt3) Then
        s1 = Format(UnitsDisplay(DefVal3, Units3), Fmt3)
      Else
        s1 = AGFormat$(UnitsDisplay(DefVal3, Units3))
      End If
      AppendStr s0, " " + Format(s1, subfmt), False
    End If
    If Not IsMissing(DefVal4) Then
      If Not IsMissing(Fmt4) Then
        s1 = Format(UnitsDisplay(DefVal4, Units4), Fmt4)
      Else
        s1 = AGFormat$(UnitsDisplay(DefVal4, Units4))
      End If
      AppendStr s0, " " + Format(s1, subfmt), False
    End If
    AppendStr s, Format$(s0, c3fmt), False
  End If
  
  'end with a CR
  AppendStr s, "", True
End Sub

Public Sub AppendReportLineCH(s As String, Name As String, _
                              Optional Units1, Optional SubCol1, _
                              Optional Units2, Optional SubCol2, _
                              Optional Units3, Optional SubCol3, _
                              Optional Units4, Optional SubCol4)
'Standard report line: 1- to 4-column table headers
  Dim s0 As String, s1 As String, s2 As String, s3 As String, s4 As String
  Dim subwid As Integer
  Dim subfmt As String
  Dim ncol As Integer
  
  'Compute subcolumn width and define format
  ncol = 0
  If Not IsMissing(SubCol1) Then ncol = 1
  If Not IsMissing(SubCol2) Then ncol = 2
  If Not IsMissing(SubCol3) Then ncol = 3
  If Not IsMissing(SubCol4) Then ncol = 4
  subwid = (c2wid - ncol - 1) / ncol
  subfmt = String$(subwid, "@")
  
  'Column 1 - the Name, left justified, + the Index Header, right justified
  AppendStr s, Format$(Name, "!" + String$(c1wid - 1, "@")) + "#", False
  
  'Construct the subcolumns
  If Not IsMissing(SubCol1) Then
    s1 = SubCol1
    If Units1 <> UN_NONE Then AppendStr s1, "(" + UnitsName(Units1) + ")", False
  End If
  If Not IsMissing(SubCol2) Then
    s2 = SubCol2
    If Units2 <> UN_NONE Then AppendStr s2, "(" + UnitsName(Units2) + ")", False
  End If
  If Not IsMissing(SubCol3) Then
    s3 = SubCol3
    If Units3 <> UN_NONE Then AppendStr s3, "(" + UnitsName(Units3) + ")", False
  End If
  If Not IsMissing(SubCol4) Then
    s4 = SubCol4
    If Units4 <> UN_NONE Then AppendStr s4, "(" + UnitsName(Units4) + ")", False
  End If
  
  'Assemble subcolumns
  s0 = ""
  If Not IsMissing(SubCol1) Then AppendStr s0, Format$(ClipStr$(s1, subwid), subfmt), False
  If Not IsMissing(SubCol2) Then AppendStr s0, " " + Format$(ClipStr$(s2, subwid), subfmt), False
  If Not IsMissing(SubCol3) Then AppendStr s0, " " + Format$(ClipStr$(s3, subwid), subfmt), False
  If Not IsMissing(SubCol4) Then AppendStr s0, " " + Format$(ClipStr$(s4, subwid), subfmt), False
  
  'Column 2 - the column headers
  AppendStr s, Format$(s0, c2fmt), False
  
'tbc removed third column
'  'Column 3 - the column headers again
'  AppendStr s, Format$(s0, c3fmt), False
  
  'end with a CR
  AppendStr s, "", True
End Sub

