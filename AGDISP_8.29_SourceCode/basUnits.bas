Attribute VB_Name = "basUnits"
'Units.bas - units conversion and display
'
Option Explicit

'Units globals
'  units systems
Public Const UN_IMPERIAL = 0
Public Const UN_METRIC = 1

'  unit types                       Internal   Imperial   Metric
Public Const UN_NONE = 0         '    -          -         -
Public Const UN_PERCENT = 1      '    %          %         %
Public Const UN_LENGTH = 2       '    m          ft        m
Public Const UN_AREA = 3         '    m2         ft2       m2
Public Const UN_AREALARGE = 4    '    ha         ac        ha
Public Const UN_VOLUME = 5       '    L          gal       L
Public Const UN_SPEED = 6        '    m/s        mph       m/s
Public Const UN_MASS = 7         '    kg         lbs       kg
Public Const UN_TEMP = 8         '    deg C      deg F     deg C
Public Const UN_PRESSURE = 9     '    bar        psig      bar
Public Const UN_AIRPRESSURE = 10 '    mb         in-hg     mb
Public Const UN_RATEMASS = 11    '    kg/ha      lb/ac     kg/ha
Public Const UN_RATEVOL = 12     '    L/ha       gal/ac    L/ha
Public Const UN_FLOWRATE = 13    '    L/min      gal/min   L/min
Public Const UN_BIGFLOWRATE = 21 '    m3/s       gal/s     m3/s
Public Const UN_RECHARGERATE = 22 '   m3/s/km    gal/s/mi  m3/s/km
Public Const UN_SMLENGTH = 23    '    m          in        cm
Public Const UN_SMLENGTH2 = 24   '    cm         in        cm
Public Const UN_STANDDENSITY = 25 '   stems/ha   stems/ac  stems/ha
Public Const UN_BIGLENGTH = 26   '    km         mi        km

'  conversion factors
Public Const UN_INCHESPERMETER = 39.37     'in/m        small length
Public Const UN_CMPERMETER = 100           'cm/m        small length
Public Const UN_FEETPERMETER = 3.2808      'ft/m        length
Public Const UN_FEET2PERMETER2 = 10.7636   'ft2/m2      area
Public Const UN_ACRESPERHECTARE = 2.471    'ac/ha       arealarge
Public Const UN_GALLONSPERLITER = 0.2642   'gal/L       volume
Public Const UN_MPSPERMPH = 0.44704        '(m/s)/(mph) speed
Public Const UN_POUNDSPERKILOGRAM = 2.2055 'kg/lb       mass
Public Const UN_PSIPERBAR = 14.5038        'psi/bar     pressure
Public Const UN_PSIGOFFSET = 14.6959488   'psig->psia gauge pressure offset
Public Const UN_INHGPERMB = 0.02953        'inhg/mb    atmosphereic pressure
Public Const UN_MILESPERKILOMETER = 0.6214 'mi/km      decayfactor

' Units System
Private UNSystem As Integer

Public Function UnitsFactorDep()
'Returns the factor required to convert Deposition in
'units of Fraction of Applied to Engineering units based on
'current preference settings for deposition units
  If UP.DepUnitsFraction = 0 Then
    agdun CLng(UD.SM.FlowrateUnits + 1), UD.AC.TypSpeed, UD.CTL.SwathWidth, _
          UD.SM.Flowrate, UD.SM.ACfrac, UD.SM.NonVGrav, _
          CLng(UP.DepUnitsNum), CLng(UP.DepUnitsDen), UnitsFactorDep
  Else
    UnitsFactorDep = 1
  End If
End Function

Public Function UnitsFactorFlux()
'Returns the factor required to convert Flux in
'units of Fraction of Applied to Engineering units based on
'current preference settings for flux units
  If UP.FluxUnitsFraction = 0 Then
    agfun CLng(UD.SM.FlowrateUnits + 1), UD.AC.TypSpeed, UD.CTL.SwathWidth, _
          UD.SM.Flowrate, UD.SM.ACfrac, UD.SM.NonVGrav, _
          CLng(UP.FluxUnitsNum), CLng(UP.FluxUnitsDen), UnitsFactorFlux
  Else
    UnitsFactorFlux = 1
  End If
End Function

Public Function UnitsNameFlux()
'Returns the units name for flux based on the
'current flux units preferences
  If UP.FluxUnitsFraction = 0 Then
    UnitsNameFlux = UnitsNameDepNum(UP.FluxUnitsNum) & "/" & _
                    UnitsNameDepDen(UP.FluxUnitsDen)
  Else
    UnitsNameFlux = "Fraction of Applied"
  End If
End Function

Public Sub UnitsSelectSystem(usys As Integer)
'Select a units system for the units conversion rotuines
'
'usys - Units system flag UN_IMPERIAL or UN_METRIC
  UNSystem = usys
End Sub

Public Function UnitsDisplay(valu, utype)
'convert a given value in internal units to display units
'based on the preference setting UNSystem
'
'valu   the value to convert, in internal units
'utype  units type
'
  UnitsDisplay = UnitsDisplaySys(valu, utype, UNSystem)
End Function

Public Function UnitsDisplaySys(valu, utype, usys)
'convert a given value in internal units to display units
'
'valu   the value to convert, in internal units
'utype  units type
'usys   units system UN_IMPERIAL or UN_METRIC
'
  Dim outval As Single

  outval = CSng(valu)  'default value

  Select Case usys
  Case UN_IMPERIAL
    Select Case utype
    Case UN_NONE
    Case UN_PERCENT
    Case UN_LENGTH
      outval = CSng(valu) * UN_FEETPERMETER
    Case UN_SMLENGTH
      outval = CSng(valu) * UN_INCHESPERMETER
    Case UN_SMLENGTH2
      outval = CSng(valu) * UN_INCHESPERMETER / UN_CMPERMETER
    Case UN_AREA
      outval = CSng(valu) * UN_FEET2PERMETER2
    Case UN_AREALARGE
      outval = CSng(valu) * UN_ACRESPERHECTARE
    Case UN_VOLUME
      outval = CSng(valu) * UN_GALLONSPERLITER
    Case UN_SPEED
      outval = CSng(valu) / UN_MPSPERMPH
    Case UN_MASS
      outval = CSng(valu) * UN_POUNDSPERKILOGRAM
    Case UN_TEMP
      outval = CDbl(valu) * (9# / 5#) + 32# 'deg C -> deg F
    Case UN_PRESSURE
      outval = CSng(valu) * UN_PSIPERBAR - UN_PSIGOFFSET 'bar -> psig
    Case UN_AIRPRESSURE
      outval = CSng(valu) * UN_INHGPERMB
    Case UN_RATEMASS
      outval = CSng(valu) * UN_POUNDSPERKILOGRAM / UN_ACRESPERHECTARE
    Case UN_RATEVOL
      outval = CSng(valu) * UN_GALLONSPERLITER / UN_ACRESPERHECTARE
    Case UN_FLOWRATE
      outval = CSng(valu) * UN_GALLONSPERLITER
    Case UN_BIGFLOWRATE
      outval = CSng(valu) * UN_GALLONSPERLITER * 1000
    Case UN_RECHARGERATE
      outval = CSng(valu) * UN_GALLONSPERLITER * 1000 / UN_MILESPERKILOMETER
    Case UN_STANDDENSITY
      outval = CSng(valu) / UN_ACRESPERHECTARE
    Case UN_BIGLENGTH
      outval = CSng(valu) * UN_MILESPERKILOMETER
    End Select
  Case UN_METRIC
    Select Case utype
    Case UN_SMLENGTH
      outval = CSng(valu) * UN_CMPERMETER
    End Select
  End Select
  UnitsDisplaySys = outval
End Function

Public Function UnitsInternal(valu, utype)
'convert a given value in display units to internal units
'based on the preference setting UNSystem
'
'valu   the value to convert, in internal units
'utype  units type
'
  UnitsInternal = UnitsInternalSys(valu, utype, UNSystem)
End Function

Public Function UnitsInternalSys(valu, utype, usys)
'convert a given value in display units to internal units
'
'valu   the value to convert, in internal units
'utype  units type
'usys   units system UN_IMPERIAL or UN_METRIC
'
  Dim outval As Single

  outval = CSng(valu)  'default value

  Select Case usys
  Case UN_IMPERIAL
    Select Case utype
    Case UN_NONE
    Case UN_PERCENT
    Case UN_LENGTH
      outval = CSng(valu) / UN_FEETPERMETER
    Case UN_SMLENGTH
      outval = CSng(valu) / UN_INCHESPERMETER
    Case UN_SMLENGTH2
      outval = CSng(valu) / UN_INCHESPERMETER * UN_CMPERMETER
    Case UN_AREA
      outval = CSng(valu) / UN_FEET2PERMETER2
    Case UN_AREALARGE
      outval = CSng(valu) / UN_ACRESPERHECTARE
    Case UN_VOLUME
      outval = CSng(valu) / UN_GALLONSPERLITER
    Case UN_SPEED
      outval = CSng(valu) * UN_MPSPERMPH
    Case UN_MASS
      outval = CSng(valu) / UN_POUNDSPERKILOGRAM
    Case UN_TEMP
      outval = (CDbl(valu) - 32#) * (5# / 9#)  'deg C <- deg F
    Case UN_PRESSURE
      outval = (CSng(valu) + UN_PSIGOFFSET) / UN_PSIPERBAR 'psig to bar abs
    Case UN_AIRPRESSURE
      outval = CSng(valu) / UN_INHGPERMB
    Case UN_RATEMASS
      outval = CSng(valu) / UN_POUNDSPERKILOGRAM * UN_ACRESPERHECTARE
    Case UN_RATEVOL
      outval = CSng(valu) / UN_GALLONSPERLITER * UN_ACRESPERHECTARE
    Case UN_FLOWRATE
      outval = CSng(valu) / UN_GALLONSPERLITER
    Case UN_BIGFLOWRATE
      outval = CSng(valu) / UN_GALLONSPERLITER / 1000
    Case UN_RECHARGERATE
      outval = CSng(valu) / UN_GALLONSPERLITER / 1000 * UN_MILESPERKILOMETER
    Case UN_STANDDENSITY
      outval = CSng(valu) * UN_ACRESPERHECTARE
    Case UN_BIGLENGTH
      outval = CSng(valu) / UN_MILESPERKILOMETER
    End Select
  Case UN_METRIC
    Select Case utype
    Case UN_SMLENGTH
      outval = CSng(valu) / UN_CMPERMETER
    End Select
  End Select
  UnitsInternalSys = outval
End Function

Public Function UnitsName(utype)
'return the abbreviated name of a unit specifier
'based on the preference setting UNSystem
'
'utype  units type
'
  UnitsName = UnitsNameSys(utype, UNSystem)
End Function

Public Function UnitsNameSys(utype, usys)
'return the abbreviated name of a unit specifier
'
'utype  units type
'usys   units system UN_IMPERIAL or UN_METRIC
'
  Dim s As String
  s = ""
  Select Case usys
  Case UN_IMPERIAL
    Select Case utype
    Case UN_NONE
      s = ""
    Case UN_PERCENT
      s = "%"
    Case UN_LENGTH
      s = "ft"
    Case UN_SMLENGTH, UN_SMLENGTH2
      s = "in"
    Case UN_AREA
      s = "ft²"
    Case UN_AREALARGE
      s = "ac"
    Case UN_VOLUME
      s = "gal"
    Case UN_SPEED
      s = "mph"
    Case UN_MASS
      s = "lbs"
    Case UN_TEMP
      s = "deg F"
    Case UN_PRESSURE
      s = "psig"
    Case UN_AIRPRESSURE
      s = "in hg"
    Case UN_RATEMASS
      s = "lb/ac"
    Case UN_RATEVOL
      s = "gal/ac"
    Case UN_FLOWRATE
      s = "gal/min"
    Case UN_BIGFLOWRATE
      s = "gal/s"
    Case UN_RECHARGERATE
      s = "gal/s/mi"
    Case UN_STANDDENSITY
      s = "stems/ac"
    Case UN_BIGLENGTH
      s = "mi"
    End Select
  Case UN_METRIC
    Select Case utype
    Case UN_NONE
      s = ""
    Case UN_PERCENT
      s = "%"
    Case UN_LENGTH
      s = "m"
    Case UN_SMLENGTH, UN_SMLENGTH2
      s = "cm"
    Case UN_AREA
      s = "m²"
    Case UN_AREALARGE
      s = "ha"
    Case UN_VOLUME
      s = "L"
    Case UN_SPEED
      s = "m/s"
    Case UN_MASS
      s = "kg"
    Case UN_TEMP
      s = "deg C"
    Case UN_PRESSURE
      s = "bar"
    Case UN_AIRPRESSURE
      s = "mb"
    Case UN_RATEMASS
      s = "kg/ha"
    Case UN_RATEVOL
      s = "L/ha"
    Case UN_FLOWRATE
      s = "L/min"
    Case UN_BIGFLOWRATE
      s = "m³/s"
    Case UN_RECHARGERATE
      s = "m³/s/km"
    Case UN_STANDDENSITY
      s = "stems/ha"
    Case UN_BIGLENGTH
      s = "km"
    End Select
  End Select
  UnitsNameSys = s
End Function

Public Function UnitsNameDep()
'Returns the units name for deposition based on the
'current depositiion units preferences
  If UP.DepUnitsFraction = 0 Then
    UnitsNameDep = UnitsNameDepNum(UP.DepUnitsNum) & "/" & _
                   UnitsNameDepDen(UP.DepUnitsDen)
  Else
    UnitsNameDep = "Fraction of Applied"
  End If
End Function

Public Function UnitsNameDepNum(utype)
'Returns the units name of the Numerator portion of
'deposition units
  Dim s As String
  Select Case utype
  Case 0: s = "drops"
  Case 1: s = "ozf"
  Case 2: s = "gal"
  Case 3: s = "lbm"
  Case 4: s = "l"
  Case 5: s = "g"
  Case 6: s = "kg"
  Case 7: s = "mg"
  Case 8: s = "µg"
  Case 9: s = "ng"
  Case Else: s = ""
  End Select
  UnitsNameDepNum = s
End Function

Public Function UnitsNameDepDen(utype)
'Returns the units name of the Denominator portion of
'deposition units
  Dim s As String
  Select Case utype
  Case 0: s = "in²"
  Case 1: s = "ft²"
  Case 2: s = "ac"
  Case 3: s = "cm²"
  Case 4: s = "m²"
  Case 5: s = "ha"
  Case Else: s = ""
  End Select
  UnitsNameDepDen = s
End Function

Public Function UnitsSystem() As Integer
'Return the currently selected units system
'returns UN_IMPERIAL or UN_METRIC
  UnitsSystem = UNSystem
End Function

