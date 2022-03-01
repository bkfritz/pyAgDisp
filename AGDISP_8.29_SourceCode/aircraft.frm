VERSION 5.00
Begin VB.Form frmAircraft 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Aircraft"
   ClientHeight    =   4260
   ClientLeft      =   1500
   ClientTop       =   1935
   ClientWidth     =   8760
   ForeColor       =   &H80000008&
   HelpContextID   =   1016
   Icon            =   "AIRCRAFT.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4260
   ScaleWidth      =   8760
   Begin VB.Frame fraDropDist 
      Caption         =   "Properties"
      ForeColor       =   &H80000008&
      Height          =   3615
      Left            =   2760
      TabIndex        =   25
      Top             =   120
      Width           =   5895
      Begin VB.TextBox txtWingVert 
         DataField       =   "EngVert"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1479
         Left            =   4440
         TabIndex        =   21
         Text            =   "WingVert"
         Top             =   2520
         Width           =   975
      End
      Begin VB.TextBox txtEngHoriz 
         DataField       =   "EngVert"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1478
         Index           =   1
         Left            =   4440
         TabIndex        =   20
         Text            =   "EngHoriz"
         Top             =   2160
         Width           =   975
      End
      Begin VB.TextBox txtEngHoriz 
         DataField       =   "EngVert"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1478
         Index           =   0
         Left            =   4440
         TabIndex        =   19
         Text            =   "EngHoriz"
         Top             =   1800
         Width           =   975
      End
      Begin VB.TextBox txtNumEng 
         DataField       =   "EngVert"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1501
         Left            =   4440
         TabIndex        =   16
         Text            =   "NumEng"
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox txtBoomVert 
         DataField       =   "TypSpeed"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1062
         Left            =   4440
         TabIndex        =   22
         Text            =   "BoomVert"
         Top             =   2880
         Width           =   975
      End
      Begin VB.TextBox txtBoomFwd 
         DataField       =   "TypSpeed"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1059
         Left            =   4440
         TabIndex        =   23
         Text            =   "BoomFwd"
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox txtName 
         DataField       =   "SemiSpan"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1016
         Left            =   720
         TabIndex        =   7
         Text            =   "txtName"
         Top             =   360
         Width           =   5055
      End
      Begin VB.ComboBox cboWingType 
         Height          =   315
         HelpContextID   =   1030
         Left            =   1320
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   720
         Width           =   1455
      End
      Begin VB.TextBox txtSemiSpan 
         DataField       =   "SemiSpan"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1026
         Left            =   1440
         TabIndex        =   9
         Text            =   "SemiSpan"
         Top             =   1080
         Width           =   975
      End
      Begin VB.TextBox txtTypSpeed 
         DataField       =   "TypSpeed"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1409
         Left            =   1440
         TabIndex        =   11
         Text            =   "TypSpeed"
         Top             =   1800
         Width           =   975
      End
      Begin VB.TextBox txtBiplSep 
         DataField       =   "BiplSep"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1055
         Left            =   1440
         TabIndex        =   14
         Text            =   "BiplSep"
         Top             =   2880
         Width           =   975
      End
      Begin VB.TextBox txtWeight 
         DataField       =   "Weight"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1032
         Left            =   1440
         TabIndex        =   10
         Text            =   "Weight"
         Top             =   1440
         Width           =   975
      End
      Begin VB.TextBox txtPlanArea 
         DataField       =   "PlanArea"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1025
         Left            =   1440
         TabIndex        =   15
         Text            =   "PlanArea"
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox txtPropRPM 
         DataField       =   "PropRPM"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1398
         Left            =   1440
         TabIndex        =   12
         Text            =   "PropRPM"
         Top             =   2160
         Width           =   975
      End
      Begin VB.TextBox txtPropRad 
         DataField       =   "PropRad"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1397
         Left            =   1440
         TabIndex        =   13
         Text            =   "PropRad"
         Top             =   2520
         Width           =   975
      End
      Begin VB.TextBox txtEngVert 
         DataField       =   "EngVert"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1369
         Left            =   4440
         TabIndex        =   17
         Text            =   "EngVert"
         Top             =   1080
         Width           =   975
      End
      Begin VB.TextBox txtEngFwd 
         DataField       =   "EngFwd"
         DataSource      =   "Data1"
         Height          =   285
         HelpContextID   =   1368
         Left            =   4440
         TabIndex        =   18
         Text            =   "EngFwd"
         Top             =   1440
         Width           =   975
      End
      Begin VB.Label lblWingVert 
         AutoSize        =   -1  'True
         Caption         =   "Wing Vert.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   53
         Top             =   2520
         Width           =   795
      End
      Begin VB.Label lblWingVertUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5520
         TabIndex        =   52
         Top             =   2520
         Width           =   420
      End
      Begin VB.Label lblEngHoriz 
         AutoSize        =   -1  'True
         Caption         =   "Engine Horiz.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   51
         Top             =   2040
         Width           =   990
      End
      Begin VB.Label lblEngHorizUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5520
         TabIndex        =   50
         Top             =   2040
         Width           =   420
      End
      Begin VB.Label lblNumEng 
         AutoSize        =   -1  'True
         Caption         =   "Engines:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   49
         Top             =   720
         Width           =   615
      End
      Begin VB.Label lblBoomVertUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5520
         TabIndex        =   48
         Top             =   2880
         Width           =   330
      End
      Begin VB.Label lblBoomVert 
         AutoSize        =   -1  'True
         Caption         =   "Boom Vert.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   47
         Top             =   2880
         Width           =   825
      End
      Begin VB.Label lblBoomFwdUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5520
         TabIndex        =   46
         Top             =   3240
         Width           =   330
      End
      Begin VB.Label lblBoomFwd 
         AutoSize        =   -1  'True
         Caption         =   "Boom Fwd.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   45
         Top             =   3240
         Width           =   840
      End
      Begin VB.Label lblName 
         AutoSize        =   -1  'True
         Caption         =   "Name:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   44
         Top             =   360
         Width           =   465
      End
      Begin VB.Label lblEngFwdUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5520
         TabIndex        =   43
         Top             =   1440
         Width           =   420
      End
      Begin VB.Label lblEngVertUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5520
         TabIndex        =   42
         Top             =   1080
         Width           =   420
      End
      Begin VB.Label lblPlanAreaUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   41
         Top             =   3240
         Width           =   420
      End
      Begin VB.Label lblBiplSepUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   40
         Top             =   2880
         Width           =   420
      End
      Begin VB.Label lblPropRadUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   39
         Top             =   2520
         Width           =   420
      End
      Begin VB.Label lblTypSpeedUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   38
         Top             =   1800
         Width           =   420
      End
      Begin VB.Label lblWeightUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   37
         Top             =   1440
         Width           =   420
      End
      Begin VB.Label lblSemiSpanUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   36
         Top             =   1080
         Width           =   420
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Type:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   26
         Top             =   720
         Width           =   405
      End
      Begin VB.Label lblSemiSpan 
         AutoSize        =   -1  'True
         Caption         =   "lblSemiSpan"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   27
         Top             =   1080
         Width           =   870
      End
      Begin VB.Label lblTypSpeed 
         AutoSize        =   -1  'True
         Caption         =   "Typ. Speed:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   28
         Top             =   1800
         Width           =   870
      End
      Begin VB.Label lblBiplSep 
         AutoSize        =   -1  'True
         Caption         =   "Biplane Sep.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   29
         Top             =   2880
         Width           =   945
      End
      Begin VB.Label lblWeight 
         AutoSize        =   -1  'True
         Caption         =   "Weight:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   30
         Top             =   1440
         Width           =   555
      End
      Begin VB.Label lblPlanArea 
         AutoSize        =   -1  'True
         Caption         =   "Planform Area:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   32
         Top             =   3240
         Width           =   1035
      End
      Begin VB.Label lblPropRPM 
         AutoSize        =   -1  'True
         Caption         =   "lblPropRPM"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   31
         Top             =   2160
         Width           =   1005
      End
      Begin VB.Label lblPropRad 
         AutoSize        =   -1  'True
         Caption         =   "Prop. Radius:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   33
         Top             =   2520
         Width           =   960
      End
      Begin VB.Label lblEngVert 
         AutoSize        =   -1  'True
         Caption         =   "Engine Vert.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   34
         Top             =   1080
         Width           =   915
      End
      Begin VB.Label lblEngFwd 
         AutoSize        =   -1  'True
         Caption         =   "Engine Fwd.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3120
         TabIndex        =   35
         Top             =   1440
         Width           =   930
      End
   End
   Begin VB.Frame fraACType 
      Caption         =   "Aircraft Type"
      ForeColor       =   &H80000008&
      Height          =   3615
      Left            =   120
      TabIndex        =   24
      Top             =   120
      Width           =   2535
      Begin VB.CommandButton cmdLibrary 
         Caption         =   "Select"
         Height          =   255
         HelpContextID   =   1020
         Index           =   2
         Left            =   1080
         TabIndex        =   6
         Top             =   2160
         Width           =   735
      End
      Begin VB.Frame fraUserLib 
         Caption         =   "User Library"
         ForeColor       =   &H80000008&
         Height          =   1215
         Left            =   360
         TabIndex        =   54
         Top             =   720
         Width           =   1935
         Begin VB.CommandButton cmdUserLibSelect 
            Caption         =   "Select From/Modify"
            Height          =   375
            HelpContextID   =   1016
            Left            =   120
            TabIndex        =   4
            Top             =   720
            Width           =   1695
         End
         Begin VB.CommandButton cmdUserLibAdd 
            Caption         =   "Add Current"
            Height          =   375
            HelpContextID   =   1016
            Left            =   120
            TabIndex        =   3
            Top             =   240
            Width           =   1695
         End
      End
      Begin VB.OptionButton optType 
         Caption         =   "&Library"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1020
         Index           =   2
         Left            =   120
         TabIndex        =   5
         Top             =   2160
         Width           =   855
      End
      Begin VB.OptionButton optType 
         Caption         =   "&User-defined"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1462
         Index           =   1
         Left            =   120
         TabIndex        =   2
         Top             =   360
         Width           =   2295
      End
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   375
      HelpContextID   =   1016
      Left            =   7800
      TabIndex        =   1
      Top             =   3840
      Width           =   855
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Default         =   -1  'True
      Height          =   375
      HelpContextID   =   1016
      Left            =   6840
      TabIndex        =   0
      Top             =   3840
      Width           =   855
   End
End
Attribute VB_Name = "frmAircraft"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' $Id: aircraft.frm,v 1.8 2016/09/13 13:19:58 tom Exp $

'Local working copy of aircraft data
Private AC As AircraftData

'this flag is used to tell the option buttons not to
'take action on their new values. This is required
'to differentiate between programatic state changes
'and user actions
Dim OptTakeAction As Integer  'if true, execute automatic change-related code
                              'for option button
Dim PropTakeAction As Integer 'if true, execute automatic change-related code
                              'for Property text boxes

Dim SaveType As Integer       'place to save Aircraft type

Private Sub DataToForm()
'transfer local data to form controls for editing
  
  'Options
  temp = OptTakeAction                      'save flag value
  OptTakeAction = False                     'disable actions for the following
  optType(AC.Type) = True                   'dist type radio buttons
  UpdateTypeControls
  OptTakeAction = temp                      'restore flag value
  
  'Properties
  temp = PropTakeAction                     'save flag value
  PropTakeAction = False                    'allow raw field modification
  txtName.Text = RTrim$(AC.Name)
  cboWingType.ListIndex = AC.WingType - 3
  txtSemiSpan.Text = AGFormat$(UnitsDisplay(AC.SemiSpan, UN_LENGTH))
  lblSemiSpanUnits.Caption = UnitsName(UN_LENGTH)
  txtTypSpeed.Text = AGFormat$(UnitsDisplay(AC.TypSpeed, UN_SPEED))
  lblTypSpeedUnits.Caption = UnitsName(UN_SPEED)
  txtBiplSep.Text = AGFormat$(UnitsDisplay(AC.BiplSep, UN_LENGTH))
  lblBiplSepUnits.Caption = UnitsName(UN_LENGTH)
  txtWeight.Text = AGFormat$(UnitsDisplay(AC.Weight, UN_MASS))
  lblWeightUnits.Caption = UnitsName(UN_MASS)
  txtPlanArea.Text = AGFormat$(UnitsDisplay(AC.PlanArea, UN_AREA))
  lblPlanAreaUnits.Caption = UnitsName(UN_AREA)
  txtPropRPM.Text = AGFormat$(AC.PropRPM)
  txtPropRad.Text = AGFormat$(UnitsDisplay(AC.PropRad, UN_LENGTH))
  lblPropRadUnits.Caption = UnitsName(UN_LENGTH)
  txtEngVert.Text = AGFormat$(UnitsDisplay(AC.EngVert, UN_LENGTH))
  lblEngVertUnits.Caption = UnitsName(UN_LENGTH)
  txtEngFwd.Text = AGFormat$(UnitsDisplay(AC.EngFwd, UN_LENGTH))
  lblEngFwdUnits.Caption = UnitsName(UN_LENGTH)
  txtNumEng.Text = AGFormat$(AC.NumEng)
  txtEngHoriz(0).Text = AGFormat$(UnitsDisplay(AC.EngHoriz(0), UN_LENGTH))
  txtEngHoriz(1).Text = AGFormat$(UnitsDisplay(AC.EngHoriz(1), UN_LENGTH))
  lblEngHorizUnits.Caption = UnitsName(UN_LENGTH)
  txtWingVert.Text = AGFormat$(UnitsDisplay(AC.WingVert, UN_LENGTH))
  lblWingVertUnits.Caption = UnitsName(UN_LENGTH)
  txtBoomVert.Text = AGFormat$(UnitsDisplay(AC.BoomVert, UN_LENGTH))
  lblBoomVertUnits.Caption = UnitsName(UN_LENGTH)
  txtBoomFwd.Text = AGFormat$(UnitsDisplay(AC.BoomFwd, UN_LENGTH))
  lblBoomFwdUnits.Caption = UnitsName(UN_LENGTH)
  UpdatePropertyControls
  PropTakeAction = temp                     'restore flag value

End Sub

Private Sub FormToData()
'Transfer the form data to local data storage
  
  Dim nlong As Long
  Dim c As Control

  'find the current type selection
  For Each c In optType()
    If c.Value Then AC.Type = c.Index: Exit For
  Next

  'get Basic selection, even if the type isn't Basic
'  AC.BasicType = cboBasicType.ListIndex  'Basic selection
  AC.Name = RTrim$(txtName.Text)
  AC.LName = Len(RTrim$(txtName.Text))
  AC.WingType = cboWingType.ListIndex + 3
  AC.SemiSpan = UnitsInternal(Val(txtSemiSpan.Text), UN_LENGTH)
  AC.TypSpeed = UnitsInternal(Val(txtTypSpeed.Text), UN_SPEED)
  AC.BiplSep = UnitsInternal(Val(txtBiplSep.Text), UN_LENGTH)
  AC.Weight = UnitsInternal(Val(txtWeight.Text), UN_MASS)
  AC.PlanArea = UnitsInternal(Val(txtPlanArea.Text), UN_AREA)
  AC.PropRPM = Val(txtPropRPM.Text)
  AC.PropRad = UnitsInternal(Val(txtPropRad.Text), UN_LENGTH)
  AC.EngVert = UnitsInternal(Val(txtEngVert.Text), UN_LENGTH)
  AC.EngFwd = UnitsInternal(Val(txtEngFwd.Text), UN_LENGTH)
  AC.NumEng = Val(txtNumEng.Text)
  AC.EngHoriz(0) = UnitsInternal(Val(txtEngHoriz(0).Text), UN_LENGTH)
  AC.EngHoriz(1) = UnitsInternal(Val(txtEngHoriz(1).Text), UN_LENGTH)
  AC.WingVert = UnitsInternal(Val(txtWingVert.Text), UN_LENGTH)
  AC.BoomVert = UnitsInternal(Val(txtBoomVert.Text), UN_LENGTH)
  AC.BoomFwd = UnitsInternal(Val(txtBoomFwd.Text), UN_LENGTH)
End Sub

Private Sub UpdatePropertyControls()
'update the state of the property controls
  'set the data labels and text boxes according
  'to the aircraft type
  Select Case cboWingType.ListIndex
    Case 0 'fixed-wing
      lblSemiSpan.Caption = "Semispan:"
      txtSemiSpan.HelpContextID = 1026
      lblPropRPM.Caption = "Propeller RPM:"
      txtPropRPM.HelpContextID = 1398
      lblPropRad.Visible = True
      txtPropRad.Visible = True
      lblPropRadUnits.Visible = True
      lblBiplSep.Visible = True
      txtBiplSep.Visible = True
      lblBiplSepUnits.Visible = True
      lblPlanArea.Visible = True
      txtPlanArea.Visible = True
      lblPlanAreaUnits.Visible = True
      lblEngVert.Visible = True
      txtEngVert.Visible = True
      lblEngVertUnits.Visible = True
      lblEngFwd.Visible = True
      txtEngFwd.Visible = True
      lblEngFwdUnits.Visible = True
      lblNumEng.Visible = True
      txtNumEng.Visible = True
      lblEngHoriz.Visible = True
      txtEngHoriz(0).Visible = True
      txtEngHoriz(1).Visible = True
      lblEngHorizUnits.Visible = True
      lblWingVert.Visible = True
      txtWingVert.Visible = True
      lblWingVertUnits.Visible = True
    Case 1 'helicopter
      lblSemiSpan.Caption = "Rotor Radius:"
      txtSemiSpan.HelpContextID = 1381
      lblPropRPM.Caption = "Rotor RPM:"
      txtPropRPM.HelpContextID = 1382
      lblPropRad.Visible = False
      txtPropRad.Visible = False
      lblPropRadUnits.Visible = False
      lblBiplSep.Visible = False
      txtBiplSep.Visible = False
      lblBiplSepUnits.Visible = False
      lblPlanArea.Visible = False
      txtPlanArea.Visible = False
      lblPlanAreaUnits.Visible = False
      lblEngVert.Visible = False
      txtEngVert.Visible = False
      lblEngVertUnits.Visible = False
      lblEngFwd.Visible = False
      txtEngFwd.Visible = False
      lblEngFwdUnits.Visible = False
      lblNumEng.Visible = False
      txtNumEng.Visible = False
      lblEngHoriz.Visible = False
      txtEngHoriz(0).Visible = False
      txtEngHoriz(1).Visible = False
      lblEngHorizUnits.Visible = False
      lblWingVert.Visible = False
      txtWingVert.Visible = False
      lblWingVertUnits.Visible = False
  End Select
End Sub

Private Sub UpdateTypeControls()
'Adjust the Type Controls to conform to the current
'setting of the Type Option buttons
  Dim i As Integer
  Dim c As Control
  Dim userctls As New Collection
  Dim libctls As New Collection

  'populate control collections
  userctls.Add fraUserLib
  userctls.Add cmdUserLibAdd
  userctls.Add cmdUserLibSelect
  
  libctls.Add cmdLibrary(2)
  
  'find the current type selection
  For Each c In optType()
    If c.Value = True Then DT = c.Index
  Next
  'save the current type selection
  SaveType = DT

  'set related controls to a known state
  For Each c In userctls
    c.Enabled = ((DT = 1) And (UP.UserLib <> "")) '1=user-def
  Next
  For Each c In libctls
    c.Enabled = (DT = 2) '2-library
  Next

End Sub

Private Sub ChangeType(NewType As Integer)
'Select a new Material Type and do what is necessary to
'get new data
  Me.MousePointer = vbHourglass 'change pointer to hourglass
  Select Case NewType
    Case 1 'user-def
      'nothing to do here, except a form cleanup on exit
    Case 2 'library
      'turn off Property actions so that the lib form can
      'modify this form's controls without triggering other actions
      temp = PropTakeAction  'save flag state
      PropTakeAction = False 'disable actions on change
      Load frmAircraftLib
      frmAircraftLib.SelectTable "Aircraft"
      frmAircraftLib.Show vbModal  'get the properties from lib
      PropTakeAction = temp  'restore flag value
      If frmAircraftLib.Tag = "False" Then 'Tag holds status info
        'reset original dist type
        temp = OptTakeAction  'save flag state
        OptTakeAction = False 'disable actions on change
        optType(SaveType).Value = True  'reset option button
        OptTakeAction = temp  'restore flag value
      End If
      Unload frmAircraftLib
  End Select
  'adjust the Type controls to reflect the current type
  UpdateTypeControls
  UpdatePropertyControls
  Me.MousePointer = vbDefault 'change pointer back to default
End Sub

Private Sub cboWingType_Click()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
  UpdatePropertyControls
End Sub

Private Sub cmdCancel_Click()
  Unload Me
End Sub

Private Sub cmdLibrary_Click(Index As Integer)
  ChangeType Index
End Sub

Private Sub cmdOK_Click()
'calculate and display Nozzle Distribution Extent
  Dim AdjustNozzles As Boolean
  Dim PosHoriz As Single
  Dim sNozzles As String
  Dim sExtent As String
  Dim sSpacing As String
  Dim i As Integer
  Dim s As String
  
  AdjustNozzles = False 'default
  
  'Ask about adjusting nozzles
  sExtent = AGFormat$(65)
  sNozzles = ""
  sSpacing = AGFormat$(UnitsDisplay(0.237, UN_LENGTH)) '9 in
  
  s = "You are about to change aircraft. " + _
      "AGDISP can automatically adjust the nozzle " + _
      "locations to be consistent with a boom length " + _
      "of " + sExtent + "% of the "
  If (cboWingType.ListIndex + 3) = 3 Then  'Fixed
    s = s + "wingspan"
  Else                                     'Helicopter
    s = s + "rotor diameter"
  End If
  s = s + " and a spacing of " + sSpacing + " " + UnitsName(UN_LENGTH) + _
          ". (Further adjustment is available under " + _
          Chr$(34) + "Nozzles" + Chr$(34) + ".) " + _
          "Make this change?"
  Select Case MsgBox(s, vbYesNoCancel + vbQuestion)
  Case vbYes
    AdjustNozzles = True
  Case vbNo
    AdjustNozzles = False
  Case vbCancel
    Exit Sub
  End Select
  
  FormToData   'Transfer form data to local data
  UD.AC = AC   'Transfer local data to userdata
  
  'Adjust the nozzles
  If AdjustNozzles Then
    If Not GenRegNozDist(UD.NZ, UD.AC.SemiSpan, sNozzles, sExtent, sSpacing) Then
      MsgBox "Nozzles were not adjusted.", vbCritical
    End If
  End If
  
  UpdateDataChangedFlag True 'Data was changed
  UC.Valid = False           'Calcs need to be redone
  
  Unload Me
End Sub

Private Sub cmdUserLibAdd_Click()
'Add the aircraft currently described by this form to the user library
  FormToData 'Update the local aircraft data with that of the form controls
  If UserLibAddAircraftRecord(AC) Then
    If UserLibSelectAircraft(AC) Then
      DataToForm
    End If
  End If
End Sub

Private Sub cmdUserLibSelect_Click()
  Dim DB As Database
  If UserLibOpen(DB) Then 'test the user library before opening the form
    DB.Close
    If UserLibSelectAircraft(AC) Then
      DataToForm
    End If
  End If
End Sub

Private Sub Form_Load()
  'Initialize the controls on this form

  'center the form
  CenterForm Me

  'init the Aircraft WingType combo box
  cboWingType.Clear
  cboWingType.AddItem "Fixed-wing"
  cboWingType.AddItem "Helicopter"

  'allow option button changes to take action
  '(see declarations section)
  OptTakeAction = True
  PropTakeAction = True

  'Save a local copy of the current data
  AC = UD.AC
  
  'fill the controls with local data
  DataToForm
End Sub

Private Sub optType_Click(Index As Integer)
  If OptTakeAction Then ChangeType Index
End Sub

Private Sub optType_DblClick(Index As Integer)
'same as click
  optType_Click Index
End Sub

Private Sub txtBiplSep_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtBoomFwd_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtBoomVert_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtEngFwd_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtEngVert_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtName_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtPlanArea_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtPropRad_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtPropRPM_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtSemiSpan_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtTypSpeed_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

Private Sub txtWeight_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    If Not optType(1).Value Then optType(1).Value = True
  End If
End Sub

