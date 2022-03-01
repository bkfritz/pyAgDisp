VERSION 5.00
Begin VB.Form frmAdvanced 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Advanced Settings"
   ClientHeight    =   7785
   ClientLeft      =   2055
   ClientTop       =   2475
   ClientWidth     =   5640
   HelpContextID   =   1013
   Icon            =   "ADVANCED.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   7785
   ScaleWidth      =   5640
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   375
      HelpContextID   =   1013
      Left            =   4560
      TabIndex        =   1
      Top             =   7320
      Width           =   975
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Default         =   -1  'True
      Height          =   375
      HelpContextID   =   1013
      Left            =   3480
      TabIndex        =   0
      Top             =   7320
      Width           =   975
   End
   Begin VB.Frame fraAdvanced 
      Caption         =   "Advanced Settings"
      ForeColor       =   &H80000008&
      Height          =   6615
      Left            =   120
      TabIndex        =   16
      Top             =   600
      Width           =   5415
      Begin VB.CheckBox chkSaveCALPUFF 
         Height          =   255
         HelpContextID   =   1583
         Left            =   3000
         TabIndex        =   43
         Top             =   4080
         Width           =   375
      End
      Begin VB.TextBox txtEvapRate 
         Height          =   285
         HelpContextID   =   1133
         Left            =   3000
         TabIndex        =   38
         Top             =   6120
         Width           =   1095
      End
      Begin VB.TextBox txtSGtank 
         Height          =   285
         HelpContextID   =   1255
         Left            =   3000
         TabIndex        =   37
         Top             =   5400
         Width           =   1095
      End
      Begin VB.TextBox txtSGnonv 
         Height          =   285
         HelpContextID   =   1255
         Left            =   3000
         TabIndex        =   36
         Top             =   5760
         Width           =   1095
      End
      Begin VB.OptionButton optSwathOffset 
         Caption         =   "0 Swath"
         Height          =   255
         HelpContextID   =   1081
         Index           =   1
         Left            =   3000
         TabIndex        =   14
         Top             =   5040
         Width           =   1215
      End
      Begin VB.OptionButton optSwathOffset 
         Caption         =   "1/2 Swath"
         Height          =   255
         HelpContextID   =   1081
         Index           =   0
         Left            =   3000
         TabIndex        =   13
         Top             =   4800
         Width           =   1215
      End
      Begin VB.CheckBox chkHalfBoom 
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1481
         Left            =   3000
         TabIndex        =   12
         Top             =   4440
         Width           =   375
      End
      Begin VB.CheckBox chkSaveTraj 
         Height          =   255
         HelpContextID   =   1253
         Left            =   3000
         TabIndex        =   11
         Top             =   3720
         Width           =   375
      End
      Begin VB.TextBox txtVortexDecayIGE 
         Height          =   285
         HelpContextID   =   1326
         Left            =   3000
         TabIndex        =   6
         Top             =   1920
         Width           =   1095
      End
      Begin VB.TextBox txtZref 
         Height          =   285
         HelpContextID   =   1503
         Left            =   3000
         TabIndex        =   10
         Top             =   3360
         Width           =   1095
      End
      Begin VB.TextBox txtMaxDownwindDist 
         Height          =   285
         HelpContextID   =   1435
         Left            =   3000
         TabIndex        =   4
         Top             =   1200
         Width           =   1095
      End
      Begin VB.TextBox txtWindHeight 
         Height          =   285
         HelpContextID   =   1340
         Left            =   3000
         TabIndex        =   2
         Top             =   480
         Width           =   1095
      End
      Begin VB.TextBox txtMaxComputeTime 
         Height          =   285
         HelpContextID   =   1177
         Left            =   3000
         TabIndex        =   3
         Top             =   840
         Width           =   1095
      End
      Begin VB.TextBox txtVortexDecayOGE 
         Height          =   285
         HelpContextID   =   1326
         Left            =   3000
         TabIndex        =   5
         Top             =   1560
         Width           =   1095
      End
      Begin VB.TextBox txtDragCoeff 
         Height          =   285
         HelpContextID   =   1017
         Left            =   3000
         TabIndex        =   7
         Top             =   2280
         Width           =   1095
      End
      Begin VB.TextBox txtPropEff 
         Height          =   285
         HelpContextID   =   1229
         Left            =   3000
         TabIndex        =   8
         Top             =   2640
         Width           =   1095
      End
      Begin VB.TextBox txtPressure 
         Height          =   285
         HelpContextID   =   1035
         Left            =   3000
         TabIndex        =   9
         Top             =   3000
         Width           =   1095
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Save CALPUFF Data:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   44
         Top             =   4080
         Width           =   2775
      End
      Begin VB.Label lblEvapRate 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Evaporation Rate:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   1350
         TabIndex        =   42
         Top             =   6165
         Width           =   1560
      End
      Begin VB.Label lblEvapRateUnits 
         AutoSize        =   -1  'True
         Caption         =   "µm²/deg C/sec"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   41
         Top             =   6180
         Width           =   1170
      End
      Begin VB.Label lblSGtank 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Specific Gravity (Carrier):"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   1170
         TabIndex        =   40
         Top             =   5445
         Width           =   1740
      End
      Begin VB.Label lblSGnonv 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Specific Gravity (Active and Additive):"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   255
         TabIndex        =   39
         Top             =   5805
         Width           =   2670
      End
      Begin VB.Label lblSwathOffset 
         Alignment       =   1  'Right Justify
         Caption         =   "Default Swath Offset:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   35
         Top             =   4920
         Width           =   2775
      End
      Begin VB.Label lblHalfBoom 
         Alignment       =   1  'Right Justify
         Caption         =   "Half Boom Effect:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   34
         Top             =   4440
         Width           =   2775
      End
      Begin VB.Label Label10 
         Alignment       =   1  'Right Justify
         Caption         =   "Save Trajectory Files:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   33
         Top             =   3720
         Width           =   2775
      End
      Begin VB.Label lblVortexDecayIGEUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   32
         Top             =   1920
         Width           =   330
      End
      Begin VB.Label Label8 
         Alignment       =   1  'Right Justify
         Caption         =   "Vortex Decay Rate IGE:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   31
         Top             =   1920
         Width           =   2775
      End
      Begin VB.Label Label12 
         Alignment       =   1  'Right Justify
         Caption         =   "Ground Reference:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   30
         Top             =   3360
         Width           =   2775
      End
      Begin VB.Label lblZrefUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   29
         Top             =   3360
         Width           =   330
      End
      Begin VB.Label lblMaxDownwindDistUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   28
         Top             =   1200
         Width           =   330
      End
      Begin VB.Label Label7 
         Alignment       =   1  'Right Justify
         Caption         =   "Maximum Downwind Distance:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   27
         Top             =   1200
         Width           =   2775
      End
      Begin VB.Label lblWindHeight 
         Alignment       =   1  'Right Justify
         Caption         =   "Height for Wind Speed Measurement:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   26
         Top             =   480
         Width           =   2775
      End
      Begin VB.Label Label3 
         Alignment       =   1  'Right Justify
         Caption         =   "Maximum Computational Time:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   25
         Top             =   840
         Width           =   2775
      End
      Begin VB.Label Label4 
         Alignment       =   1  'Right Justify
         Caption         =   "Vortex Decay Rate OGE:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   24
         Top             =   1560
         Width           =   2775
      End
      Begin VB.Label Label5 
         Alignment       =   1  'Right Justify
         Caption         =   "Aircraft Drag Coefficient:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   23
         Top             =   2280
         Width           =   2775
      End
      Begin VB.Label Label6 
         Alignment       =   1  'Right Justify
         Caption         =   "Propeller Efficiency:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   22
         Top             =   2640
         Width           =   2775
      End
      Begin VB.Label Label9 
         Alignment       =   1  'Right Justify
         Caption         =   "Ambient Pressure:"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   120
         TabIndex        =   21
         Top             =   3000
         Width           =   2775
      End
      Begin VB.Label lblWindHeightUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   20
         Top             =   480
         Width           =   330
      End
      Begin VB.Label Label13 
         Caption         =   "sec"
         ForeColor       =   &H80000008&
         Height          =   285
         Left            =   4200
         TabIndex        =   19
         Top             =   840
         Width           =   375
      End
      Begin VB.Label lblVortexDecayOGEUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   18
         Top             =   1560
         Width           =   330
      End
      Begin VB.Label lblPressureUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   4200
         TabIndex        =   17
         Top             =   3000
         Width           =   330
      End
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Caption         =   "Knowledge of these parameters is essential before changing any of them."
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   720
      TabIndex        =   15
      Top             =   120
      Width           =   3495
   End
End
Attribute VB_Name = "frmAdvanced"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' $Id: advanced.frm,v 1.11 2016/09/13 13:19:57 tom Exp $

Option Explicit

Private Sub DataToForm()
  If UD.MET.WindType = WIND_TYPE_SINGLE Then
    txtWindHeight.Text = AGFormat$(UnitsDisplay(UD.MET.WH, UN_LENGTH))
  Else
    txtWindHeight.Text = ""
  End If
  txtMaxComputeTime.Text = AGFormat$(UD.CTL.MaxComputeTime)
  txtMaxDownwindDist.Text = AGFormat$(UnitsDisplay(UD.CTL.MaxDownwindDist, UN_LENGTH))
  txtVortexDecayOGE.Text = AGFormat$(UnitsDisplay(UD.MET.VortexDecayOGE, UN_SPEED))
  txtVortexDecayIGE.Text = AGFormat$(UnitsDisplay(UD.MET.VortexDecayIGE, UN_SPEED))
  txtDragCoeff.Text = AGFormat$(UD.AC.DragCoeff)
  txtPropEff.Text = AGFormat$(UD.AC.PropEff)
  txtPressure.Text = AGFormat$(UnitsDisplay(UD.MET.Pressure, UN_AIRPRESSURE))
  txtZref.Text = AGFormat$(UnitsDisplay(UD.TRN.Zref, UN_LENGTH))
  chkSaveTraj.Value = UD.CTL.SaveTraj
  chkSaveCALPUFF.Value = UD.CALPUFFFLAG
  chkHalfBoom.Value = UD.CTL.HalfBoom
  Select Case UD.CTL.SwathOffset
  Case 0
    optSwathOffset(0).Value = True
  Case 1
    optSwathOffset(1).Value = True
  End Select
  txtSGtank.Text = AGFormat$(UD.SM.SpecGrav)         'specific gravity (tank)
  txtSGnonv.Text = AGFormat$(UD.SM.NonVGrav)         'specific gravity (nonv)
  txtEvapRate.Text = AGFormat$(UD.SM.EvapRate)   'Evaporation rate
End Sub

Private Sub FormToData()
  UD.MET.WH = UnitsInternal(Val(txtWindHeight.Text), UN_LENGTH)
  UD.CTL.MaxComputeTime = Val(txtMaxComputeTime.Text)
  UD.CTL.MaxDownwindDist = UnitsInternal(Val(txtMaxDownwindDist.Text), UN_LENGTH)
  UD.MET.VortexDecayOGE = UnitsInternal(Val(txtVortexDecayOGE.Text), UN_SPEED)
  UD.MET.VortexDecayIGE = UnitsInternal(Val(txtVortexDecayIGE.Text), UN_SPEED)
  UD.AC.DragCoeff = Val(txtDragCoeff.Text)
  UD.AC.PropEff = Val(txtPropEff.Text)
  UD.MET.Pressure = UnitsInternal(Val(txtPressure.Text), UN_AIRPRESSURE)
  UD.TRN.Zref = UnitsInternal(Val(txtZref.Text), UN_LENGTH)
  UD.CTL.SaveTraj = chkSaveTraj.Value
  UD.CALPUFFFLAG = chkSaveCALPUFF.Value
  UD.CTL.HalfBoom = chkHalfBoom.Value
  If optSwathOffset(0).Value Then
    UD.CTL.SwathOffset = 0
  End If
  If optSwathOffset(1).Value Then
    UD.CTL.SwathOffset = 1
  End If
  UD.SM.SpecGrav = Val(txtSGtank.Text)           'specific gravity (tank)
  UD.SM.NonVGrav = Val(txtSGnonv.Text)           'specific gravity (nonv)
  UD.SM.EvapRate = Val(txtEvapRate.Text)     'Evaporation rate
  
  UpdateDataChangedFlag True 'Data was changed
  UC.Valid = False 'Calcs need to be redone
End Sub

Private Sub cmdCancel_Click()
  Unload Me
End Sub

Private Sub cmdOK_Click()
  FormToData
  Unload Me
End Sub

Private Sub Form_Load()
  CenterForm Me
  
  'units
  If UD.MET.WindType = WIND_TYPE_SINGLE Then
    lblWindHeight.Enabled = True
    txtWindHeight.Enabled = True
    lblWindHeightUnits.Enabled = True
  Else
    lblWindHeight.Enabled = False
    txtWindHeight.Enabled = False
    lblWindHeightUnits.Enabled = False
  End If
  lblWindHeightUnits = UnitsName(UN_LENGTH)
  lblMaxDownwindDistUnits = UnitsName(UN_LENGTH)
  lblVortexDecayOGEUnits = UnitsName(UN_SPEED)
  lblVortexDecayIGEUnits = UnitsName(UN_SPEED)
  lblPressureUnits = UnitsName(UN_AIRPRESSURE)
  lblZrefUnits = UnitsName(UN_LENGTH)
  
  'Controls
  lblHalfBoom.Enabled = (UD.AerialType = AD_LIQUID)
  chkHalfBoom.Enabled = (UD.AerialType = AD_LIQUID)
  
  DataToForm
End Sub

