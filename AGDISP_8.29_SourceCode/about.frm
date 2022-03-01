VERSION 5.00
Begin VB.Form frmAbout 
   AutoRedraw      =   -1  'True
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "About AGDISP"
   ClientHeight    =   5400
   ClientLeft      =   2985
   ClientTop       =   1680
   ClientWidth     =   7245
   ForeColor       =   &H80000008&
   HelpContextID   =   1005
   Icon            =   "ABOUT.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5400
   ScaleWidth      =   7245
   Begin VB.CommandButton cmdGOI 
      Caption         =   "General Operating Instructions"
      Height          =   375
      HelpContextID   =   1558
      Left            =   2520
      TabIndex        =   1
      Top             =   4080
      Width           =   2535
   End
   Begin VB.PictureBox picLogo 
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   2280
      ScaleHeight     =   615
      ScaleWidth      =   2775
      TabIndex        =   5
      Top             =   240
      Width           =   2775
      Begin VB.Label lblLogo 
         Caption         =   "AGDISP"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   24
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000008&
         Height          =   540
         Left            =   600
         TabIndex        =   6
         Top             =   0
         Width           =   2025
      End
      Begin VB.Image Image1 
         Height          =   525
         Left            =   0
         Picture         =   "ABOUT.frx":0CCA
         Stretch         =   -1  'True
         Top             =   0
         Width           =   555
      End
   End
   Begin VB.CheckBox cbxShowAtStartup 
      Caption         =   "&Display this form at Startup"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   5040
      Width           =   2295
   End
   Begin VB.CommandButton cmdAboutOK 
      Cancel          =   -1  'True
      Caption         =   "&OK"
      Height          =   375
      HelpContextID   =   1005
      Left            =   3360
      TabIndex        =   0
      Top             =   4920
      Width           =   855
   End
   Begin VB.Label lblAbout 
      Caption         =   $"ABOUT.frx":1994
      ForeColor       =   &H80000008&
      Height          =   855
      Index           =   1
      Left            =   120
      TabIndex        =   8
      Top             =   2880
      Width           =   6975
   End
   Begin VB.Label lblAbout 
      Caption         =   $"ABOUT.frx":1A37
      ForeColor       =   &H80000008&
      Height          =   1095
      Index           =   0
      Left            =   120
      TabIndex        =   7
      Top             =   1680
      Width           =   6975
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "USDA Forest Service Spray Modeling Software"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   1770
      TabIndex        =   3
      Top             =   960
      Width           =   3345
   End
   Begin VB.Label lblVersion 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "lblVersion"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   720
      TabIndex        =   2
      Top             =   1320
      Width           =   5415
   End
End
Attribute VB_Name = "frmAbout"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' $Id: about.frm,v 1.10 2016/12/05 15:36:27 tom Exp $

Private Sub cmdAboutOK_Click()
  Unload Me
End Sub

Private Sub cmdGOI_Click()
  'Display the help topic for gen op instr in a pop-up window
  HtmlHelp Me.hWnd, App.HelpFile & Chr$(0), HH_HELP_CONTEXT, ByVal CLng(cmdGOI.HelpContextID)
End Sub

Private Sub Form_Click()
  Unload Me
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
  'Exit when (almost) any key is released
  '
  'We do it this way (rather than KeyDown) so that the user
  'may capture the about screen with Alt-PrtSc. The only
  'side effect of this approach is that if the program is started
  'by pressing a key, e.g. F5 in the development environment,
  'this form appears only fleetingly.
  Select Case KeyCode
  Case vbKeyF1, vbKeyF5 'These keys do not dismiss the form
  Case Else
    Unload Me
  End Select
End Sub

Private Sub Form_Load()
  'Center the form on the screen
  CenterForm Me
  'display the version string
  lblVersion.Caption = "Version " & Format$(AGDISPVERSION, "0.00")
  'update the pref checkbox
  cbxShowAtStartup.Value = Abs(UP.ShowAboutOnStartup)
End Sub

Private Sub Form_Unload(Cancel As Integer)
'See if Prefs need saving
  If CBool(cbxShowAtStartup.Value) <> CBool(UP.ShowAboutOnStartup) Then
    UP.ShowAboutOnStartup = CBool(cbxShowAtStartup.Value)
    WriteGeneralPrefs
  End If
End Sub

Private Sub Label1_Click()
  Unload Me
End Sub

Private Sub Label2_Click()
  Unload Me
End Sub

Private Sub Label3_Click()
  Unload Me
End Sub

Private Sub Label4_Click()
  Unload Me
End Sub

Private Sub Label5_Click()
  Unload Me
End Sub

Private Sub lblLogo_Click()
  Unload Me
End Sub

Private Sub lblVersion_Click()
  Unload Me
End Sub

Private Sub picLogo_Click()
  Unload Me
End Sub
