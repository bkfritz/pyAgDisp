VERSION 5.00
Begin VB.Form frmCalcLog 
   BackColor       =   &H8000000F&
   Caption         =   "Calculation Log"
   ClientHeight    =   4920
   ClientLeft      =   360
   ClientTop       =   1695
   ClientWidth     =   6975
   ForeColor       =   &H80000008&
   HelpContextID   =   1065
   Icon            =   "CALCLOG.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4920
   ScaleWidth      =   6975
   Begin VB.CommandButton cmdSave 
      Caption         =   "&Save"
      Height          =   375
      HelpContextID   =   1065
      Left            =   4320
      TabIndex        =   3
      Top             =   4440
      Width           =   735
   End
   Begin VB.CommandButton cmdPrint 
      Caption         =   "&Print"
      Height          =   375
      HelpContextID   =   1065
      Left            =   5160
      TabIndex        =   1
      Top             =   4440
      Width           =   735
   End
   Begin VB.ListBox lstSummary 
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3840
      HelpContextID   =   1065
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   6735
   End
   Begin VB.CommandButton cmdOk 
      Cancel          =   -1  'True
      Caption         =   "&OK"
      Default         =   -1  'True
      Height          =   375
      HelpContextID   =   1065
      Left            =   6000
      TabIndex        =   0
      Top             =   4440
      Width           =   855
   End
End
Attribute VB_Name = "frmCalcLog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' $Id: calclog.frm,v 1.2 2002/05/03 18:38:18 tom Exp $

Private Sub cmdOk_Click()
  Unload Me
End Sub

Private Sub cmdPrint_Click()
'print the current UserData
  Dim BeginPage As Integer
  Dim EndPage As Integer
  Dim NumCopies As Integer
  Dim ReportText As String
  Dim i As Integer
  Dim pages As Integer

  If PrinterExists() Then
    If PrintDialog(BeginPage, EndPage, NumCopies) Then
      ReportText = GenFormData()
      For i = 1 To NumCopies
        PrintData ReportText, False, pages, Mag
      Next
    End If
  End If
End Sub

Private Sub cmdSave_Click()
  Dim fn As String
  
  If Not FileDialog(FD_SAVEAS, FD_TYPE_TEXT, fn) Then
    Exit Sub
  End If
  On Error GoTo ErrHandcmdSave
  Open fn For Output As #1
  Print #1, GenFormData()
  Close #1
ExitcmdSave:
  Exit Sub
  
ErrHandcmdSave:
  MsgBox "Error writing file: " + fn + vbCr + Error$(Err)
  Resume ExitcmdSave
End Sub

Private Sub DisplaySummary()
'Place Calc Log in the form's list box
   
  'add the log text, line by line, to the display control
  lstSummary.Clear
  ib = 1 'set the begin point to the beginning of the report string
  Do
    lstSummary.AddItem LineFromString(UC.MessageLog, ib)
  Loop While ib > 0
End Sub

Private Sub Form_Load()
  InitForm
End Sub

Private Sub Form_Resize()
  ResizeForm
End Sub

Private Function GenFormData() As String
'Generate report text for this form to be used for printing
  
  Dim gfd As String  'temporary storage for report text
  Dim s As String        'workspace string

  gfd = "" 'start with a blank string
  
  AppendStr gfd, "AGDISP Calculation Log", True
  AppendStr gfd, "", True
  
  AppendStr gfd, UC.MessageLog, True

  GenFormData = gfd
End Function

Private Sub InitForm()
'initialize this form
  
  'Center the form on the screen
  CenterForm Me
  
  'initialize the data
  DisplaySummary
End Sub

Private Sub ResizeForm()
'Resize the controls on the form to match the new form size
  'guard against too small a form
  If Me.Height < 2000 Then Me.Height = 2000
  If Me.Width < 1000 Then Me.Width = 1000
  
  'Ok button
  cmdOK.Top = Me.ScaleHeight - cmdOK.Height - 100
  cmdOK.Left = Me.ScaleWidth - cmdOK.Width - 100
  cmdPrint.Top = cmdOK.Top
  cmdPrint.Left = cmdOK.Left - cmdPrint.Width - 100

  'List box
  lstSummary.Top = 100
  lstSummary.Left = 100
  lstSummary.Width = Me.ScaleWidth - 100 - 100
  lstSummary.Height = cmdOK.Top - 100 - 100
End Sub

