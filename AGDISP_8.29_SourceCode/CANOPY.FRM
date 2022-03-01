VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "msflxgrd.ocx"
Begin VB.Form frmCanopy 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Canopy"
   ClientHeight    =   6330
   ClientLeft      =   2070
   ClientTop       =   1695
   ClientWidth     =   7125
   ForeColor       =   &H80000008&
   HelpContextID   =   1487
   Icon            =   "CANOPY.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6330
   ScaleWidth      =   7125
   ShowInTaskbar   =   0   'False
   Begin VB.Frame fraCanProp 
      Caption         =   "Height Properties"
      ForeColor       =   &H80000008&
      Height          =   4935
      Index           =   3
      Left            =   720
      TabIndex        =   50
      Top             =   2040
      Width           =   3975
      Begin VB.TextBox txtBasicHeight 
         Height          =   285
         HelpContextID   =   1484
         Left            =   1440
         TabIndex        =   10
         Top             =   480
         Width           =   735
      End
      Begin VB.Label lblBasicHeightUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2280
         TabIndex        =   52
         Top             =   480
         Width           =   330
      End
      Begin VB.Label lblBasicHeightLabel 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Canopy Height:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   240
         TabIndex        =   51
         Top             =   480
         Width           =   1095
      End
   End
   Begin VB.Frame fraCanProp 
      Caption         =   "LAI Canopy Properties"
      ForeColor       =   &H80000008&
      Height          =   4935
      Index           =   2
      Left            =   480
      TabIndex        =   29
      Top             =   1800
      Width           =   3975
      Begin VB.Frame Frame6 
         Caption         =   "Leaf Area Index Envelope"
         ForeColor       =   &H80000008&
         Height          =   2415
         Left            =   480
         TabIndex        =   38
         Top             =   2400
         Width           =   3375
         Begin VB.TextBox txtLAIEdit 
            BorderStyle     =   0  'None
            Height          =   285
            HelpContextID   =   1487
            Left            =   1560
            TabIndex        =   49
            Text            =   "txtLAIEdit"
            Top             =   960
            Width           =   855
         End
         Begin VB.CommandButton cmdInsertLAI 
            Caption         =   "&Insert"
            Height          =   375
            HelpContextID   =   1487
            Left            =   480
            TabIndex        =   17
            Top             =   1920
            Width           =   735
         End
         Begin VB.CommandButton cmdDeleteLAI 
            Caption         =   "&Delete"
            Height          =   375
            HelpContextID   =   1487
            Left            =   1320
            TabIndex        =   18
            Top             =   1920
            Width           =   735
         End
         Begin VB.CommandButton cmdClearLAI 
            Caption         =   "Clea&r"
            Height          =   375
            HelpContextID   =   1487
            Left            =   2160
            TabIndex        =   19
            Top             =   1920
            Width           =   735
         End
         Begin MSFlexGridLib.MSFlexGrid grdLAI 
            Height          =   1575
            Left            =   120
            TabIndex        =   53
            Top             =   240
            Width           =   3135
            _ExtentX        =   5530
            _ExtentY        =   2778
            _Version        =   393216
            WordWrap        =   -1  'True
            ScrollBars      =   2
            Appearance      =   0
         End
      End
      Begin VB.CommandButton cmdXfer 
         Caption         =   "<-"
         Height          =   255
         Left            =   2160
         TabIndex        =   15
         Top             =   1530
         Width           =   255
      End
      Begin VB.TextBox txtCanLAI 
         Height          =   285
         HelpContextID   =   1498
         Left            =   1080
         TabIndex        =   14
         Top             =   1680
         Width           =   735
      End
      Begin VB.TextBox txtCanHgt 
         Height          =   285
         HelpContextID   =   1484
         Left            =   1080
         TabIndex        =   13
         Top             =   1320
         Width           =   735
      End
      Begin VB.OptionButton optOptType 
         Caption         =   "User-defined"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1487
         Index           =   1
         Left            =   240
         TabIndex        =   16
         Top             =   2160
         Width           =   1335
      End
      Begin VB.ComboBox cboCanLib 
         Height          =   315
         HelpContextID   =   1491
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   12
         Top             =   480
         Width           =   3615
      End
      Begin VB.OptionButton optOptType 
         Caption         =   "Library"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1491
         Index           =   2
         Left            =   240
         TabIndex        =   11
         Top             =   240
         Width           =   855
      End
      Begin VB.Label lblCanLibDataQuality 
         BorderStyle     =   1  'Fixed Single
         Height          =   255
         Left            =   1080
         TabIndex        =   58
         Top             =   960
         Width           =   735
      End
      Begin VB.Label lblCanLibDataQualityLabel 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Quality:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   465
         TabIndex        =   57
         Top             =   960
         Width           =   525
      End
      Begin VB.Label Label3 
         Alignment       =   2  'Center
         Caption         =   "Library Defaults"
         ForeColor       =   &H80000008&
         Height          =   375
         Left            =   2400
         TabIndex        =   48
         Top             =   840
         Width           =   975
      End
      Begin VB.Label lblCanLibLAI 
         BorderStyle     =   1  'Fixed Single
         Height          =   255
         Left            =   2520
         TabIndex        =   47
         Top             =   1680
         Width           =   735
      End
      Begin VB.Label lblCanLibHgt 
         BorderStyle     =   1  'Fixed Single
         Height          =   255
         Left            =   2520
         TabIndex        =   46
         Top             =   1320
         Width           =   735
      End
      Begin VB.Label lblCanLAI 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "LAI:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   690
         TabIndex        =   34
         Top             =   1680
         Width           =   285
      End
      Begin VB.Label lblCanHgt 
         Alignment       =   1  'Right Justify
         AutoSize        =   -1  'True
         Caption         =   "Height:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   465
         TabIndex        =   33
         Top             =   1320
         Width           =   510
      End
      Begin VB.Label lblCanHgtUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   1920
         TabIndex        =   32
         Top             =   1320
         Width           =   330
      End
   End
   Begin VB.Frame fraName 
      Caption         =   "Canopy Name"
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   120
      TabIndex        =   30
      Top             =   120
      Width           =   6975
      Begin VB.TextBox txtName 
         Height          =   285
         HelpContextID   =   1487
         Left            =   120
         MaxLength       =   42
         TabIndex        =   2
         Top             =   240
         Width           =   6735
      End
   End
   Begin VB.Frame fraProp 
      Caption         =   "Properties"
      ForeColor       =   &H80000008&
      Height          =   1695
      Left            =   4200
      TabIndex        =   35
      Top             =   720
      Width           =   2895
      Begin VB.ComboBox cboEleTyp 
         Height          =   315
         HelpContextID   =   1555
         Left            =   1440
         Style           =   2  'Dropdown List
         TabIndex        =   56
         Top             =   600
         Width           =   1335
      End
      Begin VB.TextBox txtHumidity 
         Height          =   285
         HelpContextID   =   1230
         Left            =   1440
         TabIndex        =   9
         Top             =   1320
         Width           =   735
      End
      Begin VB.TextBox txtTemp 
         Height          =   285
         HelpContextID   =   1270
         Left            =   1440
         TabIndex        =   8
         Top             =   960
         Width           =   735
      End
      Begin VB.TextBox txtEleSiz 
         Height          =   285
         HelpContextID   =   1490
         Left            =   1440
         TabIndex        =   7
         Top             =   240
         Width           =   735
      End
      Begin VB.Label lblEleTyp 
         Caption         =   "Element Type:"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   55
         Top             =   600
         Width           =   1215
      End
      Begin VB.Label lblHumidityUnits 
         AutoSize        =   -1  'True
         Caption         =   "%"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2280
         TabIndex        =   44
         Top             =   1320
         Width           =   120
      End
      Begin VB.Label lblHumidity 
         Caption         =   "Relative Humidity:"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   43
         Top             =   1320
         Width           =   1335
      End
      Begin VB.Label lblTempUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2280
         TabIndex        =   42
         Top             =   960
         Width           =   330
      End
      Begin VB.Label lblTemp 
         Caption         =   "Temperature:"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   41
         Top             =   960
         Width           =   1095
      End
      Begin VB.Label lblEleSiz 
         Caption         =   "Element Size:"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   37
         Top             =   240
         Width           =   975
      End
      Begin VB.Label lblEleSizUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2280
         TabIndex        =   36
         Top             =   240
         Width           =   330
      End
   End
   Begin VB.Frame fraCanType 
      Caption         =   "Canopy Type"
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   120
      TabIndex        =   31
      Top             =   720
      Width           =   3975
      Begin VB.OptionButton optCanType 
         Caption         =   "Height"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1487
         Index           =   3
         Left            =   1200
         TabIndex        =   4
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton optCanType 
         Caption         =   "LAI"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1487
         Index           =   2
         Left            =   3000
         TabIndex        =   6
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton optCanType 
         Caption         =   "None"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1487
         Index           =   0
         Left            =   240
         TabIndex        =   3
         Top             =   240
         Width           =   735
      End
      Begin VB.OptionButton optCanType 
         Caption         =   "Story"
         ForeColor       =   &H80000008&
         Height          =   255
         HelpContextID   =   1487
         Index           =   1
         Left            =   2160
         TabIndex        =   5
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Height          =   375
      HelpContextID   =   1487
      Left            =   5280
      TabIndex        =   0
      Top             =   5880
      Width           =   855
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "&Cancel"
      Height          =   375
      HelpContextID   =   1487
      Left            =   6240
      TabIndex        =   1
      Top             =   5880
      Width           =   855
   End
   Begin VB.Frame fraPreview 
      Caption         =   "Preview"
      ForeColor       =   &H80000008&
      Height          =   3375
      Left            =   4200
      TabIndex        =   39
      Top             =   2400
      Width           =   2895
      Begin VB.PictureBox picPreview 
         BackColor       =   &H00FFFFFF&
         Height          =   2895
         Left            =   120
         ScaleHeight     =   2835
         ScaleWidth      =   2595
         TabIndex        =   40
         Top             =   360
         Width           =   2655
      End
   End
   Begin VB.Data datCanopy 
      Caption         =   "datCanopy"
      Connect         =   "Access"
      DatabaseName    =   ""
      DefaultCursorType=   0  'DefaultCursor
      DefaultType     =   2  'UseODBC
      Exclusive       =   0   'False
      Height          =   300
      Left            =   0
      Options         =   0
      ReadOnly        =   0   'False
      RecordsetType   =   1  'Dynaset
      RecordSource    =   ""
      Top             =   0
      Visible         =   0   'False
      Width           =   2535
   End
   Begin VB.Frame fraCanProp 
      Caption         =   "Story Canopy Properties"
      ForeColor       =   &H80000008&
      Height          =   4935
      Index           =   1
      Left            =   240
      TabIndex        =   25
      Top             =   1560
      Width           =   3975
      Begin VB.Frame Frame2 
         Caption         =   "Tree Envelope"
         ForeColor       =   &H80000008&
         Height          =   4095
         Left            =   120
         TabIndex        =   28
         Top             =   720
         Width           =   3735
         Begin VB.TextBox txtStoryEdit 
            BorderStyle     =   0  'None
            Height          =   285
            HelpContextID   =   1487
            Left            =   1080
            TabIndex        =   21
            Text            =   "txtStoryEdit"
            Top             =   1440
            Width           =   855
         End
         Begin VB.CommandButton cmdInsertStory 
            Caption         =   "&Insert"
            Height          =   375
            HelpContextID   =   1487
            Left            =   600
            TabIndex        =   22
            Top             =   3600
            Width           =   735
         End
         Begin VB.CommandButton cmdDeleteStory 
            Caption         =   "&Delete"
            Height          =   375
            HelpContextID   =   1487
            Left            =   1440
            TabIndex        =   23
            Top             =   3600
            Width           =   735
         End
         Begin VB.CommandButton cmdClearStory 
            Caption         =   "Clea&r"
            Height          =   375
            HelpContextID   =   1487
            Left            =   2280
            TabIndex        =   24
            Top             =   3600
            Width           =   735
         End
         Begin MSFlexGridLib.MSFlexGrid grdStory 
            Height          =   3255
            Left            =   120
            TabIndex        =   54
            Top             =   240
            Width           =   3495
            _ExtentX        =   6165
            _ExtentY        =   5741
            _Version        =   393216
            Cols            =   3
            WordWrap        =   -1  'True
            ScrollBars      =   2
            Appearance      =   0
         End
      End
      Begin VB.TextBox txtStanDen 
         Height          =   285
         HelpContextID   =   1492
         Left            =   1440
         TabIndex        =   20
         Top             =   360
         Width           =   735
      End
      Begin VB.Label lblStanDenUnits 
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   2280
         TabIndex        =   27
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label1 
         Caption         =   "Stand Density:"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   240
         TabIndex        =   26
         Top             =   360
         Width           =   1095
      End
   End
   Begin VB.Frame fraCanProp 
      Caption         =   "Canopy Properties"
      Enabled         =   0   'False
      ForeColor       =   &H80000008&
      Height          =   4935
      Index           =   0
      Left            =   120
      TabIndex        =   45
      Top             =   1320
      Width           =   3975
   End
End
Attribute VB_Name = "frmCanopy"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'canopy.frm - canopy form
'
Dim PropTakeAction As Integer 'if true, take action
Dim xCAN As CanopyData 'temporary work data
Dim egLAI As New clsEditGrid
Dim egStory As New clsEditGrid
  
'An escape key means Cancel. Press it and the form goes
'away. Normally, we would set the Cancel property to True
'for the Cancel button and we would be done, but this form
'contain EditGrids. EditGrids rely on the Escape key to
'cancel an edit. If the Cancel property of the Cancel
'button is true, this behavior doesn't work. The desired
'behavior is for the Escape key to cancel an edit operation
'in an EditGrid, and to dismiss the form in all other cases.
'To that end, we employ this method:
'- Set the Cancel property to False for the Cancel button
'- Set the KeyPreview property to True for the form
'- Examine KeyPress events at the form level and pass Escapes
'  through to EditGrid text boxes, and dismiss the form for
'  all other cases.
'Here we define a collection to hold all EditGrid text boxes
'for this form. If, when an escape key is pressed, one of
'the controls in this collection is the ActiveControl, the
'program continues normally and the Text control receives a
'KeyPress event. If the ActiveControl is not one in the
'collection, the cmdCancel_Click event routine is invoked.
'See Form_KeyPress below.
Private ControlsThatMayReceiveEscape As New Collection

Private Sub DataToForm()
'transfer user data to form controls for editing
  Dim RS As Recordset
  Dim Name As String
  Dim Height As Single
  Dim LAI As Single
  Dim B As Single
  Dim c As Single
  Dim DataQuality As Integer
  
  temp = PropTakeAction                     'save flag value
  
  PropTakeAction = False                    'allow raw field modification
  
  'Canopy Type
  optCanType(xCAN.Type).Value = True
  
  'Properties and preview
  UpdateGeneralCanControls
  
  'Basic Canopy
  UpdateBasicCanControls
  
  'Story Canopy
  txtStanDen.Text = AGFormat$(UnitsDisplay(xCAN.StanDen, UN_STANDDENSITY))
  egStory.ArrayToGrid 1, xCAN.NumEnv, xCAN.EnvHgt, UN_LENGTH
  egStory.ArrayToGrid 2, xCAN.NumEnv, xCAN.EnvDiam, UN_LENGTH
  egStory.ArrayToGrid 3, xCAN.NumEnv, xCAN.EnvPop
  
  'Optical Canopy
  optOptType(xCAN.optType).Value = True
  If xCAN.optType = 2 Then 'Library
    For i = 0 To cboCanLib.ListCount - 1
      If cboCanLib.List(i) = Trim$(xCAN.Name) Then
        cboCanLib.ListIndex = i
        Exit For
      End If
    Next
  Else
    cboCanLib.ListIndex = 0
  End If
  GetCanLibEntry cboCanLib.ListIndex, Name, Height, LAI, B, c, DataQuality
  xCAN.LibB = B
  xCAN.LibC = c
  xCAN.LibDataQuality = DataQuality
  lblCanLibDataQuality.Caption = GetNameCanDataQuality(DataQuality)
  lblCanLibHgt.Caption = AGFormat$(UnitsDisplay(Height, UN_LENGTH))
  lblCanLibLAI.Caption = AGFormat$(LAI)
  egLAI.ArrayToGrid 1, xCAN.NumLAI, xCAN.LAIHgt, UN_LENGTH
  egLAI.ArrayToGrid 2, xCAN.NumLAI, xCAN.LAICum
  UpdateOpticalCanControls
  
  PropTakeAction = temp
End Sub

'---------------------------------------------------------------------------
' GetCanLibEntry:
'    Retrieve a Canopy Library record based on record number (0 is first).
'
' Arguments:
'     Name: Canopy Name
'     Height: Canopy Height
'     LAI: Leaf Area Index
'     B:
'     c:
'     DataQuality: 1=High, 2=Medium, 3=Low
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2004-05-14  TBC  Changed to retrieve by number, rather than name
'
'---------------------------------------------------------------------------
Private Sub GetCanLibEntry(intRec As Integer, _
                           Name As String, _
                           Height As Single, LAI As Single, _
                           B As Single, c As Single, DataQuality As Integer)
  Dim i As Integer
  Dim RS As Recordset
    
  Set RS = datCanopy.Recordset
  RS.MoveFirst
  For i = 1 To intRec
    RS.MoveNext
  Next
  
  Name = Trim$(RS.Fields("Name"))
  Height = RS.Fields("Height")
  LAI = RS.Fields("LAI")
  B = RS.Fields("B")
  c = RS.Fields("C")
  DataQuality = RS.Fields("DataQuality")
End Sub

Private Sub UpdateLAITable()
'Get a fresh copy of the LAI table from the grid
  egLAI.GridToArray 1, xCAN.NumLAI, xCAN.LAIHgt, UN_LENGTH
  egLAI.GridToArray 2, xCAN.NumLAI, xCAN.LAICum
  'recalc canopy height from table info
  xCAN.Height = 0
  For i = 0 To xCAN.NumLAI - 1
    If xCAN.LAIHgt(i) > xCAN.Height Then xCAN.Height = xCAN.LAIHgt(i)
  Next
End Sub

Private Sub UpdateStoryTable()
'Get a fresh copy of the LAI table from the grid
  Dim i As Integer
  
  egStory.GridToArray 1, xCAN.NumEnv, xCAN.EnvHgt, UN_LENGTH
  egStory.GridToArray 2, xCAN.NumEnv, xCAN.EnvDiam, UN_LENGTH
  egStory.GridToArray 3, xCAN.NumEnv, xCAN.EnvPop
  'recalc canopy height from table info
  xCAN.Height = 0
  For i = 0 To xCAN.NumEnv - 1
    If xCAN.EnvHgt(i) > xCAN.Height Then xCAN.Height = xCAN.EnvHgt(i)
  Next
End Sub

Private Sub UpdateGeneralCanControls()
'Update the state of the general canopy controls
'i.e. Canopy Type, Properties, and individual Type Properties

  Dim c As Control
  Dim ExtraCanControls As Boolean
  
  'Show the appropriate frame for the canopy type
  For Each c In fraCanProp()
    c.Visible = (c.Index = xCAN.Type)
  Next
  
  'Name box
  If xCAN.Type = 0 Then 'none
    txtName.Text = ""
  Else
    txtName.Text = Trim$(xCAN.Name)
  End If
  txtName.Enabled = (xCAN.Type <> 0)
  
  'Update the other Property controls
  Select Case xCAN.Type
  Case 1, 2 'story, optical
    txtEleSiz.Text = AGFormat$(UnitsDisplay(xCAN.EleSiz, UN_SMLENGTH))
    cboEleTyp.ListIndex = xCAN.EleTyp
    txtTemp.Text = AGFormat$(UnitsDisplay(xCAN.temp, UN_TEMP))
    txtHumidity.Text = AGFormat$(xCAN.Humidity)
    ExtraCanControls = True
  Case Else
    txtEleSiz.Text = ""
    cboEleTyp.ListIndex = 0 'TODO: how to clear a dropdown list?
    txtTemp.Text = ""
    txtHumidity.Text = ""
    ExtraCanControls = False
  End Select
  fraProp.Enabled = ExtraCanControls
  lblEleSiz.Enabled = ExtraCanControls
  txtEleSiz.Enabled = ExtraCanControls
  lblEleTyp.Enabled = ExtraCanControls
  cboEleTyp.Enabled = ExtraCanControls
  lblEleSizUnits.Enabled = ExtraCanControls
  lblTemp.Enabled = ExtraCanControls
  txtTemp.Enabled = ExtraCanControls
  lblTempUnits.Enabled = ExtraCanControls
  lblHumidity.Enabled = ExtraCanControls
  txtHumidity.Enabled = ExtraCanControls
  lblHumidityUnits.Enabled = ExtraCanControls
  
  'update the Preview
  fraPreview.Enabled = ExtraCanControls
  picPreview.Enabled = ExtraCanControls
End Sub

Private Sub UpdateBasicCanControls()
'Update the state of the Basic Canopy controls
  txtBasicHeight.Text = AGFormat$(UnitsDisplay(xCAN.Height, UN_LENGTH))
End Sub

Private Sub UpdateOpticalCanControls()
'Update the state of the Optical Canopy controls
  'xCAN.OptType: 2-Library 1=user-defined
  
  cboCanLib.Enabled = (xCAN.optType = 2)
  lblCanLibDataQualityLabel.Enabled = (xCAN.optType = 2)
  lblCanLibDataQuality.Enabled = (xCAN.optType = 2)
  lblCanHgt.Enabled = (xCAN.optType = 2)
  txtCanHgt.Enabled = (xCAN.optType = 2)
  lblCanHgtUnits.Enabled = (xCAN.optType = 2)
  lblCanLAI.Enabled = (xCAN.optType = 2)
  txtCanLAI.Enabled = (xCAN.optType = 2)
  lblCanLibHgt.Enabled = (xCAN.optType = 2)
  lblCanLibLAI.Enabled = (xCAN.optType = 2)
  If xCAN.optType = 2 Then 'library
    txtCanHgt.Text = AGFormat$(UnitsDisplay(xCAN.LibHgt, UN_LENGTH))
    txtCanLAI.Text = AGFormat$(xCAN.LibLAI)
  Else              'user-def
    txtCanHgt.Text = ""
    txtCanLAI.Text = ""
  End If
End Sub

Private Sub DrawCanopyPreview()
'Draw a picture of the canopy in the preview area
  Dim p As PictureBox
  Dim i As Integer
  Dim aspect As Single
  Dim hmax As Single
  Dim dmax As Single
  Dim dens As Integer
  Dim Y As Single
  Dim Z As Single
  
  Set p = picPreview
  aspect = p.Height / p.Width
  p.Cls
  p.ScaleMode = 0 'User units
  
  If xCAN.Type = 1 Then 'Story
    If xCAN.NumEnv > 0 Then
      hmax = xCAN.EnvHgt(0)
      dmax = xCAN.EnvDiam(0)
      For i = 1 To xCAN.NumEnv - 1
        If xCAN.EnvHgt(i) > hmax Then hmax = xCAN.EnvHgt(i)
        If xCAN.EnvDiam(i) > dmax Then dmax = xCAN.EnvDiam(i)
      Next
      If dmax > 0 Then
        If hmax / dmax >= aspect Then
          p.ScaleHeight = -1.1 * hmax
          p.ScaleWidth = -p.ScaleHeight / aspect
        Else
          p.ScaleWidth = 1.1 * dmax
          p.ScaleHeight = -p.ScaleWidth * aspect
        End If
        p.ScaleLeft = -p.ScaleWidth / 2
        p.ScaleTop = -p.ScaleHeight
        dens = xCAN.EnvPop(0) * 255
        If dens > 255 Then dens = 255
        If dens < 0 Then dens = 0
        p.Line (-xCAN.EnvDiam(0) / 2, 0)- _
                (xCAN.EnvDiam(0) / 2, xCAN.EnvHgt(0)), _
                RGB(dens, dens, dens), BF
        For i = 1 To xCAN.NumEnv - 1
          dens = xCAN.EnvPop(i) * 255
          If dens > 255 Then dens = 255
          If dens < 0 Then dens = 0
          p.Line (-xCAN.EnvDiam(i) / 2, xCAN.EnvHgt(i))- _
                  (xCAN.EnvDiam(i) / 2, xCAN.EnvHgt(i - 1)), _
                  RGB(dens, dens, dens), BF
        Next
      End If
    End If
  
  ElseIf xCAN.Type = 2 Then 'Optical
    If xCAN.optType = 2 Then 'Library
      p.ScaleHeight = -1
      p.ScaleWidth = 1.01 'allow a little extra room so that the plot line is not hidden
      p.ScaleLeft = 0
      p.ScaleTop = -p.ScaleHeight
      p.CurrentX = 0
      p.CurrentY = 1
      For i = 20 To 0 Step -1
        Y = i / 20
        Z = (1 - Exp(-(((1 - Y) / xCAN.LibB) ^ xCAN.LibC))) / _
            (1 - Exp(-((1 / xCAN.LibB) ^ xCAN.LibC)))
        p.Line -(Z, Y), vbBlack
      Next
    ElseIf xCAN.optType = 1 Then 'user-def
      If xCAN.NumLAI > 1 Then
        hmax = xCAN.LAIHgt(0)
        dmax = xCAN.LAICum(0)
        For i = 1 To xCAN.NumLAI - 1
          If xCAN.LAIHgt(i) > hmax Then hmax = xCAN.LAIHgt(i)
          If xCAN.LAICum(i) > dmax Then dmax = xCAN.LAICum(i)
        Next
        If dmax > 0 Then
          p.ScaleHeight = -hmax
          p.ScaleWidth = dmax
          p.ScaleLeft = 0
          p.ScaleTop = -p.ScaleHeight
          p.CurrentX = xCAN.LAICum(0)
          p.CurrentY = xCAN.LAIHgt(0)
          For i = 1 To xCAN.NumLAI - 1
            p.Line -(xCAN.LAICum(i), xCAN.LAIHgt(i)), vbBlack
          Next
        End If
      End If
    End If
  End If
End Sub

Private Sub cboCanLib_Click()
  Dim Name As String
  Dim Height As Single
  Dim LAI As Single
  Dim B As Single
  Dim c As Single
  Dim DataQuality As Integer
  Dim i As Integer
  
  If PropTakeAction Then
    PropTakeAction = False
    GetCanLibEntry cboCanLib.ListIndex, Name, Height, LAI, B, c, DataQuality
  
    If optCanType(2).Value = True And optOptType(2).Value = True Then 'optical,library
      'make sure canopy name is library name
      xCAN.Name = Name
      txtName.Text = Name
    End If
    lblCanLibDataQuality.Caption = GetNameCanDataQuality(DataQuality)
    lblCanLibHgt.Caption = AGFormat$(UnitsDisplay(Height, UN_LENGTH))
    lblCanLibLAI.Caption = AGFormat$(LAI)
    If txtCanHgt.Text = "" Then xCAN.LibHgt = Height
    If txtCanLAI.Text = "" Then xCAN.LibLAI = LAI
    xCAN.LibB = B
    xCAN.LibC = c
    xCAN.LibDataQuality = DataQuality
    UpdateOpticalCanControls
    DrawCanopyPreview
    PropTakeAction = True
  End If
End Sub

Private Sub cboEleTyp_Click()
  If PropTakeAction Then
    xCAN.EleTyp = cboEleTyp.ListIndex
  End If
End Sub

Private Sub cmdCancel_Click()
  Unload Me
End Sub

Private Sub cmdClearLAI_Click()
  egLAI.ClearSelected
  UpdateLAITable
  DrawCanopyPreview
End Sub

Private Sub cmdDeleteLAI_Click()
  egLAI.DeleteRow
  UpdateLAITable
  DrawCanopyPreview
End Sub

Private Sub cmdInsertLAI_Click()
  egLAI.InsertRow
  UpdateLAITable
  DrawCanopyPreview
End Sub

Private Sub cmdClearStory_Click()
  egStory.ClearSelected
  UpdateStoryTable
  DrawCanopyPreview
End Sub

Private Sub cmdDeleteStory_Click()
  egStory.DeleteRow
  UpdateStoryTable
  DrawCanopyPreview
End Sub

Private Sub cmdInsertStory_Click()
  egStory.InsertRow
  UpdateStoryTable
  DrawCanopyPreview
End Sub

Private Sub cmdOK_Click()
  Dim i As Integer
  
  'Set the canopy height to be consistant with other canopy data
  Select Case xCAN.Type
  Case 0 'no canopy
    xCAN.Height = 0
  Case 1 'story canopy
    xCAN.Height = 0
    For i = 0 To xCAN.NumEnv - 1
      If xCAN.EnvHgt(i) > xCAN.Height Then xCAN.Height = xCAN.EnvHgt(i)
    Next
  Case 2 'optical canopy
    'special message for optical canopies
    If Not UP.SuppressModelFeatureWarnings Then
      MsgBox "The engineering approach implemented in this " & _
             "feature is based on previous published work " & _
             "but has not been validated in this context.", _
             vbInformation
    End If
    Select Case xCAN.optType
    Case 1 'user-defined
      xCAN.Height = 0
      For i = 0 To xCAN.NumLAI - 1
        If xCAN.LAIHgt(i) > xCAN.Height Then xCAN.Height = xCAN.LAIHgt(i)
      Next
    Case 2 'library
      xCAN.Height = xCAN.LibHgt
    End Select
  Case 3 'Basic
    'height is already set
  End Select
  
  'now that we know the canopy height, set the canopy type to "none" if
  'the height is zero
  If xCAN.Type <> 0 And xCAN.Height <= 0 Then
    xCAN.Type = 0
    xCAN.Height = 0
  End If
  
  UD.CAN = xCAN 'transer temporary data to user data
  UpdateDataChangedFlag True 'Data was changed
  UC.Valid = False 'Calcs need to be redone
  Unload Me
End Sub

Private Sub cmdXfer_Click()
  txtCanHgt.Text = lblCanLibHgt.Caption
  txtCanLAI.Text = lblCanLibLAI.Caption
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
  Dim c As Control
  Select Case KeyAscii
  Case 13 'Enter
    For Each c In ControlsThatMayReceiveEscape
      If c Is Me.ActiveControl Then
        Exit Sub
      End If
    Next
    cmdOK.Value = True 'Same as clicking the button
  Case 27 'Escape
    For Each c In ControlsThatMayReceiveEscape
      If c Is Me.ActiveControl Then
        Exit Sub
      End If
    Next
    cmdCancel.Value = True 'Same as clicking the button
  End Select
End Sub

Private Sub Form_Load()
'Initialize the controls on this form
  Dim c As Control

  'center the form
  CenterForm Me

  'Initialize the collection of controls that may receive
  'an escape character. This allows Escape to dismiss the
  'form OR abort an EditGrid edit.
  With ControlsThatMayReceiveEscape
    .Add txtLAIEdit
    .Add txtStoryEdit
  End With
  
  'Copy the canopy data into the work area
  xCAN = UD.CAN
  
  'set the database and initial query for the data controls
  datCanopy.ReadOnly = True 'open database as read only
  datCanopy.DatabaseName = UI.LibraryPath
  datCanopy.RecordSource = "Canopy"
  datCanopy.Refresh
  
  'units
  lblEleSizUnits.Caption = UnitsName(UN_SMLENGTH)
  lblTempUnits.Caption = UnitsName(UN_TEMP)
  lblCanHgtUnits.Caption = UnitsName(UN_LENGTH)
  
  'Align the Canopy Property frames
  For Each c In fraCanProp()
    c.Left = fraCanType.Left
    c.Top = fraCanType.Top + fraCanType.Height
  Next
  
  'Canopy Properties frame ---------------------------------
  With cboEleTyp
    .Clear
    .AddItem GetTypeNameCanopyElement(0) 'Flat Plate
    .AddItem GetTypeNameCanopyElement(1) 'Cylinder
    .AddItem GetTypeNameCanopyElement(2) 'Sphere
  End With
  
  'Basic Canopy frame --------------------------------------
  lblBasicHeightUnits.Caption = UnitsName(UN_LENGTH)
  
  'Story Canopy frame --------------------------------------
  lblStanDenUnits.Caption = UnitsName(UN_STANDDENSITY)
  
  'Tree Envelope Grid
  egStory.Setup grdStory, txtStoryEdit, MAX_ENVELOPE, , True
  egStory.AddColumn "Tree Height (" + UnitsName(UN_LENGTH) + ")"
  egStory.AddColumn "Tree Diam. (" + UnitsName(UN_LENGTH) + ")"
  egStory.AddColumn "Prob. of Penetration"
  egStory.Resize

  'Optical Canopy frame --------------------------------------
  
  'Canopy Library
  datCanopy.Recordset.MoveFirst
  Do Until datCanopy.Recordset.EOF
    cboCanLib.AddItem Trim$(datCanopy.Recordset.Fields("Name"))
    datCanopy.Recordset.MoveNext
  Loop
  
  'Leaf Area Index Grid
  egLAI.Setup grdLAI, txtLAIEdit, MAX_LAI
  egLAI.AddColumn "Tree Height (" + UnitsName(UN_LENGTH) + ")"
  egLAI.AddColumn "Cumulative LAI"
  egLAI.Resize
  
  'allow option button changes to take action
  '(see declarations section)
  PropTakeAction = True
  
  DataToForm
  DrawCanopyPreview
End Sub

Private Sub grdLAI_DblClick()
  egLAI.GridDblClick
End Sub

Private Sub grdLAI_KeyDown(KeyCode As Integer, Shift As Integer)
  egLAI.GridKeyDown KeyCode, Shift
End Sub

Private Sub grdLAI_KeyPress(KeyAscii As Integer)
  egLAI.GridKeyPress KeyAscii
End Sub

Private Sub grdLAI_Scroll()
  egLAI.GridScroll
End Sub

Private Sub grdStory_DblClick()
  egStory.GridDblClick
End Sub

Private Sub grdStory_KeyDown(KeyCode As Integer, Shift As Integer)
  egStory.GridKeyDown KeyCode, Shift
End Sub

Private Sub grdStory_KeyPress(KeyAscii As Integer)
  egStory.GridKeyPress KeyAscii
End Sub

Private Sub grdStory_Scroll()
  egStory.GridScroll
End Sub

Private Sub optCanType_Click(Index As Integer)
'Turn controls on or off to match the canopy type
  If PropTakeAction Then
    PropTakeAction = False
    xCAN.Type = Index
    UpdateGeneralCanControls
    DrawCanopyPreview
    'Because Canopy Height is derrived from the Story
    'and Optical tables or can be input directly for
    'Basic, make sure the value is up-to-date when
    'changing types
    Select Case xCAN.Type
    Case 0 'none
      xCAN.Height = 0
    Case 1 'story
      UpdateStoryTable
    Case 2 'optical
      UpdateLAITable
    Case 3 'basic
      UpdateBasicCanControls
    End Select
    PropTakeAction = True
  End If
End Sub

Private Sub optOptType_Click(Index As Integer)
  If PropTakeAction Then
    PropTakeAction = False
    xCAN.optType = Index
    If xCAN.optType = 2 Then 'Library
      'make sure the canopy name is the library name
      xCAN.Name = cboCanLib.Text
      txtName.Text = Trim$(xCAN.Name)
    End If
    UpdateOpticalCanControls
    DrawCanopyPreview
    PropTakeAction = True
  End If
End Sub

Private Sub picPreview_Paint()
  DrawCanopyPreview
End Sub

Private Sub txtBasicHeight_Change()
  If PropTakeAction Then
    xCAN.Height = UnitsInternal(Val(txtBasicHeight.Text), UN_LENGTH)
  End If
End Sub

Private Sub txtCanHgt_Change()
  If PropTakeAction Then
    xCAN.LibHgt = UnitsInternal(Val(txtCanHgt.Text), UN_LENGTH)
  End If
End Sub

Private Sub txtCanLAI_Change()
  If PropTakeAction Then
    xCAN.LibLAI = Val(txtCanLAI.Text)
  End If
End Sub

Private Sub txtEleSiz_Change()
  If PropTakeAction Then
    xCAN.EleSiz = UnitsInternal(Val(txtEleSiz.Text), UN_SMLENGTH)
  End If
End Sub

Private Sub txtHumidity_Change()
  If PropTakeAction Then
    xCAN.Humidity = Val(txtHumidity.Text)
  End If
End Sub

Private Sub txtLAIEdit_KeyDown(KeyCode As Integer, Shift As Integer)
  egLAI.TextKeyDown KeyCode, Shift
End Sub

Private Sub txtLAIEdit_KeyPress(KeyAscii As Integer)
  egLAI.TextKeyPress KeyAscii
  Select Case KeyAscii
  Case 13 'CR
    UpdateLAITable
    DrawCanopyPreview
    KeyAscii = 0
  Case 27 'Esc
    KeyAscii = 0
  End Select
End Sub

Private Sub txtLAIEdit_LostFocus()
  egLAI.TextLostFocus
  UpdateLAITable
  DrawCanopyPreview
End Sub

Private Sub txtStoryEdit_KeyDown(KeyCode As Integer, Shift As Integer)
  egStory.TextKeyDown KeyCode, Shift
End Sub

Private Sub txtStoryEdit_KeyPress(KeyAscii As Integer)
  egStory.TextKeyPress KeyAscii
  Select Case KeyAscii
  Case 13 'CR
    UpdateStoryTable
    DrawCanopyPreview
    KeyAscii = 0
  Case 27 'Esc
    KeyAscii = 0
  End Select
End Sub

Private Sub txtStoryEdit_LostFocus()
  egStory.TextLostFocus
  UpdateStoryTable
  DrawCanopyPreview
End Sub

Private Sub txtName_Change()
'if this field is changed by the user, flip to user-defined
  If PropTakeAction Then
    xCAN.Name = txtName.Text
    If Not optOptType(1).Value Then optOptType(1).Value = True
  End If
End Sub

Private Sub txtStanDen_Change()
  If PropTakeAction Then
    xCAN.StanDen = UnitsInternal(Val(txtStanDen.Text), UN_STANDDENSITY)
  End If
End Sub

Private Sub txtTemp_Change()
  If PropTakeAction Then
    xCAN.temp = UnitsInternal(Val(txtTemp.Text), UN_TEMP)
  End If
End Sub
