VERSION 5.00
Begin VB.Form frmAircraftLib 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Aircraft Library"
   ClientHeight    =   5760
   ClientLeft      =   2955
   ClientTop       =   2040
   ClientWidth     =   5985
   ForeColor       =   &H80000008&
   Icon            =   "aircrlib.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5760
   ScaleWidth      =   5985
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   375
      HelpContextID   =   1020
      Left            =   5040
      TabIndex        =   1
      Top             =   5280
      Width           =   855
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Height          =   375
      HelpContextID   =   1020
      Left            =   4080
      TabIndex        =   0
      Top             =   5280
      Width           =   855
   End
   Begin VB.Frame fraFilter 
      Caption         =   "Filter"
      ForeColor       =   &H80000008&
      Height          =   975
      Left            =   120
      TabIndex        =   15
      Top             =   120
      Width           =   5775
      Begin VB.ComboBox cboFilter 
         Height          =   315
         HelpContextID   =   1020
         Index           =   0
         Left            =   960
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   360
         Width           =   4335
      End
      Begin VB.Label Label11 
         AutoSize        =   -1  'True
         Caption         =   "Name:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   10
         Top             =   405
         Width           =   465
      End
   End
   Begin VB.Frame fraDatabase 
      Caption         =   "Browse Filtered Entries"
      ForeColor       =   &H80000008&
      Height          =   4095
      Left            =   120
      TabIndex        =   12
      Top             =   1080
      Width           =   5775
      Begin VB.CommandButton cmdPrev 
         Caption         =   "Prev"
         Height          =   375
         HelpContextID   =   1020
         Left            =   720
         TabIndex        =   4
         Top             =   3600
         Width           =   615
      End
      Begin VB.CommandButton cmdNext 
         Caption         =   "Next"
         Height          =   375
         HelpContextID   =   1020
         Left            =   1320
         TabIndex        =   5
         Top             =   3600
         Width           =   615
      End
      Begin VB.CommandButton cmdFirst 
         Caption         =   "1st"
         Height          =   375
         HelpContextID   =   1020
         Left            =   120
         TabIndex        =   3
         Top             =   3600
         Width           =   615
      End
      Begin VB.CommandButton cmdLast 
         Caption         =   "Last"
         Height          =   375
         HelpContextID   =   1020
         Left            =   1920
         TabIndex        =   6
         Top             =   3600
         Width           =   615
      End
      Begin VB.Label Label24 
         AutoSize        =   -1  'True
         Caption         =   "Boom Fwd:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   55
         Top             =   3240
         Width           =   795
      End
      Begin VB.Label lblBoomFwd 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "BoomFwd"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   4320
         TabIndex        =   54
         Top             =   3240
         Width           =   975
      End
      Begin VB.Label lblBoomFwdUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5400
         TabIndex        =   53
         Top             =   3240
         Width           =   420
      End
      Begin VB.Label Label21 
         AutoSize        =   -1  'True
         Caption         =   "Boom Vert:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   52
         Top             =   2880
         Width           =   780
      End
      Begin VB.Label lblBoomVert 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "BoomVert"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   4320
         TabIndex        =   51
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label lblBoomVertUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5400
         TabIndex        =   50
         Top             =   2880
         Width           =   420
      End
      Begin VB.Label Label18 
         AutoSize        =   -1  'True
         Caption         =   "Wing Vert:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   49
         Top             =   2520
         Width           =   750
      End
      Begin VB.Label lblWingVert 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "WingVert"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   4320
         TabIndex        =   48
         Top             =   2520
         Width           =   975
      End
      Begin VB.Label lblWingVertUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5400
         TabIndex        =   47
         Top             =   2520
         Width           =   420
      End
      Begin VB.Label lblEngHoriz 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "EngHoriz"
         ForeColor       =   &H80000008&
         Height          =   255
         Index           =   1
         Left            =   4320
         TabIndex        =   46
         Top             =   2160
         Width           =   975
      End
      Begin VB.Label Label17 
         AutoSize        =   -1  'True
         Caption         =   "Engine Horiz.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   45
         Top             =   2040
         Width           =   990
      End
      Begin VB.Label lblEngHoriz 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "EngHoriz"
         ForeColor       =   &H80000008&
         Height          =   255
         Index           =   0
         Left            =   4320
         TabIndex        =   44
         Top             =   1800
         Width           =   975
      End
      Begin VB.Label lblEngHorizUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5400
         TabIndex        =   43
         Top             =   2040
         Width           =   420
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "Engines.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   42
         Top             =   720
         Width           =   660
      End
      Begin VB.Label lblNumEng 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "NumEng"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   4320
         TabIndex        =   41
         Top             =   720
         Width           =   975
      End
      Begin VB.Label lblBiplSepUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   33
         Top             =   2880
         Width           =   420
      End
      Begin VB.Label lblPlanAreaUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   40
         Top             =   3240
         Width           =   420
      End
      Begin VB.Label lblEngVertUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5400
         TabIndex        =   39
         Top             =   1080
         Width           =   420
      End
      Begin VB.Label lblEngFwdUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   5400
         TabIndex        =   38
         Top             =   1440
         Width           =   420
      End
      Begin VB.Label lblPropRadUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   37
         Top             =   2520
         Width           =   420
      End
      Begin VB.Label lblTypSpeedUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   36
         Top             =   1800
         Width           =   420
      End
      Begin VB.Label lblWeightUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   35
         Top             =   1440
         Width           =   420
      End
      Begin VB.Label lblSemiSpanUnits 
         AutoSize        =   -1  'True
         Caption         =   "units"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2520
         TabIndex        =   34
         Top             =   1080
         Width           =   420
      End
      Begin VB.Label lblName 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Name"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   14
         Top             =   360
         Width           =   4095
      End
      Begin VB.Label Label15 
         AutoSize        =   -1  'True
         Caption         =   "Name:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   22
         Top             =   360
         Width           =   555
      End
      Begin VB.Label lblEngFwd 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "EngFwd"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   4320
         TabIndex        =   7
         Top             =   1440
         Width           =   975
      End
      Begin VB.Label lblEngVert 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "EngVert"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   4320
         TabIndex        =   8
         Top             =   1080
         Width           =   975
      End
      Begin VB.Label lblPropRad 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "PropRad"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   9
         Top             =   2520
         Width           =   975
      End
      Begin VB.Label lblPropRPM 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "PropRPM"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   11
         Top             =   2160
         Width           =   975
      End
      Begin VB.Label lblPlanArea 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "PlanArea"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   13
         Top             =   3240
         Width           =   975
      End
      Begin VB.Label lblWeight 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Weight"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   16
         Top             =   1440
         Width           =   975
      End
      Begin VB.Label lblBiplSep 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "BiplSep"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   17
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label lblTypSpeed 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "TypSpeed"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   18
         Top             =   1800
         Width           =   975
      End
      Begin VB.Label lblSemiSpan 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "SemiSpan"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   20
         Top             =   1080
         Width           =   975
      End
      Begin VB.Label lblWingType 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "WingType"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   1440
         TabIndex        =   21
         Top             =   720
         Width           =   975
      End
      Begin VB.Label Label13 
         AutoSize        =   -1  'True
         Caption         =   "Engine Fwd.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   23
         Top             =   1440
         Width           =   1125
      End
      Begin VB.Label Label12 
         AutoSize        =   -1  'True
         Caption         =   "Engine Vert.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   3000
         TabIndex        =   24
         Top             =   1080
         Width           =   1125
      End
      Begin VB.Label Label10 
         AutoSize        =   -1  'True
         Caption         =   "Prop Rad.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   25
         Top             =   2520
         Width           =   930
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "Propeller RPM:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   26
         Top             =   2160
         Width           =   1290
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "Planform Area:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   28
         Top             =   3240
         Width           =   1260
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Weight:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   30
         Top             =   1440
         Width           =   675
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Biplane Sep.:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   31
         Top             =   2880
         Width           =   1155
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Typ. Speed:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   32
         Top             =   1800
         Width           =   1050
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Semispan:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   27
         Top             =   1080
         Width           =   885
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Type:"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   29
         Top             =   720
         Width           =   495
      End
      Begin VB.Label lblPosition 
         AutoSize        =   -1  'True
         Caption         =   "0 of 0 matches"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   2760
         TabIndex        =   19
         Top             =   3720
         Width           =   1050
      End
   End
   Begin VB.Data datAircraft 
      Caption         =   "datAircraft"
      Connect         =   "Access"
      DatabaseName    =   ""
      DefaultCursorType=   0  'DefaultCursor
      DefaultType     =   2  'UseODBC
      Exclusive       =   0   'False
      Height          =   345
      Left            =   3600
      Options         =   0
      ReadOnly        =   -1  'True
      RecordsetType   =   1  'Dynaset
      RecordSource    =   ""
      Top             =   5280
      Visible         =   0   'False
      Width           =   2175
   End
End
Attribute VB_Name = "frmAircraftLib"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' $Id: aircrlib.frm,v 1.4 2009/09/22 18:31:44 tom Exp $
Dim PositionCount As Integer  'the current recordset position
Dim TableName As String       'The database table from which to draw

Public Sub SelectTable(tname As String)
'Set the Database table from which this form will draw
  Dim DStmp As Recordset
  Dim c As Control
  Dim s As String
  Dim dslist As String

  TableName = tname

  'set the database and initial query for the data controls
  datAircraft.ReadOnly = True 'open database as read only
  datAircraft.DatabaseName = UI.LibraryPath
  datAircraft.RecordSource = TableName
  datAircraft.Refresh
  UpdateNumRecs

  'Add the special filter items
  With cboFilter(0)
    .Clear
    .AddItem "(Any)"
    .AddItem "(Any Fixed Wing)"
    .AddItem "(Any Helicopter)"
  End With
  
  'Fill the combo box list with the unique lists from
  'the database
  'Pad integer fields with zeros so that they sort properly
  dslist = TableName
  Set DStmp = datAircraft.Database.OpenRecordset(dslist)
  DStmp.MoveFirst
  Do Until DStmp.EOF
    cboFilter(0).AddItem DStmp.Fields(0)
    DStmp.MoveNext
  Loop
  DStmp.Close
  
  'Set combo box to first item
  cboFilter(0).ListIndex = 0
  
  UpdateUnitsLabels
  UpdatePropertyControls
End Sub

Private Sub cboFilter_Click(Index As Integer)
  'Update the data controls recordset when the Filters change
  GetNewRecordset
End Sub

Private Sub cmdCancel_Click()
  Me.Hide
End Sub

Private Sub cmdFirst_Click()
  If datAircraft.Recordset.RecordCount > 0 Then
    datAircraft.Recordset.MoveFirst
    PositionCount = 1
    UpdatePositionCount
    UpdatePropertyControls
  End If
End Sub

Private Sub cmdLast_Click()
  If datAircraft.Recordset.RecordCount > 0 Then
    datAircraft.Recordset.MoveLast
    PositionCount = datAircraft.Recordset.RecordCount
    UpdatePositionCount
    UpdatePropertyControls
  End If
End Sub

Private Sub cmdNext_Click()
  If Not datAircraft.Recordset.EOF Then
    datAircraft.Recordset.MoveNext
    'before we update the Position, see if
    'we've moved off the last record
    If Not datAircraft.Recordset.EOF Then
      PositionCount = PositionCount + 1
      UpdatePositionCount
      UpdatePropertyControls
    Else
      datAircraft.Recordset.MovePrevious 'go back to where we were
    End If
  End If
End Sub

Private Sub cmdOK_Click()
  If TransferData() Then
    Me.Tag = "True"  'indicate success
    Me.Hide
  Else
    s$ = "No library entry has been selected."
    t% = vbCritical + vbOKOnly
    MsgBox s$, t%
  End If
End Sub

Private Sub cmdPrev_Click()
  If Not datAircraft.Recordset.BOF Then
    datAircraft.Recordset.MovePrevious
    'before we update the Position, see if
    'we've moved off the first record
    If Not datAircraft.Recordset.BOF Then
      PositionCount = PositionCount - 1
      UpdatePositionCount
      UpdatePropertyControls
    Else
      datAircraft.Recordset.MoveNext 'go back to where we were
    End If
  End If
End Sub

Private Sub ConditionalAddItem(s As String, c As Control)
'Add an item to a List control only if it is not
'already in the list
'
  For i = 0 To c.ListCount - 1
    If (s = c.List(i)) Then Exit Sub
  Next
  c.AddItem s
End Sub

Private Sub Form_Load()
'Initialize this form and its controls
  'This form uses the Tag property to return status
  'true for success, false for failure
  'set to "false by default
  Me.Tag = "False"

  'Center the form
  CenterForm Me

End Sub

Private Sub GetNewRecordset()
'Build a new query from the values of the control array
'and retrieve a new recordset from
'a data control by resetting its RecordSource
'
'
  Dim s As String        'the new query
  Dim needsep As Integer 'flag
  Dim CompStr As String  'component value for search
  Dim QueryStr As String 'String to hold query
  Dim DS As Recordset      'Recordset of components for search

  'set the basic query that would return all records
  'from the Nozzles Table
  s = "SELECT * FROM " + TableName
  With cboFilter(0)
    Select Case .ListIndex
    Case 0 'Any
      'No additional SQL needed
    Case 1 'Any Fixed Wing
      s = s + " WHERE Type=3"
    Case 2 'Any Helicopter
      s = s + " WHERE Type=4"
    Case Else 'Specific name
      s = s + " WHERE Name= '" & .List(.ListIndex) + "'"
    End Select
  End With
  
  'Reset the RecordSource property and refresh to get the
  'new recordset
  datAircraft.RecordSource = s
  datAircraft.Refresh
  'Update the display for number of records in this set
  UpdateNumRecs
  UpdatePropertyControls
End Sub

Private Function TransferData() As Integer
'Transfer the selected data to the parent form
  Dim tmpEng(1) As Single
  Dim g As Control
  Dim DS As Recordset
  Dim s As String

  Set fm = frmAircraft 'select the parent form
  Set DS = datAircraft.Recordset  'select the current recordset
  
  'make sure there is a current record
  If DS.BOF And DS.EOF Then
    TransferData = False
    Exit Function
  End If
  
  'transfer the database record to the output controls
  With frmAircraft
    .txtName.Text = Trim$(DS.Fields("Name"))
    .cboWingType.ListIndex = DS.Fields("Type") - 3
    .txtSemiSpan.Text = AGFormat$(UnitsDisplay(DS.Fields("SemiSpan"), UN_LENGTH))
    .txtTypSpeed.Text = AGFormat$(UnitsDisplay(DS.Fields("TypSpeed"), UN_SPEED))
    .txtBiplSep.Text = AGFormat$(UnitsDisplay(DS.Fields("BiplSep"), UN_LENGTH))
    .txtWeight.Text = AGFormat$(UnitsDisplay(DS.Fields("Weight"), UN_MASS))
    .txtPlanArea.Text = AGFormat$(UnitsDisplay(DS.Fields("PlanArea"), UN_AREA))
    .txtPropRPM.Text = AGFormat$(DS.Fields("PropRPM"))
    .txtPropRad.Text = AGFormat$(UnitsDisplay(DS.Fields("PropRad"), UN_LENGTH))
    .txtEngVert.Text = AGFormat$(UnitsDisplay(DS.Fields("EngVert"), UN_LENGTH))
    .txtEngFwd.Text = AGFormat$(UnitsDisplay(DS.Fields("EngFwd"), UN_LENGTH))
    .txtNumEng.Text = AGFormat$(DS.Fields("NumEng"))
    FieldToArray DS.Fields("EngHoriz"), tmpEng()
    .txtEngHoriz(0).Text = AGFormat$(UnitsDisplay(tmpEng(0), UN_LENGTH))
    .txtEngHoriz(1).Text = AGFormat$(UnitsDisplay(tmpEng(1), UN_LENGTH))
    .txtWingVert.Text = AGFormat$(UnitsDisplay(DS.Fields("WingVert"), UN_LENGTH))
    .txtBoomVert.Text = AGFormat$(UnitsDisplay(DS.Fields("BoomVert"), UN_LENGTH))
    .txtBoomFwd.Text = AGFormat$(UnitsDisplay(DS.Fields("BoomFwd"), UN_LENGTH))
  End With

  TransferData = True
End Function

Private Sub UpdateNumRecs()
'Ensure the validity of RecordCount by moving to the
'last record. Also update the PositionCount variable
'that is global to this form

  'if BOF and EOF are true, there are no records
  If Not datAircraft.Recordset.BOF And Not datAircraft.Recordset.EOF Then
    datAircraft.Recordset.MoveLast
    datAircraft.Recordset.MoveFirst
    PositionCount = 1
  Else
    PositionCount = 0
  End If
  UpdatePositionCount
End Sub

Private Sub UpdatePositionCount()
'update the caption of the position label
  lblPosition.Caption = Format$(PositionCount) + " of " + _
                        Format$(datAircraft.Recordset.RecordCount) & " matches"
End Sub

Private Sub UpdatePropertyControls()
'Update the property display controls
  
  Dim s As String
  Dim temph(1) As Single
  Dim DS As Recordset

  Set DS = datAircraft.Recordset

  'If there is no current record, turn on the labels and exit
  If DS.EOF Then
    lblName.Caption = ""
    lblWingType.Caption = ""
    lblSemiSpan.Caption = ""
    lblTypSpeed.Caption = ""
    lblBiplSep.Caption = ""
    lblWeight.Caption = ""
    lblPlanArea.Caption = ""
    lblPropRPM.Caption = ""
    lblPropRad.Caption = ""
    lblEngVert.Caption = ""
    lblEngFwd.Caption = ""
    lblNumEng.Caption = ""
    lblEngHoriz(0).Caption = ""
    lblEngHoriz(1).Caption = ""
    lblWingVert.Caption = ""
    lblBoomVert.Caption = ""
    lblBoomFwd.Caption = ""
    
    Label2.Caption = "Semispan:"
    Label9.Caption = "Propeller RPM:"
    Label10.Visible = True
    lblPropRad.Visible = True
    lblPropRadUnits.Visible = True
    Label4.Visible = True
    lblBiplSep.Visible = True
    lblBiplSepUnits.Visible = True
    Label7.Visible = True
    lblPlanArea.Visible = True
    lblPlanAreaUnits.Visible = True
    Label12.Visible = True
    lblEngVert.Visible = True
    lblEngVertUnits.Visible = True
    Label13.Visible = True
    lblEngFwd.Visible = True
    lblEngFwdUnits.Visible = True
    Label8.Visible = True
    lblNumEng.Visible = True
    Label17.Visible = True
    lblEngHoriz(0).Visible = True
    lblEngHoriz(1).Visible = True
    lblEngHorizUnits.Visible = True
    Label18.Visible = True
    lblWingVert.Visible = True
    lblWingVertUnits.Visible = True
    Exit Sub
  End If

  'transfer the current record data to the controls
  lblName.Caption = DS.Fields("Name")
  lblSemiSpan.Caption = AGFormat$(UnitsDisplay(DS.Fields("SemiSpan"), UN_LENGTH))
  lblTypSpeed.Caption = AGFormat$(UnitsDisplay(DS.Fields("TypSpeed"), UN_SPEED))
  lblBiplSep.Caption = AGFormat$(UnitsDisplay(DS.Fields("BiplSep"), UN_LENGTH))
  lblWeight.Caption = AGFormat$(UnitsDisplay(DS.Fields("Weight"), UN_MASS))
  lblPlanArea.Caption = AGFormat$(UnitsDisplay(DS.Fields("PlanArea"), UN_AREA))
  lblPropRPM.Caption = AGFormat$(DS.Fields("PropRPM"))
  lblPropRad.Caption = AGFormat$(UnitsDisplay(DS.Fields("PropRad"), UN_LENGTH))
  lblEngVert.Caption = AGFormat$(UnitsDisplay(DS.Fields("EngVert"), UN_LENGTH))
  lblEngFwd.Caption = AGFormat$(UnitsDisplay(DS.Fields("EngFwd"), UN_LENGTH))
  lblNumEng.Caption = AGFormat$(DS.Fields("NumEng"))
  FieldToArray DS.Fields("EngHoriz"), temph()
  lblEngHoriz(0).Caption = AGFormat$(UnitsDisplay(temph(0), UN_LENGTH))
  lblEngHoriz(1).Caption = AGFormat$(UnitsDisplay(temph(1), UN_LENGTH))
  lblWingVert.Caption = AGFormat$(UnitsDisplay(DS.Fields("WingVert"), UN_LENGTH))
  lblBoomVert.Caption = AGFormat$(UnitsDisplay(DS.Fields("BoomVert"), UN_LENGTH))
  lblBoomFwd.Caption = AGFormat$(UnitsDisplay(DS.Fields("BoomFwd"), UN_LENGTH))
  
  'update the field labels and properties
  Select Case DS.Fields("Type")
    Case 3 'fixed wing
      lblWingType.Caption = "Fixed-wing"
      Label2.Caption = "Semispan:"
      Label9.Caption = "Propeller RPM:"
      Label10.Visible = True
      lblPropRad.Visible = True
      lblPropRadUnits.Visible = True
      Label4.Visible = True
      lblBiplSep.Visible = True
      lblBiplSepUnits.Visible = True
      Label7.Visible = True
      lblPlanArea.Visible = True
      lblPlanAreaUnits.Visible = True
      Label12.Visible = True
      lblEngVert.Visible = True
      lblEngVertUnits.Visible = True
      Label13.Visible = True
      lblEngFwd.Visible = True
      lblEngFwdUnits.Visible = True
      Label8.Visible = True
      lblNumEng.Visible = True
      Label17.Visible = True
      lblEngHoriz(0).Visible = True
      lblEngHoriz(1).Visible = True
      lblEngHorizUnits.Visible = True
      Label18.Visible = True
      lblWingVert.Visible = True
      lblWingVertUnits.Visible = True
    Case 4 'helicopter
      lblWingType.Caption = "Helicopter"
      Label2.Caption = "Rotor Radius:"
      Label9.Caption = "Rotor RPM:"
      Label10.Visible = False
      lblPropRad.Visible = False
      lblPropRadUnits.Visible = False
      Label4.Visible = False
      lblBiplSep.Visible = False
      lblBiplSepUnits.Visible = False
      Label7.Visible = False
      lblPlanArea.Visible = False
      lblPlanAreaUnits.Visible = False
      Label12.Visible = False
      lblEngVert.Visible = False
      lblEngVertUnits.Visible = False
      Label13.Visible = False
      lblEngFwd.Visible = False
      lblEngFwdUnits.Visible = False
      Label8.Visible = False
      lblNumEng.Visible = False
      Label17.Visible = False
      lblEngHoriz(0).Visible = False
      lblEngHoriz(1).Visible = False
      lblEngHorizUnits.Visible = False
      Label18.Visible = False
      lblWingVert.Visible = False
      lblWingVertUnits.Visible = False
  End Select
End Sub

Private Sub UpdateUnitsLabels()
  lblSemiSpanUnits.Caption = UnitsName(UN_LENGTH)
  lblWeightUnits.Caption = UnitsName(UN_MASS)
  lblTypSpeedUnits.Caption = UnitsName(UN_SPEED)
  lblPropRadUnits.Caption = UnitsName(UN_LENGTH)
  lblBiplSepUnits.Caption = UnitsName(UN_LENGTH)
  lblPlanAreaUnits.Caption = UnitsName(UN_AREA)
  lblEngVertUnits.Caption = UnitsName(UN_LENGTH)
  lblEngFwdUnits.Caption = UnitsName(UN_LENGTH)
  lblEngHorizUnits.Caption = UnitsName(UN_LENGTH)
  lblWingVertUnits.Caption = UnitsName(UN_LENGTH)
  lblBoomVertUnits.Caption = UnitsName(UN_LENGTH)
  lblBoomFwdUnits.Caption = UnitsName(UN_LENGTH)
End Sub

