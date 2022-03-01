VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsEditGrid"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'$Id: clsEditGridOld.cls,v 1.1 2003/07/03 17:06:00 tom Exp $
'This class works with a number of user-created
'controls to provide an editable table of numbers
'
'Usage:
'- Create a Grid and a TextBox on a form
'    grdTable, txtEditGrid
'- Create an instance of this class (form-global)
'    Public eg as New clsEditGrid
'- Connect the controls to the class
'    eg.Setup grdTable, txtEditGrid
'- Wire some Grid events to the class
'    Private Sub grdTable_DblClick()
'      eg.GridDblClick
'    End Sub
'
'    Private Sub grdTable_KeyDown(KeyCode As Integer, Shift As Integer)
'      eg.GridKeyDown KeyCode, Shift
'    End Sub
'
'    Private Sub grdTable_KeyPress(KeyAscii As Integer)
'      eg.GridKeyPress KeyAscii
'    End Sub
'- Wire some TextBox events to the class
'    Private Sub txtEditGrid_KeyDown(KeyCode As Integer, Shift As Integer)
'      eg.TextKeyDown KeyCode, Shift
'    End Sub
'
'    Private Sub txtEditGrid_KeyPress(KeyAscii As Integer)
'      eg.TextKeyPress KeyAscii
'      If KeyAscii = 13 then KeyAscii = 0
'    End Sub
'
'    Private Sub txtEditGrid_LostFocus()
'      eg.TextLostFocus
'    End Sub
'- (Optional) make some editing buttons
'    Private Sub cmdClear_Click()
'      eg.ClearSelected
'    End Sub
'
'    Private Sub cmdDelete_Click()
'      eg.DeleteRow
'    End Sub
'
'    Private Sub cmdInsert_Click()
'      eg.InsertRow
'    End Sub
'
Option Explicit

Private g As Grid        'The grid control associated
Private txt As TextBox   'A Text Box for editing
Private gRow As Integer  'The "current" row
Private gCol As Integer  'The "current" column
Private NoColumnsAdded As Boolean 'True until after first column
Private MaxRows As Integer 'Max number of rows permitted (0=unlimited)
Private NumberedRows As Boolean 'If True the first column is numbered
Private SortedRows As Boolean   'If True sort on first column
Private AscendingSort As Boolean 'If True do an ascending sort on the first column
'column flags
Private Editable() As Boolean  'if true, value can be edited

Private Sub Class_Initialize()
  Set g = Nothing
  Set txt = Nothing
End Sub

Public Sub Setup(inGrid As Grid, inText As TextBox, _
                 Optional inMaxRows As Variant, _
                 Optional inNumberedRows As Variant, _
                 Optional inSortedRows As Variant, _
                 Optional inAscendingSort As Variant)
  
  'Initialize class variables
  gRow = 0
  gCol = 0
  NoColumnsAdded = True 'becomes false after AddColumn is called
  MaxRows = 0
  NumberedRows = True
  SortedRows = True
  AscendingSort = True
  ReDim Editable(0): Editable(0) = False 'numbering column never editable
  
  'Connect the Grid and TextBox controls to this class instance
  Set g = inGrid
  Set txt = inText
  
  'modify class variables based on optional arguments
  If Not IsMissing(inMaxRows) Then MaxRows = CInt(inMaxRows)
  If Not IsMissing(inNumberedRows) Then NumberedRows = CBool(inNumberedRows)
  If Not IsMissing(inSortedRows) Then SortedRows = CBool(inSortedRows)
  If Not IsMissing(inAscendingSort) Then AscendingSort = CBool(inAscendingSort)
  
  'Init the text box
  txt.Visible = False  'make invisible until it is needed
  
  'Init the grid control
  With g
    .Rows = 2 'Must have one regular row besides fixed row
    .cols = 2 'Must have one regular column besides fixed column
    .FixedRows = 1 'Row 0: Headers
    .FixedCols = 1 'Column 0: Row numbers
    If MaxRows > 0 Then
      .Rows = MaxRows + .FixedRows
    Else
      .Rows = 1 + .FixedRows
    End If
    .FixedAlignment(0) = 2 'centered
'    .ColAlignment(0) = 2
  
    'fixed row height/column width
    .RowHeight(0) = 430  'set height of header row
    .ColWidth(0) = 500   'set width of first column
    .Row = 1
    .Col = 1
    gRow = .Row
    gCol = .Col
  End With
  Renumber
End Sub

Public Sub AddColumn(Header As String, Optional inEditable As Variant)
'Append a new column to the edit grid with the properties provided
  Dim saverow As Integer
  Dim savecol As Integer
  With g
    saverow = .Row
    savecol = .Col
    'add a new column (unless this is the first one,
    'inwhich case there is already a blank column
    'which we should use)
    If NoColumnsAdded Then   'First time through!
      NoColumnsAdded = False
    Else
      .cols = .cols + 1
    End If
    'header
    .Row = 0: .Col = .cols - 1
    .Text = Header
    .FixedAlignment(.cols - 1) = 2 'centered
    'Column variables
    ReDim Preserve Editable(.cols - 1)
    If IsMissing(inEditable) Then
      Editable(.cols - 1) = True
    Else
      Editable(.cols - 1) = CBool(inEditable)
    End If
    .Row = saverow
    .Col = savecol
  End With
End Sub

Public Sub Resize()
'Redo the grid columns according to the size of the control
  Dim gridwid As Single
  Dim col0wid As Single
  Dim wid As Single
  Dim i As Integer
  Dim saverow As Integer
  Dim savecol As Integer
  
  With g
    saverow = .Row
    savecol = .Col
    'compute width available *inside* grid (minus borders)
    gridwid = CSng(.Width)
    gridwid = gridwid - 325 'allow for scroll bar
    If .BorderStyle = 1 Then
      gridwid = gridwid - CSng(2 * Screen.TwipsPerPixelX)
    End If
    'compute the width of column zero (number column)
    col0wid = CSng(.ColWidth(0))
    If .GridLines Then
      col0wid = col0wid + CSng(.GridLineWidth * Screen.TwipsPerPixelX)
    End If
    'compute the width od the remaining columns
    wid = (gridwid - col0wid) / CSng(.cols - 1)
    'adjust the widths of the remaining columns
    For i = 1 To .cols - 1
      .ColWidth(i) = wid - (.GridLineWidth * Screen.TwipsPerPixelX)
    Next
    .Row = saverow
    .Col = savecol
  End With
End Sub

Private Sub Renumber()
'Redo the row numbering on the grid
  Dim saverow As Integer
  Dim savecol As Integer
  Dim i As Integer
  
  saverow = g.Row
  savecol = g.Col

  g.Col = 0
  For i = 1 To g.Rows - 1
    g.Row = i
    g.Text = Format(i)
  Next

  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub GridDblClick()
'Bind this function to the DblClick event of the grid control
  EditCell
End Sub

Public Sub GridKeyDown(KeyCode As Integer, Shift As Integer)
'Bind this function to the KeyDown event of the grid control
  'PgUp and PgDn mess up the grid control
  If KeyCode = 33 Or KeyCode = 34 Then
    KeyCode = 0
  End If
End Sub

Public Sub GridKeyPress(KeyAscii As Integer)
'Bind this function to the KeyPress event of the grid control
  Select Case KeyAscii
  Case Asc(vbCr)
    NextCell
  Case 27 'Escape
    'do nothing, the host form might use escape
    'as a "cancel" key
  Case Else
    EditCell KeyAscii
  End Select
End Sub

Public Sub TextKeyDown(KeyCode As Integer, Shift As Integer)
'Bind this function to the KeyDown event of the text control
'Change the function of some special keys
  Select Case KeyCode
  Case vbKeyDown
    g.SetFocus ' Set focus back to grid, see txt_LostFocus.
    SendKeys "{DOWN}" 'send a downarrow to the grid
  Case vbKeyUp
    g.SetFocus ' Set focus back to grid, see txt_LostFocus.
    SendKeys "{UP}"  'send an uparrow to the grid
  End Select
End Sub

Public Sub TextKeyPress(KeyAscii As Integer)
'Bind this function to the KeyPress event of the text control
  Select Case KeyAscii
  Case 13    'Enter
    g.SetFocus ' Set focus back to grid, see txt_LostFocus.
    NextCell
  Case 27 'Escape
    'cancel edit
    txt.Text = g.Text 'revert text to original
    g.SetFocus ' Set focus back to grid, see txt_LostFocus.
  End Select
  'Note: don't set KeyAscii to 0 here because the
  'host form might want to test for it.
End Sub

Public Sub TextLostFocus()
'Bind this function to the LostFocus event of the text control
  Dim tmpRow As Integer
  Dim tmpCol As Integer

  ' Save current settings of Grid Row and col. This is needed only if
  ' the focus is set somewhere else in the Grid.
  tmpRow = g.Row
  tmpCol = g.Col

  ' Set Row and Col back to what they were before Text1_LostFocus:
  g.Row = gRow
  g.Col = gCol

  g.Text = txt.Text  ' Transfer text back to grid.
  txt.SelStart = 0       ' Return caret to beginning.
  txt.Visible = False    ' Disable text box.

  'Return row and Col contents:
  g.Row = tmpRow
  g.Col = tmpCol
End Sub

Public Sub NextCell()
'Advance rowwise to the next editable cell
  If g.Col < g.cols - 1 Then
    g.Col = g.Col + 1
  Else
    If g.Row < g.Rows - 1 Then
      g.Col = 1
      g.Row = g.Row + 1
    Else
      Beep
    End If
  End If
  g.SelStartCol = g.Col
  g.SelEndCol = g.Col
  g.SelStartRow = g.Row
  g.SelEndRow = g.Row
End Sub

Public Sub ArrayToGrid(Col As Integer, nval As Integer, gval() As Single, _
                       Optional units As Variant)
'Transfer the given array to the grid control
'col - the desired grid column
'nval - the number of values to transfer
'gval - the values to transfer
'units - the units flag of the values
'
  Dim i As Integer
  Dim saverow As Integer
  Dim savecol As Integer
  
  'transfer the array elements to the grid control
  saverow = g.Row
  savecol = g.Col
  If Not IsMissing(units) Then
    For i = 0 To nval - 1
      g.Row = i + 1
      g.Col = Col
      g.Text = AGFormat$(UnitsDisplay(gval(i), CInt(units)))
    Next
  Else
    For i = 0 To nval - 1
      g.Row = i + 1
      g.Col = Col
      g.Text = AGFormat$(gval(i))
    Next
  End If
  'clean out the rest of the grid control
  For i = nval + 1 To g.Rows - 1
    g.Row = i
    g.Col = Col
    g.Text = ""
  Next
  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub ArrayToGridI(Col As Integer, nval As Integer, gval() As Integer)
'Transfer the given array of integers to the grid control
'col - the desired grid column
'nval - the number of values to transfer
'gval - the values to transfer
'
  Dim i As Integer
  Dim saverow As Integer
  Dim savecol As Integer
  
  'transfer the array elements to the grid control
  saverow = g.Row
  savecol = g.Col
  For i = 0 To nval - 1
    g.Row = i + 1
    g.Col = Col
    g.Text = Format(gval(i))
  Next
  'clean out the rest of the grid control
  For i = nval + 1 To g.Rows - 1
    g.Row = i
    g.Col = Col
    g.Text = ""
  Next
  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub GridToArray(Col As Integer, nval As Integer, gval() As Single, _
                       Optional unitsi As Variant)
'Transfer the contents of the grid control to an array
'col - the desired grid column
'nval - the number of values to transfer
'gval - the values to transfer
'units - the units flag of the values
'
  Dim i As Integer
  Dim j As Integer
  Dim RowIsBlank As Boolean
  Dim saverow As Integer
  Dim savecol As Integer
  Dim units As Integer
  
  If IsMissing(unitsi) Then
    units = UN_NONE
  Else
    units = CInt(unitsi)
  End If
  
  saverow = g.Row
  savecol = g.Col
  nval = 0
  For i = 1 To g.Rows - 1
    g.Row = i 'set current row
    'check for blank rows (all editable columns are blank)
    RowIsBlank = True
    For j = 1 To g.cols - 1
      If Editable(j) Then
        g.Col = j
        If Trim$(g.Text) <> "" Then
          RowIsBlank = False
          Exit For
        End If
      End If
    Next
    If Not RowIsBlank Then
      g.Col = Col
      gval(nval) = UnitsInternal(Val(g.Text), units)
      nval = nval + 1
    End If
  Next
  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub GridToArrayI(Col As Integer, nval As Integer, gval() As Integer)
'Transfer the contents of the grid control to an array of integers
'col - the desired grid column
'nval - the number of values to transfer
'gval - the values to transfer
'units - the units flag of the values
'
  Dim i As Integer
  Dim j As Integer
  Dim RowIsBlank As Boolean
  Dim saverow As Integer
  Dim savecol As Integer
  
  saverow = g.Row
  savecol = g.Col
  nval = 0
  For i = 1 To g.Rows - 1
    g.Row = i 'set current row
    'check for blank rows (all editable columns are blank)
    RowIsBlank = True
    For j = 1 To g.cols - 1
      If Editable(j) Then
        g.Col = j
        If Trim$(g.Text) <> "" Then
          RowIsBlank = False
          Exit For
        End If
      End If
    Next
    If Not RowIsBlank Then
      g.Col = Col
      gval(nval) = Val(g.Text)
      nval = nval + 1
    End If
  Next
  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub ClearAll()
'blank out all the grid rows
  Dim i As Integer, j As Integer
  Dim saverow As Integer
  Dim savecol As Integer
  
  saverow = g.Row
  savecol = g.Col
  For i = g.Rows - 1 To 1 Step -1
    g.Row = i
    For j = 0 To g.cols - 1
      g.Col = j
      g.Text = ""
    Next
  Next
  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub ClearSelected()
'clear the selected cells in a grid
  Dim saverow As Integer, savecol As Integer
  Dim ir As Integer, ic As Integer
  
  saverow = g.Row
  savecol = g.Col
  For ic = g.SelStartCol To g.SelEndCol
    g.Col = ic
    For ir = g.SelStartRow To g.SelEndRow
      g.Row = ir
      g.Text = ""
    Next
  Next
  g.Row = saverow
  g.Col = savecol
End Sub

Public Sub DeleteRow()
'Delete the selected row(s) from the grid
'and add a new blank one to the end
'
  Dim saverow As Integer
  Dim saverows As Integer
  Dim i As Integer
  Dim iStart As Integer
  Dim iEnd As Integer
  
  saverow = g.Row
  saverows = g.Rows          'save the original num of rows
  iStart = g.SelStartRow
  For i = 1 To g.SelEndRow - g.SelStartRow + 1
    g.RemoveItem iStart      'delete the current row
    g.Rows = saverows        'add blank rows to the end
  Next
  g.Row = saverow
  g.SelStartRow = saverow
  g.SelEndRow = saverow
  Renumber                'renumber the grid
End Sub

Public Sub EditCell(Optional KeyAscii As Variant)
'Start editing the current grid cell
'KeyAscii - an initial key code to pass to the text box. This is
'           useful if a trapped keypress initiated the edit.
'
  If Not Editable(g.Col) Then Beep: Exit Sub
  
  'Move the text box to the current grid cell:
  PositionTextBox

  'Save the position of the grids Row and Col for later:
  gRow = g.Row
  gCol = g.Col

  'Make text box same size as current grid cell:
  txt.Width = g.ColWidth(g.Col) - 2 * Screen.TwipsPerPixelX
  txt.Height = g.RowHeight(g.Row) - 2 * Screen.TwipsPerPixelY

  'Transfer the grid cell text:
  txt.Text = g.Text
  txt.SelStart = 0
  txt.SelLength = Len(g.Text)
  
  'Show the text box:
  txt.Visible = True
  txt.ZOrder 0
  txt.SetFocus

  'Redirect this KeyPress event to the text box:
  If Not IsMissing(KeyAscii) Then
    If KeyAscii <> 13 Then 'Enter
       SendKeys Chr$(KeyAscii)
    End If
  End If
End Sub

Public Sub InsertRow()
'Insert a blank row of cells in a grid above the current row
  Dim saverow As Integer
  Dim s1 As String, s2 As String
  
'tbc fix this - relies on two columns
  saverow = g.Row
  g.Row = g.Rows - 1   'move to the end of the table
  g.Col = 1
  s1 = g.Text
  g.Col = 2
  s2 = g.Text
  If s1 = "" And s2 = "" Then
    g.RemoveItem g.Rows - 1  'remove the last row
    g.AddItem "", saverow          'add a new row
  End If
  g.Row = saverow
  Renumber
End Sub

Private Sub PositionTextBox()
'Move the txt Text to cover the current grid cell
  Dim X As Single   ' x position of current grid cell.
  Dim Y As Single   ' y position of current grid cell.
  Dim i As Integer  ' Column/row index.

  'Skip grid border:
  X = g.Left
  Y = g.Top
  If g.BorderStyle = 1 Then
    X = X + Screen.TwipsPerPixelX
    Y = Y + Screen.TwipsPerPixelY
  End If

  ' Skip fixed columns and rows:
  For i = 0 To g.FixedCols - 1
    X = X + g.ColWidth(i)
    If g.GridLines Then
      X = X + Screen.TwipsPerPixelX
    End If
  Next
  For i = 0 To g.FixedRows - 1
    Y = Y + g.RowHeight(i)
    If g.GridLines Then
      Y = Y + Screen.TwipsPerPixelY
    End If
  Next

  'Find current data cell:
  For i = g.LeftCol To g.Col - 1
    X = X + g.ColWidth(i)
    If g.GridLines Then
      X = X + Screen.TwipsPerPixelX
    End If
  Next
  For i = g.TopRow To g.Row - 1
    Y = Y + g.RowHeight(i)
    If g.GridLines Then
      Y = Y + Screen.TwipsPerPixelY
    End If
  Next

  'Move the Text Box, and make small adjustments:
  txt.Move X + Screen.TwipsPerPixelX, Y + Screen.TwipsPerPixelY
End Sub

