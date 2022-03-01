Attribute VB_Name = "basPLOT"
' $Id: basPlot.bas,v 1.6 2016/09/13 13:19:58 tom Exp $
'PLOT.BAS - These routine define and manipulate
'           2D plot data

'**************** User-defined constants *****************
'Plot type
Global Const GQ_XYPLOT = 0
Global Const GQ_ISOPLETH = 1
Global Const GQ_COLORMAP = 2
Global Const GQ_BAR = 3

'Special point values
Global Const GQ_DRAWTO = 0  'draw from the last point to this point
Global Const GQ_MOVETO = 1  'mov to this point without drawing
'**************** User-defined data types ****************
'define a data type to hold the plot data
Type PlotXYdata
  n As Integer    'number of points
  X() As Single   'X locations
  Y() As Single   'Y locations
  Attr() As Byte  'Point Attributes
  Lbl() As String 'Point labels
  Xmin As Single
  Xmax As Single
  Ymin As Single
  Ymax As Single
End Type

'define a type for font information
Type FontData
  Name As String         'FontName
  Size As Single         'FontSize
  Bold As Integer        'FontBold
  Italic As Integer      'FontItalic
  Underline As Integer   'FontUnderline
  Strikethru As Integer  'FontStrikethru
  Transparent As Integer 'FontTransparent
  Color As Long          'Color
End Type

'define a type for the three plot titles
Type TitleData
  Text As String         'the text of the title
  Font As FontData       'Font info for the title
  PosX As Single         'relative X position of title
  PosY As Single         'relative Y position of title
End Type

'define a type for the Legend
Type LegendData
  On As Integer          'If true, the legend appears
  Font As FontData       'Font info for the Legend text
  PosX As Single         'relative X position of Legend
  PosY As Single         'relative Y position of Legend
End Type

'define a data type to hold Plot Settings
Type PlotSettingData
  'General
  Caption As String          'Caption for Plot Form
  HelpID As Long             'HelpContextID
  Changed As Integer         'Tracks User changes to settings
  PlotType As Integer        'Plot type 0=XY 1=isopleth 2=colormap 3=bar 4=pie
  'Viewport sizes
  XDS As Single              'X Start of Viewport, Device coords
  XDE As Single              'X End of Viewport, Device coords
  YDS As Single              'Y Start of Viewport, Device coords
  YDE As Single              'Y End of Viewport, Device coords
  'Titles
  RunTitle As TitleData      'Run title
  PlotTitle As TitleData     'Plot title
  'Plot Data additional information
  DataTitle(4) As String     'Text for data in legend
  DataSource(4)  As String   'names of addt'l data files
  DataColor(4) As Long       'color value of each data set
  DataStyle(4) As Integer    'line style of each data set
  'X (horizontal) axis
  XTitle As TitleData        'Title for X axis
  Xunits As String           'Units string to be added to title
  Xauto As Integer           'set to TRUE for autoscaling
  Xmin As Single             'lower bound of axis
  Xmax As Single             'upper bound of axis
  Xinc As Single             'distance between tics
  XminorTics As Integer      'number of minor tics between major tics
  Xgrid As Integer           'set to true to show grid lines
  Xlog As Integer            'set to true for log scale
  XScaleFont As FontData     'font data for scale labels
  'Y (vertical) axis
  YTitle As TitleData        'Title for Y axis
  Yunits As String           'Units string to be added to title
  Yauto As Integer           'set to TRUE for autoscaling
  Ymin As Single             'lower bound of axis
  Ymax As Single             'upper bound of axis
  Yinc As Single             'distance between tics
  YminorTics As Integer      'number of minor tics between
  Ygrid As Integer           'set to true to show grid lines
  Ylog As Integer            'set to true for log scale
  YScaleFont As FontData     'font data for scale labels
  'Legend
  Legend As LegendData       'Plot legend
  'Note
  Note As TitleData
End Type

'******************** Global definitions *****************
Public PD(4) As PlotXYdata
Public PS As PlotSettingData

Public Sub PlotGraph(dest As Control)
'display graphics that go in the picPlot area
'
' dest is the plot destination control
'
  On Error GoTo PlotGraphErrHand

  Dim XDS As Single
  Dim XDE As Single
  Dim YDS As Single
  Dim YDE As Single

  Dim i As Integer
  Dim s As String
  Dim FoundData As Integer
  Dim IDfont As FontData
'
'Init the graphics system
'
  If Not (dest Is Printer) Then dest.Cls
  initq dest
'
'tbc change this to generic LeftNote
' Print the Run ID in the lower left corner
'
  IDfont.Name = "Arial"
  IDfont.Size = 8
  IDfont.Bold = True
  IDfont.Italic = False
  IDfont.Underline = False
  IDfont.Strikethru = False
  IDfont.Transparent = False
  IDfont.Color = vbBlack
'
' Check the plot data for potential problems
' Do this before autoscaling in case points are removed
'
  PlotCheckData
'
' Set up autoscaling
'
  'Only do autoscaling for GQ_XYPLOTs. Bar charts always override
  If PS.Xauto Then
    Select Case PS.PlotType
    Case GQ_XYPLOT
      FoundData = False 'Set to true when data is first found
      For i = 0 To 4 'scan the data sets
        If PD(i).n > 0 Then
          If FoundData Then
            If PD(i).Xmin < PS.Xmin Then PS.Xmin = PD(i).Xmin
            If PD(i).Xmax > PS.Xmax Then PS.Xmax = PD(i).Xmax
          Else
            PS.Xmin = PD(i).Xmin
            PS.Xmax = PD(i).Xmax
            FoundData = True
          End If
        End If
      Next
      If Not FoundData Then
        PS.Xmin = 0
        PS.Xmax = 0
      End If
      PS.XminorTics = 1
      Call autoscq(PS.Xlog, PS.Xmin, PS.Xmax, PS.Xinc)
    Case GQ_BAR
      PS.Xmin = 0
      PS.Xmax = 1
      PS.Xinc = 0.1
    End Select
  End If
    
  If PS.Yauto Then
    Select Case PS.PlotType
    Case GQ_XYPLOT, GQ_BAR
      FoundData = False 'Set to true when data is first found
      For i = 0 To 4 'scan the rest of the data sets
        If PD(i).n > 0 Then
          If FoundData Then
            If PD(i).Ymin < PS.Ymin Then PS.Ymin = PD(i).Ymin
            If PD(i).Ymax > PS.Ymax Then PS.Ymax = PD(i).Ymax
          Else
            PS.Ymin = PD(i).Ymin
            PS.Ymax = PD(i).Ymax
            FoundData = True
          End If
        End If
      Next
      If Not FoundData Then
        PS.Ymin = 0
        PS.Ymax = 0
      End If
      PS.YminorTics = 1
      'special case: force min of zero for bar charts
      If PS.PlotType = GQ_BAR Then PS.Ymin = 0
      Call autoscq(PS.Ylog, PS.Ymin, PS.Ymax, PS.Yinc)
    End Select
  End If
'
' plot the RunID
'
  s = GetRunID()
  PlotSetFont IDfont, dest
  dest.CurrentX = 50
  dest.CurrentY = frmPlot.picArea.Height - dest.TextHeight(s) - 50
  dest.Print s
'
' Define the view and draw the axes
'
  Call viewq(PS.XDS, PS.XDE, PS.YDS, PS.YDE, PS.Xmin, PS.Xmax, PS.Ymin, PS.Ymax, GQ_BOX)
  PlotSetFont PS.XScaleFont, dest
  Select Case PS.PlotType
  Case GQ_XYPLOT
    Call axisq(GQ_XAXIS, PS.Xlog, PS.Xgrid, PS.Xinc, PS.XminorTics)
  Case GQ_BAR
    Call axisq(GQ_XAXIS, False, False, 0, 0)
  End Select
  PlotSetFont PS.YScaleFont, dest
  Call axisq(GQ_YAXIS, PS.Ylog, PS.Ygrid, PS.Yinc, PS.YminorTics)
'
' Plot the data if there is any, say so if there isn't
'
  FoundData = False 'Set to true when data is first found
  For i = 0 To 4 'scan the rest of the data sets
    If PD(i).n > 0 Then FoundData = True: Exit For
  Next
  If FoundData Then 'there is data to plot
    'plot the data
    Select Case PS.PlotType
    Case GQ_XYPLOT
      For i = 0 To 4
        If (PD(i).n > 0) Then
          Call colorq(PS.DataColor(i))
          Call styleq(PS.DataStyle(i))
          PlotXYdata i
        End If
      Next
      Call colorq(0) 'Set color back to black
      Call styleq(0) 'Set style back to solid
'tbc    Case GQ_ISOPLETH
'tbc      PlotContourData 0
'tbc    Case GQ_COLORMAP
'tbc      PlotContourData 1
    Case GQ_BAR
      For i = 0 To 4
        If (PD(i).n > 0) Then
'tbc changing the color here sets one color for all bars on one
'tbc set of data. We changed this to color individual bars.
'tbc          Call colorq(PS.DataColor(i))
'tbc          Call styleq(PS.DataStyle(i))
          PlotBarData i
        End If
      Next
      Call colorq(0) 'Set color back to black
      Call styleq(0) 'Set style back to solid
    End Select
  
  Else 'there is no data to plot
    colorq 0 'Set color back to black
    styleq 0 'Set style back to solid
    textq "No Data", GQ_DEVICE, _
          (PS.XDS + PS.XDE) * 0.5, (PS.YDS + PS.YDE) * 0.5, _
          GQ_ALIGN_CENTER, GQ_ALIGN_CENTER
  End If
'
' plot the labels if we are printing
'
'tbc fix this - remove reliance on plot form
  PlotLabel dest, frmPlot.lblRunTitle
  PlotLabel dest, frmPlot.lblPlotTitle
  PlotLabel dest, frmPlot.lblXaxis
  PlotLabel dest, frmPlot.lblYaxis
'
' plot a legend
'
  frmPlot.PlotLegend dest

PlotGraphExit:
  Exit Sub

PlotGraphErrHand:
  MsgBox "Error " + Format$(Err.Number) + " during plotting: " + Err.Description
  Resume PlotGraphExit

End Sub

Private Sub PlotCheckData()
'Check the plot data for potential problems. This is mostly for
'Log scales. If we have a log scale and there is nonpositive data
'for it, remove the errant data and rescale
  
  Dim problem As Integer
  Dim seeded As Integer
  Dim i As Integer
  Dim j As Integer
  Dim jp As Integer
  Dim s As String

  'init flag for problem identification
  problem = False

  'log scale limit check
  'if Log scales are selected for either axis, check the
  'data min/max for nonpositive values. If there are some
  'present, sweep through the data to remove them. The
  'min/max values must be reset too.
  s = "Zero or negative data will be removed for log scales."
  If PS.Xlog = GQ_LOG Then
    For i = 0 To 4             'loop over all slots
      If PD(i).n > 0 Then      'if data is present
        If (PD(i).Xmin < 1E-36) Or (PD(i).Xmax < 1E-36) Then
          problem = True            'set problem flag
          jp = 0                    'init pointer
          seeded = False            'true when min/max is seeded
          'sweep through data, removing bad points
          For j = 0 To PD(i).n - 1
            If (PD(i).X(j) >= 1E-36) Then
              PD(i).X(jp) = PD(i).X(j)
              If seeded Then
                If (PD(i).X(jp) < PD(i).Xmin) Then PD(i).Xmin = PD(i).X(jp)
                If (PD(i).X(jp) > PD(i).Xmax) Then PD(i).Xmax = PD(i).X(jp)
              Else
                PD(i).Xmin = PD(i).X(0)   'seed min
                PD(i).Xmax = PD(i).X(0)   'seed max
                seeded = True
              End If
              jp = jp + 1
            End If
          Next
          PD(i).n = jp 'reset data counter
        End If
      End If
    Next
  End If
  If PS.Ylog = GQ_LOG Then
    For i = 0 To 4          'loop over all slots
      If PD(i).n > 0 Then   'if data is present
        If (PD(i).Ymin < 1E-36) Or (PD(i).Ymax < 1E-36) Then
          problem = True            'set problem flag
          jp = 0                    'init pointer
          seeded = False            'true when min/max is seeded
          'sweep through data, removing bad points
          For j = 0 To PD(i).n - 1
            If (PD(i).Y(j) >= 1E-36) Then
              PD(i).Y(jp) = PD(i).Y(j)
              If seeded Then
                If (PD(i).Y(jp) < PD(i).Ymin) Then PD(i).Ymin = PD(i).Y(jp)
                If (PD(i).Y(jp) > PD(i).Ymax) Then PD(i).Ymax = PD(i).Y(jp)
              Else
                PD(i).Ymin = PD(i).Y(0)   'seed min
                PD(i).Ymax = PD(i).Y(0)   'seed max
                seeded = True
              End If
              jp = jp + 1
            End If
          Next
          PD(i).n = jp 'reset data counter
        End If
      End If
    Next
  End If

  'alert the user if we removed data
  If problem Then MsgBox s, vbExclamation

End Sub

Public Sub PlotLabel(dest As Control, Lbl As Label)
'Plot a label
'
' dest is the plot destination
' lbl  is the label to plot
'
  Dim s As String
  Dim spos As Integer
  Dim crpos As Integer
  Dim lblCenter As Single
  
  On Error GoTo PlotLabelErrHand

  If Lbl.Caption = "" Then Exit Sub
  
  dest.FontName = Lbl.FontName
  dest.FontSize = Lbl.FontSize
  dest.FontBold = Lbl.FontBold
  dest.FontItalic = Lbl.FontItalic
  dest.FontUnderline = Lbl.FontUnderline
  dest.FontStrikethru = Lbl.FontStrikethru
  dest.ForeColor = Lbl.ForeColor

  lblCenter = Lbl.Left + Lbl.Width * 0.5
  dest.CurrentY = Lbl.Top
  spos = 1  'init starting position
  Do
    crpos = InStr(spos, Lbl.Caption, vbCrLf) 'search for crlf
    If crpos > 0 Then
      s = Mid$(Lbl.Caption, spos, crpos - spos)
      dest.CurrentX = lblCenter - 0.5 * dest.TextWidth(s)
      dest.Print s;
      dest.CurrentY = dest.CurrentY + dest.TextHeight(s)
      spos = crpos + 2
    Else
      If spos < Len(Lbl.Caption) Then
        s = Mid(Lbl.Caption, spos)
        dest.CurrentX = lblCenter - 0.5 * dest.TextWidth(s)
        dest.Print s;
      End If
      Exit Do
    End If
  Loop

PlotLabelExit:
  Exit Sub
  
PlotLabelErrHand:
  MsgBox "Error " + Format$(Err.Number) + " during printing: " + Err.Description
  Resume PlotLabelExit

End Sub

Private Sub PlotXYmarkers(i As Integer)
'Plot data set
  Dim ip As Integer
  
  ip = 0 'start at the first point
  
  'locate and mark the next good point
  'if we have log scales, skip out of range points
  While (ip <= PD(i).n - 1)
    If Not ((PS.Xlog And (PD(i).X(ip) < 1E-36)) Or (PS.Ylog And (PD(i).Y(ip) < 1E-36))) Then
      Call markerq(GQ_WORLD, PD(i).X(ip), PD(i).Y(ip))
    End If
    ip = ip + 1 'advance the pointer
  Wend
End Sub

Private Sub PlotXYdata(slot As Integer)
'Plot data set
'
' i is the slot to plot (how poetic)
'
  Dim i As Integer
  
  'log plots filter out bad points
  If PS.Xlog Or PS.Xlog Then
    For i = 0 To PD(slot).n - 1
      If Not ((PS.Xlog And (PD(slot).X(i) < 1E-36)) Or _
              (PS.Ylog And (PD(slot).Y(i) < 1E-36))) Then
        Select Case PD(slot).Attr(i)
        Case GQ_MOVETO
          moveq GQ_WORLD, PD(slot).X(i), PD(slot).Y(i)
        Case GQ_DRAWTO
          drawq GQ_WORLD, PD(slot).X(i), PD(slot).Y(i)
        End Select
      Else
'tbc need code to cause breaks for bad data
      End If
    Next
  Else
    For i = 0 To PD(slot).n - 1
      Select Case PD(slot).Attr(i)
      Case GQ_MOVETO
        moveq GQ_WORLD, PD(slot).X(i), PD(slot).Y(i)
      Case GQ_DRAWTO
        drawq GQ_WORLD, PD(slot).X(i), PD(slot).Y(i)
      End Select
    Next
  End If
End Sub

Private Sub PlotBarData(slot As Integer)
'Plot data set as a bar chart
'
' i is the slot to plot (how poetic)
' The X values are ignored. The Y values
' determine the height of each bar

  Dim i As Integer
  Dim nbars As Integer
  Dim barwidth As Single
  Dim X1 As Single, Y1 As Single
  Dim X2 As Single, Y2 As Single
  Dim barlabel As String
  
  nbars = PD(slot).n
  barwidth = 1! / (2! * CSng(nbars) + 1!)
  
  For i = 0 To nbars - 1
    'Set the colors here to change the colors on individual bars
    'The original setup set all bars for one data set to be the same
    Call colorq(PS.DataColor(i))
'tbc    Call styleq(PS.DataStyle(i))
    'define lower right corner
    X1 = (2 * i + 1) * barwidth
    Y1 = 0
    'define the upper right corner
    X2 = X1 + barwidth
    Y2 = PD(slot).Y(i)
    'draw a box
    boxq GQ_WORLD, X1, Y1, X2, Y2, True
    'label the bar
    X1 = (2 * i + 1.5) * barwidth
    Y1 = 0
    textq PD(slot).Lbl(i), GQ_WORLD, X1, Y1, GQ_ALIGN_CENTER, GQ_ALIGN_TOP
  Next
End Sub

Public Sub PlotSetFont(FD As FontData, ctrl As Control)
'setup font information on the output control
'
' FD is the Font Data to which to set the output device
'
 
  Dim s As String

  On Error GoTo PlotSetFontErrHand
'
  Dim ErrKey As Integer
'
  ErrKey = 0 'used to identify the location of an error

  ErrKey = 1: ctrl.FontName = FD.Name
  ErrKey = 2: ctrl.FontSize = FD.Size
  ErrKey = 3: ctrl.FontBold = FD.Bold
  ErrKey = 4: ctrl.FontItalic = FD.Italic
  ErrKey = 5: ctrl.FontUnderline = FD.Underline
  ErrKey = 6: ctrl.FontStrikethru = FD.Strikethru
  ErrKey = 7: ctrl.ForeColor = FD.Color
  Exit Sub

PlotSetFontErrHand:
  Select Case Err
  Case 380 'Invalid Property Value
    If dest Is Printer Then
      s = "Could not set printer font "
    ElseIf dest = THISFORM Then
      s = "Could not set display font "
    End If
    Select Case ErrKey
    Case 1
      s = s + "to " + Chr$(34) + FD.Name + Chr$(34)
    Case 2
      s = s + "size to " + Format$(FD.Size)
    Case 3
      s = s + "Bold property."
    Case 4
      s = s + "Italic property."
    Case 5
      s = s + "Underline property."
    Case 6
      s = s + "Strikethru property."
    Case 7
      s = s + "color to &H" & Hex$(FD.Color)
    End Select
    MsgBox s, vbExclamation + vbOKOnly
    Resume Next
  Case Else
    Select Case UnexpectedError("PlotSetFont")
    Case vbAbort  'Abort - Stop the whole program
      End
    Case vbRetry  'Retry - Resume at the same line
      Resume
    Case vbIgnore 'Ignore - Resume at the next line
      Resume Next
    End Select
  End Select
End Sub

Sub PlotDataAddPoint(slot As Integer, X As Single, Y As Single, _
                     Attr As Byte, _
                     Optional Lbli)
'Add a point to the plot data
' Slot - slot number. 0-4
  Dim Lbl As String
  Dim inew As Integer
  
  If IsMissing(Lbli) Then
    Lbl = ""
  Else
    Lbl = CStr(Lbli)
  End If
  
  inew = PD(slot).n               'get array position for the new point
  PD(slot).n = inew + 1           'increment the point counter
  ReDim Preserve PD(slot).X(inew) 'allocate space for the new point
  ReDim Preserve PD(slot).Y(inew) 'allocate space for the new point
  ReDim Preserve PD(slot).Attr(inew) 'allocate space for the new point
  ReDim Preserve PD(slot).Lbl(inew) 'allocate space for the new point
  PD(slot).X(inew) = X            'store the new point
  PD(slot).Y(inew) = Y            'store the new point
  PD(slot).Attr(inew) = Attr      'store the new point
  PD(slot).Lbl(inew) = Lbl        'store the new point
  'update mins and maxs
  If inew = 0 Then     'init mins and maxs if first point
    PD(slot).Xmax = X
    PD(slot).Xmin = X
    PD(slot).Ymax = Y
    PD(slot).Ymin = Y
  Else
    If X > PD(slot).Xmax Then PD(slot).Xmax = X
    If X < PD(slot).Xmin Then PD(slot).Xmin = X
    If Y > PD(slot).Ymax Then PD(slot).Ymax = Y
    If Y < PD(slot).Ymin Then PD(slot).Ymin = Y
  End If
End Sub

Sub PlotDataAddPoints(slot As Integer, _
                      np As Integer, X() As Single, Y() As Single, _
                      Lbl() As String)
'Add a set of points to the plot data
' Slot - slot number. 0-4
  Dim inew As Integer
  '
  If np <= 0 Then Exit Sub
  inew = PD(slot).n               'get starting array position for new points
  PD(slot).n = inew + np          'increment the point counter
  ReDim Preserve PD(slot).X(inew + np - 1) 'allocate space
  ReDim Preserve PD(slot).Y(inew + np - 1) 'allocate space
  ReDim Preserve PD(slot).Attr(inew + np - 1) 'allocate space
  ReDim Preserve PD(slot).Lbl(inew + np - 1) 'allocate space
  'init mins and maxs if first point
  If inew = 0 Then
    PD(slot).Xmax = X(0)
    PD(slot).Xmin = X(0)
    PD(slot).Ymax = Y(0)
    PD(slot).Ymin = Y(0)
  End If
  'store new points
  For i = 0 To np - 1
    PD(slot).X(inew + i) = X(i)
    PD(slot).Y(inew + i) = Y(i)
    PD(slot).Attr(inew + i) = GQ_DRAWTO 'First point fixed below
    PD(slot).Lbl(inew + i) = Lbl(i)
    'update mins and maxs
    If X(i) > PD(slot).Xmax Then PD(slot).Xmax = X(i)
    If X(i) < PD(slot).Xmin Then PD(slot).Xmin = X(i)
    If Y(i) > PD(slot).Ymax Then PD(slot).Ymax = Y(i)
    If Y(i) < PD(slot).Ymin Then PD(slot).Ymin = Y(i)
  Next
'tbc
  PD(slot).Attr(inew) = GQ_MOVETO
End Sub

Sub PlotXYDataReset()
'Clear out existing plot data in all slots
  Dim i As Integer
  For i = 0 To 4
    PlotXYDataResetSlot i
  Next
End Sub

Sub PlotXYDataResetSlot(slot As Integer)
'Reset plot data in a particular slot
  PD(slot).n = 0
  ReDim PD(slot).X(0)
  ReDim PD(slot).Y(0)
  ReDim PD(slot).Attr(0)
  ReDim PD(slot).Lbl(0)
End Sub

Sub PlotGetDataN(slot As Integer, n As Integer)
'return the current number of points in the given slot
'
' slot   i   the slot from which to get the data
' n      o   the number of data points returned
'
  n = PD(slot).n
End Sub

Sub PlotGetDataSlot(slot As Integer, n As Integer, xdat() As Single, ydat() As Single, attrdat() As Byte)
'return the current plot data arrays for the given slot
'
' slot   i   the slot from which to get the data
' n      o   the number of data points returned
' xdat   o   array of x points
' ydat   o   array of y points
'
  n = PD(slot).n
  For i = 0 To n - 1
    xdat(i) = PD(slot).X(i)
    ydat(i) = PD(slot).Y(i)
    attrdat(i) = PD(slot).Attr(i)
  Next
End Sub

Function PlotGetDataSource(i) As String
'get the DataSource value for plot data slot i
  PlotGetDataSource = PS.DataSource(i)
End Function

Function PlotGetDataTitle(i) As String
'get the DataTitle for slot i
  PlotGetDataTitle = PS.DataTitle(i)
End Function

Sub PlotOptionsDialog()
'bring up the plot options dialog
  frmPlotOpt.Show vbModal
  'act on the contents of the returned Tag
  ' blank:    do nothing
  ' "regen":  regen plot data and replot
  ' "replot": just replot the data
  Select Case frmPlotOpt.Tag
    Case "regen"
      frmPlot.lblRemoteControl = "regen"
    Case "replot"
      frmPlot.lblRemoteControl = "replot"
  End Select
  Unload frmPlotOpt
End Sub

Function PlotOptionsSimpleDialog() As Boolean
'bring up the simple plot options dialog
'Returns True if there were no problems
'Returns False if there was an error
  Dim strErrLocation As String
  On Error GoTo Error_Handler
  
  PlotOptionsSimpleDialog = False 'Default return value
  
  frmPlotOptSimple.Show vbModal
  PlotOptionsSimpleDialog = Not frmPlotOptSimple.Cancelled
  Unload frmPlotOptSimple
  
  
'====================================================
'Exit Point for PlotOptionsSimpleDialog
'====================================================
Exit_PlotOptionsSimpleDialog:
  Exit Function


'====================================================
'            ERROR HANDLER ROUTINE(S)
'====================================================
Error_Handler:
  gobjErrors.Append Err, "PlotOptionsSimpleDialog", "basPlot", strErrLocation

  gobjErrors.UserMessage
  gobjErrors.WriteToErrorLog
  gobjErrors.Clear
  Resume Exit_PlotOptionsSimpleDialog
End Function

Sub PlotPrefsRead(xPS As PlotSettingData)
'Read Plot Settings from the .ini file
'
'If there are settings in the .ini file, they will
'override the current settings. If a setting is not
'found, the current value will remain. Therefore, do
'not use this routine to initialize the plot settings
'to default values, just to override existing settings
'with those stored in the .ini file.
'
  Dim fn As String      '.ini file name
  Dim an As String      'name for .ini file section
  Dim bufsz As Integer
  Dim buf As String

  'build the settings file name from the Application
  fn = App.Path & Chr$(92) & App.EXEName & ".ini"
  
  'retrieve preferences
  buf = Space$(80)
  bufsz = Len(buf)
  an = "Plot Settings"
  xPS.RunTitle.Font.Name = Left$(buf, GetPrivateProfileString(an, "RunTitle.Font.Name", xPS.RunTitle.Font.Name, buf, bufsz, fn))
  xPS.RunTitle.Font.Size = Val(Left$(buf, GetPrivateProfileString(an, "RunTitle.Font.Size", Format$(xPS.RunTitle.Font.Size), buf, bufsz, fn)))
  xPS.RunTitle.Font.Bold = GetPrivateProfileInt(an, "RunTitle.Font.Bold", xPS.RunTitle.Font.Bold, fn)
  xPS.RunTitle.Font.Italic = GetPrivateProfileInt(an, "RunTitle.Font.Italic", xPS.RunTitle.Font.Italic, fn)
  xPS.RunTitle.Font.Underline = GetPrivateProfileInt(an, "RunTitle.Font.Underline", xPS.RunTitle.Font.Underline, fn)
  xPS.RunTitle.Font.Strikethru = GetPrivateProfileInt(an, "RunTitle.Font.Strikethru", xPS.RunTitle.Font.Strikethru, fn)
  xPS.RunTitle.Font.Transparent = GetPrivateProfileInt(an, "RunTitle.Font.Transparent", xPS.RunTitle.Font.Transparent, fn)
  xPS.RunTitle.Font.Color = Val(Left$(buf, GetPrivateProfileString(an, "RunTitle.Font.Color", Format$(xPS.RunTitle.Font.Color), buf, bufsz, fn)))
  xPS.RunTitle.PosX = Val(Left$(buf, GetPrivateProfileString(an, "RunTitle.PosX", Format$(xPS.RunTitle.PosX), buf, bufsz, fn)))
  xPS.RunTitle.PosY = Val(Left$(buf, GetPrivateProfileString(an, "RunTitle.PosY", Format$(xPS.RunTitle.PosY), buf, bufsz, fn)))
  '
  xPS.PlotTitle.Font.Name = Left$(buf, GetPrivateProfileString(an, "PlotTitle.Font.Name", xPS.PlotTitle.Font.Name, buf, bufsz, fn))
  xPS.PlotTitle.Font.Size = Val(Left$(buf, GetPrivateProfileString(an, "PlotTitle.Font.Size", Format$(xPS.PlotTitle.Font.Size), buf, bufsz, fn)))
  xPS.PlotTitle.Font.Bold = GetPrivateProfileInt(an, "PlotTitle.Font.Bold", xPS.PlotTitle.Font.Bold, fn)
  xPS.PlotTitle.Font.Italic = GetPrivateProfileInt(an, "PlotTitle.Font.Italic", xPS.PlotTitle.Font.Italic, fn)
  xPS.PlotTitle.Font.Underline = GetPrivateProfileInt(an, "PlotTitle.Font.Underline", xPS.PlotTitle.Font.Underline, fn)
  xPS.PlotTitle.Font.Strikethru = GetPrivateProfileInt(an, "PlotTitle.Font.Strikethru", xPS.PlotTitle.Font.Strikethru, fn)
  xPS.PlotTitle.Font.Transparent = GetPrivateProfileInt(an, "PlotTitle.Font.Transparent", xPS.PlotTitle.Font.Transparent, fn)
  xPS.PlotTitle.Font.Color = Val(Left$(buf, GetPrivateProfileString(an, "PlotTitle.Font.Color", Format$(xPS.PlotTitle.Font.Color), buf, bufsz, fn)))
  xPS.PlotTitle.PosX = Val(Left$(buf, GetPrivateProfileString(an, "PlotTitle.PosX", Format$(xPS.PlotTitle.PosX), buf, bufsz, fn)))
  xPS.PlotTitle.PosY = Val(Left$(buf, GetPrivateProfileString(an, "PlotTitle.PosY", Format$(xPS.PlotTitle.PosY), buf, bufsz, fn)))
  '
  xPS.DataTitle(0) = Left$(buf, GetPrivateProfileString(an, "DataTitle(0)", xPS.DataTitle(0), buf, bufsz, fn))
  xPS.DataSource(0) = Left$(buf, GetPrivateProfileString(an, "DataSource(0)", xPS.DataSource(0), buf, bufsz, fn))
  xPS.DataColor(0) = Val(Left$(buf, GetPrivateProfileString(an, "DataColor(0)", Format$(xPS.DataColor(0)), buf, bufsz, fn)))
  xPS.DataStyle(0) = GetPrivateProfileInt(an, "DataStyle(0)", xPS.DataStyle(0), fn)
  xPS.DataTitle(1) = Left$(buf, GetPrivateProfileString(an, "DataTitle(1)", xPS.DataTitle(1), buf, bufsz, fn))
  xPS.DataSource(1) = Left$(buf, GetPrivateProfileString(an, "DataSource(1)", xPS.DataSource(1), buf, bufsz, fn))
  xPS.DataColor(1) = Val(Left$(buf, GetPrivateProfileString(an, "DataColor(1)", Format$(xPS.DataColor(1)), buf, bufsz, fn)))
  xPS.DataStyle(1) = GetPrivateProfileInt(an, "DataStyle(1)", xPS.DataStyle(1), fn)
  xPS.DataTitle(2) = Left$(buf, GetPrivateProfileString(an, "DataTitle(2)", xPS.DataTitle(2), buf, bufsz, fn))
  xPS.DataSource(2) = Left$(buf, GetPrivateProfileString(an, "DataSource(2)", xPS.DataSource(2), buf, bufsz, fn))
  xPS.DataColor(2) = Val(Left$(buf, GetPrivateProfileString(an, "DataColor(2)", Format$(xPS.DataColor(2)), buf, bufsz, fn)))
  xPS.DataStyle(2) = GetPrivateProfileInt(an, "DataStyle(2)", xPS.DataStyle(2), fn)
  xPS.DataTitle(3) = Left$(buf, GetPrivateProfileString(an, "DataTitle(3)", xPS.DataTitle(3), buf, bufsz, fn))
  xPS.DataSource(3) = Left$(buf, GetPrivateProfileString(an, "DataSource(3)", xPS.DataSource(3), buf, bufsz, fn))
  xPS.DataColor(3) = Val(Left$(buf, GetPrivateProfileString(an, "DataColor(3)", Format$(xPS.DataColor(3)), buf, bufsz, fn)))
  xPS.DataStyle(3) = GetPrivateProfileInt(an, "DataStyle(3)", xPS.DataStyle(3), fn)
  xPS.DataTitle(4) = Left$(buf, GetPrivateProfileString(an, "DataTitle(4)", xPS.DataTitle(4), buf, bufsz, fn))
  xPS.DataSource(4) = Left$(buf, GetPrivateProfileString(an, "DataSource(4)", xPS.DataSource(4), buf, bufsz, fn))
  xPS.DataColor(4) = Val(Left$(buf, GetPrivateProfileString(an, "DataColor(4)", Format$(xPS.DataColor(4)), buf, bufsz, fn)))
  xPS.DataStyle(4) = GetPrivateProfileInt(an, "DataStyle(4)", xPS.DataStyle(4), fn)
  '
  xPS.XTitle.Font.Name = Left$(buf, GetPrivateProfileString(an, "XTitle.Font.Name", xPS.XTitle.Font.Name, buf, bufsz, fn))
  xPS.XTitle.Font.Size = Val(Left$(buf, GetPrivateProfileString(an, "XTitle.Font.Size", Format$(xPS.XTitle.Font.Size), buf, bufsz, fn)))
  xPS.XTitle.Font.Bold = GetPrivateProfileInt(an, "XTitle.Font.Bold", xPS.XTitle.Font.Bold, fn)
  xPS.XTitle.Font.Italic = GetPrivateProfileInt(an, "XTitle.Font.Italic", xPS.XTitle.Font.Italic, fn)
  xPS.XTitle.Font.Underline = GetPrivateProfileInt(an, "XTitle.Font.Underline", xPS.XTitle.Font.Underline, fn)
  xPS.XTitle.Font.Strikethru = GetPrivateProfileInt(an, "XTitle.Font.Strikethru", xPS.XTitle.Font.Strikethru, fn)
  xPS.XTitle.Font.Transparent = GetPrivateProfileInt(an, "XTitle.Font.Transparent", xPS.XTitle.Font.Transparent, fn)
  xPS.XTitle.Font.Color = Val(Left$(buf, GetPrivateProfileString(an, "XTitle.Font.Color", Format$(xPS.XTitle.Font.Color), buf, bufsz, fn)))
  xPS.XTitle.PosX = Val(Left$(buf, GetPrivateProfileString(an, "XTitle.PosX", Format$(xPS.XTitle.PosX), buf, bufsz, fn)))
  xPS.XTitle.PosY = Val(Left$(buf, GetPrivateProfileString(an, "XTitle.PosY", Format$(xPS.XTitle.PosY), buf, bufsz, fn)))
  xPS.Xauto = GetPrivateProfileInt(an, "Xauto", xPS.Xauto, fn)
  xPS.Xmin = Val(Left$(buf, GetPrivateProfileString(an, "Xmin", Format$(xPS.Xmin), buf, bufsz, fn)))
  xPS.Xmax = Val(Left$(buf, GetPrivateProfileString(an, "Xmax", Format$(xPS.Xmax), buf, bufsz, fn)))
  xPS.Xinc = Val(Left$(buf, GetPrivateProfileString(an, "Xinc", Format$(xPS.Xinc), buf, bufsz, fn)))
  xPS.XminorTics = GetPrivateProfileInt(an, "XminorTics", xPS.XminorTics, fn)
  xPS.Xgrid = GetPrivateProfileInt(an, "Xgrid", xPS.Xgrid, fn)
  xPS.Xlog = GetPrivateProfileInt(an, "Xlog", xPS.Xlog, fn)
  xPS.XScaleFont.Name = Left$(buf, GetPrivateProfileString(an, "XScaleFont.Name", xPS.XScaleFont.Name, buf, bufsz, fn))
  xPS.XScaleFont.Size = Val(Left$(buf, GetPrivateProfileString(an, "XScaleFont.Size", Format$(xPS.XScaleFont.Size), buf, bufsz, fn)))
  xPS.XScaleFont.Bold = GetPrivateProfileInt(an, "XScaleFont.Bold", xPS.XScaleFont.Bold, fn)
  xPS.XScaleFont.Italic = GetPrivateProfileInt(an, "XScaleFont.Italic", xPS.XScaleFont.Italic, fn)
  xPS.XScaleFont.Underline = GetPrivateProfileInt(an, "XScaleFont.Underline", xPS.XScaleFont.Underline, fn)
  xPS.XScaleFont.Strikethru = GetPrivateProfileInt(an, "XScaleFont.Strikethru", xPS.XScaleFont.Strikethru, fn)
  xPS.XScaleFont.Transparent = GetPrivateProfileInt(an, "XScaleFont.Transparent", xPS.XScaleFont.Transparent, fn)
  xPS.XScaleFont.Color = Val(Left$(buf, GetPrivateProfileString(an, "XScaleFont.Color", Format$(xPS.XScaleFont.Color), buf, bufsz, fn)))
  '
  xPS.YTitle.Font.Name = Left$(buf, GetPrivateProfileString(an, "YTitle.Font.Name", xPS.YTitle.Font.Name, buf, bufsz, fn))
  xPS.YTitle.Font.Size = Val(Left$(buf, GetPrivateProfileString(an, "YTitle.Font.Size", Format$(xPS.YTitle.Font.Size), buf, bufsz, fn)))
  xPS.YTitle.Font.Bold = GetPrivateProfileInt(an, "YTitle.Font.Bold", xPS.YTitle.Font.Bold, fn)
  xPS.YTitle.Font.Italic = GetPrivateProfileInt(an, "YTitle.Font.Italic", xPS.YTitle.Font.Italic, fn)
  xPS.YTitle.Font.Underline = GetPrivateProfileInt(an, "YTitle.Font.Underline", xPS.YTitle.Font.Underline, fn)
  xPS.YTitle.Font.Strikethru = GetPrivateProfileInt(an, "YTitle.Font.Strikethru", xPS.YTitle.Font.Strikethru, fn)
  xPS.YTitle.Font.Transparent = GetPrivateProfileInt(an, "YTitle.Font.Transparent", xPS.YTitle.Font.Transparent, fn)
  xPS.YTitle.Font.Color = Val(Left$(buf, GetPrivateProfileString(an, "YTitle.Font.Color", Format$(xPS.YTitle.Font.Color), buf, bufsz, fn)))
  xPS.YTitle.PosX = Val(Left$(buf, GetPrivateProfileString(an, "YTitle.PosX", Format$(xPS.YTitle.PosX), buf, bufsz, fn)))
  xPS.YTitle.PosY = Val(Left$(buf, GetPrivateProfileString(an, "YTitle.PosY", Format$(xPS.YTitle.PosY), buf, bufsz, fn)))
  xPS.Yauto = GetPrivateProfileInt(an, "Yauto", xPS.Yauto, fn)
  xPS.Ymin = Val(Left$(buf, GetPrivateProfileString(an, "Ymin", Format$(xPS.Ymin), buf, bufsz, fn)))
  xPS.Ymax = Val(Left$(buf, GetPrivateProfileString(an, "Ymax", Format$(xPS.Ymax), buf, bufsz, fn)))
  xPS.Yinc = Val(Left$(buf, GetPrivateProfileString(an, "Yinc", Format$(xPS.Yinc), buf, bufsz, fn)))
  xPS.YminorTics = GetPrivateProfileInt(an, "YminorTics", xPS.YminorTics, fn)
  xPS.Ygrid = GetPrivateProfileInt(an, "Ygrid", xPS.Ygrid, fn)
  xPS.Ylog = GetPrivateProfileInt(an, "Ylog", xPS.Ylog, fn)
  xPS.YScaleFont.Name = Left$(buf, GetPrivateProfileString(an, "YScaleFont.Name", xPS.YScaleFont.Name, buf, bufsz, fn))
  xPS.YScaleFont.Size = Val(Left$(buf, GetPrivateProfileString(an, "YScaleFont.Size", Format$(xPS.YScaleFont.Size), buf, bufsz, fn)))
  xPS.YScaleFont.Bold = GetPrivateProfileInt(an, "YScaleFont.Bold", xPS.YScaleFont.Bold, fn)
  xPS.YScaleFont.Italic = GetPrivateProfileInt(an, "YScaleFont.Italic", xPS.YScaleFont.Italic, fn)
  xPS.YScaleFont.Underline = GetPrivateProfileInt(an, "YScaleFont.Underline", xPS.YScaleFont.Underline, fn)
  xPS.YScaleFont.Strikethru = GetPrivateProfileInt(an, "YScaleFont.Strikethru", xPS.YScaleFont.Strikethru, fn)
  xPS.YScaleFont.Transparent = GetPrivateProfileInt(an, "YScaleFont.Transparent", xPS.YScaleFont.Transparent, fn)
  xPS.YScaleFont.Color = Val(Left$(buf, GetPrivateProfileString(an, "YScaleFont.Color", Format$(xPS.YScaleFont.Color), buf, bufsz, fn)))
  '
  xPS.Legend.On = GetPrivateProfileInt(an, "Legend.On", xPS.Legend.On, fn)
  xPS.Legend.Font.Name = Left$(buf, GetPrivateProfileString(an, "YTitle.Font.Name", xPS.Legend.Font.Name, buf, bufsz, fn))
  xPS.Legend.Font.Size = Val(Left$(buf, GetPrivateProfileString(an, "Legend.Font.Size", Format$(xPS.Legend.Font.Size), buf, bufsz, fn)))
  xPS.Legend.Font.Bold = GetPrivateProfileInt(an, "Legend.Font.Bold", xPS.Legend.Font.Bold, fn)
  xPS.Legend.Font.Italic = GetPrivateProfileInt(an, "Legend.Font.Italic", xPS.Legend.Font.Italic, fn)
  xPS.Legend.Font.Underline = GetPrivateProfileInt(an, "Legend.Font.Underline", xPS.Legend.Font.Underline, fn)
  xPS.Legend.Font.Strikethru = GetPrivateProfileInt(an, "Legend.Font.Strikethru", xPS.Legend.Font.Strikethru, fn)
  xPS.Legend.Font.Transparent = GetPrivateProfileInt(an, "Legend.Font.Transparent", xPS.Legend.Font.Transparent, fn)
  xPS.Legend.Font.Color = Val(Left$(buf, GetPrivateProfileString(an, "Legend.Font.Color", Format$(xPS.Legend.Font.Color), buf, bufsz, fn)))
  xPS.Legend.PosX = Val(Left$(buf, GetPrivateProfileString(an, "Legend.PosX", Format$(xPS.Legend.PosX), buf, bufsz, fn)))
  xPS.Legend.PosY = Val(Left$(buf, GetPrivateProfileString(an, "Legend.PosY", Format$(xPS.Legend.PosY), buf, bufsz, fn)))
End Sub

Sub PlotPrefsWrite(xPS As PlotSettingData)
'Save current Plot Settings in the .ini file
'
  Dim fn As String      '.ini file name
  Dim an As String      'name for .ini file section
  Dim bufsz As Integer
  Dim buf As String
  Dim stat As Integer

  'build the settings file name from the Application
  fn = App.Path & Chr$(92) & App.EXEName & ".ini"
  
  'retrieve preferences
  buf = Space$(80)
  bufsz = Len(buf)
  an = "Plot Settings"
  stat = WritePrivateProfileString(an, "RunTitle.Font.Name", xPS.RunTitle.Font.Name, fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Size", Format$(xPS.RunTitle.Font.Size), fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Bold", Format$(xPS.RunTitle.Font.Bold), fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Italic", Format$(xPS.RunTitle.Font.Italic), fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Underline", Format$(xPS.RunTitle.Font.Underline), fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Strikethru", Format$(xPS.RunTitle.Font.Strikethru), fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Transparent", Format$(xPS.RunTitle.Font.Transparent), fn)
  stat = WritePrivateProfileString(an, "RunTitle.Font.Color", Format$(xPS.RunTitle.Font.Color), fn)
  stat = WritePrivateProfileString(an, "RunTitle.PosX", Format$(xPS.RunTitle.PosX), fn)
  stat = WritePrivateProfileString(an, "RunTitle.PosY", Format$(xPS.RunTitle.PosY), fn)
  '
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Name", xPS.PlotTitle.Font.Name, fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Size", Format$(xPS.PlotTitle.Font.Size), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Bold", Format$(xPS.PlotTitle.Font.Bold), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Italic", Format$(xPS.PlotTitle.Font.Italic), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Underline", Format$(xPS.PlotTitle.Font.Underline), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Strikethru", Format$(xPS.PlotTitle.Font.Strikethru), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Transparent", Format$(xPS.PlotTitle.Font.Transparent), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.Font.Color", Format$(xPS.PlotTitle.Font.Color), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.PosX", Format$(xPS.PlotTitle.PosX), fn)
  stat = WritePrivateProfileString(an, "PlotTitle.PosY", Format$(xPS.PlotTitle.PosY), fn)
  '
  stat = WritePrivateProfileString(an, "DataTitle(0)", xPS.DataTitle(0), fn)
  stat = WritePrivateProfileString(an, "DataSource(0)", xPS.DataSource(0), fn)
  stat = WritePrivateProfileString(an, "DataColor(0)", Format$(xPS.DataColor(0)), fn)
  stat = WritePrivateProfileString(an, "DataStyle(0)", Format$(xPS.DataStyle(0)), fn)
  stat = WritePrivateProfileString(an, "DataTitle(1)", xPS.DataTitle(1), fn)
  stat = WritePrivateProfileString(an, "DataSource(1)", xPS.DataSource(1), fn)
  stat = WritePrivateProfileString(an, "DataColor(1)", Format$(xPS.DataColor(1)), fn)
  stat = WritePrivateProfileString(an, "DataStyle(1)", Format$(xPS.DataStyle(1)), fn)
  stat = WritePrivateProfileString(an, "DataTitle(2)", xPS.DataTitle(2), fn)
  stat = WritePrivateProfileString(an, "DataSource(2)", xPS.DataSource(2), fn)
  stat = WritePrivateProfileString(an, "DataColor(2)", Format$(xPS.DataColor(2)), fn)
  stat = WritePrivateProfileString(an, "DataStyle(2)", Format$(xPS.DataStyle(2)), fn)
  stat = WritePrivateProfileString(an, "DataTitle(3)", xPS.DataTitle(3), fn)
  stat = WritePrivateProfileString(an, "DataSource(3)", xPS.DataSource(3), fn)
  stat = WritePrivateProfileString(an, "DataColor(3)", Format$(xPS.DataColor(3)), fn)
  stat = WritePrivateProfileString(an, "DataStyle(3)", Format$(xPS.DataStyle(3)), fn)
  stat = WritePrivateProfileString(an, "DataTitle(4)", xPS.DataTitle(4), fn)
  stat = WritePrivateProfileString(an, "DataSource(4)", xPS.DataSource(4), fn)
  stat = WritePrivateProfileString(an, "DataColor(4)", Format$(xPS.DataColor(4)), fn)
  stat = WritePrivateProfileString(an, "DataStyle(4)", Format$(xPS.DataStyle(4)), fn)
  '
  stat = WritePrivateProfileString(an, "XTitle.Font.Name", xPS.XTitle.Font.Name, fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Size", Format$(xPS.XTitle.Font.Size), fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Bold", Format$(xPS.XTitle.Font.Bold), fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Italic", Format$(xPS.XTitle.Font.Italic), fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Underline", Format$(xPS.XTitle.Font.Underline), fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Strikethru", Format$(xPS.XTitle.Font.Strikethru), fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Transparent", Format$(xPS.XTitle.Font.Transparent), fn)
  stat = WritePrivateProfileString(an, "XTitle.Font.Color", Format$(xPS.XTitle.Font.Color), fn)
  stat = WritePrivateProfileString(an, "XTitle.PosX", Format$(xPS.XTitle.PosX), fn)
  stat = WritePrivateProfileString(an, "XTitle.PosY", Format$(xPS.XTitle.PosY), fn)
  stat = WritePrivateProfileString(an, "Xauto", Format$(xPS.Xauto), fn)
  stat = WritePrivateProfileString(an, "Xmin", Format$(xPS.Xmin), fn)
  stat = WritePrivateProfileString(an, "Xmax", Format$(xPS.Xmax), fn)
  stat = WritePrivateProfileString(an, "Xinc", Format$(xPS.Xinc), fn)
  stat = WritePrivateProfileString(an, "XminorTics", Format$(xPS.XminorTics), fn)
  stat = WritePrivateProfileString(an, "Xgrid", Format$(xPS.Xgrid), fn)
  stat = WritePrivateProfileString(an, "Xlog", Format$(xPS.Xlog), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Name", xPS.XScaleFont.Name, fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Size", Format$(xPS.XScaleFont.Size), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Bold", Format$(xPS.XScaleFont.Bold), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Italic", Format$(xPS.XScaleFont.Italic), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Underline", Format$(xPS.XScaleFont.Underline), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Strikethru", Format$(xPS.XScaleFont.Strikethru), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Transparent", Format$(xPS.XScaleFont.Transparent), fn)
  stat = WritePrivateProfileString(an, "XScaleFont.Color", Format$(xPS.XScaleFont.Color), fn)
  '
  stat = WritePrivateProfileString(an, "YTitle.Font.Name", xPS.YTitle.Font.Name, fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Size", Format$(xPS.YTitle.Font.Size), fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Bold", Format$(xPS.YTitle.Font.Bold), fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Italic", Format$(xPS.YTitle.Font.Italic), fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Underline", Format$(xPS.YTitle.Font.Underline), fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Strikethru", Format$(xPS.YTitle.Font.Strikethru), fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Transparent", Format$(xPS.YTitle.Font.Transparent), fn)
  stat = WritePrivateProfileString(an, "YTitle.Font.Color", Format$(xPS.YTitle.Font.Color), fn)
  stat = WritePrivateProfileString(an, "YTitle.PosX", Format$(xPS.YTitle.PosX), fn)
  stat = WritePrivateProfileString(an, "YTitle.PosY", Format$(xPS.YTitle.PosY), fn)
  stat = WritePrivateProfileString(an, "Yauto", Format$(xPS.Yauto), fn)
  stat = WritePrivateProfileString(an, "Ymin", Format$(xPS.Ymin), fn)
  stat = WritePrivateProfileString(an, "Ymax", Format$(xPS.Ymax), fn)
  stat = WritePrivateProfileString(an, "Yinc", Format$(xPS.Yinc), fn)
  stat = WritePrivateProfileString(an, "YminorTics", Format$(xPS.YminorTics), fn)
  stat = WritePrivateProfileString(an, "Ygrid", Format$(xPS.Ygrid), fn)
  stat = WritePrivateProfileString(an, "Ylog", Format$(xPS.Ylog), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Name", xPS.YScaleFont.Name, fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Size", Format$(xPS.YScaleFont.Size), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Bold", Format$(xPS.YScaleFont.Bold), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Italic", Format$(xPS.YScaleFont.Italic), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Underline", Format$(xPS.YScaleFont.Underline), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Strikethru", Format$(xPS.YScaleFont.Strikethru), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Transparent", Format$(xPS.YScaleFont.Transparent), fn)
  stat = WritePrivateProfileString(an, "YScaleFont.Color", Format$(xPS.YScaleFont.Color), fn)
  '
  stat = WritePrivateProfileString(an, "Legend.On", Format$(xPS.Legend.On), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Name", xPS.Legend.Font.Name, fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Size", Format$(xPS.Legend.Font.Size), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Bold", Format$(xPS.Legend.Font.Bold), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Italic", Format$(xPS.Legend.Font.Italic), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Underline", Format$(xPS.Legend.Font.Underline), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Strikethru", Format$(xPS.Legend.Font.Strikethru), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Transparent", Format$(xPS.Legend.Font.Transparent), fn)
  stat = WritePrivateProfileString(an, "Legend.Font.Color", Format$(xPS.Legend.Font.Color), fn)
  stat = WritePrivateProfileString(an, "Legend.PosX", Format$(xPS.Legend.PosX), fn)
  stat = WritePrivateProfileString(an, "Legend.PosY", Format$(xPS.Legend.PosY), fn)
End Sub

Sub PlotPrintDialog()
'Start the dialog box for printing the plot
  'use the remote control label
  frmPlot!lblRemoteControl.Caption = "printdialog"
End Sub

Sub PlotSetCaption(s As String)
'Set the PlotCaption
  PS.Caption = s
End Sub

Sub PlotSetDataSource(i, s As String)
'set the DataSource label for dataset i to s
  PS.DataSource(i) = s
End Sub

Sub PlotSetDataTitle(i, s As String)
'set the DataTitle label for dataset i to s
  PS.DataTitle(i) = s
End Sub

Sub PlotSetHelpID(id As Long)
'set the help context id for the plot form
  PS.HelpID = id
End Sub

Sub PlotSetPlotTitle(s As String)
'Set the plot title
  PS.PlotTitle.Text = s
End Sub

Sub PlotSetRunTitle(s As String)
'Set the plot title
  PS.RunTitle.Text = s
End Sub

Sub PlotSetType(ptype As Integer)
'set plot type
'ptype is one of:
' GQ_XYPLOT
' GQ_ISOPLETH
' GQ_COLORMAP
' GQ_BAR

  PS.PlotType = ptype
End Sub

Sub PlotSettingsInit(xPS As PlotSettingData)
  'Init Settings that affect plotting
  xPS.Caption = "Plot"
  xPS.HelpID = 0
  xPS.Changed = False 'True if user has changed settings
  '
  xPS.RunTitle.Text = "Run Title"
  xPS.RunTitle.Font.Name = "Times New Roman"
  xPS.RunTitle.Font.Size = 10
  xPS.RunTitle.Font.Bold = True
  xPS.RunTitle.Font.Italic = False
  xPS.RunTitle.Font.Underline = False
  xPS.RunTitle.Font.Strikethru = False
  xPS.RunTitle.Font.Transparent = False
  xPS.RunTitle.Font.Color = vbBlack
  xPS.RunTitle.PosX = 0.5
  xPS.RunTitle.PosY = 100
  '
  xPS.PlotTitle.Text = "Plot Title"
  xPS.PlotTitle.Font.Name = "Times New Roman"
  xPS.PlotTitle.Font.Size = 10
  xPS.PlotTitle.Font.Bold = True
  xPS.PlotTitle.Font.Italic = False
  xPS.PlotTitle.Font.Underline = False
  xPS.PlotTitle.Font.Strikethru = False
  xPS.PlotTitle.Font.Transparent = False
  xPS.PlotTitle.Font.Color = vbBlack
  xPS.PlotTitle.PosX = 0.5
  xPS.PlotTitle.PosY = 400
  '
  xPS.DataTitle(0) = "Current Data"
  xPS.DataSource(0) = "Current Data"
  xPS.DataColor(0) = &H0& 'Black
  xPS.DataStyle(0) = 0
  xPS.DataTitle(1) = ""
  xPS.DataSource(1) = ""
  xPS.DataColor(1) = &HFF& 'Red
  xPS.DataStyle(1) = 1
  xPS.DataTitle(2) = ""
  xPS.DataSource(2) = ""
  xPS.DataColor(2) = &H80FF& 'Orange
  xPS.DataStyle(2) = 2
  xPS.DataTitle(3) = ""
  xPS.DataSource(3) = ""
  xPS.DataColor(3) = &HFF0000   'Blue
  xPS.DataStyle(3) = 3
  xPS.DataTitle(4) = ""
  xPS.DataSource(4) = ""
  xPS.DataColor(4) = &HFF00& 'Green
  xPS.DataStyle(4) = 4
  '
  xPS.XTitle.Text = "X"
  xPS.XTitle.Font.Name = "Times New Roman"
  xPS.XTitle.Font.Size = 10
  xPS.XTitle.Font.Bold = True
  xPS.XTitle.Font.Italic = False
  xPS.XTitle.Font.Underline = False
  xPS.XTitle.Font.Strikethru = False
  xPS.XTitle.Font.Transparent = False
  xPS.XTitle.Font.Color = vbBlack
  xPS.XTitle.PosX = 0.5
  xPS.XTitle.PosY = 500
  xPS.Xauto = True
  xPS.Xmin = 0
  xPS.Xmax = 1
  xPS.Xinc = 0.5
  xPS.XminorTics = 1
  xPS.Xgrid = 0
  xPS.Xlog = 0
  xPS.XScaleFont.Name = "Times New Roman"
  xPS.XScaleFont.Size = 9.75
  xPS.XScaleFont.Bold = True
  xPS.XScaleFont.Italic = False
  xPS.XScaleFont.Underline = False
  xPS.XScaleFont.Strikethru = False
  xPS.XScaleFont.Transparent = False
  xPS.XScaleFont.Color = vbBlack
  '
  xPS.YTitle.Text = "Y"
  xPS.YTitle.Font.Name = "Times New Roman"
  xPS.YTitle.Font.Size = 10
  xPS.YTitle.Font.Bold = True
  xPS.YTitle.Font.Italic = False
  xPS.YTitle.Font.Underline = False
  xPS.YTitle.Font.Strikethru = False
  xPS.YTitle.Font.Transparent = False
  xPS.YTitle.Font.Color = vbBlack
  xPS.YTitle.PosX = 150
  xPS.YTitle.PosY = 0.4
  xPS.Yauto = True
  xPS.Ymin = 0
  xPS.Ymax = 1
  xPS.Yinc = 0.5
  xPS.YminorTics = 1
  xPS.Ygrid = 0
  xPS.Ylog = 0
  xPS.YScaleFont.Name = "Times New Roman"
  xPS.YScaleFont.Size = 9.75
  xPS.YScaleFont.Bold = True
  xPS.YScaleFont.Italic = False
  xPS.YScaleFont.Underline = False
  xPS.YScaleFont.Strikethru = False
  xPS.YScaleFont.Transparent = False
  xPS.YScaleFont.Color = vbBlack
  '
  xPS.Legend.On = True
  xPS.Legend.Font.Name = "Times New Roman"
  xPS.Legend.Font.Size = 10
  xPS.Legend.Font.Bold = True
  xPS.Legend.Font.Italic = False
  xPS.Legend.Font.Underline = False
  xPS.Legend.Font.Strikethru = False
  xPS.Legend.Font.Transparent = False
  xPS.Legend.Font.Color = vbBlack
  xPS.Legend.PosX = 640
  xPS.Legend.PosY = 950

'tbc contour plotting needs rework
  'Contour Settings
'tbc  GQCD.LevelColor(1) = vbRed
'tbc  GQCD.LevelColor(2) = vbYellow
'tbc  GQCD.LevelColor(3) = vbBlue
'tbc  GQCD.LevelColor(4) = vbCyan
'tbc  GQCD.LevelColor(5) = vbMagenta
End Sub

Sub PlotSetXauto(iauto As Integer)
'set x axis autoscaling
  PS.Xauto = iauto
End Sub

Sub PlotSetXlog(ilog As Integer)
'set the X axis to log (ilog=true=-1) or linear (ilog=false=0)
  PS.Xlog = Abs(ilog)
End Sub

Sub PlotSetXtitle(s As String)
'Set the plot X axis title
  PS.XTitle.Text = s
End Sub

Sub PlotSetXunits(s As String)
'Set the plot X units to be appended to the X Title
  PS.Xunits = s
End Sub

Sub PlotSetYauto(iauto As Integer)
'set y axis autoscaling
  PS.Yauto = iauto
End Sub

Sub PlotSetYlog(ilog As Integer)
'set the Y axis to log (ilog=true=-1) or linear (ilog=false=0)
  PS.Ylog = Abs(ilog)
End Sub

Sub PlotSetYtitle(s As String)
'Set the plot Y axis title
  PS.YTitle.Text = s
End Sub

Sub PlotSetYunits(s As String)
'Set the plot Y units to be appended to the Y Title
  PS.Yunits = s
End Sub

