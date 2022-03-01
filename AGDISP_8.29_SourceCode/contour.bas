Attribute VB_Name = "basCONTOUR"
'basCONTOUR
'
'Contour plotting routines
Option Explicit

'declare a data type to hold contour data
Type GQContourData
  NX As Long             '# of grid points in X
  NY As Long             '# of grid points in Y
  X(1 To 100) As Single  'X values of grid
  Y(1 To 50) As Single   'Y values of grid
  Z(1 To 100, 1 To 50) As Single 'Z values of grid intersections
  Xmin As Single         'Min X value
  Xmax As Single         'Max X value
  Ymin As Single         'Min Y value
  Ymax As Single         'Max Y value
  Zmin As Single         'Min Z value
  Zmax As Single         'Max Z value
  nLevel As Long         '# of contour levels
  Level(1 To 10) As Single    'contour levels
  LevelColor(1 To 10) As Long 'contour level colors
End Type
Public GQCD As GQContourData

Public Sub PlotContourData(ptype As Long)
'display graphics that go in the picPlot area
'
'ptype  i   plot type: 0=isopleths 1=colormap

  Dim i As Integer
  Dim j As Integer
  Dim X1 As Single
  Dim Y1 As Single
  Dim X2 As Single
  Dim Y2 As Single
  Dim ArrowX As Single
  Dim ArrowY As Single
  Dim ArrowBig As Single
  Dim ArrowSmall As Single
  Dim ArrowSin As Single
  Dim ArrowCos As Single
  Dim ArrowAngle As Single
  Dim aspect As Single  'Aspect Ratio of plot
  
  Const PI = 3.14159265358979
'
' plot the grid outline
'
  i = 1
  j = 1
  X1 = UnitsDisplay(GQCD.X(i), UN_LENGTH)
  Y1 = UnitsDisplay(GQCD.Y(j), UN_LENGTH)
  Call moveq(GQ_WORLD, X1, Y1)
  For i = 2 To GQCD.NX
    X1 = UnitsDisplay(GQCD.X(i), UN_LENGTH)
    Y1 = UnitsDisplay(GQCD.Y(j), UN_LENGTH)
    Call drawq(GQ_WORLD, X1, Y1)
  Next
  DoEvents
  i = GQCD.NX
  For j = 2 To GQCD.NY
    X1 = UnitsDisplay(GQCD.X(i), UN_LENGTH)
    Y1 = UnitsDisplay(GQCD.Y(j), UN_LENGTH)
    Call drawq(GQ_WORLD, X1, Y1)
  Next
  DoEvents
  i = 1
  j = 1
  X1 = UnitsDisplay(GQCD.X(i), UN_LENGTH)
  Y1 = UnitsDisplay(GQCD.Y(j), UN_LENGTH)
  Call moveq(GQ_WORLD, X1, Y1)
  For j = 2 To GQCD.NY
    X1 = UnitsDisplay(GQCD.X(i), UN_LENGTH)
    Y1 = UnitsDisplay(GQCD.Y(j), UN_LENGTH)
    Call drawq(GQ_WORLD, X1, Y1)
  Next
  DoEvents
  j = GQCD.NY
  For i = 2 To GQCD.NX
    X1 = UnitsDisplay(GQCD.X(i), UN_LENGTH)
    Y1 = UnitsDisplay(GQCD.Y(j), UN_LENGTH)
    Call drawq(GQ_WORLD, X1, Y1)
  Next
  DoEvents
'
' plot the data
'
  Call contrq(ptype, CLng(GQCD.NX), CLng(GQCD.NY), GQCD.X(), GQCD.Y(), GQCD.Z(), GQCD.nLevel, GQCD.Level())
'
' plot the flight line
'
'tbc  X1 = UnitsDisplay(UC.FlightLineXS, UN_LENGTH)
'tbc  Y1 = UnitsDisplay(UC.FlightLineYS, UN_LENGTH)
'tbc  X2 = UnitsDisplay(UC.FlightLineXE, UN_LENGTH)
'tbc  Y2 = UnitsDisplay(UC.FlightLineYE, UN_LENGTH)
  Call colorq(vbBlack)
  Call styleq(0)
'tbc  Call widthq(3)
  Call moveq(GQ_WORLD, X1, Y1)
  Call drawq(GQ_WORLD, X2, Y2)
  
  'init arrow centers and sizes
  ArrowX = 600
  ArrowY = 600
  ArrowBig = 600
  ArrowSmall = 200

  'find aspect ratios for adjusting arrows
  aspect = (frmPlot.picPlot.Height / frmPlot.picPlot.Width) / _
           ((PS.Ymax - PS.Ymin) / (PS.Xmax - PS.Xmin))
  
  'plot north arrow
  Call colorq(vbBlue)
'tbc  Call widthq(2)
'tbc  ArrowAngle = (180 - UD.CTL.FlightDir) * PI / 180 'radians
  ArrowSin = Sin(ArrowAngle)
  ArrowCos = Cos(ArrowAngle)
  'Adjust arrow angle for data aspect ratio
  If (Abs(ArrowCos) > 0.0001) Then
    ArrowAngle = Atn(Sin(ArrowAngle) / Cos(ArrowAngle) / aspect)
    ArrowSin = Abs(Sin(ArrowAngle)) * Sgn(ArrowSin)
    ArrowCos = Abs(Cos(ArrowAngle)) * Sgn(ArrowCos)
  End If
  PlotArrow GQ_DEVICE, 1, ArrowX, ArrowY, ArrowBig, ArrowSin, ArrowCos
  
  'plot wind direction arrows
  Call colorq(vbRed)
'  Call widthq(2)
  For i = 0 To UD.MET.NumLevels - 1
    If UD.MET.WD(i) <> VFLAG_NONE Then 'if there is a wind direction
      ArrowAngle = (UD.MET.WD(i) - UD.CTL.FlightDir) * PI / 180
      ArrowSin = Sin(ArrowAngle)
      ArrowCos = Cos(ArrowAngle)
      'Adjust arrow angle for data aspect ratio
      If (Abs(ArrowCos) > 0.0001) Then
        ArrowAngle = Atn(Sin(ArrowAngle) / Cos(ArrowAngle) / aspect)
        ArrowSin = Abs(Sin(ArrowAngle)) * Sgn(ArrowSin)
        ArrowCos = Abs(Cos(ArrowAngle)) * Sgn(ArrowCos)
      End If
      X1 = ArrowX + ArrowBig * 0.5 * ArrowSin
      Y1 = ArrowY - ArrowBig * 0.5 * ArrowCos
      PlotArrow GQ_DEVICE, 2, X1, Y1, ArrowSmall, ArrowSin, ArrowCos
    End If
  Next
  
'tbc  Call widthq(1)
  Call colorq(vbBlack)

End Sub

Public Sub PlotContourDataReset()
'Reset contour plot data
  GQCD.NX = 0
  GQCD.NY = 0
End Sub

Public Sub clrbxq(NC&, CONTR!(), NU&, nv&, U!(), V!(), w!())
'
' Draw colored boxes in subdivided cells
' Boxes are colored according to the closest smaller contour value.
' Thus boxes with a value less than the smallest contour value are
' not colored. The CONTR array is assumed to be in order of increasing
' value.
'
''      DIMENSION CONTR(NC), U(NU), V(NV), W(NU, NV)
''      INTEGER ICLR(10)
''      DATA ICLR/5,12,14,10,3,13,6,4,9,2/
'
'       loop over subdivided grid cell
'       if the value of this box is greater than or equal to the current
'       contour value, but less than the next one (if there is one) draw
'       the box in the current color.
'
        Dim boxval As Single

        For j = 1 To nv - 1
          For i = 1 To NU - 1
            boxval = (w(i, j) + w(i + 1, j) + w(i + 1, j + 1) + w(i, j + 1)) / 4#
            IHIT = False
            For ic = 1 To NC
              If (boxval >= CONTR(ic)) Then
                Call colorq(GQCD.LevelColor(ic))
                IHIT = True
                Exit For
              End If
            Next
            If IHIT Then
              Call boxq(GQ_WORLD, U(i), V(j), U(i + 1), V(j + 1), 1)
            End If
          Next
        Next
End Sub

Public Sub contrq(IMAP As Long, NX As Long, NY As Long, X() As Single, Y() As Single, Z() As Single, NC As Long, CONTR() As Single)
'
' makes a contour plot over the given grid.
'
'
' IMAP    I   the type of contour plot 0=isopleths 1=colormap
' NX,NY   I   number of X and Y grid points, respectively
' X,Y     I   arrays of X and Y locations of grid points
' Z       I   2D array of grid values at grid points
' NC      I   number of contour values in CONTR
' CONTR   I   array of contour values
'
'
' NC is the number of contour levels.  return if there are none to plot
'
  If (NC <= 0) Then Exit Sub

  If IMAP = 0 Then     'Isopleth
    ReDim TU(1 To 100) As Single
    ReDim TV(1 To 100) As Single
    For i = 1 To NX
      TU(i) = UnitsDisplay(X(i), UN_LENGTH)
    Next
    For j = 1 To NY
      TV(j) = UnitsDisplay(Y(j), UN_LENGTH)
    Next
    Call isoplq(NC, CONTR(), CLng(NX), CLng(NY), TU(), TV(), Z())
  ElseIf IMAP = 1 Then 'Colormap
    ReDim TU(1 To 2) As Single
    ReDim TV(1 To 2) As Single
    ReDim tw(1 To 2, 1 To 2) As Single
    For j = 1 To NY - 1
      TV(1) = UnitsDisplay(Y(j), UN_LENGTH)
      TV(2) = UnitsDisplay(Y(j + 1), UN_LENGTH)
      For i = 1 To NX - 1
        TU(1) = UnitsDisplay(X(i), UN_LENGTH)
        TU(2) = UnitsDisplay(X(i + 1), UN_LENGTH)
        tw(1, 1) = Z(i, j)
        tw(2, 1) = Z(i + 1, j)
        tw(2, 2) = Z(i + 1, j + 1)
        tw(1, 2) = Z(i, j + 1)
        Call clrbxq(NC, CONTR(), 2, 2, TU(), TV(), tw())
        DoEvents
      Next
    Next
  End If
End Sub

Public Sub contrqsurf(IMAP As Long, NX As Long, NY As Long, X() As Single, Y() As Single, Z() As Single, NC As Long, CONTR() As Single)
'
' makes a contour plot over the given grid.
'
' The input grid is divided into main cells, each of which is bounded by four
' local grid points.  Each main cell is divided into a subgrid, which has a
' surface fit over it based on the Z values of the corner points.  Isopleths
' are then drawn from subcell edge to subcell edge until they end, or reach
' an edge of the main cell. Alternately, a color map is drawn from the
' subdivided grid by coloring each small block
'
' IMAP    I   the type of contour plot 0=isopleths 1=colormap
' NX,NY   I   number of X and Y grid points, respectively
' X,Y     I   arrays of X and Y locations of grid points
' Z       I   2D array of grid values at grid points
' NC      I   number of contour values in CONTR
' CONTR   I   array of contour values
'
''      DIMENSION X(NX), Y(NY), Z(100, 100), CONTR(NC)
''      DIMENSION TU(4), TV(4), TW(4, 4)
''      DIMENSION U(28), V(28), W(28, 28)
''      DIMENSION UII(10), VI(10), WI(10, 10)
  ReDim TU(1 To 4) As Single
  ReDim TV(1 To 4) As Single
  ReDim tw(1 To 4, 1 To 4) As Single
  ReDim U(1 To 28) As Single
  ReDim V(1 To 28) As Single
  ReDim w(1 To 28, 1 To 28) As Single
  ReDim UII(1 To 10) As Single
  ReDim VI(1 To 10) As Single
  ReDim WI(1 To 10, 1 To 10) As Single
  Dim LX As Long
  Dim LY As Long
  Dim MX As Long
  Dim MY As Long
  Dim NU As Long
  Dim nv As Long

'
' NC is the number of contour levels.  return if there are none to plot
'
      If (NC <= 0) Then Exit Sub
'
' LX,LY are the number of input x and y grid points
' MX,MY are the number of x and y subcells to be made (between points)
' NU,NV are the number of subgrid points to be produced
'
      LX = 4
      LY = 4
      MX = 9
      MY = 9
      NU = (LX - 1) * MX + 1
      nv = (LY - 1) * MY + 1
'
' loop over all the main cells.  Store the four corner points in a
' temporary array (TU,TV,TW).  Send the temp array off to SFCFIT to
' interpolate a subgrid, then call ISOPLQ to draw isopleths through
' the cell
'
'
' The temporary arrays TU, TV, and TW are filled with values from the
' X, Y, and Z arrays.  A 3x3 array of cells surrounding the central
' cell is extracted so that the surrounding cells may influence the
' surface fit in the central cell.  When the central cell is on the
' edge of a main cell surrounding Z values are assumed to be 0.  X and
' Y values are extrapolated based on the distance between the two grid
' points closest to the edge.
'
      For i = 1 To NX - 1
        If (i = 1) Then
          TU(1) = X(i) - (X(i + 1) - X(i))
          TU(2) = X(i)
          TU(3) = X(i + 1)
          TU(4) = X(i + 2)
        ElseIf (i = NX - 1) Then
          TU(1) = X(i - 1)
          TU(2) = X(i)
          TU(3) = X(i + 1)
          TU(4) = X(i + 1) + (X(i + 1) - X(i))
        Else
          TU(1) = X(i - 1)
          TU(2) = X(i)
          TU(3) = X(i + 1)
          TU(4) = X(i + 2)
        End If
        For j = 1 To NY - 1
          If (j = 1) Then
            TV(1) = Y(j) - (Y(j + 1) - Y(j))
            TV(2) = Y(j)
            TV(3) = Y(j + 1)
            TV(4) = Y(j + 2)
          ElseIf (j = NY - 1) Then
            TV(1) = Y(j - 1)
            TV(2) = Y(j)
            TV(3) = Y(j + 1)
            TV(4) = Y(j + 1) + (Y(j + 1) - Y(j))
          Else
            TV(1) = Y(j - 1)
            TV(2) = Y(j)
            TV(3) = Y(j + 1)
            TV(4) = Y(j + 2)
          End If

          If (j = 1) Then
            tw(1, 1) = 0#
            tw(2, 1) = 0#
            tw(3, 1) = 0#
            tw(4, 1) = 0#
          Else
            If (i = 1) Then
              tw(1, 1) = 0#
            Else
              tw(1, 1) = Z(i - 1, j - 1)
            End If
            tw(2, 1) = Z(i, j - 1)
            tw(3, 1) = Z(i + 1, j - 1)
            If (i = NX - 1) Then
              tw(4, 1) = 0#
            Else
              tw(4, 1) = Z(i + 2, j - 1)
            End If
          End If

          If (i = 1) Then
            tw(1, 2) = 0#
          Else
            tw(1, 2) = Z(i - 1, j)
          End If
          tw(2, 2) = Z(i, j)
          tw(3, 2) = Z(i + 1, j)
          If (i = NX - 1) Then
            tw(4, 2) = 0#
          Else
            tw(4, 2) = Z(i + 2, j)
          End If

          If (i = 1) Then
            tw(1, 3) = 0#
          Else
            tw(1, 3) = Z(i - 1, j + 1)
          End If
          tw(2, 3) = Z(i, j + 1)
          tw(3, 3) = Z(i + 1, j + 1)
          If (i = NX - 1) Then
            tw(4, 3) = 0#
          Else
            tw(4, 3) = Z(i + 2, j + 1)
          End If

          If (j = NY - 1) Then
            tw(1, 4) = 0#
            tw(2, 4) = 0#
            tw(3, 4) = 0#
            tw(4, 4) = 0#
          Else
            If (i = 1) Then
              tw(1, 4) = 0#
            Else
              tw(1, 4) = Z(i - 1, j + 2)
            End If
            tw(2, 4) = Z(i, j + 2)
            tw(3, 4) = Z(i + 1, j + 2)
            If (i = NX - 1) Then
              tw(4, 4) = 0#
            Else
              tw(4, 4) = Z(i + 2, j + 2)
            End If
          End If
'
' Test the center cell of the 3x3 array to see if any of the
' contours pass through it.  If not, move on to the next group.
' For colormap comtour plots, if any corner is greater than one of
' the contour values, process the block.
'
          ICROSS = 0
          If (IMAP = 0) Then
            For II = 1 To NC
              If ((tw(2, 2) - CONTR(II)) * (tw(3, 2) - CONTR(II)) < 0) Then ICROSS = 1
              If ((tw(3, 2) - CONTR(II)) * (tw(2, 3) - CONTR(II)) < 0) Then ICROSS = 1
              If ((tw(3, 3) - CONTR(II)) * (tw(2, 3) - CONTR(II)) < 0) Then ICROSS = 1
              If ((tw(2, 3) - CONTR(II)) * (tw(2, 2) - CONTR(II)) < 0) Then ICROSS = 1
            Next
          Else
            For II = 1 To NC
              If (tw(2, 2) >= CONTR(II)) Then ICROSS = 1
              If (tw(3, 2) >= CONTR(II)) Then ICROSS = 1
              If (tw(3, 3) >= CONTR(II)) Then ICROSS = 1
              If (tw(2, 3) >= CONTR(II)) Then ICROSS = 1
            Next
          End If
'
          If (ICROSS > 0) Then
'
' Now take the 3x3 cell array and surface fit a new grid to it.
' Each original cell is divided into a 9x9 grid of subcells. The
' new grid is returned in the arrays U, V, and W
'
            Call surfitq(LX, LY, TU(1), TV(1), tw(1, 1), MX, MY, NU, nv, U(1), V(1), w(1, 1))
'
' For drawing isopleths, we are only interested in the center cell
' of the 3x3 grid (now 27x27).  So we extract the center section
' (10 to 19) and store it in the arrays UII, VI, WI for the next operation
'
            For II = 1 To 10
'tbc units conversion - handle this better someday
              UII(II) = UnitsDisplay(U(II + 9), UN_LENGTH)
              For JJ = 1 To 10
'tbc units conversion - handle this better someday
                VI(JJ) = UnitsDisplay(V(JJ + 9), UN_LENGTH)
                WI(II, JJ) = w(II + 9, JJ + 9)
              Next
            Next
'
' Now draw all the contours stored in the CONTR array
' across the current cell.
' For a colormap plot, color all the subdivided squares
'
            If (IMAP = 0) Then
              Call isoplq(NC, CONTR(), 10, 10, UII(), VI(), WI())
            Else
              Call clrbxq(NC, CONTR(), 10, 10, UII(), VI(), WI())
            End If
'
          End If
'
          DoEvents
        Next 'end of Y loop
        DoEvents
      Next   'end of X loop
End Sub

Public Sub isoplq(NC As Long, CONTR() As Single, NU As Integer, nv As Integer, U() As Single, V() As Single, w() As Single)
'
' Draw isopleth lines
'C
'C        THESE LOOPS CHECK ALL FOUR SIDES OF EACH SUBCELL FOR ISOPLETH
'C        CROSSINGS. THE ORDER IS NORMALLY ((IA=1,NU),JA=1,NV)
'C        UNTIL A CROSSING IS FOUND, THEN THE ISOPLETH IS 'FOLLOWED'
'C        FROM EACH SUBCELL TO THE NEXT, AND EACH SUBCELL IS MARKED
'C        (IFL(I,J)=1) UNTIL THE ISOPLETH BECOMES CLOSED OR LEAVES
'C        THE MAIN GRID CELL. XX1,YY1 IS THE FIRST CROSSING POINT FOUND
'C        IN A SUBCELL, AND (XX2,Y2) IS THE SECOND. I AND J ARE THE
'C        X AND Y COUNTERS FOR THE SUBCELLS. I1 IS THE FLAG FOR HOW MANY
'C        (1 OR 2) CROSSINGS HAVE BEEN FOUND (AND WHICH OF THE 4 SIDES
'C        (XX2,YY2) IS ON) FOR THE CURRENT SUBCELL. KFL IS THE FLAG TO
'C        PREVENT PLOTTER PEN MOVEMENT IF THE PEN IS ALREADY LOCATED AT
'C        (XX1,YY1).
'C        JFL IS THE FLAG WHICH SIGNIFIES THAT A REVERSAL OF DIRECTION
'C        HAS JUST BEEN MADE IN 'FOLLOWING' THE ISOPLETH. LFL IS THE
'C        FLAG WHICH SIGNIFIES WHETHER ISOPLETHS IN THE CURRENT MAIN
'C        CELL AND THE MAIN CELL DIRECTLY TO THE LEFT HAVE BEEN LABELED.
'C
''      DIMENSION CONTR(NC), U(NU), V(NV), W(NU, NV)
''      DIMENSION IFL(50, 50)
''      INTEGER ICLR(10)
''      DATA ICLR/5,12,14,10,3,13,6,4,9,2/
      ReDim IFL(1 To 100, 1 To 50) As Integer
      Dim XX1 As Single
      Dim YY1 As Single
      Dim XX2 As Single
      Dim YY2 As Single
'C
'C        DO LOOP OVER ISOPLETH VALUES, HIGH TO LOW
'C
      For KS = 1 To NC
        Call colorq(GQCD.LevelColor(KS))
        For JSS = 1 To nv
          For ISS = 1 To NU
            IFL(ISS, JSS) = 0
          Next
        Next
        C = CONTR(KS)
'C
'C        LOOPS OVER SUBDIVIDED GRID CELL
'C
        For JA = 1 To nv - 1
          For IA = 1 To NU - 1
            JSS = JA
            ISS = IA
            I1 = 0
            If (IFL(ISS, JSS) = 0) Then
              KFL = 0
              JFL = 0
'tbc debug
'Call boxq(GQ_WORLD, U(IA), V(JA), U(IA + 1), V(JA + 1), 0)

'C        CHECK LOWER SIDE OF SUBCELL
510           If ((w(ISS, JSS) - C) * (w(ISS + 1, JSS) - C) > 0#) Then GoTo 550
'C            LINEAR INTERPOLATION FOR X-COORDINATE OF ISOPLETH
              XX2 = w(ISS, JSS) - w(ISS + 1, JSS)
              DU = U(ISS + 1) - U(ISS)
              If (XX2 <> 0) Then
                XX2 = (w(ISS, JSS) - C) / XX2
                XX2 = U(ISS) + DU * XX2
              Else
                XX2 = U(ISS)
              End If
              YY2 = V(JSS)
'C            DETERMINE IF 2 ISOPLETH INTERSECTIONS HAVE BEEN FOUND FOR
'C            CURRENT SUBCELL
              If (I1 <> 0) Then GoTo 680  'mark cell
              XX1 = XX2
              YY1 = YY2
              I1 = 1


'C        CHECK LEFT SIDE OF SUBCELL
550           If ((w(ISS, JSS) - C) * (w(ISS, JSS + 1) - C) > 0#) Then GoTo 590
'C            LINEAR INTERPOLATION FOR Y-COORDINATE OF ISOPLETH
              YY2 = w(ISS, JSS) - w(ISS, JSS + 1)
              dv = V(JSS + 1) - V(JSS)
              If (YY2 <> 0) Then
                YY2 = (w(ISS, JSS) - C) / YY2
                YY2 = V(JSS) + dv * YY2
              Else
                YY2 = V(JSS)
              End If
              XX2 = U(ISS)
'C            DETERMINE IF 2 ISOPLETH INTERSECTIONS HAVE BEEN FOUND FOR
'C            CURRENT SUBCELL
              If (I1 <> 0) Then GoTo 710
              XX1 = XX2
              YY1 = YY2
              I1 = 2


'C        CHECK UPPER SIDE OF SUBCELL
590           If ((w(ISS, JSS + 1) - C) * (w(ISS + 1, JSS + 1) - C) > 0#) Then GoTo 630
'C            LINEAR INTERPOLATION FOR X-COORDINATE OF ISOPLETH
              XX2 = w(ISS, JSS + 1) - w(ISS + 1, JSS + 1)
              DU = U(ISS + 1) - U(ISS)
              If (XX2 <> 0) Then
                XX2 = (w(ISS, JSS + 1) - C) / XX2
                XX2 = U(ISS) + DU * XX2
              Else
                XX2 = U(ISS)
              End If
              YY2 = V(JSS + 1)
'C            DETERMINE IF 2 ISOPLETH INTERSECTIONS HAVE BEEN FOUND FOR
'C            CURRENT SUBCELL
              If (I1 <> 0) Then GoTo 740
              XX1 = XX2
              YY1 = YY2
              I1 = 3


'C            DETERMINE IF 2 ISOPLETH INTERSECTIONS HAVE BEEN FOUND FOR
'C            CURRENT SUBCELL
630           If (I1 = 0) Then GoTo nextIA


'C        CHECK RIGHT SIDE OF SUBCELL
640           If ((w(ISS + 1, JSS) - C) * (w(ISS + 1, JSS + 1) - C) > 0#) Then GoTo 510
'C            LINEAR INTERPOLATION FOR Y-COORDINATE OF ISOPLETH
              YY2 = w(ISS + 1, JSS) - w(ISS + 1, JSS + 1)
              dv = V(JSS + 1) - V(JSS)
              If (YY2 <> 0) Then
                YY2 = (w(ISS + 1, JSS) - C) / YY2
                YY2 = V(JSS) + dv * YY2
              Else
                YY2 = V(JSS)
              End If
              XX2 = U(ISS + 1)
              
              GoTo 770

'==================================================================

'         Lower side of subcell
'C        MARK CURRENT SUBCELL AS HAVING BEEN CHECKED FOR CURRENT
'C        ISOPLETH VALUE
680           IFL(ISS, JSS) = 1
'C        LOOK FOR NEXT SUBCELL AT EITHER END OF THE LINE SEGMENT IN THE
'C        CURRENT SUBCELL
690           If (JSS = 1) Then GoTo 800
              If (IFL(ISS, JSS - 1) <> 0) Then GoTo 800
              JSS = JSS - 1
              If (KFL = 0) Then
                KFL = 1
                Call moveq(GQ_WORLD, XX1, YY1)
              Else
                Call drawq(GQ_WORLD, XX2, YY2)
              End If
              I1 = 3
              JFL = 0
              XX1 = XX2
              YY1 = YY2
              GoTo 640  'right side check


'         Left side of subcell
'C        MARK CURRENT SUBCELL AS HAVING BEEN CHECKED FOR CURRENT
'C        ISOPLETH VALUE
710           IFL(ISS, JSS) = 1
'C        LOOK FOR NEXT SUBCELL AT EITHER END OF THE LINE SEGMENT IN THE
'C        CURRENT SUBCELL
720           If (ISS = 1) Then GoTo 800
              If (IFL(ISS - 1, JSS) <> 0) Then GoTo 800
              ISS = ISS - 1
              If (KFL = 0) Then
                KFL = 1
                Call moveq(GQ_WORLD, XX1, YY1)
              Else
                Call drawq(GQ_WORLD, XX2, YY2)
              End If
              I1 = 4
              JFL = 0
              XX1 = XX2
              YY1 = YY2
              GoTo 510   'lower side check


'         Upper side of subcell
'C        MARK CURRENT SUBCELL AS HAVING BEEN CHECKED FOR CURRENT
'C        ISOPLETH VALUE
740           IFL(ISS, JSS) = 1
'C        LOOK FOR NEXT SUBCELL AT EITHER END OF THE LINE SEGMENT IN THE
'C        CURRENT SUBCELL
750           If (JSS = nv - 1) Then GoTo 800
              If (IFL(ISS, JSS + 1) <> 0) Then GoTo 800
              JSS = JSS + 1
              If (KFL = 0) Then
                KFL = 1
                Call moveq(GQ_WORLD, XX1, YY1)
              Else
                Call drawq(GQ_WORLD, XX2, YY2)
              End If
              I1 = 1
              JFL = 0
              XX1 = XX2
              YY1 = YY2
              GoTo 550    'Left side check


'         Right side of subcell
'C        MARK CURRENT SUBCELL AS HAVING BEEN CHECKED FOR CURRENT
'C        ISOPLETH VALUE
770           IFL(ISS, JSS) = 1
'C        LOOK FOR NEXT SUBCELL AT EITHER END OF THE LINE SEGMENT IN THE
'C        CURRENT SUBCELL
780           If (ISS = NU - 1) Then GoTo 800
              If (IFL(ISS + 1, JSS) <> 0) Then GoTo 800
              ISS = ISS + 1
              If (KFL = 0) Then
                KFL = 1
                Call moveq(GQ_WORLD, XX1, YY1)
              Else
                Call drawq(GQ_WORLD, XX2, YY2)
              End If
              I1 = 2
              JFL = 0
              XX1 = XX2
              YY1 = YY2
              GoTo 590  'Upper side check

'=======================================================================

'C        REVERSE DIRECTION OF FOLLOWING ISOPLETH AND TRY AGAIN, OR IF
'C        REVERSAL HAS ALREADY BEEN MADE, DRAW LINE SEGMENT AND GO TO EN
'C        OF LOOP FOR IA
800           If (JFL = 0) Then
                JFL = 1
                temp = XX1
                XX1 = XX2
                XX2 = temp
                temp = YY1
                YY1 = YY2
                YY2 = temp
                If (I1 = 1) Then
                  GoTo 690  'lower side
                ElseIf (I1 = 2) Then
                  GoTo 720  'left side
                ElseIf (I1 = 3) Then
                  GoTo 750  'Upper side
                ElseIf (I1 = 4) Then
                  GoTo 780  'right side
                End If
              End If
              If (KFL = 0) Then
                Call moveq(GQ_WORLD, XX2, YY2)
              Else
                Call drawq(GQ_WORLD, XX1, YY1)
              End If
            
            End If  'if(ifl(iss,jss)=0)then...
nextIA:
          Next      'IA
          DoEvents
        Next        'JA
      Next          'KS (contour value)

      Call colorq(0)
End Sub

