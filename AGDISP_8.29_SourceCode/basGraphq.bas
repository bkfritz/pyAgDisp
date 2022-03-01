Attribute VB_Name = "basGRAPHQ"
' $Id: basGraphq.bas,v 1.1 2001/10/09 18:49:59 tom Exp $
'graphq.bas - plotting library, developed from fortran
'             version.
'
'user constants
'
Public Const GQ_DEVICE = 0
Public Const GQ_WORLD = 1
'
Public Const GQ_XAXIS = 0
Public Const GQ_YAXIS = 1
'
Public Const GQ_LINEAR = 0
Public Const GQ_LOG = 1
'
Public Const GQ_NOGRID = 0
Public Const GQ_GRID = 1
'
Public Const GQ_NOBOX = 0
Public Const GQ_BOX = 1
'
Public Const GQ_ALIGN_LEFT = 0
Public Const GQ_ALIGN_CENTER = 1
Public Const GQ_ALIGN_RIGHT = 2
Public Const GQ_ALIGN_BOTTOM = 0
Public Const GQ_ALIGN_TOP = 2
'
'internal graphq constants
'
Const GQ_MAJ_TIC_HEIGHT = 150  'tic height in twips
Const GQ_MIN_TIC_HEIGHT = 75
Const GQ_TICL_OFFSET = 75      'tic label offset from axis

'define a data type to hold Graphq info
Type GRAPHQ
  VXDS As Single    'viewport start in device coords for X
  VXDE As Single    'viewport end in device coords for X
  VXDL As Single    'viewport width in device coords for X
  VYDS As Single    'viewport start in device coords for Y
  VYDE As Single    'viewport end in device coords for Y
  VYDL As Single    'viewport width in device coords for Y
  VXWS As Single    'viewport start in world coords for X
  VXWE As Single    'viewport end in world coords for X
  VXWL As Single    'viewport width in world coords for X
  VYWS As Single    'viewport start in world coords for Y
  VYWE As Single    'viewport end in world coords for Y
  VYWL As Single    'viewport width in world coords for Y
  DCWCX As Single   'ratio of DC to WC for x
  DCWCY As Single   'ratio of DC to WC for y
  XDOLD As Single   'Last x point of move or draw in DC
  YDOLD As Single   'Last y point of move or draw in DC
  XWOLD As Single   'Last x point of move or draw in WC
  YWOLD As Single   'Last y point of move or draw in WC
  IXLOG As Integer  'Log/Linear flag for X axis
  IYLOG As Integer  'Log/Linear flag for Y axis
  CurColor As Long  'Current color for drawing
  CurStyle As Integer  'Current line style
  CurMarker As Integer 'Current line marker type
End Type

'module-wide variable definition
Dim GQ As GRAPHQ      'type to hold most of the info
Dim GQoutObj As Control 'output object, if outSel is GQ_CONTROL

Public Sub isoplq(NC As Long, CONTR() As Single, NU As Integer, nv As Integer, U() As Single, V() As Single, w() As Single)
'
' Generate isopleth geometry
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
      ReDim IFL(1 To NU, 1 To nv) As Integer
      Dim XX1 As Single
      Dim YY1 As Single
      Dim XX2 As Single
      Dim YY2 As Single
      Dim slot As Integer
      Dim Xtbc As Single
      Dim Ytbc As Single
'C
'C        DO LOOP OVER ISOPLETH VALUES, HIGH TO LOW
'C
      For KS = 1 To NC
        For JSS = 1 To nv
          For ISS = 1 To NU
            IFL(ISS, JSS) = 0
          Next
        Next
        c = CONTR(KS - 1)
        slot = KS - 1
        PlotSetDataTitle slot, AGFormat$(c)
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
510           If ((w(ISS, JSS) - c) * (w(ISS + 1, JSS) - c) > 0#) Then GoTo 550
'C            LINEAR INTERPOLATION FOR X-COORDINATE OF ISOPLETH
              XX2 = w(ISS, JSS) - w(ISS + 1, JSS)
              DU = U(ISS + 1) - U(ISS)
              If (XX2 <> 0) Then
                XX2 = (w(ISS, JSS) - c) / XX2
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
550           If ((w(ISS, JSS) - c) * (w(ISS, JSS + 1) - c) > 0#) Then GoTo 590
'C            LINEAR INTERPOLATION FOR Y-COORDINATE OF ISOPLETH
              YY2 = w(ISS, JSS) - w(ISS, JSS + 1)
              dv = V(JSS + 1) - V(JSS)
              If (YY2 <> 0) Then
                YY2 = (w(ISS, JSS) - c) / YY2
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
590           If ((w(ISS, JSS + 1) - c) * (w(ISS + 1, JSS + 1) - c) > 0#) Then GoTo 630
'C            LINEAR INTERPOLATION FOR X-COORDINATE OF ISOPLETH
              XX2 = w(ISS, JSS + 1) - w(ISS + 1, JSS + 1)
              DU = U(ISS + 1) - U(ISS)
              If (XX2 <> 0) Then
                XX2 = (w(ISS, JSS + 1) - c) / XX2
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
640           If ((w(ISS + 1, JSS) - c) * (w(ISS + 1, JSS + 1) - c) > 0#) Then GoTo 510
'C            LINEAR INTERPOLATION FOR Y-COORDINATE OF ISOPLETH
              YY2 = w(ISS + 1, JSS) - w(ISS + 1, JSS + 1)
              dv = V(JSS + 1) - V(JSS)
              If (YY2 <> 0) Then
                YY2 = (w(ISS + 1, JSS) - c) / YY2
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
                Xtbc = XX1
                Ytbc = YY1
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_MOVETO
              Else
                Xtbc = XX2
                Ytbc = YY2
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_DRAWTO
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
                Xtbc = XX1
                Ytbc = YY1
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_MOVETO
              Else
                Xtbc = XX2
                Ytbc = YY2
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_DRAWTO
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
                Xtbc = XX1
                Ytbc = YY1
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_MOVETO
              Else
                Xtbc = XX2
                Ytbc = YY2
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_DRAWTO
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
                Xtbc = XX1
                Ytbc = YY1
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_MOVETO
              Else
                Xtbc = XX2
                Ytbc = YY2
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_DRAWTO
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
                Xtbc = XX2
                Ytbc = YY2
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_MOVETO
              Else
                Xtbc = XX1
                Ytbc = YY1
                Call agrtrn(Xtbc, Ytbc)
                PlotDataAddPoint slot, Xtbc, Ytbc, GQ_DRAWTO
              End If
            
            End If  'if(ifl(iss,jss)=0)then...
nextIA:
          Next      'IA
          DoEvents
        Next        'JA
      Next          'KS (contour value)

      Call colorq(0)
End Sub

Sub autoscq(iflog As Integer, smin As Single, smax As Single, sinc As Single)
'Autoscale axis limits
'  Call this routine before axisq and your axis will
'  be prettier.
'
' iflog    I    log scale flag 1=log 0=linear
' smin    I/O   lower value for scale
' smax    I/O   upper value for scale
' sinc     O    distance between major tics
'
  'linear axes
  If iflog = GQ_LINEAR Then
    'check for zero width
    If (smax! - smin!) <= 0! Then
      smin! = 0
      smax! = 1
      sinc! = 0.5
      Exit Sub
    End If
    wdth! = Log(smax! - smin!) / 2.3026
    iwdth = Fix(wdth!)
    tem! = iwdth
    If tem! > wdth! Then iwdth = iwdth - 1
    tem! = iwdth
    factor! = wdth! - tem!
    xwdth! = 10! ^ iwdth
    xsdlt! = 0.2 * xwdth!
    If factor! > 0.2 Then xsdlt! = 0.5 * xwdth!
    If factor! > 0.6 Then xsdlt! = xwdth!
    If factor! > 0.9 Then xsdlt! = 2! * xwdth!
    tem! = Fix(smin! / xsdlt!)
    xsmin! = xsdlt! * tem!
    If xsmin! > smin! Then xsmin! = xsmin! - xsdlt!
    tem! = Fix(smax! / xsdlt!)
    xsmax! = xsdlt! * tem!
    If xsmax! < smax! Then xsmax! = xsmax! + xsdlt!
    smin! = xsmin!
    smax! = xsmax!
    sinc! = xsdlt!
  'log axes
  ElseIf iflog = GQ_LOG Then
    'check for bad limits
    If smin! < 1E-36 Then smin! = 1E-36
    If smax! < 1E-36 Then smax! = 1E-36

    'check for zero width
    If (smax! - smin!) <= 0! Then
      smin! = 1
      smax! = 10
      sinc! = 1
      Exit Sub
    End If
    Nmin = Int(log10q(smin!))
    If smin! < 10! ^ Nmin Then Nmin = Nmin - 1
    smin! = 10! ^ Nmin
    Nmax = Int(log10q(smax!))
    If smax! > 10! ^ Nmax Then Nmax = Nmax + 1
    smax! = 10! ^ Nmax
    'adjust exponent increment for about 10 labels
    sinc! = Int((Nmax - Nmin) / 10! + 0.5)
    If sinc! < 1 Then sinc! = 1
  End If
End Sub

Sub axisq(IXY As Integer, ilog As Integer, IGRD As Integer, DMAJ As Single, NMINOR As Integer)
'
' USER ROUTINE
'
' Sets axis data and optionally draws axis
' Use this routine to do one or all of the following:
'   - Make an axis log or linear
'   - Draw tic marks on an axis and label them
'   - Make a grid on the plot window
'
' Argument  I/O  Purpose
' ========  ===  =======
' IXY        I   Choose x or y axis
'                 0 = x axis, labels on bottom
'                 1 = y axis, labels on left
'                 2 = x axis, labels on top
'                 3 = y axis, labels on right
' ILOG       I   Choose log or linear
'                 0 = linear
'                 1  = log
' IGRD       I   Grid flag.  If set, the major tics marks run from
'                one side of the plot window to the other, creating
'                a grid.
'                 0 = no grid, normal tic marks
'                 1 = make a grid by running the tics marks across
'                     the plot window
' DMAJ       I   Width of the space between major tic marks (in WC)
'                 set to 0.0 to defeat plotting of tic marks
' NMINOR     I   Number of minor tics between major tics
'                 set to 0 for no minor tics
'
' Save the value of ILOG in the common block if required
' If log is requested, redefine out-of-bounds limits and recalc
' some parameters.
'
      If (IXY And 1) = 0 Then
        GQ.IXLOG = ilog
        If (ilog = GQ_LOG) Then
'         adjust the lower bound to lie on a decade
          If GQ.VXWS <= 0 Then GQ.VXWS = 1E-30
          X = GQ.VXWS
          n = Int(log10q(X))
          If (X < 10 ^ n) Then n = n - 1
          X = 10 ^ n
          GQ.VXWS = X
'
'         adjust the upper bound to lie on a decade
          If GQ.VXWE <= 0 Then GQ.VXWE = 1E-30
          X = GQ.VXWE
          n = Int(log10q(X))
          If (X > 10# ^ n) Then n = n + 1
          X = 10# ^ n
          GQ.VXWE = X
'
          GQ.VXWL = GQ.VXWE - GQ.VXWS
          GQ.DCWCX = (GQ.VXDE - GQ.VXDS) / (log10q(GQ.VXWE) - log10q(GQ.VXWS))
        End If
      ElseIf (IXY And 1) = 1 Then
        GQ.IYLOG = ilog
        If (ilog = GQ_LOG) Then
'         adjust the lower bound to lie on a decade
          If GQ.VYWS <= 0 Then GQ.VYWS = 1E-30
          X = GQ.VYWS
          n = Int(log10q(X))
          If (X < 10# ^ n) Then n = n - 1
          X = 10# ^ n
          GQ.VYWS = X
'
'         adjust the upper bound to lie on a decade
          If GQ.VYWE <= 0 Then GQ.VYWE = 1E-30
          X = GQ.VYWE
          n = Int(log10q(X))
          If (X > 10# ^ n) Then n = n + 1
          X = 10# ^ n
          GQ.VYWE = X
'
          GQ.VYWL = GQ.VYWE - GQ.VYWS
          GQ.DCWCY = (GQ.VYDE - GQ.VYDS) / (log10q(GQ.VYWE) - log10q(GQ.VYWS))
        End If
      End If
'
' Draw the tic marks if required
'
      If (DMAJ > 0#) Then
        If (ilog = GQ_LINEAR) Then
          Call tics1q(IXY, IGRD, DMAJ, NMINOR)
        Else
          Call tics2q(IXY, IGRD, DMAJ, NMINOR)
        End If
      End If
'
' Draw tic labels if required
'
      If (ilog = GQ_LINEAR) Then
        Call ticl1q(IXY, DMAJ)
      Else
        Call ticl2q(IXY, DMAJ)
      End If
End Sub

Sub bounq(X As Single, Y As Single, CR As Single, CL As Single, CT As Single, CB As Single, IEL() As Integer)
'
' Checks point X,Y to see if it lies within the borders CR,CL,CT,CB
' and returns IEL array to show status
'
      If (CR > CL) Then
        If (X > CR) Then
          IEL(1) = 1
        Else
          IEL(1) = 0
        End If
        If (X < CL) Then
          IEL(2) = 1
        Else
          IEL(2) = 0
        End If
      Else
        If (X < CR) Then
          IEL(1) = 1
        Else
          IEL(1) = 0
        End If
        If (X < CL) Then
          IEL(2) = 1
        Else
          IEL(2) = 0
        End If
      End If

      If (CT > CB) Then
        If (Y > CT) Then
          IEL(3) = 1
        Else
          IEL(3) = 0
        End If
        If (Y < CB) Then
          IEL(4) = 1
        Else
          IEL(4) = 0
        End If
      Else
        If (Y < CT) Then
          IEL(3) = 1
        Else
          IEL(3) = 0
        End If
        If (Y > CB) Then
          IEL(4) = 1
        Else
          IEL(4) = 0
        End If
      End If
End Sub

Sub boxq(ICS As Integer, XA As Single, YA As Single, XB As Single, YB As Single, IFILL As Integer)
'
' Creates a box in the current color that is optionally filled
'
' Argument   I/O  Purpose
' ========   ===  =======
' ICS         I   coordinate system: 0=device coords  1=world coords
' XA,YA,XB,YB I   X,Y diagonal corner points of box (DC or WC)
' IFILL       I   fill flag 0=outline of box 1=filled box
'
      Dim XA1 As Single
      Dim YA1 As Single
      Dim XB1 As Single
      Dim YB1 As Single
'
' Translate to DC if necessary
'
      If (ICS = 0) Then
        XA1 = XA
        YA1 = YA
        XB1 = XB
        YB1 = YB
      Else
        Call wcdcq(XA, YA, XA1, YA1)
        Call wcdcq(XB, YB, XB1, YB1)
      End If
'
' Call the appropriate macine-specific routine
'
      If IFILL = 0 Then
        GQoutObj.Line (XA1, YA1)-(XB1, YB1), GQ.CurColor, B
      Else
        GQoutObj.Line (XA1, YA1)-(XB1, YB1), GQ.CurColor, BF
      End If
End Sub

Sub clipq(XS As Single, YS As Single, XE As Single, YE As Single, CR As Single, CL As Single, CT As Single, CB As Single, ICLIP As Integer)
'
' Clips the endpoints of the given line segment according to the borders
' CR,CL,CT,CB and returns status in ICLIP
'
'  INTERNAL ROUTINE
'
' Cohen-Sutherland two-step clipping algorithm taken from
' 'Applied Concepts in Microcomputer Graphics' Bruce Artwick
'
' Argument  I/O  Purpose
' ========  ===  =======
' XS,YS     I/O  Start point of line segment
' XE,YE     I/O  End point of line segment
' CR,CL,     I   right, left, top, and bottom borders
' CT,CB
' ICLIP      O   return status
'                 0 = no clipping necessary
'                 1 = one or both endpoints clipped
'                 2 = no clipping possible, segment out of view
'
      Static ist(4) As Integer, ien(4) As Integer
'
' Swap flag keeps track of original order of points
'
      ISWAP = False
      ICLIP = 0
'
' Is the end point within the boundaries?
'
      Do
        Call bounq(XS, YS, CR, CL, CT, CB, ist())
        Call bounq(XE, YE, CR, CL, CT, CB, ien())
'       is the end point within borders?
        If (ien(1) + ien(2) + ien(3) + ien(4) = 0) Then
'         is the start point within borders?
          If (ist(1) + ist(2) + ist(3) + ist(4) = 0) Then
'           done. swap back the start and end points if necessary, and return
            If (ISWAP) Then
              XT = XS
              XS = XE
              XE = XT
              YT = YS
              YS = YE
              YE = YT
            End If
            Exit Sub
          Else
'           swap the start and end points
            ISWAP = Not ISWAP
            XT = XS
            XS = XE
            XE = XT
            YT = YS
            YS = YE
            YE = YT
            For i = 1 To 4
              it = ist(i)
              ist(i) = ien(i)
              ien(i) = it
            Next
          End If
        End If
'
' if any corrsponding elements are both 1, line is off the screen
'
        For i = 1 To 4
          If (ist(i) = 1 And ien(i) = 1) Then
            ICLIP = 2
            Exit Sub
          End If
        Next
'
' Push the end point
'
        ICLIP = 1
        Call pushq(XS, YS, XE, YE, CR, CL, CT, CB, ien())
'
      Loop
End Sub

Sub colorq(c As Long)
'Set the current drawing color
  GQ.CurColor = c
End Sub

Sub drawq(ICS As Integer, XI As Single, YI As Single)
'
' Draws a line on the output device from the previous position to
' the point specified in the arguments.  The point may be specified
' in WC or DC, as specified by ICS.  All clipping is done in DC.
' Points specified in DC are clipped according to the visible screen
' area.  Points specified in WC are clipped according to the current
' view window limits.
'
' Argument  I/O  Purpose
' ========  ===  =======
' ICS        I   coordinate system: 0=device coords  1=world coords
' XI         I   X coord of point to which line should be drawn (DC or WC)
' YI         I   Y coord of point to which line should be drawn (DC or WC)
'
      Dim X As Single
      Dim Y As Single
      Dim X1 As Single
      Dim Y1 As Single
      Dim X2 As Single
      Dim Y2 As Single
      Dim CL As Single
      Dim CR As Single
      Dim CT As Single
      Dim CB As Single
      Dim ICLIP As Integer

      On Error Resume Next
'
' Load input values into local storage
'
      X = XI
      Y = YI
'
' guard against bad points for LOG scales
'
      If (GQ.IXLOG = GQ_LOG And X < 1E-36) Then Exit Sub
      If (GQ.IYLOG = GQ_LOG And Y < 1E-36) Then Exit Sub
'
' Recall the 'current' point in DC and place in working storage
'
      X1 = GQ.XDOLD
      Y1 = GQ.YDOLD
'
' place the new point in working storage, translating to DC if necessary
'
      If (ICS = 0) Then
        X2 = X
        Y2 = Y
      Else
        Call wcdcq(X, Y, X2, Y2)
      End If
'
' clip the new line segment
'
'     set the appropriate clipping boundaries
      If (ICS = 0) Then
        CL = 0#
        CR = Screen.Width
        CT = Screen.Height
        CB = 0#
      Else
        CL = GQ.VXDS
        CR = GQ.VXDE
        CT = GQ.VYDS
        CB = GQ.VYDE
      End If
'
'     Clip the endpoints, return status
'     ICLIP=0  no clipping necessary, segment in view
'     ICLIP=1  clipped one or both endpoint(s)
'     ICLIP=2  no clipping possible, segment out of view
      Call clipq(X1, Y1, X2, Y2, CR, CL, CT, CB, ICLIP)
'
'     If the line is visible, draw it
      If (ICLIP = 0) Then
        GQoutObj.Line -(X2, Y2), GQ.CurColor
      ElseIf (ICLIP = 1) Then
        GQoutObj.Line (X1, Y1)-(X2, Y2), GQ.CurColor
      End If
'
' Save the new unclipped 'current' positions
'
      If (ICS = 0) Then
        GQ.XDOLD = X
        GQ.YDOLD = Y
      Else
        GQ.XWOLD = X
        GQ.YWOLD = Y
        Call wcdcq(X, Y, X1, Y1)
        GQ.XDOLD = X1
        GQ.YDOLD = Y1
      End If
End Sub

Sub initq(outObj As Control)
'initialize graphq system and select output destination
'
' outObj  i   output destination object, e.g. Picturebox or Printer
'
' Store the inputs in the data area
Set GQoutObj = outObj
'
' Fiddle with printer scales to fill printed page,
' but retain plot aspect ratio
'
'tbcIf GQoutObj Is Printer Then
'tbc  'sync the scales modes
'tbc  Printer.ScaleMode = GQoutObj.ScaleMode
'tbc  'calculate aspect ratios for comparison
'tbc  ARobj = GQoutObj.Width / GQoutObj.Height
'tbc  ARprinter = Printer.Width / Printer.Height
'tbc  'Set scaling on the Printer object
'tbc  If ARobj > ARprinter Then
'tbc    WidthFactor = GQoutObj.Width / Printer.Width
'tbc    Printer.ScaleMode = 0  'User mode
'tbc    Printer.ScaleWidth = GQoutObj.Width
'tbc    Printer.ScaleHeight = Printer.Height * WidthFactor
'tbc  Else
'tbc    HeightFactor = GQoutObj.Height / Printer.Height
'tbc    Printer.ScaleMode = 0  'User mode
'tbc    Printer.ScaleHeight = GQoutObj.Height
'tbc    Printer.ScaleWidth = Printer.Width * HeightFactor
'tbc  End If
'tbcEnd If
'
' The output object is set to twips
'
  outObj.ScaleMode = vbTwips
'
'Set internal GRAPHQ variables to initial values
'
  GQ.VXDS = 0
  GQ.VXDE = 0
  GQ.VXDL = 0
  GQ.VYDS = 0
  GQ.VYDE = 0
  GQ.VYDL = 0
  GQ.VXWS = 0
  GQ.VXWE = 0
  GQ.VXWL = 0
  GQ.VYWS = 0
  GQ.VYWE = 0
  GQ.VYWL = 0
  GQ.DCWCX = 0
  GQ.DCWCY = 0
  GQ.XDOLD = 0
  GQ.YDOLD = 0
  GQ.XWOLD = 0
  GQ.YWOLD = 0
  GQ.IXLOG = 0
  GQ.IYLOG = 0
  GQ.CurColor = 0
  GQ.CurStyle = 0
  GQ.CurMarker = 0
End Sub

Function log10q(X) As Single
'compute the log base 10 of a number
  log10q = Log(X) / Log(10)
End Function

Sub moveq(ICS As Integer, XI As Single, YI As Single)
'
' Moves the current position from the previous position to
' the point specified in the arguments.  The point may be specified
' in WC or DC, as specified by ICS
'
' Argument  I/O  Purpose
' ========  ===  =======
' ICS        I   coordinate system: 0=device coords  1=world coords
' XI         I   X coord of point to which to move (DC or WC)
' YI         I   Y coord of point to which to move (DC or WC)
'
      Dim X As Single
      Dim Y As Single

      On Error Resume Next
'
' guard against bad points for LOG scales
'
      If (GQ.IXLOG = GQ_LOG And XI < 1E-36) Then Exit Sub
      If (GQ.IYLOG = GQ_LOG And YI < 1E-36) Then Exit Sub
'
' Translate to DC if necessary
'
      If (ICS = 0) Then
        X = XI
        Y = YI
      Else
        Call wcdcq(XI, YI, X, Y)
      End If
'
' Call the machine-specific move routine
'
      GQoutObj.CurrentX = X
      GQoutObj.CurrentY = Y
'
' Save the new point as the current point
'
      GQ.XDOLD = X
      GQ.YDOLD = Y
      If (ICS = 1) Then
        GQ.XWOLD = XI
        GQ.YWOLD = YI
      End If
End Sub

Sub pushq(XS As Single, YS As Single, XE As Single, YE As Single, CR As Single, CL As Single, CT As Single, CB As Single, ien() As Integer)
'
' "pushes" the end point of a line segment back to the edge of the
'  specifed boundaries.
'
'  INTERNAL ROUTINE
'
' Argument  I/O  Purpose
' ========  ===  =======
' XS,YS     I/O  Start point of line segment
' XE,YE     I/O  End point of line segment
' CR,CL,     I   right, left, top, and bottom borders
' CT,CB
' IEN        I   array of flags indicating whether the end point of the
'                line is beyond the right,left,top, and bottom borders.
'                  0 = point is within border
'                  1 = point lies beyond border
'
      Dim BORDER As Single, SL As Single
'
      If (ien(1) = 1) Then
        BORDER = CR
        SL = (YE - YS) / (XE - XS)
        XE = BORDER
        YE = SL * (BORDER - XS) + YS
      ElseIf (ien(2) = 1) Then
        BORDER = CL
        SL = (YE - YS) / (XE - XS)
        XE = BORDER
        YE = SL * (BORDER - XS) + YS
      ElseIf (ien(3) = 1) Then
        BORDER = CT
        SL = (XE - XS) / (YE - YS)
        XE = SL * (BORDER - YS) + XS
        YE = BORDER
      Else
        BORDER = CB
        SL = (XE - XS) / (YE - YS)
        XE = SL * (BORDER - YS) + XS
        YE = BORDER
      End If
End Sub

Sub styleq(Sty As Integer)
'Change the current drawing style for lines
'  Sty  I  Style 0=Solid (default)
'                1=Dash
'                2=Dot
'                3=Dash-Dot
'                4=Dash-Dot-Dot
'                5=Transparent
'                6=Inside Solid
'
  GQ.CurStyle = Sty
  GQoutObj.DrawStyle = Sty
End Sub

Public Sub textq(s As String, _
                 Optional ICSI, Optional XI, Optional YI, _
                 Optional haligni, Optional valigni)
'
' Draws text at the current position, or the supplied position. The text is
' optionally aligned horizontally and/or vertically.
' If specified, the point may be in WC or DC, as indicated by ICS
'
' Argument  I/O  Purpose
' ========  ===  =======
' S          I   string to draw
' ICSI       I   coordinate system: 0=device coords  1=world coords
' XI         I   X coord of point to which to move (DC or WC)
' YI         I   Y coord of point to which to move (DC or WC)
' HALIGNI    I   horizonal alignment (default: GQ_ALIGN_LEFT)
' VALIGNI    I   Vertical alignment (default: GQ_ALING_BOTTOM)
'
  Dim ICS As Integer
  Dim X As Single
  Dim Y As Single
  Dim Halign As Integer
  Dim Valign As Integer

  Dim tw As Single
  Dim th As Single
  
  If IsMissing(ICS) Or IsMissing(XI) Or IsMissing(YI) Then
    ICS = GQ_DEVICE
    X = GQoutObj.CurrentX
    Y = GQoutObj.CurrentY
  Else
    ICS = ICSI
    X = XI
    Y = YI
  End If
  
  If IsMissing(haligni) Then
    Halign = GQ_ALIGN_LEFT
  Else
    Halign = haligni
  End If
  If IsMissing(valigni) Then
    Valign = GQ_ALIGN_BOTTOM
  Else
    Valign = valigni
  End If

  On Error Resume Next
'
' guard against bad points for LOG scales
'
  If (GQ.IXLOG = GQ_LOG And X < 1E-36) Then Exit Sub
  If (GQ.IYLOG = GQ_LOG And Y < 1E-36) Then Exit Sub
'
' Translate to DC if necessary
'
  If ICS = GQ_WORLD Then Call wcdcq(X, Y, X, Y)
'
' Shift the start location
'
  tw = GQoutObj.TextWidth(s)
  th = GQoutObj.TextHeight(s)
  Select Case Halign
  Case GQ_ALIGN_CENTER
    X = X - 0.5 * tw
  Case GQ_ALIGN_RIGHT
    X = X - tw
  End Select
  Select Case Valign
  Case GQ_ALIGN_CENTER
    Y = Y - 0.5 * th
  Case GQ_ALIGN_BOTTOM
    Y = Y - th
  End Select
'
' Call the machine-specific move routine
'
  GQoutObj.CurrentX = X
  GQoutObj.CurrentY = Y
'
' Draw the text
'
  GQoutObj.Print s;
'
' Save the new point as the current point
'
  GQ.XDOLD = X
  GQ.YDOLD = Y
  If (ICS = 1) Then
    GQ.XWOLD = XI
    GQ.YWOLD = YI
  End If
End Sub

Sub ticdq(X, Y, DCLEN, IDIR)
'
' INTERNAL ROUTINE
'
' Draws a tic mark of length DCLEN starting ay x,y (in DC) in the
' direction of IDIR.
'
'
' Argument  I/O  Purpose
' ========  ===  =======
' X,Y        I   Starting point of tic mark in DC
' DCLEN      I   Length of tic mark in DC
' IDIR       I   Direction of tic mark
'                 0 = up
'                 1 = right
'                 2 = down
'                 3 = left
'
' calc the endpoint of the tic mark
'   X,Y is the start point
'   X1,Y1 is the endpoint
'
      X1 = X
      Y1 = Y
      If (IDIR = 0) Then
        Y1 = Y1 - DCLEN
      ElseIf (IDIR = 1) Then
        X1 = X1 + DCLEN
      ElseIf (IDIR = 2) Then
        Y1 = Y1 + DCLEN
      Else
        X1 = X1 - DCLEN
      End If
'
' draw the tic mark
'
      GQoutObj.Line (X, Y)-(X1, Y1)
End Sub

Sub ticl1q(IXY As Integer, DMAJ As Single)
'
' Draw labels under/beside tic marks - linear
'
' Argument  I/O  Purpose
' ========  ===  =======
' IXY        I   Choose x or y axis
'                 bit 0: axis                        0=x  1=y
'                 bit 1: place labels on other side  0=no 1=yes
' so, IXY:
'                   0 = x axis, bottom;  2 = x axis, top
'                   1 = y axis, left;    3 = y axis, right
'
' DMAJ       I   Width of the space between major tic marks (in WC)
'                 set to 0.0 to defeat plotting of tic marks
'
      Dim X1 As Single, Y1 As Single
      Dim ST As String
      Dim twidth As Single
      Dim SDMAJ As Single
      Dim th As Single, tw As Single
      Dim fmt As String
'
' check for no tics
'
      If (DMAJ <= 0#) Then Exit Sub
'
' save the precision of DMAJ for formatting the labels
'
  fmt = "0"  'by default the format has no decimal point
  ST = Format$(DMAJ)  'convert the increment into a string
  'if the number has a decimal point, but is not in
  'scientific notation, count the number of places after
  'the decimal point and set up the format to match.
  If InStr(ST, ".") > 0 And InStr(ST, "E") = 0 Then
    nplaces = Len(ST) - InStr(ST, ".")
    fmt = fmt + "." + String$(nplaces, "0")
  End If
'
'  twidth of window in WC
'  SDMAJ is DMAJ with the sign of twidth
'
      If (IXY And 1) = 0 Then
        twidth = GQ.VXWL
      ElseIf (IXY And 1) = 1 Then
        twidth = GQ.VYWL
      End If
      SDMAJ = DMAJ * Sgn(twidth)
'
' Main tic drawing loop
'
'     figure out how many major tics there are, then loop that many times
      NTICS = Int(twidth / DMAJ)
      For i = 0 To Abs(NTICS)
'       calc the starting pos of the tic marks and save the label value
        If (IXY And 1) = 0 Then             'x axis
          X1 = GQ.VXWS + (i * SDMAJ)
          If ((IXY / 2) And 1) = 0 Then
            Y1 = GQ.VYWS
          ElseIf ((IXY / 2) And 1) = 1 Then
            Y1 = GQ.VYWE
          End If
          VLAB = X1
        ElseIf (IXY And 1) = 1 Then         'y axis
          If ((IXY / 2) And 1) = 0 Then
            X1 = GQ.VXWS
          ElseIf ((IXY / 2) And 1) = 1 Then
            X1 = GQ.VXWE
          End If
          Y1 = GQ.VYWS + (i * SDMAJ)
          VLAB = Y1
        End If
'       convert world coords to device coords
        Call wcdcq(X1, Y1, X1, Y1)
'       convert the label value to a string
        ST = Format$(VLAB, fmt)
'       adjust the x,y location for the string
'       x-axis: label is centered on the tic and one character below/above it
'       y-axis: label is centered on the tic and a character to the left/right
        'find the width and height of the text
        tw = GQoutObj.TextWidth(ST)
        th = GQoutObj.TextHeight(ST)
        If (IXY And 3) = 0 Then         'x axis, bottom
          Y1 = Y1 + GQ_TICL_OFFSET
          X1 = X1 - (tw / 2#)
        ElseIf (IXY And 3) = 2 Then     'x axis, top
          Y1 = Y1 - th - GQ_TICL_OFFSET
          X1 = X1 - (tw / 2#)
        ElseIf (IXY And 3) = 1 Then     'y axis, left
          X1 = X1 - tw - GQ_TICL_OFFSET
          Y1 = Y1 - (th / 2#)
        ElseIf (IXY And 3) = 3 Then     'y axis, right
          X1 = X1 + GQ_TICL_OFFSET
          Y1 = Y1 - (th / 2#)
        End If
        GQoutObj.CurrentX = X1
        GQoutObj.CurrentY = Y1
        GQoutObj.Print ST
      Next
End Sub

Sub ticl2q(IXY As Integer, DMAJ As Single)
'
' Draw labels under/beside tic marks - log
'
' Argument  I/O  Purpose
' ========  ===  =======
' IXY        I   Choose x or y axis
'                 bit 0: axis                        0=x  1=y
'                 bit 1: place labels on other side  0=no 1=yes
' so, IXY:
'                   0 = x axis, bottom;  2 = x axis, top
'                   1 = y axis, left;    3 = y axis, right
'
' DMAJ       I   Width of the space between major tic marks (in WC)
'                 set to 0.0 to defeat plotting of tic marks
'
      Dim ST1 As String
      Dim ST2 As String
      Dim X1 As Single, Y1 As Single
      Dim X1D As Single, Y1D As Single
'
      If ((IXY And 1) = 0) Then
        If GQ.VXWS < GQ.VXWE Then
          TSTRT = GQ.VXWS
        Else
          TSTRT = GQ.VXWE
        End If
        If GQ.VXWS > GQ.VXWE Then
          TLIM = GQ.VXWS
        Else
          TLIM = GQ.VXWE
        End If
        If ((IXY / 2 And 1) = 0) Then
          Y1 = GQ.VYWS
        ElseIf (IXY / 2 And 1) = 1 Then
          Y1 = GQ.VYWE
        End If
      ElseIf ((IXY And 1) = 1) Then
        If GQ.VYWS < GQ.VYWE Then
          TSTRT = GQ.VYWS
        Else
          TSTRT = GQ.VYWE
        End If
        If GQ.VYWS > GQ.VYWE Then
          TLIM = GQ.VYWS
        Else
          TLIM = GQ.VYWE
        End If
        If (IXY / 2 And 1) = 0 Then
          X1 = GQ.VXWS
        ElseIf (IXY / 2 And 1) = 1 Then
          X1 = GQ.VXWE
        End If
      End If
      n = Int(log10q(TSTRT))
      If (TSTRT < 10# ^ n) Then n = n - 1
      NXMAJ = n
'
' Label the next major tic, if it will fit
'
    Do While (True)
      If (IXY And 1) = 0 Then
        X1 = 10# ^ NXMAJ
        If (X1 > TLIM) Then Exit Do
      ElseIf (IXY And 1) = 1 Then
        Y1 = 10# ^ NXMAJ
        If (Y1 > TLIM) Then Exit Do
      End If
'     convert to device coords
      Call wcdcq(X1, Y1, X1D, Y1D)
'     convert the exponent value to a string
      ST2 = Format$(NXMAJ)
'     define a string to hold the "10"
      ST1 = "10"
'     find the size of the strings
      SZ1 = GQoutObj.TextWidth(ST1)
      HT1 = GQoutObj.TextHeight(ST1)
      SZ2 = GQoutObj.TextWidth(ST2)
      HT2 = GQoutObj.TextHeight(ST2)
'
'     The label will be displayed as a "10" raised to the appropriate
'     exponent.  The 10 is plotted at the current character height.
'     the exponent is plotted at this height, and is positioned
'     at the centerline of the "10".
'
'     adjust the x,y location for the string
'     x-axis: label is centered on the tic and one character below/above it
'     y-axis: label is centered on the tic and a character to the left/right
      If (IXY And 3) = 0 Then         'x axis, bottom
        Y1D = Y1D + 0.5 * HT1 + GQ_TICL_OFFSET
        X1D = X1D - (0.5 * (SZ1 + SZ2))
      ElseIf (IXY And 3) = 2 Then     'x axis, top
        Y1D = Y1D - HT1 - GQ_TICL_OFFSET
        X1D = X1D - (0.5 * (SZ1 + SZ2))
      ElseIf (IXY And 3) = 1 Then     'y axis, left
        X1D = X1D - SZ1 - SZ2 - GQ_TICL_OFFSET
        'Y1D = Y1D - (.5 * (HT1 + HT2))
      ElseIf (IXY And 3) = 3 Then     'y axis, right
        X1D = X1D + GQ_TICL_OFFSET
        'Y1D = Y1D - (.5 * (HT1 + HT2))
      End If
'     draw the "10"
      GQoutObj.CurrentX = X1D
      GQoutObj.CurrentY = Y1D
      GQoutObj.Print ST1
'     readjust for the exponent
      X1D = X1D + SZ1
      Y1D = Y1D - 0.5 * HT1
'     draw the exponent
      GQoutObj.CurrentX = X1D
      GQoutObj.CurrentY = Y1D
      GQoutObj.Print ST2
'
' Next major tic
'
      NXMAJ = NXMAJ + Int(DMAJ)
    Loop
'
End Sub

Sub tics1q(IXY, IGRD, DMAJ, NMINOR)
'
' INTERNAL ROUTINE
'
' Draw tic marks on the axis and on the oppostie side of the window
' Linear axis
'
' Argument  I/O  Purpose
' ========  ===  =======
' IXY        I   Choose x or y axis
'                 bit 0: 0 = x axis
'                        1 = y axis
' IGRD       I   Grid flag.  If set, the major tics marks run from
'                one side of the plot window to the other, creating
'                a grid.
'                 0 = no grid, normal tic marks
'                 1 = make a grid by running the tics marks across
'                     the plot window
' DMAJ       I   TWIDTH of the space between major tic marks (in WC)
'                 set to 0.0 to defeat plotting of tic marks
' NMINOR     I   Number of minor tics between major tics
'                 set to 0 for no minor tics
'
      Dim X1 As Single
      Dim Y1 As Single
      Dim X2 As Single
      Dim Y2 As Single
'
' Set up some parameters
'  TWIDTH IS the width of the axis is WC
'  TIC1,TIC2 are the lengths of the major and minor tic marks in DC
'   if a grid is desired, the length of the tics is equal to half
'   of the width of the plot window
'  IDIR is the direction of the tic mark (see DTICQ for definition)
'  SDMAJ is the value of DMAJ with the sign of TWIDTH
'
'     width and direction and tic size (for grid option)
      If (IXY And 1) = 0 Then
        twidth = GQ.VXWL
        TIC1 = -GQ.VYDL * 0.5
        IDIR = 0
        SDMAJ = DMAJ * Sgn(twidth)
      ElseIf (IXY And 1) = 1 Then
        twidth = GQ.VYWL
        TIC1 = GQ.VXDL * 0.5
        IDIR = 1
        SDMAJ = DMAJ * Sgn(twidth)
      End If
'     if a grid is not desired, reset tic length
      If (IGRD = 0) Then
        TIC1 = GQ_MAJ_TIC_HEIGHT
      End If
'     minor tics are always this long
      TIC2 = GQ_MIN_TIC_HEIGHT
'
' Main tic drawing loop
'
'     figure out how many major tics will fit, then loop that many times
'
      NTICS = Int(twidth / DMAJ)
      For i = 0 To Abs(NTICS)
'       calc the starting pos of the tic marks
        If (IXY And 1) = 0 Then       'x axis
          X1 = GQ.VXWS + (i * SDMAJ)
          Y1 = GQ.VYWS
          X2 = GQ.VXWS + (i * SDMAJ)
          Y2 = GQ.VYWE
        ElseIf (IXY And 1) = 1 Then   'y axis
          X1 = GQ.VXWS
          Y1 = GQ.VYWS + (i * SDMAJ)
          X2 = GQ.VXWE
          Y2 = GQ.VYWS + (i * SDMAJ)
        End If
'       convert to device coords
        Call wcdcq(X1, Y1, X1, Y1)
        Call wcdcq(X2, Y2, X2, Y2)
'       draw the tic marks
        Call ticdq(X1, Y1, TIC1, IDIR)
        Call ticdq(X2, Y2, TIC1, IDIR + 2)
'       deal with minor tic marks if necessary
        If (NMINOR > 0) Then
'         calc how many minor tics will fit in space left
          limit = (Abs(twidth) - (CSng(i) * DMAJ)) / (DMAJ / CSng(NMINOR + 1))
'         if we can't fit all the minor tics, draw what we can
          If limit < NMINOR Then
            n = limit
          Else
            n = NMINOR
          End If
          For j = 1 To n
'           calc start pos of major tic
            If (IXY And 1) = 0 Then
              X1 = GQ.VXWS + (i * SDMAJ) + (j * (SDMAJ / (NMINOR + 1)))
              Y1 = GQ.VYWS
              X2 = GQ.VXWS + (i * SDMAJ) + (j * (SDMAJ / (NMINOR + 1)))
              Y2 = GQ.VYWE
            ElseIf (IXY And 1) = 1 Then
              X1 = GQ.VXWS
              Y1 = GQ.VYWS + (i * SDMAJ) + (j * (SDMAJ / (NMINOR + 1)))
              X2 = GQ.VXWE
              Y2 = GQ.VYWS + (i * SDMAJ) + (j * (SDMAJ / (NMINOR + 1)))
            End If
'           convert to device coords
            Call wcdcq(X1, Y1, X1, Y1)
            Call wcdcq(X2, Y2, X2, Y2)
'           draw the tic marks
            Call ticdq(X1, Y1, TIC2, IDIR)
            Call ticdq(X2, Y2, TIC2, IDIR + 2)
          Next
        End If
      Next
End Sub

Sub tics2q(IXY, IGRD, DMAJ, NMINOR)
'
' INTERNAL ROUTINE
'
' Draw tic marks on the axis and on the opposite side of the window
' Log axis
'
' Argument  I/O  Purpose
' ========  ===  =======
' IXY        I   Choose x or y axis
'                 bit 0: 0 = x axis
'                        1 = y axis
' IGRD       I   Grid flag.  If set, the major tics marks run from
'                one side of the plot window to the other, creating
'                a grid.
'                 0 = no grid, normal tic marks
'                 1 = make a grid by running the tics marks across
'                     the plot window
' DMAJ       I   Log of the width of the space between major tic marks (in WC)
'                 set to 0.0 to defeat plotting of tic marks
' NMINOR     I   Minor tic flag
'                 set to 0 for no minor tics
'                 set to >0 to plot minor tics
'
' Set up some parameters
'  TSTRT is the minimum value of the axis in WC
'  TLIM is the maximum value of the axis in WC
'  TIC1,TIC2 are the lengths of the major and minor tic marks in DC
'   if a grid is desired, the length of the tics is equal to half
'   of the width of the plot window
'  IDIR is the direction of the tic mark (see DTICQ for definition)
'
      Dim X1 As Single
      Dim Y1 As Single
      Dim X2 As Single
      Dim Y2 As Single
      Dim X1D As Single
      Dim Y1D As Single
      Dim X2D As Single
      Dim Y2D As Single

      If ((IXY And 1) = 0) Then
        If GQ.VXWS < GQ.VXWE Then
          TSTRT = GQ.VXWS
        Else
          TSTRT = GQ.VXWE
        End If
        If GQ.VXWS > GQ.VXWE Then
          TLIM = GQ.VXWS
        Else
          TLIM = GQ.VXWE
        End If
        TIC1 = -GQ.VYDL * 0.5
        IDIR = 0
      ElseIf ((IXY And 1) = 1) Then
        If GQ.VYWS < GQ.VYWE Then
          TSTRT = GQ.VYWS
        Else
          TSTRT = GQ.VYWE
        End If
        If GQ.VYWS > GQ.VYWE Then
          TLIM = GQ.VYWS
        Else
          TLIM = GQ.VYWE
        End If
        TIC1 = GQ.VXDL * 0.5
        IDIR = 1
      End If
'     if a grid is not desired, reset tic length
      If (IGRD = 0) Then
        TIC1 = GQ_MAJ_TIC_HEIGHT
      End If
'     minor tics are always this long
      TIC2 = GQ_MIN_TIC_HEIGHT
'
' Calc the initial exponents of the increment between minor tics,
' the next major tic, and the number of minor tics
' to plot before the next major tic
'
' some definitions:
'   NXMAJ - the exponent of the next major tic. This value
'           is incremented through the tic drawing loop.
'           Thus, if the axis goes from .1 to 1000, NXMAJ
'           will go from -1 to 3
'   NTICS - the number of minor tics to be drawn between
'           the major tics
'   INC   - the step between the exponent of the major
'           tic locations
'
      n = Int(log10q(TSTRT))
      If (TSTRT < 10# ^ n) Then n = n - 1
      NXMAJ = n + Int(DMAJ)
      INC = n - 1 + Int(DMAJ)
      If NMINOR > 0 Then
        NTICS = 8  'set the number of minor tics to draw
      Else
        NTICS = 0
      End If
'
' Main tic loop
    While (True)
'     calc the initial values of the tic starting points
      If ((IXY And 1) = 0) Then
        X1 = 10# ^ NXMAJ - CSng(NTICS) * 10# ^ INC
        Y1 = GQ.VYWS
        X2 = X1
        Y2 = GQ.VYWE
      ElseIf ((IXY And 1) = 1) Then
        X1 = GQ.VXWS
        Y1 = 10# ^ NXMAJ - CSng(NTICS) * 10# ^ INC
        X2 = GQ.VXWE
        Y2 = Y1
      End If
'
'     minor tic loop
'
      For i = 1 To NTICS
'       jump out of this routine if no more tics will fit
        If ((IXY And 1) = 0) Then
          If (X1 > TLIM) Then Exit Sub
        ElseIf ((IXY And 1) = 1) Then
          If (Y1 > TLIM) Then Exit Sub
        End If
'       convert to device coords
        Call wcdcq(X1, Y1, X1D, Y1D)
        Call wcdcq(X2, Y2, X2D, Y2D)
'       draw the tic marks
        Call ticdq(X1D, Y1D, TIC2, IDIR)
        Call ticdq(X2D, Y2D, TIC2, IDIR + 2)
'       increment the starting positions
        If ((IXY And 1) = 0) Then
          X1 = X1 + 10# ^ INC
          X2 = X1
        ElseIf ((IXY And 1) = 1) Then
          Y1 = Y1 + 10# ^ INC
          Y2 = Y1
        End If
      Next
'
' Draw the major tic, if it will fit
'
      If ((IXY And 1) = 0) Then
        X1 = 10# ^ NXMAJ
        X2 = X1
        If (X1 > TLIM) Then Exit Sub
      ElseIf ((IXY And 1) = 1) Then
        Y1 = 10# ^ NXMAJ
        Y2 = Y1
        If (Y1 > TLIM) Then Exit Sub
      End If
'     convert to device coords
      Call wcdcq(X1, Y1, X1D, Y1D)
      Call wcdcq(X2, Y2, X2D, Y2D)
'     draw the tic marks
      Call ticdq(X1D, Y1D, TIC1, IDIR)
      Call ticdq(X2D, Y2D, TIC1, IDIR + 2)
'
' Update and do it again
'
      INC = INC + Int(DMAJ)
      NXMAJ = NXMAJ + Int(DMAJ)
'
' Loop back for the next set of minor tics
'
    Wend
End Sub

Sub viewq(XDS As Single, XDE As Single, YDS As Single, YDE As Single, XWS As Single, XWE As Single, YWS As Single, YWE As Single, IBOX As Integer)
'
'Start a viewport within the current plotting area.
'This routine enables the mapping of World Cordinates (WC) to
'Device Coordinates (DC).
' Argument  I/O  Purpose
' ========  ===  =======
' XDS        I   x axis start of viewport (left edge) in device coordinates
' XDE        I   x axis end of viewport (right edge) in device coordinates
'                note: XDS < XDE
' YDS        I   y axis start of viewport (bottom edge) in device coordinates
' YDE        I   y axis end of viewport (top edge) in device coordinates
'                note: YDS < YDE
' XWS        I   x axis start of viewport (left edge) in world cordinates
' XWE        I   x axis end of viewport (right edge) in world coordinates
'                note: XWS <> XWE
' YWS        I   y axis start of viewport (bottom edge) in world coordinates
' YWE        I   y axis end of viewport (top edge) in world coordinates
'                note: YWS <> YWE
' IBOX       I   Draw a box around the viewport  0=don't draw  1=draw
'
' Transfer the command line arguments to storage
'
' Make sure that the device coord X start is less
' than the device X coord end. Also make sure that
'the reverse is true for Y (since in Device Space
'Y increases going down. Switch them if they're not.
'
      If (XDS < XDE) Then
        GQ.VXDS = XDS
        GQ.VXDE = XDE
      Else
        GQ.VXDS = XDE
        GQ.VXDE = XDS
      End If
      If (YDS > YDE) Then
        GQ.VYDS = YDS
        GQ.VYDE = YDE
      Else
        GQ.VYDS = YDE
        GQ.VYDE = YDS
      End If
      GQ.VXWS = XWS
      GQ.VXWE = XWE
      GQ.VYWS = YWS
      GQ.VYWE = YWE
'
' Calculate the lengths of the viewport edges in device and world
' coordinates
'
      GQ.VXDL = GQ.VXDE - GQ.VXDS
      GQ.VYDL = GQ.VYDE - GQ.VYDS
      GQ.VXWL = GQ.VXWE - GQ.VXWS
      GQ.VYWL = GQ.VYWE - GQ.VYWS
'
' Calculate the ratio of device coords to world coords.
' Used in transforms.
'
      GQ.DCWCX = GQ.VXDL / GQ.VXWL
      GQ.DCWCY = GQ.VYDL / GQ.VYWL
'
' Draw a box around the viewport, if requested
'
      GQoutObj.Line (GQ.VXDS, GQ.VYDS)-(GQ.VXDE, GQ.VYDE), , B
End Sub

Sub wcdcq(XWC As Single, YWC As Single, XDC As Single, YDC As Single)
'
' Transforms world coordinates into device coordinates
'
'
      If (GQ.IXLOG = GQ_LINEAR) Then
        XDC = GQ.DCWCX * (XWC - GQ.VXWS) + GQ.VXDS
      Else
        XDC = GQ.DCWCX * (log10q(XWC) - log10q(GQ.VXWS)) + GQ.VXDS
      End If
      If (GQ.IYLOG = GQ_LINEAR) Then
        YDC = GQ.DCWCY * (YWC - GQ.VYWS) + GQ.VYDS
      Else
        YDC = GQ.DCWCY * (log10q(YWC) - log10q(GQ.VYWS)) + GQ.VYDS
      End If
End Sub


Public Sub markstq(Sty As Integer)
'Set marker style for marker drawing
' 0 = Circles
'
  GQ.CurMarker = Sty
End Sub

Public Sub markerq(ICS As Integer, XI As Single, YI As Single)
'
' Draws a marker on the output device at
' the point specified in the arguments.  The point may be specified
' in WC or DC, as specified by ICS.  All clipping is done in DC.
' Points specified in DC are clipped according to the visible screen
' area.  Points specified in WC are clipped according to the current
' view window limits.
'
' Argument  I/O  Purpose
' ========  ===  =======
' ICS        I   coordinate system: 0=device coords  1=world coords
' XI         I   X coord of point at which marker should be drawn (DC or WC)
' YI         I   Y coord of point at which marker should be drawn (DC or WC)
'
      Dim X As Single
      Dim Y As Single
      Dim X2 As Single
      Dim Y2 As Single
      Dim CL As Single
      Dim CR As Single
      Dim CT As Single
      Dim CB As Single
      Dim ICLIP As Integer

      On Error Resume Next
'
' Load input values into local storage
'
      X = XI
      Y = YI
'
' guard against bad points for LOG scales
'
      If (GQ.IXLOG = GQ_LOG And X < 1E-36) Then Exit Sub
      If (GQ.IYLOG = GQ_LOG And Y < 1E-36) Then Exit Sub
'
' place the new point in working storage, translating to DC if necessary
'
      If (ICS = 0) Then
        X2 = X
        Y2 = Y
      Else
        Call wcdcq(X, Y, X2, Y2)
      End If
'
' clip the new line segment
'
'     set the appropriate clipping boundaries
      If (ICS = 0) Then
        CL = 0#
        CR = Screen.Width
        CT = Screen.Height
        CB = 0#
      Else
        CL = GQ.VXDS
        CR = GQ.VXDE
        CT = GQ.VYDS
        CB = GQ.VYDE
      End If
'
'     Clip the endpoints, return status
'     ICLIP=0  marker in view
'     ICLIP=1  marker out of view
      ICLIP = 0
      If X2 < CL Or X2 > CR Or Y2 < CB Or Y2 > CT Then ICLIP = 1
'
'     If the marker is visible, draw it
      If (ICLIP = 0) Then
        GQoutObj.Circle (X2, Y2), 50, GQ.CurColor
      End If
'
' Save the new unclipped 'current' positions
'
      If (ICS = 0) Then
        GQ.XDOLD = X
        GQ.YDOLD = Y
      Else
        GQ.XWOLD = X
        GQ.YWOLD = Y
        Call wcdcq(X, Y, X2, Y2)
        GQ.XDOLD = X2
        GQ.YDOLD = Y2
      End If
End Sub
