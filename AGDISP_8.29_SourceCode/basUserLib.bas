Attribute VB_Name = "basUserLib"
'$Id: basUserLib.bas,v 1.2 2001/12/26 16:12:22 tom Exp $
'basUserLib.bas - User Library code
Option Explicit

'User Library Database name
'This string occupies the DBName field in the info table
'and identifies the database as an AGDISP User Library.
'This helps provent users from mixing up installed libraries
'and user libraries
Public Const USERLIBRARYDBNAME = "AGDISP User Library"

'User Library database format version
'Lib vers 1, AGDISP 8.00: Baseline user library format
Public Const USERLIBRARYVERSION = 1

'=====================================================================
'General Library Functions
'=====================================================================

Public Function UserLibCreate() As Boolean
'Create a new AGDISP user library from scratch
'The new database will contain the Info table with the appropriately
'formatted Info record, but no other tables.
'
'Returns: True if the database was successfully created
'         False othewise
'
  Dim DB As Database

  UserLibCreate = False
  
  On Error GoTo ErrHandUserLibCreate
  
  Set DB = Workspaces(0).CreateDatabase(UP.UserLib, dbLangGeneral)
  
  'Create the obligatory Information table
  If Not UserLibAddInfoTable(DB) Then Exit Function
  If Not UserLibAddInfoRecord(DB) Then Exit Function
  
  UserLibCreate = True

ExitUserLibCreate:
  Exit Function

ErrHandUserLibCreate:
  MsgBox "Error " & CStr(Err.Number) & ", " & Err.Description
  Resume ExitUserLibCreate
  
End Function

Public Function UserLibOpen(DB As Database, Optional EnableWriting) As Boolean
'Open AGDISP User Library Database
'If the database does not exist, the user is given the opertunity
'to create it.
'
'Returns: True if the database was successfully opened
'         False if not
'
  Dim mbtitle As String
  Dim mbflags As Integer
  Dim mbmsg As String
  Dim RS As Recordset
  Dim ReadOnly As Boolean
  
  'Deal with optional arguments
  ReadOnly = True
  If Not IsMissing(EnableWriting) Then
    If EnableWriting Then
      ReadOnly = False
    End If
  End If
  
  UserLibOpen = False 'default return value
  
  On Error GoTo ErrHandUserLibOpen

TopUserLibOpen:
  'Try to open the library read-only. That way if it's the wrong library,
  'we won't risk modifying it.
  Set DB = Workspaces(0).OpenDatabase(UP.UserLib, , True)

  'Database is open. Check version.
  'First look for the Info table. If it is not there, the library
  'is wrong. Then match the DBName field in the table to that which
  'wired in this program. A mismatch here probably indicates that
  'we are looking at a built-in AGDISP library, not a User Library.
  'Finally, match the Version field in the database to the version
  'wired in this program.
  If LibOpenRS(DB, "Info", RS) Then
    If RS.Fields("DBName") = USERLIBRARYDBNAME Then
      If RS.Fields("Version") = USERLIBRARYVERSION Then
        RS.Close
        'Okay, the library is correct. Now reopen for writing if necessary
        If Not ReadOnly Then
          DB.Close 'close this read-only connection
          Set DB = Workspaces(0).OpenDatabase(UP.UserLib, , False)
        End If
        UserLibOpen = True
        Exit Function  'Success!
      Else
        'Version mismatch
        RS.Close
        Error 10001 'Raise a fake error for Version Mismatch
      End If
    Else
      'DBName mismatch - wrong database
      RS.Close
      Error 10000 'Raise a fake error
    End If
  Else
    'No Info table, wrong database
    Error 10000 'Raise a fake error
  End If
  
ExitUserLibOpen:
  Exit Function

CreateDBUserLibOpen:
  'Create a library database when none exists
  Select Case MsgBox("User Library does not exist. Create?", _
                     vbQuestion + vbOKCancel)
  Case vbOK
    'try to create a new library
    If UserLibCreate() Then
      GoTo TopUserLibOpen 'Loop around and try to open the new library
    End If
  End Select
  Exit Function
  
ErrHandUserLibOpen:
  'Error Numbers:
  '3024  Database file not found
  '3265  Item not found in this collection (DBName or Version field not present)
  '3343  Unrecognized database format
  '10000 (user-defined) No Info table, DBName mismatch
  '10001 (user-defined) Library Version mismatch
  '
  Select Case Err.Number
  Case 3024  'Database file not found
    'Give the user an oppertunity to create the library
    Resume CreateDBUserLibOpen
  Case 3265, 3343, 10000
    MsgBox "Selected file is not an AGDISP User Library. Check Preferences", vbExclamation
    Resume ExitUserLibOpen
  Case 10001  '(user-defined) Library Version mismatch
    MsgBox "The AGDISP User Library file is the wrong version."
    Resume ExitUserLibOpen
  End Select
  MsgBox "Error " & Err.Number & ", " & Err.Description
  Resume ExitUserLibOpen
End Function

Public Function UserLibOpenRS(DB As Database, TableName As String, RS As Recordset) As Boolean
'Open a Recordset within an open Database
  
  On Error GoTo ErrHandUserLibOpenRS
  
  UserLibOpenRS = False 'default return value
  
  Set RS = DB.OpenRecordset(TableName, dbOpenDynaset)
  UserLibOpenRS = True
  
ExitUserLibOpenRS:
  Exit Function
  
ErrHandUserLibOpenRS:
  Select Case Err.Number
  Case 3078  'Table not found
    Resume ExitUserLibOpenRS
  End Select
  MsgBox "Error " & Err.Number & ", " & Err.Description
  Resume ExitUserLibOpenRS
End Function

'=====================================================================
'Info
'=====================================================================

Private Function UserLibAddInfoTable(DB As Database) As Boolean
'Add tables for Info to database
'Unlike the Public functions, this routine requires a database argument
'
'*********************************************************
' Database:
'   Table: Info
'     Field: DBName   a name for the database
'     Field: Version  the current database format version

  Dim TD As TableDef
  Dim FD As Field

  UserLibAddInfoTable = False
  
  'create table ****************************************
  Set TD = New TableDef      'create a new table
  TD.Name = "Info"           'name the new table
  
  Set FD = New Field
  FD.Name = "DBName"
  FD.Type = dbText
  TD.Fields.Append FD

  Set FD = New Field
  FD.Name = "Version"
  FD.Type = dbInteger
  TD.Fields.Append FD

  DB.TableDefs.Append TD     'append the table to the database

  UserLibAddInfoTable = True
End Function

Public Function UserLibAddInfoRecord(DB As Database) As Boolean
  Dim fn As String

  Dim DS As Dynaset
  
  UserLibAddInfoRecord = False
  
  'create dynaset of records to work on
  Set DS = DB.CreateDynaset("Info")

  'Add the new record to the dropsize table
  DS.AddNew
  DS.Fields("DBName") = USERLIBRARYDBNAME
  DS.Fields("Version") = USERLIBRARYVERSION
  DS.Update

  DS.Close

  UserLibAddInfoRecord = True
End Function

'=====================================================================
'Aircraft
'=====================================================================

Public Function UserLibAddAircraftTable() As Boolean
'Add an empty Aircraft Table to the database
'Add tables for Evaporation info to database
'
'*********************************************************
' Database:
'   Table: Aircraft
'     Field: Name
'     Field: Type
'     Field: SemiSpan
'     Field: TypSpeed
'     Field: BiplSep
'     Field: Weight
'     Field: PlanArea
'     Field: PropRPM
'     Field: PropRad
'     Field: EngVert
'     Field: EngFwd
'     Field: NumEng
'     Field: EngHoriz
'     Field: WingVert
'     Field: BoomVert
'     Field: BoomFwd
'
  Dim DB As Database
  Dim TD As TableDef
  Dim FD As Field

  'open the database for writing
  If Not UserLibOpen(DB, True) Then Exit Function
  
  'create Dropsize table ****************************************
  Set TD = New TableDef
  TD.Name = "Aircraft"    'name the new table
  
  Set FD = New Field
  FD.Name = "Name"        'name the field
  FD.Type = dbText        'type the field
  FD.Size = 40            'size the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "Type"        'name the field
  FD.Type = dbInteger    'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "SemiSpan"    'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "TypSpeed"    'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "BiplSep"     'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "Weight"      'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "PlanArea"    'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "PropRPM"     'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "PropRad"     'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "EngVert"     'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "EngFwd"      'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "NumEng"      'name the field
  FD.Type = dbInteger    'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "EngHoriz"    'name the field
  FD.Type = dbLongBinary 'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "WingVert"    'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "BoomVert"    'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "BoomFwd"     'name the field
  FD.Type = dbSingle     'type the field
  TD.Fields.Append FD     'append the field to the table
  
  DB.TableDefs.Append TD  'append the table to the database

  'close the database
  DB.Close
  
  UserLibAddAircraftTable = True
End Function

Public Function UserLibAddAircraftRecord(AC As AircraftData) As Boolean
'Retrieve an Aircraft library entry
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  Dim AddMe As Boolean
  
  UserLibAddAircraftRecord = False 'default return value
  
  If UserLibOpen(DB, True) Then
    'Open the aircraft table. If it doesn't exist, add it.
    If Not UserLibOpenRS(DB, "Aircraft", RS) Then
      If Not UserLibAddAircraftTable Then Exit Function
      If Not UserLibOpenRS(DB, "Aircraft", RS) Then Exit Function
    End If
    'Look for an existing record
    RS.FindFirst "Name='" & AC.Name & "'"
    AddMe = True 'Flag
    If RS.NoMatch Then 'new entry
      RS.AddNew 'new, blank entry
    Else 'entry exists!
      If MsgBox("An Aircraft with the same name already exists. Replace?", _
                vbQuestion + vbOKCancel) <> vbOK Then
        AddMe = False 'do not add the record
      Else
        RS.Edit 'edit existing record
      End If
    End If
    'Possibly add/replace the record
    If AddMe Then
      'transfer local data to database
      'AC.Type
      'AC.BasicType
      RS.Fields("Name") = AC.Name
      'AC.LName
      RS.Fields("Type") = AC.WingType
      RS.Fields("SemiSpan") = AC.SemiSpan
      RS.Fields("TypSpeed") = AC.TypSpeed
      RS.Fields("BiplSep") = AC.BiplSep
      RS.Fields("Weight") = AC.Weight
      RS.Fields("PlanArea") = AC.PlanArea
      RS.Fields("PropRPM") = AC.PropRPM
      RS.Fields("PropRad") = AC.PropRad
      'AC.PropEff
      RS.Fields("EngVert") = AC.EngVert
      RS.Fields("EngFwd") = AC.EngFwd
      RS.Fields("NumEng") = AC.NumEng
      ArrayToField RS.Fields("EngHoriz"), AC.EngHoriz(), 2
      RS.Fields("WingVert") = AC.WingVert
      RS.Fields("BoomVert") = AC.BoomVert
      RS.Fields("BoomFwd") = AC.BoomFwd
      'AC.DragCoeff
      RS.Update
      
      'success!
      UserLibAddAircraftRecord = True
    End If
    RS.Close
    DB.Close
  End If
End Function

Public Function UserLibDeleteAircraftRecord(ACName As String) As Boolean
'Delete an Aircraft library entry
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  
  UserLibDeleteAircraftRecord = False 'default return value
  
  If UserLibOpen(DB, True) Then 'open the database
    If UserLibOpenRS(DB, "Aircraft", RS) Then 'open the table
      If Not (RS.BOF And RS.EOF) Then 'see if the table is empty
        'Look for an existing record
        RS.FindFirst "Name='" & ACName & "'" 'find a matching entry
        If Not RS.NoMatch Then
          RS.Delete 'delete the record
          'success!
          UserLibDeleteAircraftRecord = True
        Else
          MsgBox "Specified Aircraft not found."
        End If
        RS.Close
      Else
        MsgBox "There are no Aircraft in the User Library to delete."
      End If
    Else
      MsgBox "There are no Aircraft in the User Library to delete."
    End If
    DB.Close
  End If
End Function

Public Function UserLibGetAircraftRecord(ACName As String, AC As AircraftData) As Boolean
'Retrieve an Aircraft library entry
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  
  UserLibGetAircraftRecord = False 'default return value
  
  If UserLibOpen(DB, False) Then
    If UserLibOpenRS(DB, "Aircraft", RS) Then
      RS.FindFirst "Name='" & ACName & "'"
      If Not RS.NoMatch Then
        'transfer record to local data
        AC.Type = 1 'user-def
        'AC.BasicType
        AC.Name = RS.Fields("Name")
        AC.LName = Len(Trim$(AC.Name))
        AC.WingType = RS.Fields("Type")
        AC.SemiSpan = RS.Fields("SemiSpan")
        AC.TypSpeed = RS.Fields("TypSpeed")
        AC.BiplSep = RS.Fields("BiplSep")
        AC.Weight = RS.Fields("Weight")
        AC.PlanArea = RS.Fields("PlanArea")
        AC.PropRPM = RS.Fields("PropRPM")
        AC.PropRad = RS.Fields("PropRad")
        'AC.PropEff
        AC.EngVert = RS.Fields("EngVert")
        AC.EngFwd = RS.Fields("EngFwd")
        AC.NumEng = RS.Fields("NumEng")
        FieldToArray RS.Fields("EngHoriz"), AC.EngHoriz()
        AC.WingVert = RS.Fields("WingVert")
        AC.BoomVert = RS.Fields("BoomVert")
        AC.BoomFwd = RS.Fields("BoomFwd")
        'AC.DragCoeff
        UserLibGetAircraftRecord = True 'Success!
      End If
      RS.Close
    End If
    DB.Close
  End If
End Function

Public Function UserLibSelectAircraft(AC As AircraftData) As Boolean
'Show the Aircraft User Library selection form
  UserLibSelectAircraft = False 'default return value
  Load frmAircraftLibUser
  frmAircraftLibUser.SelectEntry AC.Name
  frmAircraftLibUser.Show vbModal
  If frmAircraftLibUser.OK Then
    If UserLibGetAircraftRecord(frmAircraftLibUser.ACName, AC) Then
      UserLibSelectAircraft = True 'success!
    End If
  End If
  Unload frmAircraftLibUser
End Function

'=====================================================================
'Drop Size Distribution
'=====================================================================

Public Function UserLibAddDropsizeTable() As Boolean
'Add an empty Dropsize Table to the database
'Add tables for Evaporation info to database
'
'*********************************************************
' Database:
'   Table: Dropsize
'     Field: Name
'     Field: NumDrop
'     Field: Diam()
'     Field: MassFrac()
'
  Dim DB As Database
  Dim TD As TableDef
  Dim FD As Field

  'open the database for writing
  If Not UserLibOpen(DB, True) Then Exit Function
  
  'create Dropsize table ****************************************
  Set TD = New TableDef
  TD.Name = "Dropsize"    'name the new table
  
  Set FD = New Field
  FD.Name = "Name"        'name the field
  FD.Type = dbText        'type the field
  FD.Size = 40            'size the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "NumDrop"    'name the field
  FD.Type = dbInteger    'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "Diam"        'name the field
  FD.Type = dbLongBinary  'type the field
  TD.Fields.Append FD     'append the field to the table
  
  Set FD = New Field
  FD.Name = "MassFrac"    'name the field
  FD.Type = dbLongBinary  'type the field
  TD.Fields.Append FD     'append the field to the table
  
  DB.TableDefs.Append TD  'append the table to the database

  'close the database
  DB.Close
  
  UserLibAddDropsizeTable = True
End Function

Public Function UserLibAddDropsizeRecord(DSDName As String, NumDrop As Integer, _
                                         Diam() As Single, MassFrac() As Single) _
                                         As Boolean
'Retrieve an Dropsize library entry
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  Dim AddMe As Boolean
  
  UserLibAddDropsizeRecord = False 'default return value
  
  If UserLibOpen(DB, True) Then
    'Open the Dropsize table. If it doesn't exist, add it.
    If Not UserLibOpenRS(DB, "Dropsize", RS) Then
      If Not UserLibAddDropsizeTable Then Exit Function
      If Not UserLibOpenRS(DB, "Dropsize", RS) Then Exit Function
    End If
    'Look for an existing record
    RS.FindFirst "Name='" & DSDName & "'"
    AddMe = True 'Flag
    If RS.NoMatch Then 'new entry
      RS.AddNew 'new, blank entry
    Else 'entry exists!
      If MsgBox("A Drop Size Distribution with the same name already exists. Replace?", _
                vbQuestion + vbOKCancel) <> vbOK Then
        AddMe = False 'do not add the record
      Else
        RS.Edit 'edit existing record
      End If
    End If
    'Possibly add/replace the record
    If AddMe Then
      'transfer local data to database
      RS.Fields("Name") = DSDName
      RS.Fields("NumDrop") = NumDrop
      ArrayToField RS.Fields("Diam"), Diam(), NumDrop
      ArrayToField RS.Fields("MassFrac"), MassFrac(), NumDrop
      RS.Update
      
      'success!
      UserLibAddDropsizeRecord = True
    End If
    RS.Close
    DB.Close
  End If
End Function

Public Function UserLibDeleteDropsizeRecord(DSDName As String) As Boolean
'Delete an Dropsize library entry
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  
  UserLibDeleteDropsizeRecord = False 'default return value
  
  If UserLibOpen(DB, True) Then 'open the database
    If UserLibOpenRS(DB, "Dropsize", RS) Then 'open the table
      If Not (RS.BOF And RS.EOF) Then 'see if the table is empty
        'Look for an existing record
        RS.FindFirst "Name='" & DSDName & "'" 'find a matching entry
        If Not RS.NoMatch Then
          RS.Delete 'delete the record
          'success!
          UserLibDeleteDropsizeRecord = True
        Else
          MsgBox "Specified Drop Size Distribution not found."
        End If
        RS.Close
      Else
        MsgBox "There are no Drop Size Distribution in the User Library to delete."
      End If
    Else
      MsgBox "There are no Drop Size Distribution in the User Library to delete."
    End If
    DB.Close
  End If
End Function

Public Function UserLibGetDropsizeRecord(DSDName As String, NumDrop As Integer, _
                                         Diam() As Single, MassFrac() As Single) _
                                         As Boolean
'Retrieve an Dropsize library entry
  Dim DB As Database
  Dim RS As Recordset
  Dim s As String
  
  UserLibGetDropsizeRecord = False 'default return value
  
  If UserLibOpen(DB, False) Then
    If UserLibOpenRS(DB, "Dropsize", RS) Then
      RS.FindFirst "Name='" & DSDName & "'"
      If Not RS.NoMatch Then
        'transfer record to local data
        DSDName = RS.Fields("Name")
        NumDrop = RS.Fields("NumDrop")
        FieldToArray RS.Fields("Diam"), Diam()
        FieldToArray RS.Fields("MassFrac"), MassFrac()
        
        UserLibGetDropsizeRecord = True 'Success!
      End If
      RS.Close
    End If
    DB.Close
  End If
End Function

Public Function UserLibSelectDropsize(DSDName As String, NumDrop As Integer, _
                                      Diam() As Single, MassFrac() As Single) _
                                      As Boolean
'Show the Dropsize User Library selection form
  UserLibSelectDropsize = False 'default return value
  Load frmDropLibUser
  frmDropLibUser.SelectEntry DSDName
  frmDropLibUser.Show vbModal
  If frmDropLibUser.OK Then
    If UserLibGetDropsizeRecord(frmDropLibUser.DSDName, NumDrop, Diam(), MassFrac()) Then
      DSDName = frmDropLibUser.DSDName
      UserLibSelectDropsize = True 'success!
    End If
  End If
  Unload frmDropLibUser
End Function

