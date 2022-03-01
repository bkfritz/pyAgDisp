VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsError"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Description = "Contains information about one error."
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
'File: clsError
'
'----------------------------------------------------------------------
'Re:
'    Contains all the information about one error.
'    Which file did it occured in, which function it occured in
'    What is the error number, description, source, etc...
'
'----------------------------------------------------------------------
'
' Application defined Errors (vbObjectError + # )
' Number         Description
' ============   =================================================
'
'
'----------------------------------------------------------------------
' CONSTANTS:
'----------------------------------------------------------------------
'
'
'
'----------------------------------------------------------------------
' PRIVATE VARIABLES:
'----------------------------------------------------------------------
Private mstrDescription As String
Private mstrSource As String
Private mdblNumber As Double
Private mstrInFunction As String
Private mstrInFile As String
Private mstrLocation As String
Private mdicArguments As Dictionary
'---------------------------------------------------------------------------
' Arguments:
'    Command line arguments that this function was using before error occured.
'
' Arguments:
'     vData: Dictionary array of all privitive data types as values and
'         keys as argument names
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Set Arguments(ByVal vData As Dictionary)
Attribute Arguments.VB_Description = "Contains names and value of function arguments"
    Set mdicArguments = vData
End Property


'---------------------------------------------------------------------------
' Arguments:
'    Command line arguments that this function was using before error occured.
'
' Return: Dictionary array of all privitive data types as values and
'         keys as argument names
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get Arguments() As Dictionary
    Set Arguments = mdicArguments
End Property



'---------------------------------------------------------------------------
' Location:
'    Where in function did this error occur.
'    This is optional.
'    Only used when function is very long and you need to know where in function did error occur.
'
' Arguments:
'     vData: Whre in function did error occur.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Let Location(ByVal vData As String)
Attribute Location.VB_Description = "Optional, user defined message indicating section in function where error occured"
    mstrLocation = vData
End Property


'---------------------------------------------------------------------------
' Location:
'    Where in function did this error occur.
'    This is optional.
'    Only used when function is very long and you need to know where in function did error occur.
'
' Return: Where in function did error occur.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get Location() As String
    Location = mstrLocation
End Property



'---------------------------------------------------------------------------
' InFile:
'    Which module, class etc... did this error get detected in.
'
' Arguments:
'     vData: Name of file in project where error was detected in.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Let InFile(ByVal vData As String)
Attribute InFile.VB_Description = "Name of file where error was generated"
    mstrInFile = vData
End Property


'---------------------------------------------------------------------------
' InFile:
'    Which module, class etc... did this error get detected in.
'
' Return: Name of file in project where error was detected in.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get InFile() As String
    InFile = mstrInFile
End Property



'---------------------------------------------------------------------------
' InFunction:
'    Name of function, sub, property that error orginated in VB code.
'
' Arguments:
'     vData: Name of function that detected error
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Let InFunction(ByVal vData As String)
Attribute InFunction.VB_Description = "Name of function where error was generated."
    mstrInFunction = vData
End Property


'---------------------------------------------------------------------------
' InFunction:
'    Name of function, sub, property that error orginated in VB code.
'
' Return: Which function did error get detected in.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get InFunction() As String
    InFunction = mstrInFunction
End Property



'---------------------------------------------------------------------------
' Number:
'    From err.Number. The error number generated.
'
' Arguments:
'     vData: Error number that was last generated.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Let Number(ByVal vData As String)
Attribute Number.VB_Description = "Error number generated"
    mdblNumber = vData
End Property


'---------------------------------------------------------------------------
' Number:
'    From err.Number. The error number generated.
'
' Return: Error number generated.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get Number() As String
    Number = mdblNumber
End Property



'---------------------------------------------------------------------------
' Source:
'    From err.source, contains where this error orginated from.
'
' Arguments:
'     vData: Source where error orginated from
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Let Source(ByVal vData As String)
Attribute Source.VB_Description = "Where did error occur?"
    mstrSource = vData
End Property


'---------------------------------------------------------------------------
' Source:
'    From err.source, contains where this error orginated from.
'
' Return: Source where error orginated.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get Source() As String
    Source = mstrSource
End Property



'---------------------------------------------------------------------------
' Description:
'    Error description normally from err.description.
'
' Arguments:
'     vData: Error message to save
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Let Description(ByVal vData As String)
Attribute Description.VB_Description = "Error message can contain your comments."
    mstrDescription = vData
End Property


'---------------------------------------------------------------------------
' Description:
'    Error description normally from err.description.
'
' Return: Error description
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get Description() As String
    Description = mstrDescription
End Property



'---------------------------------------------------------------------------
' Class_Terminate:
'    Remove all dictionary objects data, don't want a memory leak!
'
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-27  AED  Created
'
'---------------------------------------------------------------------------
Private Sub Class_Terminate()
    If Not mdicArguments Is Nothing Then
        mdicArguments.RemoveAll
    End If
    Set mdicArguments = Nothing
End Sub



