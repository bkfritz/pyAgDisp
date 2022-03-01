VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsErrors"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"AspmsError"
Attribute VB_Ext_KEY = "Member0" ,"AspmsError"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
'File: clsErrors
'
'----------------------------------------------------------------------
'Re:
'    Contains the collection of clsError objects.
'    Allows the calling function to retreive a stack trace and
'    allow this error to be persisted in error log.
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
Private mCol As Collection
Private mstrOverrideSummaryErrMsg As String        'Use this instead of err.description

'---------------------------------------------------------------------------
' WriteToErrorLog:
'    Write error information to a log.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'
' NOTE: The layout of this data matches UserMessage fn.
'       If you change the data layout here, you should also update UserMessage code.
'---------------------------------------------------------------------------
Public Sub WriteToErrorLog()
Attribute WriteToErrorLog.VB_Description = "Take collection of AspmsError objects and write to file"
    Dim strMsg As String
    
    
    Dim intIndexCollection As Long
    Dim intIndexDictionary As Long
    Dim objError As clsError
    Dim objDictionary As Dictionary
    
    '============================
    'Now handle generated error:
    '============================
    strMsg = "Application: " & CStr(App.EXEName) & vbCrLf
    strMsg = strMsg & "Description: " & mCol.Item(1).Description & vbCrLf
    strMsg = strMsg & "Source of Error: " & mCol.Item(1).Source & vbCrLf
    strMsg = strMsg & "Error Number: " & mCol.Item(1).Number & vbCrLf & vbCrLf
    
    '===================
    'Handle stack trace
    '===================
    strMsg = strMsg & "Stack Trace" & vbCrLf

    
    For intIndexCollection = 1 To mCol.Count
        Set objError = mCol.Item(intIndexCollection)
        With objError
            '=================================
            'Show module name : function name
            '=================================
            strMsg = strMsg & Space(4) & .InFile & ":" & .InFunction & vbCrLf
            
            '===================================
            'Show location of error in function
            '===================================
            If Len(Trim(.Location)) > 0 Then
                strMsg = strMsg & Space(8) & "Location: " & .Location & vbCrLf
            End If
            
            
            '============================
            'Show command line arguments
            '============================
            If Not .Arguments Is Nothing Then
                Set objDictionary = .Arguments
                With objDictionary
                    strMsg = strMsg & Space(8) & "Arguments:" & vbCrLf
                    For intIndexDictionary = 0 To .Count - 1
                        strMsg = strMsg & Space(12) & .Keys(intIndexDictionary) & _
                            " = " & .Items(intIndexDictionary) & vbCrLf
                    Next        'intIndexDictionary = 0 To .Count
                End With        'objDictionary
            End If              'Not .Arguments Is Nothing
        
            
        End With                'objError
        Set objError = Nothing
    Next                        'intIndexCollection = 1 To mCol.Count

'    On Error Resume Next
'    Dim cnErrorTrans As ADODB.Connection
'    Set cnErrorTrans = GetDBConnection(CONNECT_AS_USER)
'    If Err.Number <> 0 Then
'        'Can't get connection so don't save msg to db
'        MsgBox "Unable to save error message to error log" & vbCrLf & _
'               "Message: " & strMsg, vbCritical, "Unable to connect to database"
'        Exit Sub
'    End If
'
'    cnErrorTrans.BeginTrans
'    Dim objError As Error
'    Set objError = New Error
'    With objError
'        .NewRecord
'        .Fields(ERROR_FIELDS.ID_FIELD) = GetNextIDForTable("ERROR", cnErrorTrans)
'        If Err.Number <> 0 Then
'            MsgBox "Unable to retrieve next ID number for error log" & vbCrLf & _
'                   "Message: " & strMsg, vbCritical, "Unable to retrieve Error Log ID"
'            cnErrorTrans.RollbackTrans
'            Exit Sub
'        End If
'        .Fields(ERROR_FIELDS.IU_INDICATOR_FIELD) = "I"
'        .Fields(ERROR_FIELDS.IU_REASON_FIELD) = "System Generated Message"
'        .Fields(ERROR_FIELDS.NAME_FIELD) = strMsg
'        If gobjLoggedInUser Is Nothing Then
'            'No user logged in yet, so save to db as admin. Need to save record!
'            .Fields(ERROR_FIELDS.USER_ID_FIELD) = 0
'        Else
'            .Fields(ERROR_FIELDS.USER_ID_FIELD) = gobjLoggedInUser.ID
'        End If
'        .DoInsert cnErrorTrans
'        If Err.Number <> 0 Then
'            MsgBox "Unable to write to Error Log" & vbCrLf & _
'                   "Message: " & strMsg, vbCritical, "Unable to write to Error Log"
'            cnErrorTrans.RollbackTrans
'            Exit Sub
'        End If
'    End With
'    If Err.Number = 0 Then
'        cnErrorTrans.CommitTrans
'    Else
'        cnErrorTrans.RollbackTrans
'    End If
'    Set objError = Nothing
    

End Sub

'---------------------------------------------------------------------------
' Clear:
'    Remove all error objects.
'    Returns this object to an initial state.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Sub Clear()
Attribute Clear.VB_Description = "Remove all AspmsError objects from collection."
    Dim intIndex As Integer
    On Error Resume Next            'Don't worry about errors just clean up the best you can.
    Do Until mCol.Count = 0
        mCol.Remove mCol.Count
    Loop
    mstrOverrideSummaryErrMsg = ""
End Sub

'---------------------------------------------------------------------------
' UserMessage:
'    Display error information.
'    Like msgbox, but with additional button for advanced error info.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'
' NOTE: The layout of data matches WriteToErrorLog() function.
'       If you change the data layout here, you should also update WriteToErrorLog code.
'---------------------------------------------------------------------------
Public Sub UserMessage()
Attribute UserMessage.VB_Description = "Display orginal error message in a message box."
    'Dim strMsg As String
    'strMsg = mCol.Item(1).Description & vbCrLf & vbCrLf & _
            "Error Number: " & CStr(mCol.Item(1).Number)
    'MsgBox strMsg, vbInformation, mCol.Item(1).Source
    Dim frmErrorMsgBoxDialog As frmErrorMsgBox
    Set frmErrorMsgBoxDialog = New frmErrorMsgBox
    With frmErrorMsgBoxDialog
        .Caption = App.EXEName & " " & App.Major & "." & App.Minor
    
        '===================================
        ' Display user summary error message
        '===================================
        With .rtbErrorSummary
            If mstrOverrideSummaryErrMsg = "" Then
                .SelColor = vbBlue
                .SelBold = True
                .SelText = "Description: "
                .SelColor = vbBlack
                .SelBold = False
                .SelHangingIndent = 300     'Indent text in twips
                .SelText = mCol.Item(1).Description & vbCrLf '& vbCrLf
    '            .SelColor = vbBlue
    '            .SelBold = True
    '            .SelText = "Error Number: "
    '            .SelColor = vbBlack
    '            .SelBold = False
    '            .SelText = mCol.Item(1).Number
            Else
                'Display over riding error message
                .SelColor = vbBlack
                .SelBold = False
                .SelHangingIndent = 0
                .SelText = mstrOverrideSummaryErrMsg
            End If
        End With
        
        
        '===============================================
        ' Display for tech support detail error message
        '===============================================
        With .rtbErrorDetails
            '===================
            'Show description
            '===================
            .SelColor = vbBlue
            .SelBold = True
            .SelText = "Description: "
            .SelColor = vbBlack
            .SelBold = False
            .SelHangingIndent = 300     'Indent text in twips
            .SelText = mCol.Item(1).Description & vbCrLf
            
            '=====================
            'Show source of error
            '=====================
            .SelColor = vbBlue
            .SelBold = True
            .SelText = "Source of Error: "
            .SelColor = vbBlack
            .SelBold = False
            .SelText = mCol.Item(1).Source & vbCrLf
            
            '====================
            'Show error number
            '====================
            .SelColor = vbBlue
            .SelBold = True
            .SelText = "Error Number: "
            .SelColor = vbBlack
            .SelBold = False
            .SelText = mCol.Item(1).Number & vbCrLf & vbCrLf
            
            
            '====================
            'Start stack trace
            '====================
            .SelColor = vbBlue
            .SelBold = True
            .SelText = "Stack Trace:" & vbCrLf
            .SelColor = vbBlack
            .SelBold = False
            
            Dim intIndexCollection As Integer           'Which clsError object
            Dim intIndexDictionary As Integer           'Which command line argument to process
            Dim objError As clsError             'One error object selected from intIndexCollection
            Dim objDictionary As Dictionary             'Contains function command line arguments
            For intIndexCollection = 1 To mCol.Count
                Set objError = mCol.Item(intIndexCollection)
                '=================================
                'Show module name : function name
                '=================================
                .SelBullet = True
                .SelText = objError.InFile & ":" & objError.InFunction & vbCrLf
                .SelBullet = False
                
                '===================================
                'Show location of error in function
                '===================================
                If Len(Trim(objError.Location)) > 0 Then
                    .SelBold = True
                    .SelText = "    Location: "
                    .SelColor = vbBlack
                    .SelBold = False
                    .SelHangingIndent = 500     'Indent text in twips for location text
                    .SelText = "    " & objError.Location & vbCrLf
                End If
                
                '============================
                'Show command line arguments
                '============================
                If Not objError.Arguments Is Nothing Then
                    Set objDictionary = objError.Arguments
                        .SelBold = True
                        .SelText = "    Arguments:" & vbCrLf
                        .SelBold = False
                        For intIndexDictionary = 0 To objDictionary.Count - 1
                            .SelText = "        "       ' 2 tabs before argument name
                            .SelBold = True
                            .SelUnderline = True
                            .SelText = objDictionary.Keys(intIndexDictionary)
                            .SelBold = False
                            .SelUnderline = False
                            .SelHangingIndent = 800     'Indent argument in twips for argument values.
                            .SelText = " = " & objDictionary.Items(intIndexDictionary) & vbCrLf
                        Next        'intIndexDictionary = 0 To .Count
                    Set objDictionary = Nothing
                End If              'Not .Arguments Is Nothing
                
                
                Set objError = Nothing
            Next                        'intIndexCollection = 1 To mCol.Count
            
            '============================
            'Move to top of rich text box
            '============================
            .SelStart = 0
            .SelLength = 0
            
        End With
        
        
        .Show vbModal
    End With
    Unload frmErrorMsgBoxDialog
    Set frmErrorMsgBoxDialog = Nothing
    
    
End Sub

'---------------------------------------------------------------------------
' RaiseError:
'    Allow error in current function to bubble up.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Sub RaiseError()
Attribute RaiseError.VB_Description = "Generate an error with last error number to bubble up error to calling fn."
    'Generate an error to cause fn to bubble up error to calling fn.
    Err.Raise mCol.Item(mCol.Count).Number, mCol.Item(mCol.Count).Source, mCol.Item(mCol.Count).Description
    
End Sub


'---------------------------------------------------------------------------
' Append:
'    Add one clsError object to collection
'
' Arguments:
'     objErr: VB Error object
'     FunctionName: Name of function in which error was detected.
'     FileName: Name of projrct file that in which error was detected.
'     ErrLocation: Optional location tag to pinpoint error location. Can be used to customize error response.
'     Arguments: A dictionary object of all primitive VB data types where keys are names of arguments
'
' Return: The clsError object just created.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Function Append(ByRef objErr As ErrObject, _
                       ByVal FunctionName As String, _
                       ByVal FileName As String, _
                       Optional ByVal ErrLocation As String = "", _
                       Optional ByRef Arguments As Dictionary) As clsError
    'create a new object
    Dim objNewMember As clsError
    Set objNewMember = New clsError


    'set the properties passed into the method
    objNewMember.Description = Err.Description
    objNewMember.Source = Err.Source
    objNewMember.Number = Err.Number
    objNewMember.InFunction = FunctionName
    objNewMember.InFile = FileName
    objNewMember.Location = ErrLocation
    If Not Arguments Is Nothing Then
        Set objNewMember.Arguments = Arguments
    End If
    
    mCol.Add objNewMember


    'return the object created
    Set Append = objNewMember
    Set objNewMember = Nothing


End Function

'---------------------------------------------------------------------------
' Item:
'    Retur one clsError from collection
'
' Arguments:
'     vntIndexKey: Which clsError object to return
'
' Return: clsError object requested.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Property Get Item(vntIndexKey As Variant) As clsError
Attribute Item.VB_UserMemId = 0
  Set Item = mCol(vntIndexKey)
End Property



'---------------------------------------------------------------------------
' Count:
'    Return number of clsError objects in collection.
'
' Return: Number of clsError objects currently stored in collection.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Property Get Count() As Long
    Count = mCol.Count
End Property


'---------------------------------------------------------------------------
' Remove:
'    Remove one clsError object from collection.
'
' Arguments:
'     vntIndexKey: Which clsError object to remove.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Sub Remove(vntIndexKey As Variant)
    mCol.Remove vntIndexKey
End Sub


'---------------------------------------------------------------------------
' NewEnum:
'    VB Internal Use only.
'
' Return: N/A
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
' 2003-08-21  AED  Created
'
'---------------------------------------------------------------------------
Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    Set NewEnum = mCol.[_NewEnum]
End Property


'---------------------------------------------------------------------------
' Class_Initialize:
'    Create the collection to be used.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Private Sub Class_Initialize()
    On Error GoTo Error_Handler
    Set mCol = New Collection


'====================================================
'Exit Point for Class_Initialize
'====================================================
Exit_Class_Initialize:
    Exit Sub


'====================================================
'            ERROR HANDLER ROUTINE(S)
'====================================================
Error_Handler:
    'Err.Raise Err.Number, Err.Source, Err.Description
    MsgBox Err.Description, vbInformation, "Class_Initialize"
    Resume Exit_Class_Initialize

End Sub


'---------------------------------------------------------------------------
' Class_Terminate:
'    Remove all clsError objects from memory before closing class.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Private Sub Class_Terminate()
    On Error GoTo Error_Handler
    Clear
    Set mCol = Nothing


'====================================================
'Exit Point for Class_Terminate
'====================================================
Exit_Class_Terminate:
    Exit Sub


'====================================================
'            ERROR HANDLER ROUTINE(S)
'====================================================
Error_Handler:
    'Err.Raise Err.Number, Err.Source, Err.Description
    MsgBox Err.Description, vbInformation, "Class_Terminate"
    Resume Exit_Class_Terminate

End Sub


'---------------------------------------------------------------------------
' AppendToSummaryErrorMsg:
'    Don't display err.description to user, instead display this message.
'
' Arguments:
'     strMsg: Message to replace err.description with.
'
' Modified:
' Date        Ini  Description
'===========  ===  =========================================================
'
'---------------------------------------------------------------------------
Public Sub AppendToSummaryErrorMsg(ByVal strMsg As String)
    If mstrOverrideSummaryErrMsg = "" Then
        mstrOverrideSummaryErrMsg = strMsg
    Else
        mstrOverrideSummaryErrMsg = mstrOverrideSummaryErrMsg & vbCrLf & _
            strMsg
    End If
End Sub
