'    GCBASIC - A BASIC Compiler for PIC microcontrollers
'    Compiler settings editor
'    Copyright (C) 2006-2007 Hugh Considine
'
'    This program is free software; you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation; either version 2 of the License, or
'    (at your option) any later version.
'
'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this program; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'
'If you have any questions about the source code, please email me: hconsidine@bigpond.com
'Any other questions, please email me or see the GCBASIC forums.

Declare Sub EditSettings
Declare Sub LoadFromCmd
Declare Sub LoadSettings
DECLARE SUB Replace (DataVar As String, Find As String, Rep As String)
Declare Sub SaveSettings
Declare Function GetParamValue(WholeString As String, ParamName As String) As String
Declare Sub ReadCompile
Declare Sub ReadMakeasm
Declare Sub ReadDownload
Declare Function ComplexEdit(InValue As String) As String
Declare Function AddQuotes (InValue As String) As String
Declare Function TrimQuotes (InValue As String) As String

Dim Shared As String InstDir, Assembler, Programmer
Dim Shared As Integer CmdLine, UseIntAsm, ReuseOutput, NoPause, Verbose, KeepLevel
Dim As String TempStr

CmdLine = 0
If Command <> "" Then
    'If command line parameters specified, read and save
    LoadFromCmd
    SaveSettings
Else
    'If no command line, read existing settings and show editor
    LoadSettings
    EditSettings
End If
End

Sub LoadFromCmd
    
    Dim As String ID
    Dim As Integer PrgIn, AsmIn
    ID = Command
    PrgIn = 0
    AsmIn = 0
    ReuseOutput = -1
    NoPause = 0
    Verbose = 0
    KeepLevel = 0
    
    ReadCommand:
    IF LEFT(ID, 2) = "IC" THEN
        PrgIn = 1
        ID = MID(ID, 3)
        GOTO ReadCommand
    ElseIF LEFT(ID, 2) = "WP" THEN
        PrgIn = 2
        ID = MID(ID, 3)
        GOTO ReadCommand
    ElseIF LEFT(ID, 2) = "MP" THEN
        AsmIn = 1
        ID = MID(ID, 3)
        GOTO ReadCommand
    ElseIF LEFT(ID, 2) = "GP" THEN
        AsmIn = 2
        ID = MID(ID, 3)
        GOTO ReadCommand
    End If
    
    InstDir = ID
    
    If PrgIn = 1 Then
        Programmer = "icprog -lcompiled.hex"
    ElseIf PrgIn = 2 Then
        Programmer = Chr(34) + "C:\Program Files\WinPic\Winpic.exe" + Chr(34) + " compiled.hex"
    Else
        Programmer = ""
    End If
    
    If AsmIn = 1 Then
        Assembler = "C:\progra~1\microc~1\mpasms~1\MPASMWIN /c- /o- /q+ /l- /x- /w1 compiled.asm"
    ElseIf AsmIn = 2 Then
        Assembler = "C:\progra~1\gputils\bin\gpasm -i -w2 compiled.asm >compiled.err"
    Else
        Assembler = "GCASM"
    End If
    
End Sub

Sub LoadSettings
    
    Dim As Integer T
    
    'Get install dir
    InstDir = Command(0)
    For T = Len(InstDir) To 1 Step -1
        If Mid(InstDir, T, 1) = "\" Or Mid(InstDir, T, 1) = "/" Then
            InstDir = Left(InstDir, T)
            Exit For
        End If
    Next
    
    'Try reading settings, load default if unsuccessful
    If Dir(InstDir + "compile.bat") <> "" Then
        If Dir(InstDir + "makeasm.bat") <> "" Then
            If Dir(InstDir + "download.bat") <> "" Then
                'Found compile, makeasm, download
                ReadCompile
                ReadMakeasm
                ReadDownload
            Else
                'Found compile, makeasm
                ReadCompile
                ReadMakeasm
            End If
        Else
            'Found compile
            ReadCompile
        End If
    Else
        'Found nothing
        ReuseOutput = -1
        NoPause = 0
        Verbose = 0
        KeepLevel = 0
        Assembler = "GCASM"
    End If
    
End Sub

Sub EditSettings
    
    Dim As String VerbMode, PauseMode, ReuseMode, AsmMode, PrgMode, Temp
    Dim As String PreserveMode
    Dim As Integer MD, PMD
    
    StartEditor:
    VerbMode = "no": If Verbose Then VerbMode = "yes"
    PauseMode = "yes": If NoPause Then PauseMode = "no"
    ReuseMode = "no": If ReuseOutput Then ReuseMode = "yes"
    
    If KeepLevel = 0 Then
        PreserveMode = "none"
    ElseIf KeepLevel = 1 Then
        PreserveMode = "comments"
    ElseIf KeepLevel = 2 Then
        PreserveMode = "full source"
    End If
    
    AsmMode = TrimQuotes(Assembler)
    If Len(AsmMode) >= 54 Then AsmMode = Left(AsmMode, 49) + " ..."
    PrgMode = TrimQuotes(Programmer)
    If Len(PrgMode) >= 54 Then PrgMode = Left(PrgMode, 49) + " ..."
    
    CLS
    Print "                       GCBASIC compile script editor                       "
    Print 
    Print "Please select an item to change:"
    Print
    Print "(1)  Verbose compiler messages  [" + VerbMode + "]"
    Print "(2)  Pause on errors  [" + PauseMode + "]"
    Print "(3)  Use compiled.* for output  [" + ReuseMode + "]"
    Print "(4)  Preserve in output  [" + PreserveMode + "]"
    Print "(5)  Select assembler  [" + AsmMode + "]"
    Print "(6)  Select programmer  [" + PrgMode + "]"
    Print
    Print "(7)  Save and exit"
    Print "(8)  Exit without saving"
    Print
    Input "Selection: ", MD
    Print
    
    Select Case MD
    Case 1:
        Do
            Input "Verbose compiler messages (y/n)?", Temp
            Temp = UCase(Left(Temp, 1))
            If Temp = "Y" Then Verbose = -1: Exit Do
            If Temp = "N" Then Verbose = 0: Exit Do
        Loop
        
    Case 2:
        Do
            Input "Pause and wait for key press when an error occurs (y/n)?", Temp
            Temp = UCase(Left(Temp, 1))
            If Temp = "Y" Then NoPause = 0: Exit Do
            If Temp = "N" Then NoPause = -1: Exit Do
        Loop
        
    Case 3:
        Do
            Input "Use compiled.* for compiler output (y/n)?", Temp
            Temp = UCase(Left(Temp, 1))
            If Temp = "Y" Then ReuseOutput = -1: Exit Do
            If Temp = "N" Then ReuseOutput = 0: Exit Do
        Loop
        
    Case 4:
        Do
            Print "Parts of original source to include in assembly file:"
            Print "(1)  None"
            Print "(2)  Comments only"
            Print "(3)  All source"
            Print "(4)  Return to previous menu"
            Print 
            Input "Selection: ", PMD
            Select Case PMD
            Case 1:
                KeepLevel = 0
                Exit Do
            Case 2:
                KeepLevel = 1
                Exit Do
            Case 3:
                KeepLevel = 2
                Exit Do
            Case 4:
                Exit Do
            End Select
            
        Loop
        
    Case 5:
        Print "Assembler:";
        Assembler = AddQuotes(ComplexEdit(TrimQuotes(Assembler)))
        
    Case 6:
        Print "Programmer:";
        Programmer = AddQuotes(ComplexEdit(TrimQuotes(Programmer)))
        
    Case 7:
        SaveSettings
        Exit Sub
    
    Case 8:
        Exit Sub
    
    End Select
    
    Goto StartEditor
    
End Sub

Sub ReadCompile
    Dim FileNo As Integer
    Dim As String DataSource, DSUpper, Temp
    FileNo = FreeFile
    
    Verbose = 0
    NoPause = 0
    ReuseOutput = 0
    KeepLevel = 0
    
    Open InstDir + "compile.bat" For input As #FileNo
    Do While Not Eof(FileNo)
        Line Input #FileNo, DataSource
        DSUpper = UCase(DataSource)
        
        If Left(DSUpper, 8) = "GCBASIC " Then
            
            If Instr(DSUpper, "COMPILED.ASM") <> 0 Then ReuseOutput = -1
            If Instr(DSUpper, "/NP") <> 0 Then NoPause = -1
            If Instr(DSUpper, "/V") <> 0 Then Verbose = -1
            
            Assembler = GetParamValue(DataSource, "/A:")
            Programmer = GetParamValue(DataSource, "/P:")
            Temp = GetParamValue(DataSource, "/K:")
            If Temp <> "" Then
                If Temp = "C" Then
                    KeepLevel = 1
                ElseIf Temp = "A" Then
                    KeepLevel = 2
                End If
            End If
        End If
        
    Loop
    Close #FileNo
End Sub

Sub ReadMakeasm
    Dim FileNo As Integer
    Dim As String DataSource, DSUpper
    FileNo = FreeFile
    
    Open InstDir + "makeasm.bat" For input As #FileNo
    Do While Not Eof(FileNo)
        Line Input #FileNo, DataSource
        DSUpper = UCase(DataSource)
        
        If Left(DSUpper, 1) <> "@" And DSUpper <> "" And Left(DSUpper, 4) <> "DEL " Then
            Assembler = DataSource
        End If
        
    Loop
    Close #FileNo
End Sub

Sub ReadDownload
    Dim FileNo As Integer
    Dim As String DataSource, DSUpper
    FileNo = FreeFile
    
    Open InstDir + "download.bat" For input As #FileNo
    Do While Not Eof(FileNo)
        Line Input #FileNo, DataSource
        DSUpper = UCase(DataSource)
        
        If Left(DSUpper, 1) <> "@" And DSUpper <> "" And Left(DSUpper, 4) <> "DEL " Then
            Programmer = DataSource
        End If
        
    Loop
    Close #FileNo
End Sub

Sub SaveSettings
    
    Dim As String AsmParam, PrgParam, PauseParam, VerboseParam, KeepParam
    
    AsmParam = ""
    If Assembler <> "" Then
        AsmParam = TrimQuotes(Assembler)
        Do While Instr(AsmParam, Chr(34)): Replace AsmParam, Chr(34), Chr(200): Loop
        Do While Instr(AsmParam, Chr(200)): Replace AsmParam, Chr(200), Chr(34) + Chr(34): Loop
        AsmParam = " /A:" + AddQuotes(AsmParam)
    End If
    PrgParam = ""
    If Programmer <> "" Then
        PrgParam = TrimQuotes(Programmer)
        Do While Instr(PrgParam, Chr(34)): Replace PrgParam, Chr(34), Chr(200): Loop
        Do While Instr(PrgParam, Chr(200)): Replace PrgParam, Chr(200), Chr(34) + Chr(34): Loop
        PrgParam = " /P:" + AddQuotes(PrgParam)
    End If
    KeepParam = ""
    If KeepLevel = 1 Then KeepParam = " /K:C"
    If KeepLevel = 2 Then KeepParam = " /K:A"
        
    PauseParam = "": If NoPause Then PauseParam = " /NP"
    VerboseParam = "": If Verbose Then VerboseParam = " /V"
    
    OPEN InstDir + "\compile.bat" FOR OUTPUT AS #1
    PRINT #1, "@ECHO OFF"
    If ReuseOutput Then
        PRINT #1, LEFT(InstDir, 2)
        PRINT #1, "cd " + MID(InstDir, 3)
        PRINT #1, "GCBASIC %1 /O:compiled.asm" + AsmParam + PrgParam + PauseParam + VerboseParam + KeepParam
    Else
        PRINT #1, "GCBASIC %1" + AsmParam + PrgParam + PauseParam + VerboseParam + KeepParam
    End If
    PRINT #1, ""
    CLOSE #1
    
    'IF Assembler = 0 THEN PRINT #1, "C:\progra~1\gputils\bin\gpasm -i -w2 compiled.asm >compiled.err"
    'IF Assembler = 1 THEN PRINT #1, "C:\progra~1\microc~1\mpasms~1\MPASMWIN /c- /o- /q+ /l- /x- /w1 compiled.asm"
    
End Sub

Function ComplexEdit(InValue As String) As String
    
    Dim As Integer SCol, SLine, CrsrPos
    Dim As String CurrentVal, CurrKey
    
    SCol = Pos
    SLine = CsrLin
    CrsrPos = Len(InValue) + 1
    CurrentVal = InValue
    
    Do
        'Draw
        Locate SLine, SCol
        Print CurrentVal + " "
        Locate SLine + (CrsrPos \ 80), SCol + (CrsrPos Mod 80) - 1
        
        'Get key
        CurrKey = ""
        Do While CurrKey = "": CurrKey = INKEY: Loop
        
        'Edit string
        'Enter
        If CurrKey = Chr(13) Then
            Return CurrentVal
        
        'Escape
        ElseIf CurrKey = Chr(27) Then
            Return InValue
            
        'Backspace
        ElseIf CurrKey = Chr(8) Then
            If CrsrPos > 1 Then
                CurrentVal = Left(CurrentVal, CrsrPos - 2) + Mid(CurrentVal, CrsrPos)
                CrsrPos -= 1
            End If
            
        ElseIf Len(CurrKey) = 2 Then
            Select Case Asc(Right(CurrKey, 1))
            'Left
            Case 75:
                If CrsrPos > 1 Then CrsrPos -= 1
            'Right
            Case 77:
                If CrsrPos <= Len(CurrentVal) Then CrsrPos += 1
            'Home
            Case 71:
                CrsrPos = 1
            'End
            Case 79:
                CrsrPos = Len(CurrentVal) + 1
            'Delete
            Case 83:
                CurrentVal = Left(CurrentVal, CrsrPos - 1) + Mid(CurrentVal, CrsrPos + 1)
            End Select
            
        Else
            CurrentVal = Left(CurrentVal, CrsrPos - 1) + CurrKey + Mid(CurrentVal, CrsrPos)
            CrsrPos += 1
            
        End If
    
    Loop
    
End Function

Function AddQuotes (InValue As String) As String
    If Instr(InValue, " ") <> 0 Then
        Return Chr(34) + InValue + Chr(34)
    Else
        Return InValue
    End If
    
End Function

Function TrimQuotes (InValue As String) As String
    If Left(InValue, 1) = Chr(34) And Right(InValue, 1) = Chr(34) Then
        Return Mid(InValue, 2, Len(InValue) - 2)
    Else
        Return InValue
    End If
    
End Function

Function GetParamValue(WholeString As String, ParamName As String) As String
    
    Dim As Integer SC, InString, PD, StringStart
    Dim As String Strings(30), DataSource, OutTemp
    
    SC = 0
    DataSource = WholeString
    Do While Instr(DataSource, Chr(34) + Chr(34)) <> 0
        Replace DataSource, Chr(34) + Chr(34), Chr(200)
    Loop
    
    InString = 0
    PD = 0
    Do
        PD += 1
        If Mid(DataSource, PD, 1) = Chr(34) Then
            If InString Then
                InString = 0
                SC += 1
                Strings(SC) = Mid(DataSource, StringStart, PD - StringStart + 1)
                DataSource = Left(DataSource, StringStart - 1) + Chr(200 + SC) + Mid(DataSource, PD + 1)
                PD = StringStart - 1
            Else
                StringStart = PD
                InString = -1
            End If
        End If
    Loop While PD < Len(DataSource)
    
    If Instr(UCase(DataSource), UCase(ParamName)) <> 0 Then
        OutTemp = Mid(DataSource, Instr(UCase(DataSource), UCase(ParamName)))
        If Instr(OutTemp, " ") <> 0 Then OutTemp = Left(OutTemp, Instr(OutTemp, " ") - 1)
        
        If Instr(ParamName, ":") <> 0 And Instr(OutTemp, ":") <> 0 Then
            OutTemp = Mid(OutTemp, Instr(OutTemp, ":") + 1)
        End If
        
        For PD = 1 to 30
            Do While Instr(OutTemp, Chr(200 + PD)) <> 0
                Replace OutTemp, Chr(200 + PD), Strings(PD)
            Loop
        Next
        Do While Instr(OutTemp, Chr(200)) <> 0
            Replace OutTemp, Chr(200), Chr(34)
        Loop
        
        Return OutTemp
        
    Else
        Return ""
    End If
    
End Function

SUB Replace (DataVar As String, Find As String, Rep As String)
    Dim As String VarTemp, FindTemp, NewData
    
    VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
    IF INSTR(VARTemp, FINDTemp) = 0 THEN DataVar = DataVar + Rep: EXIT SUB
    
    NewData = Left(DataVar, INSTR(VARTemp, FINDTemp) - 1)
    NewData = NewData + Rep
    NewData = NewData + Mid(DataVar, INSTR(VARTemp, FINDTemp) + LEN(Find))
    
    DataVar = NewData
END SUB
