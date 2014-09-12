'	GCBASIC - A BASIC Compiler for microcontrollers
'	 Miscellaneous routines
'	Copyright (C) 2006 - 2012 Hugh Considine
'
'	This program is free software; you can redistribute it and/or modify
'	it under the terms of the GNU General Public License as published by
'	the Free Software Foundation; either version 2 of the License, or
'	(at your option) any later version.
'
'	This program is distributed in the hope that it will be useful,
'	but WITHOUT ANY WARRANTY; without even the implied warranty of
'	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'	GNU General Public License for more details.
'
'	You should have received a copy of the GNU General Public License
'	along with this program; if not, write to the Free Software
'	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'
'If you have any questions about the source code, please email me: hconsidine@internode.on.net
'Any other questions, please email me or see the GCBASIC forums.

'Windows API
'Needed for ShortFileName
#Ifdef __FB_WIN32__
	'#Include Once "windows.bi"
	type LPCWSTR as wstring Ptr
	type LPCSTR as zstring Ptr
	type LPWSTR as wstring Ptr
	type LPSTR as zstring Ptr
	type DWORD as UInteger
	
	extern "windows" lib "kernel32"
		#ifdef UNICODE
			declare function GetShortPathName alias "GetShortPathNameW" (byval as LPCWSTR, byval as LPWSTR, byval as DWORD) as DWORD
		#Else 
			declare function GetShortPathName alias "GetShortPathNameA" (byval as LPCSTR, byval as LPSTR, byval as DWORD) as DWORD
		#EndIf
	End Extern
#EndIf

Function AddFullPath(CurrPath As String, FullPathIn As String = "") As String
	'Will return a fully qualified path from CurrPath.
	'FullPathIn is that path that should be added to CurrPath if not FQ
	
	Dim As String OutPath, FullPath
	OutPath = CurrPath
	
	'Get full path
	If FullPathIn = "" Then
		FullPath = CurDir
	Else
		FullPath = FullPathIn
	End If
	
	#Ifdef __FB_LINUX__
		'Remove trailing slash from full path
		If Right(FullPath, 1) = "/" Then FullPath = Left(FullPath, Len(FullPath) - 1)
		'If CurrPath not FQ, add full path
		If Left(OutPath, 1) <> "/" And Left(OutPath, 1) <> "~" Then
			OutPath = FullPath + "/" + CurrPath
		End If
	
	#Else
		'Remove trailing slash from full path
		If Right(FullPath, 1) = "\" Then FullPath = Left(FullPath, Len(FullPath) - 1)
		'If CurrPath not FQ, add full path
		If Mid(OutPath, 2, 1) <> ":" And Left(OutPath, 2) <> "\\" Then
			OutPath = FullPath + "\" + CurrPath
		End If
	#EndIf
	
	Return OutPath
End Function

SUB Calculate (SUM As String)
	
	Dim As String Act, ACT2, CTemp, Temp
	Dim As String OSS, CLDO, TS, N1S, N2S, OS, Ans, M
	Dim As Single Res, N1, N2
	Dim As Integer CA, PD, T, FD, AP, FS, SOL, CO, SC
	
	SUM = UCase(SUM)
	
	REM Replace AND, OR, NOT, XOR with symbols
	REPLOGIC:
	'IF INSTR(SUM, "AND") <> 0 THEN Replace SUM, "AND", "&": GOTO REPLOGIC
	'IF INSTR(SUM, "OR") <> 0 THEN Replace SUM, "OR", "|": GOTO REPLOGIC
	'IF INSTR(SUM, "XOR") <> 0 THEN Replace SUM, "XOR", "#": GOTO REPLOGIC
	WholeReplace SUM, "AND", "&"
	WholeReplace SUM, "OR", "|"
	WholeReplace SUM, "XOR", "#"
	
	REM Replace High and Low
	DO WHILE INSTR(SUM, "HIGH") <> 0: Replace SUM, "HIGH", "H": LOOP
	DO WHILE INSTR(SUM, "LOW") <> 0: Replace SUM, "LOW", "L": LOOP
	
	BEGINCALC:
	REM Remove all Spaces
	Replace SUM, " ", ""
	IF INSTR(SUM, " ") <> 0 THEN GOTO BEGINCALC
	SUM = " " + LCase(SUM)
	IF INSTR(SUM, "%") <> 0 THEN GOTO PERCDIFF
	
	REM Calculate Sine, Cosine, Tangent, Arctangent and Pi
	PISWAP:
	IF INSTR(SUM, "pi") <> 0 THEN
		Replace SUM, "pi", "3.14159"
		GOTO PISWAP
	END IF
	
	INSMULT:
	IF INSTR(SUM, ")(") <> 0 THEN
		Replace SUM, ")(", ")*("
		GOTO INSMULT
	END IF
	
	FILE(1) = "cos("
	FILE(2) = "sin("
	FILE(3) = "tan("
	FILE(4) = "atn("
	FILE(5) = "sqr("
	FILE(6) = "abs("
	FILE(7) = "sgn("
	FILE(8) = "int("
	FILE(9) = "log("
	FILE(10) = "rnd("
	
	FOR CA = 1 TO 10
		CALCLOOP:
		IF INSTR(LCase(SUM), FILE(CA)) <> 0 THEN
			
			CTemp = ""
			PD = INSTR(LCase(SUM), FILE(CA)) + 4
			T = 1
			GETFUNCTION: 'Is this label still used?
			Do
				IF PD > LEN(SUM) THEN SUM = SUM + ")"
				CTemp = CTemp + Mid(SUM, PD, 1)
				IF Mid(SUM, PD, 1) = "(" THEN T = T + 1
				IF Mid(SUM, PD, 1) = ")" THEN T = T - 1
				PD = PD + 1
			Loop While T > 0
			IF T = 0 Then CTemp = Left(CTemp, LEN(CTemp) - 1)
			
			OSS = FILE(CA) + CTemp + ")"
			CLDO = CLD
			CLD = "no"
			Calculate CTemp
			CLD = CLDO
			
			Select Case CA
				Case 1: CTemp = Str(COS(VAL(CTemp)))
				Case 2: CTemp = Str(SIN(VAL(CTemp)))
				Case 3: CTemp = Str(TAN(VAL(CTemp)))
				Case 4: CTemp = Str(ATN(VAL(CTemp)))
				Case 5: CTemp = Str(SQR(VAL(CTemp)))
				Case 6: CTemp = Str(ABS(VAL(CTemp)))
				Case 7: CTemp = Str(SGN(VAL(CTemp)))
				Case 8: CTemp = Str(INT(VAL(CTemp)))
				Case 9: CTemp = Str(LOG(VAL(CTemp)))
				Case 10: CTemp = Str(RND(VAL(CTemp)))
			End Select
			SCICONV CTemp
			IF Left(CTemp, 1) = " " THEN CTemp = Mid(CTemp, 2)
			Replace SUM, OSS, CTemp
			'IF CLD = "" THEN PRINT SUM
			GOTO CALCLOOP
		END IF
	NEXT
	
	REM Solve Brackets using recursion
	BRACKETS:
	IF INSTR(SUM, "(") = 0 AND INSTR(SUM, ")") = 0 THEN GOTO ENDBRACKET
	TS = ""
	PD = INSTR(SUM, "(")
	T = 0
	Do
		TS = TS + Mid(SUM, PD, 1)
		IF Mid(SUM, PD, 1) = "(" THEN T = T + 1
		IF Mid(SUM, PD, 1) = ")" THEN T = T - 1
		PD = PD + 1
	Loop While T > 0
	OSS = TS
	Replace TS, "(", "": Replace TS, ")", ""
	
	CLDO = CLD
	CLD = "no"
	Calculate TS
	SCICONV TS
	CLD = CLDO
	DO WHILE INSTR(TS, " ") <> 0: Replace TS, " ", "": LOOP
	Replace SUM, OSS, TS
	GOTO BRACKETS
	ENDBRACKET:
	
	REM Find high/low
	HIGHLOW:
	IF INSTR(LCase(SUM), "l") = 0 AND INSTR(LCase(SUM), "h") = 0 THEN GOTO ENDHIGHLOW
	FD = 0
	Do
		FD = FD + 1
		Temp = LCase(Mid(SUM, FD, 1))
	Loop While Temp <> "h" AND Temp <> "l"
	Act = Temp: AP = FD
	
	FOR FS = AP - 1 TO 1 STEP -1
		Temp = Mid(SUM, FS, 1)
		IF (Temp <> Str(VAL(Temp)) AND Temp <> ".") OR Temp = " " THEN FS = FS + 1: EXIT For
		M = Mid(SUM, FS - 1, 1)
		IF Temp = "-" AND (M <> LTrim(Str(VAL(M))) AND M <> "l" AND M <> "h" AND M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN FS = FS - 1: EXIT FOR
	NEXT
	
	N2S = Mid(SUM, AP + 1)
	
	'PRINT N2, Act
	
	IF INSTR(N2S, "h") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "h") - 1)
	IF INSTR(N2S, "l") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "l") - 1)
	IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
	IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
	IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
	IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
	
	N2 = VAL(N2S)
	OS = Act + N2S
	DO WHILE INSTR(OS, " ") <> 0
		Replace OS, " ", ""
	LOOP
	'IF CLD = "" THEN PRINT SUM
	
	RES = 0
	IF Act = "l" THEN RES = N2 AND 255
	IF Act = "h" THEN RES = (N2 AND 65280) / 256
	
	ANS = Str(RES)
	SCICONV ANS
	DO WHILE INSTR(ANS, " ") <> 0: Replace ANS, " ", "": LOOP
	Replace SUM, OS, ANS
	GOTO HIGHLOW
	ENDHIGHLOW:
	
	'Find rotate, translate into multiply/divide by power
	Do While InStr(Sum, "<<") <> 0
		Replace Sum, "<<", "*2^"
	Loop
	Do While InStr(Sum, ">>") <> 0
		Replace Sum, ">>", "/2^"
	Loop
	
REM Find Exponentiation
EXPONENT:
IF INSTR(SUM, "^") = 0 THEN GOTO ENDEXPONENT
FD = 0
FINDEXP:
FD = FD + 1
Temp = Mid(SUM, FD, 1)
IF Temp <> "^" THEN GOTO FINDEXP
Act = Temp: AP = FD

FOR FS = AP - 1 TO 1 STEP -1
 Temp = Mid(SUM, FS, 1)
 IF (Temp <> Right(Str(VAL(Temp)), 1) AND Temp <> "." AND Temp <> "-" AND Temp <> "E") OR Temp = " " THEN FS = FS + 1: EXIT FOR
 M = Mid(SUM, FS - 1, 1)
 IF Temp = "-" AND (M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN EXIT FOR
NEXT

N1S = Mid(SUM, FS, AP - FS)
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

If INSTR(N2S, "^") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "^") - 1)
IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
Replace OS, " ", ""
LOOP

IF N1 = 0 AND N2 = 0 THEN Replace SUM, OS, "0": GOTO EXPONENT

ANS = ""
'IF CLD = "" THEN PRINT SUM

RES = N1 ^ N2

ANS = Str(RES)
IF Left(ANS, 1) = " " THEN ANS = Mid(ANS, 2)
SCICONV ANS
IF Left(ANS, 1) = " " THEN ANS = Mid(ANS, 2)
Replace SUM, OS, ANS
GOTO EXPONENT
ENDEXPONENT:

	'Find Division and Multiplication
	DIVMULT:
	IF INSTR(SUM, "/") = 0 AND INSTR(SUM, "*") = 0 AND INSTR(SUM, "x") = 0 THEN GOTO ENDDIVMULT
	FD = 0
	FINDDIV:
	FD = FD + 1
	Temp = Mid(SUM, FD, 1)
	IF Temp <> "/" AND Temp <> "*" AND Temp <> "x" THEN GOTO FINDDIV
	Act = Temp: AP = FD
	
	FOR FS = AP - 1 TO 1 STEP -1
		Temp = Mid(SUM, FS, 1)
		If (Temp <> Str(VAL(Temp)) AND Temp <> ".") OR Temp = " " THEN FS = FS + 1: EXIT FOR
		M = Mid(SUM, FS - 1, 1)
		If Temp = "-" AND (M <> LTrim(Str(VAL(M))) AND M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN FS = FS - 1: EXIT FOR
	NEXT
	
	N1S = Mid(SUM, FS, AP - FS)
	N1 = VAL(N1S)
	N2S = Mid(SUM, AP + 1)
	
	IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
	IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
	IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
	IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
	
	N2 = VAL(N2S)
	OS = N1S + Act + N2S
	DO WHILE INSTR(OS, " ") <> 0
		Replace OS, " ", ""
	LOOP
	
	RES = 0
	IF Act = "/" AND N2 <> 0 THEN RES = N1 / N2
	IF Act = "*" THEN RES = N1 * N2
	ANS = Str(RES)
	SCICONV ANS
	DO WHILE INSTR(ANS, " ") <> 0: Replace ANS, " ", "": LOOP
	Replace SUM, OS, ANS
	GOTO DIVMULT
	ENDDIVMULT:

REM Replace all occurances of "--" with "+"
REMMINUS:
IF INSTR(SUM, "--") <> 0 THEN
 Replace SUM, "--", "+"
 GOTO REMMINUS
END IF
IF INSTR(SUM, "+-") <> 0 THEN
 Replace SUM, "+-", "-"
 GOTO REMMINUS
END IF

REM Addition and Subtraction
FD = 0
ADDSUB:
IF INSTR(SUM, "+") = 0 AND INSTR(FD + 1, SUM, "-") = 0 THEN GOTO ENDADDSUB
FINDADD:
 FD = FD + 1
 Temp = Mid(SUM, FD, 1)
IF Temp <> "+" AND Temp <> "-" THEN GOTO FINDADD

Act = Temp: AP = FD
FOR FS = AP - 1 TO 1 STEP -1
	Temp = Mid(SUM, FS, 1)
	'IF (Temp <> Right(Str(VAL(Temp)), 1) AND Temp <> "." AND Temp <> "-" AND Temp <> "E") OR Temp = " " Then
	IF IsCalcDivider(Temp) And Temp <> "-" Then
		FS = FS + 1
		Exit For
	End If
	M = Mid(SUM, FS - 1, 1)
	'If Temp = "-" AND (M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") Then
	If Temp = "-" AND Not IsCalcDivider(M) Then
		FS = FS - 1
		EXIT For
	End If
NEXT

N1S = Mid(SUM, FS, AP - FS)
IF N1S = " " OR N1S = "" THEN GOTO ADDSUB
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
IF INSTR(N2S, "&") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "&") - 1)
IF INSTR(N2S, "|") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "|") - 1)
IF INSTR(N2S, "#") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "#") - 1)

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
	Replace OS, " ", ""
LOOP

IF Act = "+" THEN ANS = Str(N1 + N2)
IF Act = "-" THEN ANS = Str(N1 - N2)
IF Left(ANS, 1) = " " THEN ANS = Mid(ANS, 2)
IF N1 = 0 AND N2 <> 0 AND Act = "-" THEN FD = FD + 1: GOTO ADDSUB
SCICONV ANS
Replace SUM, OS, ANS
FD = FD + LEN(OS) - LEN(ANS) + 1
FD = 0

MINUS:
IF INSTR(SUM, "--") <> 0 THEN
 Replace SUM, "--", "+"
 GOTO MINUS
END IF
GOTO ADDSUB
ENDADDSUB:

'Test Sum
TESTSUM:
IF INSTR(SUM, "=") = 0 AND INSTR(SUM, "~") = 0 AND InStr(SUM, "<") = 0 AND INSTR(SUM, ">") = 0 AND INSTR(SUM, "{") = 0 AND INSTR(SUM, "}") = 0 THEN GOTO ENDTESTSUM
FD = 0
FINDTEST:
	FD = FD + 1
	Temp = Mid(SUM, FD, 1)
IF Temp <> "=" AND Temp <> "~" And Temp <> "<" AND Temp <> ">" AND Temp <> "{" AND Temp <> "}" THEN GOTO FINDTEST
Act = Temp: AP = FD

'Find value before op
FOR FS = AP - 1 TO 1 STEP -1
	Temp = Mid(SUM, FS, 1)
	'IF (Temp <> Str(VAL(Temp)) AND Temp <> "." AND Temp <> "-" AND Temp <> "E") OR Temp = " " Then
	IF IsCalcDivider(Temp) And Temp <> "-" Then
		FS = FS + 1
		EXIT For
	End If
	M = Mid(SUM, FS - 1, 1)
	IF Temp = "-" AND (M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+") Then
		 FS = FS - 1
		 EXIT For
	End If
NEXT

N1S = Mid(SUM, FS, AP - FS)
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

'IF INSTR(N2S, "=") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "=") - 1)
'IF INSTR(N2S, "<") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "<") - 1)
'IF INSTR(N2S, ">") <> 0 THEN N2S = Left(N2S, INSTR(N2S, ">") - 1)
'IF INSTR(N2S, "{") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "{") - 1)
'IF INSTR(N2S, "}") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "}") - 1)
'IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
'IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
For FS = 2 To Len(N2S)
	If IsCalcDivider(Mid(N2S, FS, 1)) Then
		N2S = Left(N2S, FS - 1)
		Exit For
	End If
Next

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
Replace OS, " ", ""
LOOP
'IF CLD = "" THEN PRINT SUM

If IsConst(N2S) And IsConst(N1S) Then
	If Act = "=" THEN RES = (N1 = N2) * -1
	If Act = "~" Then RES = (N1 <> N2) * -1
	If Act = "<" THEN RES = (N1 < N2) * -1
	If Act = ">" THEN RES = (N1 > N2) * -1
	If Act = "{" THEN RES = (N1 <= N2) * -1
	If Act = "}" THEN RES = (N1 >= N2) * -1
Else
	If Act = "=" THEN RES = (N1S = N2S) * -1
	If Act = "~" Then RES = (N1S <> N2S) * -1
	If Act = "<" THEN RES = (N1S < N2S) * -1
	If Act = ">" THEN RES = (N1S > N2S) * -1
	If Act = "{" THEN RES = (N1S <= N2S) * -1
	If Act = "}" THEN RES = (N1S >= N2S) * -1
End If

ANS = Str(RES)
SCICONV ANS
IF Left(ANS, 1) = " " THEN ANS = Right(ANS, LEN(ANS) - 1)
Replace SUM, OS, ANS
GoTo TESTSUM
ENDTESTSUM:

REM Find Logical operation results (AND, OR, XOR)
LOGICALOP:
IF INSTR(SUM, "&") = 0 AND INSTR(SUM, "|") = 0 AND INSTR(SUM, "#") = 0 THEN GOTO ENDLOGICALOP
FD = 0
FINDLOGIC:
FD = FD + 1
Temp = Mid(SUM, FD, 1)
IF Temp <> "&" AND Temp <> "|" AND Temp <> "#" THEN GOTO FINDLOGIC
Act = Temp: AP = FD

FOR FS = AP - 1 TO 1 STEP -1
 Temp = Mid(SUM, FS, 1)
 IF (Temp <> Str(VAL(Temp)) AND Temp <> ".") OR Temp = " " THEN FS = FS + 1: EXIT FOR
 M = Mid(SUM, FS - 1, 1)
 IF Temp = "-" AND (M <> LTrim(Str(VAL(M))) AND M <> "^" AND M <> "/" AND M <> "*" AND M <> "-" AND M <> "+" AND M <> "&" AND M <> "#" AND M <> "|" AND M <> "!") THEN FS = FS - 1: EXIT FOR
NEXT

N1S = Mid(SUM, FS, AP - FS)
N1 = VAL(N1S)
N2S = Mid(SUM, AP + 1)

IF INSTR(N2S, "/") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "/") - 1)
IF INSTR(N2S, "*") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "*") - 1)
IF INSTR(2, N2S, "-") <> 0 THEN N2S = Left(N2S, INSTR(2, N2S, "-") - 1)
IF INSTR(N2S, "+") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "+") - 1)
IF INSTR(N2S, "&") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "&") - 1)
IF INSTR(N2S, "|") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "|") - 1)
IF INSTR(N2S, "#") <> 0 THEN N2S = Left(N2S, INSTR(N2S, "#") - 1)

N2 = VAL(N2S)
OS = N1S + Act + N2S
DO WHILE INSTR(OS, " ") <> 0
Replace OS, " ", ""
LOOP
'IF CLD = "" THEN PRINT SUM

RES = 0
IF Act = "&" AND N2 <> 0 THEN RES = N1 AND N2
IF Act = "|" THEN RES = N1 OR N2
IF Act = "#" THEN RES = N1 XOR N2
ANS = Str(RES)
SCICONV ANS
DO WHILE INSTR(ANS, " ") <> 0: Replace ANS, " ", "": LOOP
Replace SUM, OS, ANS
GOTO LOGICALOP
ENDLOGICALOP:

Rem End of normal calculate
SOL = VAL(SUM)
EXIT SUB

REM Percentage Section
PERCDIFF:
FD = 0
FINDSIGN:
FD = FD + 1
Temp = Mid(SUM, FD, 1)
IF Temp <> "+" AND Temp <> "." AND Temp <> "-" AND Temp <> "x" AND Temp <> "*" AND Temp <> "/" THEN GOTO FINDSIGN
Act = Temp: AP = FD

FOR FS = AP - 1 TO 1 STEP -1
Temp = Mid(SUM, FS, 1)
IF Temp <> Right(Str(VAL(Temp)), 1) OR Temp = " " AND Temp <> "." THEN EXIT FOR
NEXT

N1 = VAL(Mid(SUM, FS, AP - FS))
N2 = VAL(Right(SUM, LEN(SUM) - AP))
ACT2 = Right(SUM, 1)

IF Act = "+" THEN SOL = (N2 + N1) / N2 * 100
IF Act = "-" THEN SOL = (N2 - N1) / N2 * 100
IF Act = "/" THEN SOL = N1 / N2 * 100
IF Act = "x" OR Act = "*" THEN
 SOL = N1 * (N2 / 100)
 IF ACT2 = "+" THEN SOL = SOL + N1
 IF ACT2 = "-" THEN SOL = SOL - N1
END IF
SOL = SOL * SGN(SOL)
SUM = Str(SOL)
END SUB

Function CountOccur (Source As String, Search As String, SearchWhole As Integer = 0) As Integer
	Dim As String Temp, SourceLower
	Dim As Integer T, CO, SC, LC, RC
	
	T = 0
	
	'Find whole string
	IF Left(Search, 2) <> "';" THEN
		FOR CO = 1 TO LEN(Source) - (1 - LEN(Search))
			IF Mid(UCase(Source), CO, LEN(Search)) = UCase(Search) Then
				If SearchWhole Then
					'Check left of find
					LC = 0
					If CO = 1 Then
						LC = -1
					ElseIf IsDivider(Mid(Source, CO - 1, 1)) Then
						LC = -1
					End If
					'Check right of find
					RC = 0
					If CO = LEN(Source) - (1 - LEN(Search)) Then
						RC = -1
					ElseIf IsDivider(Mid(Source, CO + Len(Search), 1)) Then
						RC = -1
					End If
					'If left and right clear, have found
					If LC And RC Then
						T = T + 1
					End If
				Else
					T = T + 1
				End If
			End If
		NEXT
	
	'Find one of the searched chars
	Else
		Temp = LCase(Mid(Search, 3))
		SourceLower = LCase(Source)
		
		FOR SC = 1 To LEN(Temp)
			FOR CO = 1 TO LEN(Source)
				IF Mid(Source, CO, 1) = Mid(Temp, SC, 1) THEN T = T + 1
			NEXT
		NEXT
	END IF
	
	CountOccur = T
END FUNCTION

Function DelType (InString As String) As String
	If Instr(InString, "[") = 0 Then Return InString
	
	Dim As String Temp, DataSource
	DataSource = UCase(InString)
	Temp = UCase(InString)
	
	If INSTR(Temp, "[BYTE]") <> 0 Then Replace DataSource, "[BYTE]", ""
	If INSTR(Temp, "[WORD]") <> 0 Then Replace DataSource, "[WORD]", ""
	If INSTR(Temp, "[INTEGER]") <> 0 Then Replace DataSource, "[INTEGER]", ""
	If INSTR(Temp, "[LONG]") <> 0 Then Replace DataSource, "[LONG]", ""
	If INSTR(Temp, "[SINGLE]") <> 0 Then Replace DataSource, "[SINGLE]", ""
	
	Return DataSource
End Function

Sub DisplayProgram
	'Debugging sub
	'Will show all program code when called
	
	Dim CurrSub As Integer
	For CurrSub = 0 To SBC
		Print Subroutine(CurrSub)->Name
		If Subroutine(CurrSub)->Required Then
			LinkedListPrint(Subroutine(CurrSub)->CodeStart)
			Print
		Else
			Print "Not used, so not shown"
			Print
		End If
	Next
	
End Sub

FUNCTION GetByte (DataSource As String, BS As Integer) As String
	
	Dim As String Temp
	Dim As LongInt OutVal
	Dim As Integer PD
	
	'BS is 0 for low, 1 for high
	
	'Check if asm constant
	If Left(DataSource, 1) = "@" Then
		If UCase(Left(DataSource, 11)) = "@SYSASMCALC" Then
			Return DataSource
		End If
		'11/4/2012: Changed from "low VAR" to "low (VAR)" on PIC
		'Same format as AVR, MPASM is ok, need to check gputils
		If BS = 0 Then
			Return "low(" + DataSource + ")"
		ElseIf BS = 1 Then
			Return "high(" + DataSource + ")"
		Else
			Return "0"
		End If
	End If
	
	'Check if variable
	IF NOT IsConst(DataSource) THEN
		IF BS = 0 THEN Return DataSource
		IF BS = 1 THEN Return DataSource + "_H"
		IF BS = 2 THEN Return DataSource + "_U"
		IF BS = 3 THEN Return DataSource + "_E"
	END IF
	
	OutVal = 0
	
	'Convert to decimal
	IF DataSource = Str(ValLng(DataSource)) THEN OutVal = VALLng(DataSource)
	
	IF INSTR(LCase(DataSource), "b'") <> 0 THEN
		Temp = Mid(DataSource, INSTR(LCase(DataSource), "b'") + 2)
		Replace Temp, "'", ""
		OutVal = 0
		FOR PD = 1 TO LEN(Temp)
			OutVal = OutVal + 2 ^ (LEN(Temp) - PD) * VAL(Mid(Temp, PD, 1))
		NEXT
	END IF
	
	IF INSTR(LCase(DataSource), "0x") <> 0 THEN
		Temp = DataSource
		Replace Temp, "0x", "&h"
		OutVal = ValLng(Temp)
	END IF
	
	IF BS = 0 THEN GetByte = Str(OutVal AND 255)
	IF BS = 1 THEN GetByte = Str((OutVal And 65280) / 256)
	If BS = 2 Then GetByte = Str((OutVal AND &H00FF0000) / 65536)
	If BS = 3 Then GetByte = Str((OutVal AND &HFF000000) / 16777216)
	
END FUNCTION

Function GetString (StringName As String, UsedInProgram As Integer = -1) As String
	
	Dim As Integer ST
	
	If Instr(UCASE(StringName), ";STRING") = 0 Then Return ""
	ST = VAL(Mid(StringName, 8))
	
	If UsedInProgram Then
		StringStore(ST).Used = -1
	End If
	
	Return StringStore(ST).Value
	
End Function

Sub GetTokens(InData As String, OutArray() As String, ByRef OutSize As Integer, DivChar As String = "", IncludeDividers As Integer = 0)
	
	'Return list of tokens from InData
	'DivChar is list of characters that separate tokens
	
	Dim As Integer GetChar, DivMode, CharIsDiv
	Dim As String CurrToken, CurrChar
	
	DivMode = 0
	If DivChar = "" Then DivMode = 1
	
	OutSize = 0
	
	CurrToken = ""
	For GetChar = 1 To Len(InData)
		CurrChar = Mid(InData, GetChar, 1)
		CharIsDiv = 0
		If DivMode = 0 Then
			If InStr(DivChar, CurrChar) <> 0 Then CharIsDiv = -1
		ElseIf DivMode = 1 Then
			If IsDivider(CurrChar) Then CharIsDiv = -1
		End If
		
		If CharIsDiv Then
			If CurrToken <> "" Then
				OutSize += 1: OutArray(OutSize) = Trim(CurrToken)
			End If
			If IncludeDividers Then
				OutSize += 1: OutArray(OutSize) = CurrChar
			End If
			CurrToken = ""
		Else
			CurrToken += CurrChar
		End If
	Next
	
	If CurrToken <> "" Then
		OutSize += 1: OutArray(OutSize) = Trim(CurrToken)
	End If
	
End Sub

Function GetTypeLetter(InType As String) As String
	Select Case UCase(InType)
		Case "BIT", "BYTE", "WORD", "INTEGER", "LONG", "FLOAT", "STRING": Return UCase(InType) + ":"
		Case Else: Return "*:"
	End Select
	
End Function

Function GetTypeSize(InType As String) As Integer
	'Returns the amount of RAM that a given data type will use
	
	Select Case UCase(InType)
		'Byte variables take 1 byte of RAM
		Case "BYTE": Return 1
		'Word and Integer variables take 2 bytes
		Case "WORD", "INTEGER": Return 2
		'Long variables take 4 bytes
		Case "LONG": Return 4
			
		'Single variables take 4 bytes
		Case "SINGLE": Return 4
		
		'String variables have a different default size depending on available RAM
		Case "STRING":
			If ChipRam < 16 Then
				Return 10
			ElseIf ChipRam < 368 Then
				Return 20
			Else
				Return 40
			End If
	End Select
	
	Return 1
End Function

FUNCTION IsCalc (Temp As String) As Integer
	Dim As String DataSource
	
	DataSource = Temp
	IF Left(DataSource, 1) = "-" And Mid(DataSource, 2, 1) = Str(VAL(Mid(DataSource, 2, 1))) Then DataSource = Mid(DataSource, 2)
	
	IsCalc = 0
	IF INSTR(DataSource, "+") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "-") <> 0 THEN IsCalc = -1
	
	IF INSTR(DataSource, "*") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "/") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "%") <> 0 THEN IsCalc = -1
	
	IF INSTR(DataSource, "=") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "~") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "<") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, ">") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "{") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "}") <> 0 THEN IsCalc = -1
	
	IF INSTR(DataSource, "&") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "|") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "!") <> 0 THEN IsCalc = -1
	IF INSTR(DataSource, "#") <> 0 THEN IsCalc = -1
	
	IF WholeINSTR(DataSource, "low") = 2 THEN IsCalc = -1
	IF WholeINSTR(DataSource, "high") = 2 THEN IsCalc = -1
	IF WholeINSTR(DataSource, "and") = 2 THEN IsCalc = -1
	IF WholeINSTR(DataSource, "or") = 2 THEN IsCalc = -1
	IF WholeINSTR(DataSource, "xor") = 2 THEN IsCalc = -1
	IF WholeINSTR(DataSource, "not") = 2 THEN IsCalc = -1
	
END FUNCTION

FUNCTION IsCalcDivider (Temp As String) As Integer
	
	Select Case Temp
		Case "", " ", "(", ")", "+", "-", "*", "/", "%", "=", "!", "<", ">", "{", "}", "~", "&", "|", "#":
			Return -1
		Case Else:
			Return 0
	End Select
	
END FUNCTION

FUNCTION IsConst (DataSource As String) As Integer
	
	Dim As String Temp

	Temp = Trim(DelType(DataSource))
	IF Left(Temp, 1) = "-" THEN Temp = Mid(Temp, 2)
	
	IsConst = 0
	IF Trim(Temp) = Trim(Str(VAL(Temp))) THEN IsConst = -1
	IF INSTR(LCase(Temp), "b'") <> 0 THEN IsConst = -1
	IF INSTR(LCase(Temp), "0x") <> 0 THEN IsConst = -1
	
	IF INSTR(Temp, "@") <> 0 THEN IsConst = -1
	If INSTR(Temp, ";STRING") <> 0 Then IsConst = -1
	
	IF INSTR(Temp, "+") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "-") > 1 THEN IsConst = 0
	IF INSTR(Temp, "*") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "/") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "%") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "&") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "|") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "!") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "#") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "=") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "<") <> 0 THEN IsConst = 0
	IF INSTR(Temp, ">") <> 0 THEN IsConst = 0

END FUNCTION

FUNCTION IsDivider (Temp As String) As Integer
	
	Select Case Temp
		Case " ", "(", ")", ",", ".", ":", ";", "+", "-", "*", "/", "%": Return -1
		Case "=", "!", "<", ">", "{", "}", "~", "&", "|", "#": Return -1
		Case "[", "]": Return -1
		Case Else: Return 0
	End Select
	
END FUNCTION

Function IsIntType(InType As String) As Integer
	'Returns true (-1) if input type is an integer variable
	Dim ThisType As String
	ThisType = LCase(InType)
	
	Select Case ThisType
		Case "const", "byte", "word", "integer", "long": Return -1
		Case Else: Return 0
	End Select
	
End Function

FUNCTION IsLet (DataSource As String) As Integer
	
	Dim As String Temp
	Dim As Integer EqLoc, CD, SpaceCount
	
	IF INSTR(DataSource, "=") = 0 Then Return 0
	Temp = DataSource
	
	Do While INSTR(Temp, "=") <> 0: Replace Temp, "=", Chr(27): Loop
	Do While INSTR(Temp, Chr(27)) <> 0: Replace Temp, Chr(27), " = ": Loop
	Do While INSTR(Temp, "  ") <> 0: Replace Temp, "  ", " ": Loop
	
	EqLoc = INSTR(Temp, "=") - 1
	SpaceCount = 0
	For CD = EqLoc To 1 Step -1
		If Mid(Temp, CD, 1) = " " Then SpaceCount += 1
	Next
	If SpaceCount = 1 Then IsLet = -1
	
END Function

Function IsValidValue(InValue As LongInt, TypeIn As String) As Integer
	'Check if a value is allowed for the given data type
	Dim ValType As String
	ValType = LCase(TypeIn)
	Dim As LongInt MinVal, MaxVal
	
	'Find allowed range based on data type
	Select Case ValType
		Case "bit"
			MinVal = 0
			MaxVal = 1
		Case "byte"
			MinVal = 0
			MaxVal = 255
		Case "word"
			MinVal = 0
			MaxVal = 65535
		Case "integer"
			MinVal = -32768
			MaxVal = 32767
		Case "long"
			MinVal = 0
			MaxVal = 2 ^ 32 - 1
		Case Else
			'Unknown type, assume not compatible
			Return 0
	End Select
	
	'Is value within range?
	If InValue < MinVal Or InValue > MaxVal Then
		Return 0
	Else
		Return -1
	End If
	
End Function

FUNCTION MakeDec (Temp As String) As LongInt
	
	Dim As String DataSource, StrTemp
	Dim As Integer CB
	Dim As LongInt T
	
	DataSource = Trim(UCase(Temp))
	
	If InStr(DataSource, "@") Then
		'Print "Trying to make " + Temp + " into a decimal!"
	End If
	
	If Instr(DataSource, "[BYTE]") <> 0 Then Replace DataSource, "[BYTE]", ""
	If Instr(DataSource, "[WORD]") <> 0 Then Replace DataSource, "[WORD]", ""
	If Instr(DataSource, "[INTEGER]") <> 0 Then Replace DataSource, "[INTEGER]", ""
	If Instr(DataSource, "[LONG]") <> 0 Then Replace DataSource, "[LONG]", ""
	If Instr(DataSource, "[SINGLE]") <> 0 Then Replace DataSource, "[SINGLE]", ""
	
	IF INSTR(DataSource, "0X") <> 0 THEN
		DataSource = Mid(DataSource, INSTR(DataSource, "0X") + 2)
		Return ValLng("&H" + DataSource)
	END IF
	
	IF DataSource = Str(VAL(DataSource)) THEN
		Return ValLng(DataSource)
	END IF
	
	IF INSTR(DataSource, "B'") <> 0 THEN
		DataSource = Mid(DataSource, INSTR(DataSource, "B'") + 2)
		DataSource = Left(DataSource, INSTR(DataSource, "'") - 1)
		T = 0
		FOR CB = LEN(DataSource) TO 1 STEP -1
			T = T + VAL(Mid(DataSource, LEN(DataSource) - CB + 1, 1)) * 2 ^ (CB - 1)
		NEXT
		Return T
	END If
	
	StrTemp = GetString(DataSource, 0)
	IF Len(StrTemp) = 1 Then Return ASC(StrTemp)
	
End FUNCTION

SUB Replace (DataVar As String, Find As String, Rep As String)
	Dim As String VarTemp, FindTemp, NewData
	
	VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
	IF INSTR(VARTemp, FINDTemp) = 0 THEN DataVar = DataVar + Rep: EXIT SUB
	
	NewData = Left(DataVar, INSTR(VARTemp, FINDTemp) - 1)
	NewData = NewData + Rep
	NewData = NewData + Mid(DataVar, INSTR(VARTemp, FINDTemp) + LEN(Find))
	
	DataVar = NewData
END Sub

SUB ReplaceAll (DataVar As String, Find As String, Rep As String)
	
	Do While InStr(UCase(DataVar), UCase(Find)) <> 0
		Replace DataVar, Find, Rep
	Loop
	
End Sub

Function ReplaceToolVariables(InData As String, FNExtension As String = "", FileNameIn As String = "", Tool As ExternalTool Pointer = 0) As String
	
	Dim As String FileName, FileNameNoExt, OutData
	Dim As String FileNoPath, PathNoFile
	Dim As Integer PD
	OutData = InData
	
	If FileNameIn = "" Then
		FileName = OFI
	Else
		FileName = FileNameIn
	End If
	
	'Get filename without extension
	For PD = Len(FileName) To 1 Step -1
		If Mid(FileName, PD, 1) = "." Then
			FileNameNoExt = Left(FileName, PD - 1)
			Exit For
		End If
	Next
	'If requested, change filename extension
	If FNExtension <> "" Then
		FileName = FileNameNoExt + "." + FNExtension
	End If
	
	'Functions
	Dim As String FunctionParams, FunctionResult, FunctionName
	Dim As Integer BL, FStart, FParams, FEnd, FindSlash
	
	Dim As String FunctionList(100)
	Dim As Integer FunctionCount, CurrFn
	FunctionCount = 2
	FunctionList(1) = "NAMEPART"
	FunctionList(2) = "PATHPART"
	
	For CurrFn = 1 To FunctionCount
		FunctionName = FunctionList(CurrFn)
		
		Do While InStr(UCase(OutData), "%" + FunctionName + "%") <> 0
			'Find parameters
			FStart = InStr(UCase(OutData), "%" + FunctionName + "%")
			FParams = FStart + 2 + Len(FunctionName)
			For PD = FParams To Len(OutData)
				If Mid(OutData, PD, 1) = "(" Then BL += 1
				If Mid(OutData, PD, 1) = ")" Then BL -= 1
				If BL = 0 Then
					FEnd = PD
					
					'Get and tidy parameters 
					FunctionParams = Mid(OutData, FParams + 1, FEnd - FParams - 1)
					FunctionParams = ReplaceToolVariables(FunctionParams, FNExtension)
					
					'Perform function
					Select Case FunctionName
						Case "PATHPART":
							'Get path part of filename
							FunctionResult = CurDir
							For FindSlash = Len(FunctionParams) To 1 Step -1
								If Mid(FunctionParams, FindSlash, 1) = "/" Or Mid(FunctionParams, FindSlash, 1) = "\" Then
									FunctionResult = Left(FunctionParams, FindSlash - 1)
									Exit For
								End If
							Next
							
						Case "NAMEPART":
							'Get name part of filename
							FunctionResult = FunctionParams
							For PD = Len(FunctionParams) To 1 Step -1
								If Mid(FunctionParams, PD, 1) = "/" Or Mid(FunctionParams, PD, 1) = "\" Then
									FunctionResult = Mid(FunctionParams, PD + 1)
									Exit For
								End If
							Next
							
						Case Else:
							FunctionResult = ""
					End Select
					
					'Put result back into output
					OutData = Left(OutData, FStart - 1) + FunctionResult + Mid(OutData, FEnd + 1)
					Exit For
				End If 
			Next
		Loop
	Next
	
	'Other tool variables
	If Tool <> 0 Then
		With *Tool
			For PD = 1 To .ExtraParams
				Do While INSTR(LCase(OutData), "%" + .ExtraParam(PD, 1) + "%") <> 0
					Replace OutData, "%" + .ExtraParam(PD, 1) + "%", .ExtraParam(PD, 2)
				Loop
			Next
		End With
	End If
	
	'Items typically found in parameters
	Do While INSTR(UCase(OutData), "%FILENAME%") <> 0: Replace OutData, "%FILENAME%", FileName: Loop
	Do While INSTR(UCase(OutData), "%SHORTNAME%") <> 0: Replace OutData, "%SHORTNAME%", ShortFileName(FileName): Loop
	Do While INSTR(UCase(OutData), "%FN_NOEXT%") <> 0: Replace OutData, "%FN_NOEXT%", FileNameNoExt: Loop
	Do While INSTR(UCase(OutData), "%CHIPMODEL%") <> 0: Replace OutData, "%CHIPMODEL%", ChipName: Loop
	
	'Items typically found in paths
	Do While InStr(LCase(OutData),"%appdata%") <> 0: Replace OutData, "%appdata%", Environ("APPDATA"): Loop
	Do While InStr(LCase(OutData),"%allusersappdata%") <> 0: Replace OutData, "%allusersappdata%", Environ("ALLUSERSPROFILE") + "\Application Data": Loop
	Do While InStr(LCase(OutData),"%temp%") <> 0: Replace OutData, "%temp%", Environ("TEMP"): Loop
	Do While InStr(LCase(OutData),"%instdir%") <> 0: Replace OutData, "%instdir%", ID: Loop
	
	Return OutData
End Function

SUB SCICONV (STemp As String)
	Dim As String Temp, DataSource
	Dim As String V, PS, VI, VF
	Dim As Integer P, S, DV, M, A, AddZero
	
	DO WHILE INSTR(STemp, " ") <> 0: Replace STemp, " ", "": LOOP
	STemp = UCase(STemp)
	
	IF INSTR(UCase(STemp), "E") <> 0 THEN
		'Temp = "00000000000000000000"
		'V = Left(STemp, INSTR(STemp, "E") - 1)
		'PS = Mid(STemp, INSTR(STemp, "E") + 1)
		'Replace V, ".", ""
		'P = VAL(PS)
		'S = 1: IF INSTR(V, "-") <> 0 THEN S = -1: Replace V, "-", ""
		'IF SGN(P) = -1 THEN STemp = "0." + Left(Temp, P * -1 - 1) + V
		'IF SGN(P) = 1 THEN STemp = V + Left(Temp, P)
		'IF S = -1 THEN STemp = "-" + STemp
		
		V = Left(STemp, INSTR(STemp, "E") - 1)
		P = Val(Mid(STemp, INSTR(STemp, "E") + 1))
		S = 1: If INSTR(V, "-") <> 0 THEN S = -1: Replace V, "-", ""
		
		IF SGN(P) = -1 Then
			STemp = "0."
			For AddZero = 1 To (-P - 1)
				STemp = STemp + "0"
			Next
			Replace V, ".", ""
			STemp = STemp + V
		Else
			'Get integer and fractional part of number
			If InStr(V, ".") <> 0 Then
				VI = Left(V, InStr(V, ".") - 1)
				VF = Mid(V, InStr(V, ".") + 1)
			Else
				VI = V
				VF = ""
			End If
			'Combine without decimal
			STemp = VI + VF
			'Add zeros
			For AddZero = 1 To P - Len(VF)
				STemp = STemp + "0"
			Next
		End If
		
		IF S = -1 THEN STemp = "-" + STemp
		
	END IF
	
	IF INSTR(UCase(STemp), "D") <> 0 THEN
		Temp = "00000000000000000000"
		DV = VAL(Mid(STemp, INSTR(STemp, "D") + 1))
		STemp = Left(STemp, INSTR(STemp, "D") - 1)
		DV = DV * SGN(DV)
		IF DV > 1 THEN DV = DV - 1
		DataSource = STemp
		Replace DataSource, ".", ""
		Replace DataSource, "-", ""
		Replace DataSource, " ", ""
		M = 0: IF INSTR(STemp, "-") <> 0 THEN M = 1
		IF DV > 7 THEN STemp = ".0": EXIT SUB
		A = 7 - DV
		STemp = "." + Left(Temp, DV)
		STemp = STemp + Left(DataSource, A)
		IF M = 1 THEN STemp = "-" + STemp
	END IF
	
	DO WHILE INSTR(STemp, " ") <> 0: Replace STemp, " ", "": LOOP
	
END SUB

Function ShortFileName(InName As String) As String
	
	#Ifdef __FB_WIN32__
		Dim As String OutName = Space(256)
		GetShortPathName(InName, OutName, 256)
		Return Trim(OutName)
	#Else
		'No need for such silliness on Linux
		Return InName
	#endif
	
End Function

FUNCTION ShortName (NameIn As String) As String
	
	Dim As String TempData
	TempData = NameIn
	
	'Get rid of "s
	IF INSTR(TempData, Chr(34)) <> 0 THEN
		TempData = Mid(TempData, INSTR(TempData, Chr(34)) + 1)
		TempData = Left(TempData, INSTR(TempData, Chr(34)) - 1)
	END IF
	
	'Linux filename conversion
	#IFDEF __FB_LINUX__
		DO WHILE INSTR(TempData, "\") <> 0: Replace TempData, "\", "/": LOOP
	#ENDIF
	
	ShortName = Trim(TempData)

END Function

Function LinkedListCreate As LinkedListElement Pointer
	
	Dim As LinkedListElement Pointer NewElement
	NewElement = Callocate(SizeOf(LinkedListElement))
	NewElement->Prev = 0
	NewElement->Next = 0
	NewElement->MetaData = 0
	
	Return NewElement
End Function

Function LinkedListInsert (Location As LinkedListElement Pointer, NewLine As String) As LinkedListElement Pointer
	Dim As LinkedListElement Pointer NewCodeLine
	NewCodeLine = Callocate(SizeOf(LinkedListElement))
	
	With *NewCodeLine
		.Value = NewLine
		.Prev = Location
		.Next = Location->Next
		Location->Next = NewCodeLine
		If .Next <> 0 Then .Next->Prev = NewCodeLine
	End With
	
	Return NewCodeLine
End Function

Function LinkedListInsert (Location As LinkedListElement Pointer, NewData As Any Pointer) As LinkedListElement Pointer
	Dim As LinkedListElement Pointer NewCodeLine
	NewCodeLine = Callocate(SizeOf(LinkedListElement))
	
	With *NewCodeLine
		.Value = "-"
		.MetaData = NewData
		.Prev = Location
		.Next = Location->Next
		Location->Next = NewCodeLine
		If .Next <> 0 Then .Next->Prev = NewCodeLine
	End With
	
	Return NewCodeLine
End Function


Function LinkedListInsertList (Location As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
	Dim As LinkedListElement Pointer NewListEnd, AfterLocation, CurrPos, StartNode
	
	'If no list passed in, do nothing
	If NewList = 0 Then Return Location
	
	'No location passed in, return null
	If Location = 0 Then Return 0
	
	'Don't do anything if new list is empty
	'(Need to check next because lists always have empty sentinel node at start)
	If NewList->Next = 0 Then Return Location
	
	'Get next element after location
	AfterLocation = Location->Next
	
	'Add new list after location
	Location->Next = NewList->Next
	NewList->Next->Prev = Location
	
	'Find end of new list
	If NewListEndIn = 0 Then
		CurrPos = NewList
		StartNode = 0
		Do While (CurrPos->Next <> 0 And StartNode <> CurrPos->Next)
			'Detect circular loops, should not happen!
			If StartNode = 0 Then StartNode = NewList->Next
			CurrPos = CurrPos->Next
		Loop
		If StartNode = CurrPos->Next Then
			Print "Internal error: circular or damaged list"
		End If
		NewListEnd = CurrPos
	Else
		NewListEnd = NewListEndIn
	End If
	
	'Found end, reattach rest of old list
	If AfterLocation <> 0 Then
		NewListEnd->Next = AfterLocation
		AfterLocation->Prev = NewListEnd
	End If
	
	Return NewListEnd
End Function

Function LinkedListAppend (ListIn As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
	Dim As LinkedListElement Pointer NewListEnd, CurrPos, Location
	
	'Get end of list
	Location = ListIn
	Do While Location->Next <> 0
		Location = Location->Next
	Loop
	
	'Don't do anything if new list is empty
	'(Need to check next because lists always have empty sentinel node at start)
	If NewList->Next = 0 Then Return Location
	
	'Add new list after location
	Location->Next = NewList->Next
	NewList->Next->Prev = Location
	
	'Find end of new list
	If NewListEndIn = 0 Then
		CurrPos = NewList
		Do While CurrPos->Next <> 0
			CurrPos = CurrPos->Next
		Loop
		NewListEnd = CurrPos
	Else
		NewListEnd = NewListEndIn
	End If
	
	Return NewListEnd
End Function

Function LinkedListDelete (Location As LinkedListElement Pointer) As LinkedListElement Pointer
	'Removes Location from the list
	'Returns the list element before Location
	Dim As LinkedListElement Pointer OldPrev, OldNext
	
	With *Location
		OldPrev = .Prev
		OldNext = .Next
		If .Prev <> 0 Then
			.Prev->Next = OldNext
		End If
		If .Next <> 0 Then
			.Next->Prev = OldPrev
		End If
		If .MetaData <> 0 Then
			DeAllocate .MetaData
		End If
	End With
	DeAllocate Location
	
	Return OldPrev
End Function

Function LinkedListDeleteList (StartLoc As LinkedListElement Pointer, EndLoc As LinkedListElement Pointer) As LinkedListElement Pointer
	Dim As LinkedListElement Pointer CurrLine, DeleteLine, LastLine
	
	'Remove items from list
	'Fix next pointer of node before start
	If StartLoc->Prev <> 0 Then
		 If EndLoc <> 0 Then
		 	StartLoc->Prev->Next = EndLoc->Next
		 Else
		 	StartLoc->Prev->Next = 0
		 End If
	End If
	LastLine = StartLoc->Prev
	'Fix prev pointer of node after end
	'If EndLoc = 0, delete the rest of the list
	If EndLoc <> 0 Then
		If EndLoc->Next <> 0 Then EndLoc->Next->Prev = StartLoc->Prev
		EndLoc->Next = 0
	End If
	'Clear prev of first node
	StartLoc->Prev = 0
	
	'Delete items from memory
	CurrLine = StartLoc
	Do While CurrLine <> 0
		DeleteLine = CurrLine
		CurrLine = CurrLine->Next
		DeAllocate DeleteLine
	Loop
	
	Return LastLine
End Function

Sub LinkedListPrint(StartNode As LinkedListElement Pointer)
	Dim As LinkedListElement Pointer CurrPos
	
	'Check for null pointer
	If StartNode = 0 Then Exit Sub
	CurrPos = StartNode->Next
	
	Do While CurrPos <> 0
		Print CurrPos->Value
		CurrPos = CurrPos->Next
	Loop
End Sub

Function LinkedListSize(StartNode As LinkedListElement Pointer) As Integer
	Dim As LinkedListElement Pointer CurrPos
	Dim As Integer OutSize
	OutSize = 0
	CurrPos = StartNode->Next
	
	Do While CurrPos <> 0
		OutSize += 1
		CurrPos = CurrPos->Next
	Loop
	
	Return OutSize
End Function

Function SubSigMatch (SubSigIn As String, CallSigIn As String) As Integer
	
	Dim As Integer OutScore
	Dim As String SubSig, CallSig
	Dim As String CurrSub, CurrCall
	
	'Force match
	'1000000 should be way more than any other score
	If CallSigIn = "ForceMatch" Then Return 1000000
	
	SubSig = SubSigIn
	CallSig = CallSigIn
	
	'If signatures match exactly, return highest possible value
	If SubSig = CallSig Then
		If Len(SubSig) <> 0 Then
			Return 20 * Len(SubSig)
		Else
			Return 20
		End If
	End If
	
	'Calculate a match score
	'If parameters don't match, 0, if they can be converted, 20 / intermediate sizes, if they match, 20.
	OutScore = 0
	Do While Len(SubSig) > 0
		CurrSub = Left(SubSig, InStr(SubSig, ":") - 1)
		CurrCall = Left(CallSig, InStr(CallSig, ":") - 1)
		'Print CurrSub, CurrCall
		
		'Deal with optional parameters
		If CurrCall = "*" Then
			If CurrSub = LCase(CurrSub) Then
				'OutScore += 2
				OutScore += 20
			Else
				Return 0 'Can't match if default parameter not specified 
			End If
		End If 
		
		'If parameter missing and not optional, can't match
		If CurrCall = "" And CurrSub <> "" Then
			Return 0
		'If there is an extra parameter, can't match
		ElseIf CurrCall <> "" And CurrSub = "" Then
			Return 0
		End If
		
		'Parameter in call matches that in sub
		If UCase(CurrCall) = UCase(CurrSub) Then OutScore += 20 'was 2
		
		'Parameter in call is of lower type than that in sub
		If CastOrder(CurrCall) < CastOrder(CurrSub) Then
			'OutScore += 1
			OutScore += 20 / CastOrder(CurrSub) - CastOrder(CurrCall)
		End If
		
		'Parameter in call is of higher type than in sub, cannot convert
		If CastOrder(CurrCall) > CastOrder(CurrSub) Then Return 0
		
		SubSig = Mid(SubSig, InStr(SubSig, ":") + 1)
		CallSig = Mid(CallSig, InStr(CallSig, ":") + 1)
	Loop
	
	Return OutScore
	
End Function

FUNCTION WholeINSTR (DataIn As String, FindIn As String, SearchAgain As Integer = -1) As Integer
	Dim As String DataSource, Temp, Find
	Dim As Integer T
	
	DataSource = UCase(DataIn): Find = UCase(FindIn)
	
	IF INSTR(DataSource, Find) = 0 THEN WholeINSTR = 0: EXIT FUNCTION
	IF LEN(DataSource) = LEN(Find) THEN WholeINSTR = 2: EXIT FUNCTION
	
	DoWholeINSTRAgain:
	T = 0
	
	IF INSTR(DataSource, Find) = 1 THEN T = 1
	IF T = 0 THEN
		Temp = Mid(DataSource, INSTR(DataSource, Find) - 1, 1)
		IF IsDivider(Temp) THEN T = 1
	END IF
	
	IF INSTR(DataSource, Find) + LEN(Find) - 1 = LEN(DataSource) THEN T = T + 1
	IF T < 2 THEN
		Temp = Mid(DataSource, INSTR(DataSource, Find) + LEN(Find), 1)
		IF IsDivider(Temp) THEN T = T + 1
	END If
	
	IF T = 1 And SearchAgain THEN
		Replace DataSource, Find, Chr(30)
		IF INSTR(DataSource, Find) <> 0 THEN GOTO DoWholeINSTRAgain
	END IF
	
	WholeINSTR = T
	
END FUNCTION

Function WholeInstrLoc(DataSource As String, FindTemp As String) As Integer
	
	Dim As Integer P, StartOK
	
	For P = 1 To Len(DataSource)
		'Check start
		StartOK = 0
		If P = 1 Then
			StartOK = -1
		Else
			If IsDivider(Mid(DataSource, P - 1, 1)) Then StartOK = -1
		EndIf
		If StartOK Then
			'Check end
			If IsDivider(Mid(DataSource, P + Len(FindTemp), 1)) Or P + Len(FindTemp) = Len(DataSource) Then
				'Check middle
				If Mid(DataSource, P, Len(FindTemp)) = FindTemp Then
					Return P
				End If
			End If
		End If
	Next
	
	Return 0
End Function

SUB WholeReplace (DataVar As String, Find As String, Rep As String)
	
	Dim As String VarTemp, FindTemp, Temp, NewData
	Dim As Integer T, RepAll
	
	RepAll = -1
	If Left(Find, 3) = "[1]" Then
		RepAll = 0
		Find = Mid(Find, 4)
	End If
	
	VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
	IF INSTR(VARTemp, FINDTemp) = 0 THEN EXIT SUB
	IF VARTemp = FINDTemp THEN DataVar = Rep: EXIT SUB
	
	Do
		'Check for occurance of whole string
		'T: 0 not found, 1 left matches, 2 right matches (all found), 3 = found but not whole 
		T = 0
		'Check start
		IF INSTR(UCase(DataVar), FINDTemp) = 1 THEN T = 1
		IF T = 0 THEN
			Temp = Mid(DataVar, INSTR(UCase(DataVar), FINDTemp) - 1, 1)
			IF IsDivider(Temp) THEN T = 1
		END IF
		'Check end
		IF INSTR(UCase(DataVar), FINDTemp) + LEN(FINDTemp) - 1 = LEN(DataVar) AND T = 1 THEN T = 2
		IF T = 1 THEN
			Temp = Mid(DataVar, INSTR(UCASE(DataVar), FINDTemp) + LEN(FINDTemp), 1)
			IF IsDivider(Temp) THEN T = 2
		END IF
		'If search string present but part of another word, T = 3
		IF T = 0 AND INSTR(UCase(DataVar), FINDTemp) <> 0 THEN T = 3
		
		'Search string not found at all, clean up and exit
		IF T = 0 Then Exit Do
		
		'If search string found but part of another word, convert to Chr(26)
		If T = 1 OR T = 3 THEN
			Replace DataVar, FINDTemp, Chr(26)
		
		'If whole string found, replace
		ElseIf T = 2 THEN
			NewData = Left(DataVar, INSTR(UCase(DataVar), FINDTemp) - 1)
			NewData = NewData + Rep
			DataVar = NewData + Mid(DataVar, INSTR(UCase(DataVar), FINDTemp) + LEN(Find))
			'If only replacing single occurance, exit
			If Not RepAll Then Exit Do
		END IF
		
	Loop
	
	'Tidy and exit
	Do While InStr(DataVar, Chr(26)) <> 0
		Replace DataVar, Chr(26), Find
	Loop
	
	Exit Sub
	
END SUB
