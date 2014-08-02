'	GCBASIC - A BASIC Compiler for microcontrollers
'	 Variable handling routines
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

Sub AddVar(VarNameIn As String, VarTypeIn As String, VarSizeIn As Integer, VarSubIn As SubType Pointer, VarPointerIn As String, OriginIn As String, FixedLocation As Integer = -1, ExplicitDeclaration As Integer = 0)
	
	Dim As String VarName, VarType, VarPointer, Origin, Temp, VarAlias, ConstName
	Dim As Integer VarSize, CL, TempSize, PD, VarSearchStart, T, VarFixedSize
	Dim As VariableType Pointer VarFound
	Dim As SubType Pointer VarSub, MainSub
	
	VarName = VarNameIn
	VarType = VarTypeIn
	VarSize = VarSizeIn
	VarPointer = VarPointerIn
	Origin = OriginIn
	VarSub = VarSubIn
	MainSub = Subroutine(0)
	
	'Do this to prevent null pointer access
	If VarSub = 0 Then
		'Print "Internal error in AddVar"
		VarSub = Subroutine(0)
	End If
	
	'Print VarName, VarSub, Origin
	
	'If VarName is a constant, exit
	If IsConst(VarName) Then Exit Sub
	If INSTR(VarName, ";STRING") <> 0 Then Exit Sub
	If INSTR(VarName, ".") <> 0 Then Exit Sub
	
	'Remove $, () from var name
	IF INSTR(VarName, "()") <> 0 Then 
		VarName = Left(VarName, INSTR(VarName, "()") - 1)
		If VarType = "" THEN VarType = "STRING"
		If VarSize <= 1 Then VarSize = 20
	End If
	IF INSTR(VarName, "$") <> 0 Then
		VarName = Left(VarName, INSTR(VarName, "$") - 1)
		If VarType = "" THEN VarType = "STRING"
		If VarSize <= 1 Then VarSize = 20
	End If
	VarName = Trim(VarName)
	IF VarName = "" THEN EXIT SUB
	
	'Remove casts from var name
	If InStr(VarName, "[") <> 0 And InStr(VarName, "]") <> 0 Then
		Temp = Mid(VarName, InStr(VarName, "["))
		Temp = Left(Temp, InStr(Temp, "]"))
		Replace VarName, Temp, ""
		VarType = Mid(Temp, 2, Len(Temp) - 2)
	End If
	
	'Implicit declaration
	If Not ExplicitDeclaration Then
		'Do something here later!
	End If
	
	'Special treatment for strings
	VarFixedSize = 0
	If Left(VarType, 6) = "STRING" Then
		
		VarSize = -1
		
		'Check for StringSize constant
		FOR CL = 1 TO DFC
			ConstName = UCase(gcDEF(CL, 1))
			IF ConstName = "STRINGSIZE" THEN
				TempSize = Val(gcDEF(CL, 2))
				If TempSize > 0 Or TempSize < ChipRam Then VarSize = TempSize
				Exit For
			END IF
		NEXT
		
		'Defaults
		If VarSize = -1 Then
			VarSize = GetTypeSize("STRING")
		End If
		
		IF INSTR(VarType, "*") <> 0 Then
			VarSize = VAL(Trim(Mid(VarType, INSTR(VarType, "*") + 1)))
			VarType = "STRING"
			If VarSize = 1 Then VarSize = 2 ' Needed to make some older code in compiler behave
			VarFixedSize = -1
		End If
	End If
	
	'Defaults
	If VarSize = 0 Then VarSize = 1
	If VarPointer = "" Then VarPointer = "REAL"
	
	'Don't try to redefine system registers
	If ModePIC Or ModeAVR Then
		IF UCase(Left(VarName, 11)) = "SYSWAITTEMP" OR UCase(Left(VarName, 9)) = "DELAYTEMP" Then
			VarPointer = "REGISTER@-2"
		End If
		IF UCase(Left(VarName, 11)) = "SYSCALCTEMP" Then VarPointer = "REGISTER@-2"
		IF UCase(Left(VarName, 11)) = "SYSBYTETEMP" Then VarPointer = "REGISTER@-2"
		IF UCase(Left(VarName, 11)) = "SYSWORDTEMP" Then VarPointer = "REGISTER@-2"
		IF UCase(Left(VarName, 14)) = "SYSINTEGERTEMP" Then VarPointer = "REGISTER@-2"
		IF UCase(Left(VarName, 11)) = "SYSLONGTEMP" Then VarPointer = "REGISTER@-2"
		If UCase(Left(VarName, 10)) = "SYSDIVMULT" Then VarPointer = "REGISTER@-2"
		If UCase(Left(VarName, 8)) = "SYSREADA" Then VarPointer = "REGISTER@-2"
		If UCase(Left(VarName, 9)) = "SYSSTRING" Then
			Temp = UCase(Mid(VarName, 10))
			If Temp = "LENGTH" Or Temp = "A" Or Temp = "A_H" Or Temp = "B" Or Temp = "B_H" Then
				VarPointer = "REGISTER@-2"
			End If
		End If
		IF UCase(VarName) = "SYSDIVLOOP" Then VarPointer = "REGISTER@-2"
		IF UCase(VarName) = "SYSSIGNBYTE" Then VarPointer = "REGISTER@-2"
		
		'Some are PIC Or AVR specific
		If ModePIC Then
			If UCase(VarName) = "SYSW" Then VarPointer = "REGISTER@-2"
			If UCase(VarName) = "SYSSTATUS" Then VarPointer = "REGISTER@-2"
		ElseIf ModeAVR Then
			IF UCase(VarName) = "SYSVALUECOPY" Then VarPointer = "REGISTER@-2"
			IF UCase(Left(VarName, 7)) = "SYSTEMP" Then
				If VarPointer = "REAL" Then VarPointer = "REGISTER@-2"
			End If
		End If
	End If
	
	'Convert "float" to "single" (although neither is yet supported)
	If VarType = "FLOAT" Then VarType = "SINGLE"
	
	'Deal with aliases
	VarAlias = ""
	If Left(VarPointer, 6) = "ALIAS:" Then
		VarAlias = Trim(Mid(VarPointer, 7))
		VarPointer = "POINTER"
		'Print VarName, "Alias:", VarAlias
	End If
	
	'Tidy input
	VarType = Trim(UCase(VarType))
	
	'Check Name
	T = 0
	IF INSTR(VarName, " ") <> 0 THEN T = 1
	IF INSTR(VarName, ",") <> 0 THEN T = 1
	IF INSTR(VarName, "(") <> 0 THEN T = 1
	IF INSTR(VarName, ")") <> 0 THEN T = 1
	If IsCalc(VarName) Then T = 1
	'Names for status flags or destination locations, cannot reuse
	IF VarName = "B" THEN T = 1
	IF VarName = "C" THEN T = 1
	IF VarName = "F" THEN T = 1
	IF VarName = "H" Then T = 1
	IF ModeAVR And VarName = "I" Then T = 1
	IF VarName = "N" Then T = 1
	IF VarName = "S" Then T = 1
	IF VarName = "T" Then T = 1
	IF VarName = "V" Then T = 1
	IF VarName = "W" THEN T = 1
	IF VarName = "Z" THEN T = 1
	IF T = 1 THEN
		Temp = Message("BadVarName")
		Replace Temp, "%var%", VarName
		LogError Temp, Origin
	END IF
	'Some one-letter names should be allowed without warnings
	If VarName = "I" THEN T = -1
	IF VarName = "J" THEN T = -1
	IF VarName = "K" THEN T = -1
	IF VarName = "L" THEN T = -1
	IF VarName = "M" THEN T = -1
	
	If Len(VarName) = 1 And VarFound = 0 And T = 0 Then
		Temp = Message("WarningShortName")
		Replace Temp, "%type%", "variable"
		Replace Temp, "%name%", VarName
		LogWarning Temp, Origin
	END IF
	
	'Check Type
	T = -1
	IF VarType = "BIT" Then T = 0
	IF VarType = "BYTE" Then T = 0
	IF VarType = "WORD" Then T = 0
	IF VarType = "INTEGER" Then T = 0
	If VarType = "LONG" Then T = 0
	'IF VarType = "SINGLE" Then T = 0 'Don't allow this yet, maybe in some future version
	IF VarType = "STRING" Then T = 0
	IF T THEN
		Temp = Message("BadVarType")
		Replace Temp, "%type%", VarType
		LogError Temp, Origin
	END If
	
	'Check to see if the var is a SFR
	FOR PD = 1 TO SVC
		IF UCase(VarName) = UCase(Trim(SysVars(PD).Name)) THEN Exit Sub
	Next
	
	'Check to see if var exists
	VarFound = 0
	'Check for local vars
	For PD = 1 to VarSub->Variables
		If UCase(Trim(VarName)) = UCase(Trim(VarSub->Variable(PD).Name)) Then
			VarFound = @(VarSub->Variable(PD))
			Exit For
		End If
	Next
	
	'If variable not found, make a new one
	If VarFound = 0 Then
		'Is variable name used for an SFR bit (this will cause confusion)
		For PD = 1 To SVBC
			If VarName = SysVarBits(PD).Name Then
				Temp = Message("WarningNameUsedSystem")
				Replace Temp, "%name%", VarName
				LogWarning Temp, Origin
				Exit For
			End If
		Next
		
		With *VarSub
			.Variables += 1
			VarFound = @(.Variable(.Variables))
			VarFound->Name = VarName
			VarFound->Size = VarSize
			VarFound->FixedLocation = -1
			VarFound->FixedSize = VarFixedSize
		End With
	End If
	
	'Choose Type
	If CastOrder(VarFound->Type) > CastOrder(VarType) Then VarType = VarFound->Type
	
	'Choose Size
	If (VarSize = 1 And VarFound->Size <> 1) Or (VarSize <> 1 And VarFound->Size = 1) Then
		If VarType <> "STRING" And VarFound->Type <> "STRING" Then
			Temp = Message("DupDef")
			Replace Temp, "%var%", VarName
			'Print VarName, VarSize, VarFound->Size, VarType, VarFound->Type
			LogError Temp, Origin
		End If
	End If
	'Upgrade size?
	'If new size is fixed, but current variable size isn't, force change
	If Not VarFound->FixedSize And VarFixedSize Then
		'Size will be changed below, but mark as fixed now
		VarFound->FixedSize = -1
	Else
		'New size is not fixed, old size might be
		If (VarFound->FixedSize And Not VarFixedSize) Or (VarFound->Size > VarSize) Then
			VarSize = VarFound->Size
		End If
	End If
	
	'Choose alias
	If (VarAlias = "" And VarFound->Alias <> "") Or (CountOccur(VarAlias, ",") < CountOccur(VarFound->Alias, ",")) Then
		'Print "Changing " + VarName + " from " + VarAlias + " to " + VarSub->Variable(VarFound).Alias
		VarAlias = VarFound->Alias
	End If
	If VarAlias <> "" Then VarPointer = "POINTER"
	
	'Choose Origin
	If VarFound->Origin <> "" Then Origin = VarFound->Origin
	
	'Choose location
	If VarFound->FixedLocation <> -1 Then FixedLocation = VarFound->FixedLocation
	
	'Apply new settings
	'Print VarName, VarType, VarSubOut, VarAlias
	With *VarFound
		.Name = VarName
		.Type = VarType
		.Size = VarSize
		.Pointer = VarPointer
		.Alias = VarAlias
		.Origin = Origin
		.FixedLocation = FixedLocation
	End With
	
END Sub

SUB AllocateRAM
	
	Dim As String TempData, Origin, Temp
	Dim As String VarName, VarType
	Dim As Integer SV, ListSorted, PD, SkipVar, CD, ArraySize, SR, VLC
	Dim As Integer HighReg, AD, VarSize, ED, FreeStart, FreeSize
	Dim As String OccursInSub(50)
	Dim As Integer OccursInSubs, CurrSub, SubVar, SearchVarList, VarListLoc
	Dim As Integer CurrByte, SRStart, SREnd, SRDir, UnallocatedVars, AllocAttempts
	Dim As Integer RegisterUsed(32)
	
	Dim As LinkedListElement Pointer CurrLine
	
	'Testing: is call tree known here? Seems to be
	'DisplayCallTree
	
	'Add calc vars to list
	For SV = 1 to TCVC
		With CalcVars(SV)
			If .MaxType <> "" Then
				VarName = "SysTemp" + Str(SV)
				VarType = .MaxType
				If ModePIC Then AddVar VarName, VarType, 1, Subroutine(0), "REAL", ""
				If ModeAVR Then
					'High reg?
					If .High Then
						AddVar VarName, VarType, 1, Subroutine(0), "REGISTERH", ""
					Else
						AddVar VarName, VarType, 1, Subroutine(0), "REGISTER", ""
					End If
				End If
			End If
		End With
	Next
	
	'Copy all variables from all used subs into a single list
	Dim As VariableType Variables(5000)
	Dim As Integer VarCount
	For CurrSub = 0 To SBC
		'If sub is required, get its variables
		With *Subroutine(CurrSub)
			If .Required Then
				For SubVar = 1 To .Variables
					'Check to see if variable with same name already added
					VarListLoc = 0
					For SearchVarList = 1 To VarCount
						If UCase(Trim(.Variable(SubVar).Name)) = UCase(Trim(Variables(SearchVarList).Name)) Then
							VarListLoc = SearchVarList
							GoTo SubVarFound
						End If
					Next
					SubVarFound:
					'Var not found, create a new one
					If VarListLoc = 0 Then
						VarCount += 1
						VarListLoc = VarCount
						Variables(VarListLoc).Name = .Variable(SubVar).Name
						Variables(VarListLoc).Type = .Variable(SubVar).Type
						Variables(VarListLoc).Size = .Variable(SubVar).Size
						Variables(VarListLoc).Pointer = .Variable(SubVar).Pointer
						Variables(VarListLoc).Alias = .Variable(SubVar).Alias
						Variables(VarListLoc).Origin = .Variable(SubVar).Origin
						Variables(VarListLoc).FixedLocation = .Variable(SubVar).FixedLocation
						Variables(VarListLoc).FixedSize = .Variable(SubVar).FixedSize
						Variables(VarListLoc).Location = -1
						
					'Apply new settings to existing variable
					Else
						'Type
						If CastOrder(Variables(VarListLoc).Type) < CastOrder(.Variable(SubVar).Type) Then
							Variables(VarListLoc).Type = .Variable(SubVar).Type
						End If
						'Size
						'Do not copy if smaller size is fixed
						If Not Variables(VarListLoc).FixedSize And .Variable(SubVar).FixedSize Then
							'Existing found is not fixed, new one is, use new size
							Variables(VarListLoc).Size = .Variable(SubVar).Size
						ElseIf Variables(VarListLoc).FixedSize And Not .Variable(SubVar).FixedSize Then
							'Existing found is fixed, new one is not fixed, use existing
							
						Else
							'Both fixed size, use larger
							'Or neither fixed, use larger
							If Variables(VarListLoc).Size < .Variable(SubVar).Size Then
								Variables(VarListLoc).Size = .Variable(SubVar).Size
							End If
						End If
						
						'Pointer
						If Variables(VarListLoc).Pointer = "" And .Variable(SubVar).Pointer <> "" Then
							Variables(VarListLoc).Pointer = .Variable(SubVar).Pointer
						End If
						'Alias
						If (CountOccur(Variables(VarListLoc).Alias, ",") < CountOccur(.Variable(SubVar).Alias, ",")) Or _
							(Variables(VarListLoc).Alias = "" And .Variable(SubVar).Alias <> "") Then
							Variables(VarListLoc).Alias = .Variable(SubVar).Alias
						End If
						'Fixed Location
						If Variables(VarListLoc).FixedLocation = -1 And .Variable(SubVar).FixedLocation <> -1 Then
							Variables(VarListLoc).FixedLocation = .Variable(SubVar).FixedLocation
						End If
						'Origin
						If Variables(VarListLoc).Origin = "" And .Variable(SubVar).Origin <> "" Then
							Variables(VarListLoc).Origin = .Variable(SubVar).Origin
						End If
					End If
				Next
			End If
		End With
	Next
	
	'Sort var list by size, name
	ListSorted = 0
	Do While ListSorted = 0
		ListSorted = 1
		For PD = 1 to VarCount - 1
			'If non fixed variable is before fixed, swap
			If Variables(PD).FixedLocation = -1 And Variables(PD + 1).FixedLocation <> -1 Then
				Swap Variables(PD), Variables(PD + 1)
				ListSorted = 0
			End If
			'If smaller variable is before larger, swap
			If Variables(PD).FixedLocation = Variables(PD + 1).FixedLocation And Variables(PD).Size < Variables(PD + 1).Size Then
				Swap Variables(PD), Variables(PD + 1)
				ListSorted = 0
			End If
			'If name of second should be before name of first, swap
			If Variables(PD).FixedLocation = Variables(PD + 1).FixedLocation And Variables(PD).Size = Variables(PD + 1).Size And Variables(PD).Name > Variables(PD + 1).Name Then
				Swap Variables(PD), Variables(PD + 1)
				ListSorted = 0
			End If
		Next
	Loop
	
	'Mark all registers as available (AVR)
	If ModeAVR Then
		For PD = 0 To 31
			RegisterUsed(PD) = 0
		Next
	End If
	
	'Allocate common (non-banked) RAM or register space to system variables
	Dim As Integer DesiredLoc, RegBytesLocated, FinalRegLoc
	RegCount = 0
	For PD = 1 To VarCount
		With Variables(PD)
			IF Left(.Pointer, 8) = "REGISTER" And .Location = -1 Then
				If ModePIC Then
					'We have a register
					'Allocate the individual bytes
					VarSize = GetTypeSize(.Type) - 1
					RegBytesLocated = 0
					For CurrByte = 0 To VarSize
						'Need to allocate it something from the shared bank
						DesiredLoc = GetRegisterLoc(GetByte(.Name, CurrByte))
						'DesiredLoc will be -1 if no location found for variable
						'Or the location otherwise
						If DesiredLoc <> -1 Then
							FinalRegLoc = NoBankLoc(1).StartLoc + DesiredLoc
							FVLC += 1
							FinalVarList(FVLC).Name = GetByte(.Name, CurrByte)
							FinalVarList(FVLC).Value = Str(FinalRegLoc)
							If .Location = -1 Then
								.Location = FinalRegLoc
							End If 
							RegBytesLocated += 1
							
							'Then mark that shared bank location as off limits to other variables
							For SR = 1 To FreeRAM
								If VarLoc(SR) = FinalRegLoc Then
									FreeRAM = FreeRAM - 1
									For CD = SR To FreeRAM
										VarLoc(CD) = VarLoc(CD + 1)
									Next
									Exit For
								End If
							Next
							
						End If
					Next
					
					'If all bytes located, mark variable as in shared bank
					If RegBytesLocated = (VarSize + 1) Then
						.IsSharedBank = -1
					Else
						'If out of room, put it in non-shared bank
						'Might result in problems, but at least it will work sometimes
						.IsSharedBank = 0
						.Location = -1
						.Pointer = "REAL" 'Mark as not a register
						LogWarning Message("WarningLowCommonRam")
					End If
				
				ElseIf ModeAVR Then
					'Is this a system register?
					AD = -1
					IF INSTR(.Pointer, "@") <> 0 Then AD = VAL(Mid(.Pointer, INSTR(.Pointer, "@") + 1))
					If AD = -2 Then
						'If address = -2, it's a system register
						'Look up proper location
						AD = GetRegisterLoc(.Name)
						
						'A location should now have been requested, give it to the variable
						If AD >= 0 Then
							'Allocate the individual bytes
							VarSize = GetTypeSize(.Type) - 1
							'Mark registers as used
							For SR = AD To AD + VarSize
								RegisterUsed(SR) = -1
							Next
							'Assign locations
							For CurrByte = 0 To VarSize
								RegCount += 1
								RegList(RegCount, 1) = GetByte(.Name, CurrByte)
								RegList(RegCount, 2) = Str(AD + CurrByte)
							Next
							.Location = AD
						Else
							'Error, system variable doesn't have a location
							
						End If
						
					End If
					
				End If	
			End If
		End With
	Next
	
	'Allocate RAM to single element vars and arrays
	Dim AliasList(10) As String
	ALC = 0
	
	'May need to iterate through list several times, alias may not be set on the first run
	'(They may refer to variables yet to be allocated)
	AllocAttempts = 0
	UnallocatedVars = -1
	Do While UnallocatedVars And AllocAttempts < 20 
		UnallocatedVars = 0
		AllocAttempts += 1
		
		FOR PD = 1 TO VarCount
			With Variables(PD)
				
				'Don't try allocating RAM to high byte of alias
				If InStr(.Name, "_") <> 0 Then
					'Get name of full variable
					TempData = UCase(Left(.Name, InStr(.Name, "_") - 1))
					'Does it match an alias?
					For CD = 1 To VarCount
						If Variables(CD).Alias <> "" Then
							If TempData = UCase(Variables(CD).Name) Then
								'Name of current var matches name of alias, don't allocate
								.Location = 0
								Exit For
							End If
						End If
					Next
				End If
				
				'Don't allocate RAM twice
				If .Location = -1 Then
					
					'Decide how much RAM to allocate
					'Note: Some types here may not be implemented properly (yet)
					'Allocate RAM to register variables
					IF Left(.Pointer, 8) = "REGISTER" THEN
						'Does the variable need to go into the high register space?
						HighReg = 0
						If Right(.Pointer, 1) = "H" Then HighReg = -1
						
						'Has a particular location been requested for the register?
						AD = -1
						IF INSTR(.Pointer, "@") <> 0 Then AD = VAL(Mid(.Pointer, INSTR(.Pointer, "@") + 1))
						'Location has not been requested, so assign one
						IF AD = -1 Then
							'Get var size
							VarSize = GetTypeSize(.Type)
							
							'Allocate low registers
							If Not HighReg Then
								'Example: LRS is 14, need to allocate 2 bytes, r14 and r15. Can do
								'If LRS is 15, need to allocate 2 bytes, r15 and r16, can't do
								'Search RAM for free locations
								FreeStart = -1
								FreeSize = 0
								For SR = 0 To 15
									'Check for a continuous block of available RAM
									IF RegisterUsed(SR) Then
										FreeSize = 0
										FreeStart = -1
									Else
										FreeSize += 1
										If FreeStart = -1 Then FreeStart = SR
									End If
									
									If FreeSize = VarSize Then Exit For
								Next
								
								If FreeSize = VarSize Then
									AD = FreeStart
									For SR = FreeStart To FreeStart + FreeSize - 1
										RegisterUsed(SR) = -1
									Next
								Else
									HighReg = -1
								End If
							End If
							
							'Allocate high registers/excess low registers
							If HighReg Then
								'If HRS = 17, need 2 bytes, r17 and r16, can do.
								'If HRS = 16 and need 2 bytes, r16 and r15, can't.
								'Need to allocate high registers backwards
								'Search RAM for free locations
								FreeStart = -1
								FreeSize = 0
								For SR = 16 To 31
									'Check for a continuous block of available RAM
									IF RegisterUsed(SR) Then
										FreeSize = 0
										FreeStart = -1
									Else
										FreeSize += 1
										If FreeStart = -1 Then FreeStart = SR
									End If
									
									If FreeSize = VarSize Then Exit For
								Next
								
								If FreeSize = VarSize Then
									AD = FreeStart
									For SR = FreeStart To FreeStart + FreeSize - 1
										RegisterUsed(SR) = -1
									Next
								Else
									LogError Message("OutOfRegSpace")
								End If
							End If
						End If
						
						'A location should now have been requested, give it to the variable
						If AD >= 0 Then
							'Allocate the individual bytes
							VarSize = GetTypeSize(.Type) - 1
							For CurrByte = 0 To VarSize
								RegCount += 1
								RegList(RegCount, 1) = GetByte(.Name, CurrByte)
								RegList(RegCount, 2) = Str(AD + CurrByte)
							Next
							.Location = AD
						End If
					
					'Variable is an alias
					ElseIf .Alias <> "" Then
						'Unused aliases don't get RAM
						SkipVar = -1
						For CD = 0 To SBC
							If Subroutine(CD)->Required Then
								CurrLine = Subroutine(CD)->CodeStart->Next
								Do While CurrLine <> 0
									If WholeINSTR(CurrLine->Value, .Name) = 2 Then SkipVar = 0: Exit For
									CurrLine = CurrLine->Next
								Loop
							End If
						Next
						'Print .Name, SkipVar
						If Not SkipVar Then
							
							'Get SFR(s) being referred to
							TempData = .Alias
							ALC = 0
							Do While INSTR(TempData, ",") <> 0
								ALC += 1: AliasList(ALC) = Trim(Left(TempData, INSTR(TempData, ",") - 1))
								TempData = Mid(TempData, INSTR(TempData, ",") + 1)
							Loop
							ALC += 1: AliasList(ALC) = Trim(TempData)
							
							'Create alias
							VarSize = GetTypeSize(.Type)
							If ALC < VarSize Then
								'If the alias is too small for the variable type, show error
								Temp = Message("BadAliasSize")
								Replace Temp, "%size%", Str(VarSize)
								Replace Temp, "%locations%", Str(ALC)
								LogError Temp, Origin
							Else
								'Define alias
								.Location = -1
								For CurrByte = 0 To VarSize - 1
									'Calculate location for alias
									.Location = CalcAliasLoc(AliasList(ALC - CurrByte))
									
									'Allocate
									If .Location <> -1 Then
										MakeSFR GetByte(.Name, CurrByte), .Location
										FinalAliasList(FALC + CurrByte + 1).Name = GetByte(.Name, CurrByte)
										'FinalAliasList(FALC + CurrByte + 1).Value = AliasList(ALC - CurrByte)
										FinalAliasList(FALC + CurrByte + 1).Value = Str(.Location)
									Else
										'Could not find location, try again
										UnallocatedVars = -1
									End If
								Next
								If .Location <> -1 Then FALC += VarSize
							End If
						End If
						
					'Pointers get no RAM at present
					ElseIf .Pointer = "POINTER" Then
						
					Else
						'Variable isn't alias, isn't register, so must be normal RAM variable or array
						
						'Array or normal variable?
						If .Size > 1 Then
							'Array
							'Calculate array size
							If .Type = "STRING" Then
								VarSize = .Size + 1
							Else
								VarSize = (.Size + 1) * GetTypeSize(.Type)
							End If
							
							'Decide which direction to search RAM for free location
							'PICs need to have arrays put at the end of RAM to reduce banking
							'AVRs need arrays at start, so as to leave end of RAM free for stack 
							If ModePIC Then
								SRStart = FreeRAM - VarSize - 1
								SREnd = 1
								SRDir = -1
							Else
								SRStart = 1
								SREnd = FreeRAM - VarSize - 1
								SRDir = 1
							End If
						Else
							'Scalar
							'Get variable size
							VarSize = GetTypeSize(.Type)
							'Set location search direction and start point
							SRStart = 1
							SREnd = FreeRAM - VarSize - 1
							SRDir = 1
						End If
						
						'Search RAM for suitable contiguous free locations
						For SR = SRStart To SREnd Step SRDir
							'Check for a continuous block of available RAM
							IF VarLoc(SR + VarSize - 1) - VarLoc(SR) = VarSize - 1 And (.FixedLocation = -1 Or .FixedLocation = VarLoc(SR)) Then
								'Allocate RAM to variable
								.Location = VarLoc(SR)
								If .Size > 1 Or LCase(.Type) = "string" Then
									'For an array, need to name first location only
									FVLC += 1
									FinalVarList(FVLC).Name = .Name
									FinalVarList(FVLC).Value = Str(VarLoc(SR))
									FinalVarList(FVLC).IsArray = -1
								Else
									'For a scalar, need to name every byte
									For CurrByte = 0 To VarSize - 1
										FVLC += 1
										FinalVarList(FVLC).Name = GetByte(.Name, CurrByte)
										FinalVarList(FVLC).Value = Str(VarLoc(SR + CurrByte))
									Next
								End If
															
								'Remove the now occupied RAM from the free list
								FreeRAM = FreeRAM - VarSize
								For CD = SR To FreeRAM
									VarLoc(CD) = VarLoc(CD + VarSize)
								Next
								StatsUsedRam += VarSize
								Exit For
							End If
						Next
						
						'Error for large variable that cannot be allocated RAM
						If .Location = -1 Then
							If .FixedLocation <> -1 Then
								'Fixed location was not free, allocate anyway but show warning
								.Location = .FixedLocation
								If .Size > 1 Then
									'For an array, need to name first location only
									FVLC += 1
									FinalVarList(FVLC).Name = .Name
									FinalVarList(FVLC).Value = Str(.FixedLocation)
									FinalVarList(FVLC).IsArray = -1
								Else
									'For a scalar, need to name every byte
									For CurrByte = 0 To VarSize - 1
										FVLC += 1
										FinalVarList(FVLC).Name = GetByte(.Name, CurrByte)
										FinalVarList(FVLC).Value = Str(.FixedLocation + CurrByte)
									Next
								End If
								TempData = Message("WarningFixedLocBad")
								Replace TempData, "%var%", .Name
								LogWarning TempData, Origin
								
							ElseIf .Size > 1 Then
								'Error for oversized array
								TempData = Message("ArrayTooBig")
								Replace TempData, "%array%", .Name
								LogError TempData, Origin
							Else
								'Error for too many variables
								LogError Message("ExcessVars")
							End If
						End If
						
					End If
					
				End If
			End With
			
		NextVarAdd:
		NEXT
	Loop
	
	'Display error here if UnallocatedVars = -1
	
	'Sort FinalVarList, remove duplicates
	PD = 0
	Do While PD < FVLC
		PD = PD + 1
		CD = PD
		Do While CD < FVLC
			CD = CD + 1
			If UCase(Trim(FinalVarList(CD).Name)) = UCase(Trim(FinalVarList(PD).Name)) Then
				'Print "Duplicate variable: " + FinalVarList(CD).Name
				For ED = CD TO FVLC
					FinalVarList(ED).Name = FinalVarList(ED + 1).Name
					FinalVarList(ED).Value = FinalVarList(ED + 1).Value
				Next
				FVLC -= 1
			End If
		Loop
	Loop
	
	'Create FinalRegList on AVR
	If ModeAVR Then
		FRLC = 0
		If RegCount > 0 Then
			For PD = 1 to RegCount
				FRLC += 1
				With FinalRegList(FRLC)
					 .Name = RegList(PD, 1)
					 .Value = "r" + RegList(PD, 2)
				End With
			Next
		End If
	End If

END SUB

Function CalcAliasLoc(LocationIn As String) As Integer
	'Calculate memory location required for an alias
	'Returns -1 if an error occurs and the location cannot be found
	
	'Can handle:
	' - SFR names
	' - Normal variable names
	' - literals
	' - calculations containing a mix of those
	
	Dim As String LineToken(100), OutTemp
	Dim As Integer FC, LineTokens, CurrToken
	
	'Print "Location: " + LocationIn
	
	'Split line into tokens
	GetTokens LocationIn, LineToken(), LineTokens, , -1
	
	'Process each token, looking for SFR or variable name
	OutTemp = ""
	For CurrToken = 1 To LineTokens
		If Not IsConst(LineToken(CurrToken)) Then
			'Check if token is a divider
			If IsDivider(LineToken(CurrToken)) Then GoTo AliasConstReplaced
			
			'Check if LocationIn is SFR
			FOR FC = 1 TO SVC
				IF SysVars(FC).Name = LineToken(CurrToken) Then
					LineToken(CurrToken) = Str(SysVars(FC).Location)
					GoTo AliasConstReplaced
				End If
			Next
			
			'Check if LocationIn is a variable
			For FC = 1 To FVLC
				If FinalVarList(FC).Name = LineToken(CurrToken) Then
					LineToken(CurrToken) = FinalVarList(FC).Value
					GoTo AliasConstReplaced
				End If
			Next
			
			'If we reach this point, there is a variable name still lurking
			Return -1
		End If	
		
		AliasConstReplaced:
		'Should have only numbers and dividers at this point
		
		OutTemp += LineToken(CurrToken)
	Next	
	
	'Calculate location, return
	'Print " = "; OutTemp;
	Calculate OutTemp
	'Print " = "; OutTemp
	Return Val(OutTemp)
End Function

Function GetWholeSFR(BitName As String) As String
	Dim As Integer FSFR
	
	For FSFR = 1 to SVBC
		If UCASE(SysVarBits(FSFR).Name) = UCASE(BitName) Then
			Return UCASE(SysVarBits(FSFR).Parent + "." + BitName)
		End If
	Next
	
	Return UCASE(BitName)
End Function

Function HasSFR(SFRName As String) As Integer
	Dim As Integer PD
	Dim As String TidiedName
	
	'Search system variable list to find register
	TidiedName = UCase(Trim(SFRName))
	For PD = 1 To SVC
		If UCase(Trim((SysVars(PD).Name))) = TidiedName Then Return -1 
	Next
	
	Return 0
End Function

Function HasSFRBit(BitName As String) As Integer
	Dim As Integer FSFR
	
	For FSFR = 1 to SVBC
		If UCASE(SysVarBits(FSFR).Name) = UCASE(BitName) Then
			Return -1
		End If
	Next
	
	Return 0
End Function

Sub MakeSFR (UserVar As String, SFRAddress As Integer)
	
	'Make UserVar an SFR, assign location
	SVC += 1
	SysVars(SVC).Name = UserVar
	SysVars(SVC).Location = SFRAddress
	
End Sub
