'	GCBASIC - A BASIC Compiler for microcontrollers
'	 Built-in assembler
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

Sub AsmOptimiser (CompSub As SubType Pointer)
	
	Dim As String Temp, BranchLine, NewBranch, OldBranch, ClearVar, OtherParam, VarName, RegName
	Dim As String EndIfLabel
	Dim As Integer LL, TRISSupport, LocValid, SL, ScanDist, SingleSkip
	Dim As LinkedListElement Pointer CurrLine, LastLine, CheckLine, DelEndIfLoc
	
	'Optimise asm code, and make final adjustments
	
	If ModePIC Then
		'Variable clearing ops
		CurrLine = CompSub->CodeStart->Next
		If CurrLine <> 0 Then
			Do While CurrLine->Next <> 0
				
				IF CurrLine->Value = " movlw 0" AND Left(CurrLine->Next->Value, 7) = " movwf " THEN
					ClearVar = Mid(CurrLine->Next->Value, 7)
					CurrLine->Value = " clrf" + ClearVar
					LinkedListDelete(CurrLine->Next)
				END If
				CurrLine = CurrLine->Next
			Loop
		End If
		
		'Delete all "bankisel" commands on PICs with <= 256 addresses
		If MemSize <= 256 Then
			CurrLine = CompSub->CodeStart->Next
			Do While CurrLine <> 0
				
				IF Left(CurrLine->Value, 10) = " bankisel " THEN
					CurrLine = LinkedListDelete(CurrLine)
				END If
				CurrLine = CurrLine->Next
			Loop
		End If
		
		'Don't copy something back into W if it's just been copied out
		CurrLine = CompSub->CodeStart->Next
		Do While CurrLine <> 0
			If Left(CurrLine->Value, 6) = " movf " And Right(CurrLine->Value, 1) = "W" Then
				Temp = Trim(Mid(CurrLine->Value, 6))
				Temp = LCase(Left(Temp, INSTR(Temp, ",") - 1))
				LastLine = CurrLine->Prev
				Do While (Left(LastLine->Value, 1) = ";" Or Left(LastLine->Value, 8) = "PRESERVE") AND LastLine->Prev <> 0: LastLine = LastLine->Prev: Loop
				If LCase(LastLine->Value) = " movwf " + Temp Then
					'Print "Deleting line reloading "; Temp
					CurrLine = LinkedListDelete(CurrLine)
				End If
			End If
			CurrLine = CurrLine->Next
		Loop
		
		'return > retlw 0(12 bit core)
		IF ChipFamily = 12 THEN
			CurrLine = CompSub->CodeStart->Next
			Do While CurrLine <> 0
				IF INSTR(LCase(CurrLine->Value), "return") <> 0 THEN CurrLine->Value = " retlw 0"
				CurrLine = CurrLine->Next
			Loop
		END IF
		
		'Replace 16F mnemonics with 18F ones
		IF ChipFamily = 16 Then
			CurrLine = CompSub->CodeStart->Next
			Do While CurrLine <> 0
				IF LCase(Trim(CurrLine->Value)) = "clrw" THEN CurrLine->Value = " clrf WREG"
				IF INSTR(LCase(CurrLine->Value), " rlf ") <> 0 THEN Replace CurrLine->Value, " rlf ", " rlcf "
				IF INSTR(LCase(CurrLine->Value), " rrf ") <> 0 THEN Replace CurrLine->Value, " rrf ", " rrcf "
				CurrLine = CurrLine->Next
			Loop
		END IF
		
	End If
	
	If ModeAVR Then
		'Conditional branches
		CurrLine = CompSub->CodeStart->Next
		Do While CurrLine <> 0
			
			If Left(CurrLine->Value, 6) = " rjmp " And Left(CurrLine->Prev->Value, 3) = " br" Then
				
				SingleSkip = 0
				DelEndIfLoc = 0
				If Right(CurrLine->Prev->Value, 6) = "PC + 2" Then
					SingleSkip = -1
				ElseIf InStr(CurrLine->Prev->Value, "ENDIF") <> 0 Then
					If CurrLine->Next <> 0 Then
						If Left(CurrLine->Next->Value, 5) = "ENDIF" Then
							EndIfLabel = Left(CurrLine->Next->Value, Len(CurrLine->Next->Value) - 1)
							If Right(CurrLine->Prev->Value, Len(EndIfLabel) + 1) = " " + EndIfLabel Then
								SingleSkip = -1
								DelEndIfLoc = CurrLine->Next
							End If
						End If
					End If
				End If
				
				If SingleSkip Then
					'Get old branch condition, rjmp destination
					OldBranch = Mid(CurrLine->Prev->Value, 2, 4)
					BranchLine = Trim(Mid(CurrLine->Value, 6))
					
					'Check that destination is with approx 63 words of origin
					LocValid = 0
					'Check last 63 instructions before current
					ScanDist = 63
					CheckLine = CurrLine->Prev
					Do While ScanDist > 0 And CheckLine <> 0
						IF CheckLine->Value = BranchLine + ":" Then
							LocValid = -1
							Exit Do
						End If
						ScanDist -= CalcLineSize(CheckLine->Value, 1)
						CheckLine = CheckLine->Prev
					Loop
					If LocValid = 0 Then
						'Check next 63 instructions after current
						ScanDist = 63
						CheckLine = CurrLine->Next
						Do While ScanDist > 0 And CheckLine <> 0
							IF CheckLine->Value = BranchLine + ":" Then
								LocValid = -1
								Exit Do
							End If
							ScanDist -= CalcLineSize(CheckLine->Value, 1)
							CheckLine = CheckLine->Next
						Loop
					End If
					
					If LocValid Then
						NewBranch = ""
						OtherParam = " "
						Select Case OldBranch
							Case "breq": NewBranch = "brne"
							Case "brne": NewBranch = "breq"
							Case "brlo": NewBranch = "brsh"
							Case "brsh": NewBranch = "brlo"
							Case "brlt": NewBranch = "brge"
							Case "brge": NewBranch = "brlt"
							Case "brbs"
								NewBranch = "brbc"
								OtherParam = " " + Mid(CurrLine->Prev->Value, 7, 1) + ","
							Case "brbc"
								NewBranch = "brbs"
								OtherParam = " " + Mid(CurrLine->Prev->Value, 7, 1) + ","
						End Select
						
						CurrLine = LinkedListDelete(CurrLine)
						CurrLine->Value = " " + NewBranch + OtherParam + BranchLine
						If DelEndIfLoc <> 0 Then
							LinkedListDelete(DelEndIfLoc)
						End If
					End If
				End If
			End If
			
			'Change ldi/mov to plain ldi where possible (AVR)
			Dim As String SourceVal, TempVar, DestVar
			If Left(CurrLine->Value, 5) = " mov " And Left(CurrLine->Prev->Value, 5) = " ldi " Then
				TempVar = Trim(Mid(CurrLine->Prev->Value, 5))
				TempVar = Left(TempVar, InStr(TempVar, ",") - 1)
				If Right(CurrLine->Value, Len(TempVar)) = TempVar Then
					SourceVal = CurrLine->Prev->Value
					SourceVal = Trim(Mid(SourceVal, InStr(SourceVal, ",") + 1))
					DestVar = Mid(CurrLine->Value, 5)
					DestVar = Trim(Left(DestVar, InStr(DestVar, ",") - 1))
					
					If Not IsLowRegister(DestVar) Then
						CurrLine = LinkedListDelete(CurrLine)
						CurrLine->Value = " ldi " + DestVar + "," + SourceVal
					End If
				End If
			End If
			
			CurrLine = CurrLine->Next
		Loop
		
		'Don't copy something back into register if it's just been copied out
		CurrLine = CompSub->CodeStart->Next
		Do While CurrLine <> 0
			If Left(CurrLine->Value, 5) = " lds " Then
				Temp = Mid(CurrLine->Value, 5)
				RegName = Trim(LCase(Left(Temp, INSTR(Temp, ",") - 1)))
				VarName = Trim(LCase(Mid(Temp, InStr(Temp, ",") + 1)))
				LastLine = CurrLine->Prev
				Do While (Left(LastLine->Value, 1) = ";" Or Left(LastLine->Value, 8) = "PRESERVE") AND LastLine->Prev <> 0: LastLine = LastLine->Prev: Loop
				If LCase(LastLine->Value) = " sts " + VarName + "," + RegName Then
					'Print "Deleting line reloading "; VarName; " into "; RegName
					CurrLine = LinkedListDelete(CurrLine)
				End If
			End If
			CurrLine = CurrLine->Next
		Loop
		
	End If
	
	'Check for subs that have a call as the last instruction
	'Turn call into a goto
	'If call is last instruction and always runs, no need for a return at the end either
	'Get last line of sub
	CompSub->HasFinalGoto = 0
	CompSub->FinalGotoDest = ""
	CurrLine = CompSub->CodeStart
	Do While CurrLine->Next <> 0
		CurrLine = CurrLine->Next
	Loop
	LastLine = CurrLine
	'Get line before last
	CheckLine = LastLine->Prev
	If CheckLine <> 0 Then
		Do While (Left(CheckLine->Value, 1) = ";" Or Left(CheckLine->Value, 8) = "PRESERVE") AND CheckLine->Prev <> 0
			CheckLine = CheckLine->Prev
		Loop
	End If
	
	If ModePIC Then
		If Left(LastLine->Value, 6) = " call " Then
			'Record presence of goto and destination for page selection later
			CompSub->HasFinalGoto = -1
			CompSub->FinalGotoDest = Trim(Mid(LastLine->Value, 6))
			LastLine->Value = " goto " + CompSub->FinalGotoDest 
			If Left(CheckLine->Value, 5) <> " btfs" Then
				CompSub->NoReturn = -1
			End If
		End If
	Else
		If Left(LastLine->Value, 7) = " rcall " Then
			CompSub->HasFinalGoto = -1
			'No need to record goto destination for page selection
			LastLine->Value = " rjmp " + Trim(Mid(LastLine->Value, 7))
			If CheckLine = 0 Then
				CompSub->NoReturn = -1
			ElseIf Left(CheckLine->Value, 3) <> " sb" And Left(CheckLine->Value, 3) <> " br" Then
				CompSub->NoReturn = -1
			End If
		
		ElseIf Left(LastLine->Value, 6) = " call " Then
			CompSub->HasFinalGoto = -1
			LastLine->Value = " jmp " + Trim(Mid(LastLine->Value, 6))
			If CheckLine = 0 Then
				CompSub->NoReturn = -1
			ElseIf Left(CheckLine->Value, 3) <> " sb" And Left(CheckLine->Value, 3) <> " br" Then
				CompSub->NoReturn = -1
			End If
		End If
	End If 
	
End Sub

Function AsmTidy (DataSource As String) As String
	
	Dim As String Temp, DSTemp
	Dim As Integer T, FS, AsmSize
	
	Temp = DataSource
	
	'Replace CLS sub and calls to it sith lbCLS on AVR
	If ModeAVR And INSTR(UCase(Temp), "CLS") <> 0 Then
		If Left(Temp, 4) <> " cls" Then WholeReplace Temp, "CLS", "lbCLS"
	End If
	
	'Remove @ if not inside data statement
	If Left(Temp, 4) <> " dw " Then
		Do While INSTR(Temp, "@") <> 0: Replace Temp, "@", "": Loop
	End If
	
	'Replace PRESERVE with value
	IF Left(Temp, 9) = "PRESERVE " THEN
		T = VAL(Mid(Temp, 10))
		Temp = ";" + PreserveCode(T)
		If Mid(Temp, 2, 1) = Chr(8) Then Temp = Mid(Temp, 3)
	END IF
	
	'Do not process comments
	IF Left(Temp, 1) = ";" AND Left(LCase(Temp), 5) <> ";sub'" THEN AsmTidy = Temp: EXIT FUNCTION
	
	'Store asm program for GCASM
	IF UCase(ASMExe) = "GCASM" THEN
		IF Trim(Temp) <> "" THEN 
			DSTemp = Temp
			DO WHILE INSTR(DSTemp, Chr(9)) <> 0: Replace DSTemp, Chr(9), " ": LOOP
			IF INSTR(DSTemp, ";") <> 0 THEN DSTemp = Left(DSTemp, INSTR(DSTemp, ";") - 1)
			DSTemp = Trim(DSTemp)
			IF Left(UCase(DSTemp), 3) <> "DT " AND Left(UCase(DSTemp), 3) <> "DW " THEN DSTemp = UCase(DSTemp)
			IF Left(UCase(DSTemp), 3) = "DT " OR Left(UCase(DSTemp), 3) = "DW " THEN DSTemp = UCase(Left(DSTemp, 3)) + Mid(DSTemp, 4)
			If Left(UCase(DSTemp), 4) = ".DB " Then DSTemp = Mid(DSTemp, 2)
			If Left(UCase(DSTemp), 5) = ".ORG " Then DSTemp = Mid(DSTemp, 2)
			If ModePIC Or (Left(DSTemp, 1) <> "." And Left(DSTemp, 1) <> "#") Then ASPC += 1: ASMProg(ASPC) = DSTemp
		Else
			ASPC += 1: ASMProg(ASPC) = ""
		END IF
	END IF

	'Format command, output result
	IF INSTR(Temp, " ") <> 0 THEN Replace Temp, " ", Chr(9)
	IF INSTR(Temp, " ") <> 0 THEN Replace Temp, " ", Chr(9)
	AsmTidy = Temp

END FUNCTION

SUB AssembleProgram
	'GCASM Assembler
	
	'Variables
	Dim As String Temp, NewData, DataSource, NDTemp, ROMData, Reg, NameTemp
	Dim As String DSTemp, HRTemp, NewRecordData, Address, RecType, RecCheck, ValueTemp
	Dim As String RecSize, HexAddress, HexContents, Byte1, Byte2, HTemp, NewCmd, Cmd
	Dim As String CurrentParam, ParamStart, ParamName, ParamValue, CurrentBit, ConfName
	Dim As String CTO, PSO
	
	Dim As Integer CD, PD, IsSFR, SFL, SP1, CSB, CurrentLocation, T, DT, DataSize
	Dim As Integer RP1, DWC, RSC, DWIC, SS, CS, RepeatBanksel, B, FB, RepeatPagesel
	Dim As Integer PRC, KeepReplacing, AW, PB, FP, PVT, TBT, HT, PV
	Dim As Integer FCO, COI, CCI, HRC, OIR, RCC, CHA, CL, DataBlockSize
	Dim As Integer CurrCmd
	
	Dim As Single CurrPerc, PercAdd, PercOld
	
	Dim As String ParamValues(100)
	Dim As Integer ParamWasLabel(100)
	Dim As Integer PValueCount, CurrentLine
	
	Dim As String Prog(20000)
	
	'Clear compiled program
	APC = 0
	
	'Check for RP1 bit
	If ModePIC Then
		'Should banksel set 1 or 2 STATUS bits?
		RP1 = HasSFRBit("RP1")
		If MemSize <= 256 Then RP1 = 0 'If 256 or less addresses, there is no RP1
	End If
	
	'Build symbol table
	If VBS = 1 Then Print SPC(5); Message("SymbolTable")
	BuildAsmSymbolTable
	
	'Check for duplicate symbols
	FOR PD = 1 to ASMSym
		FOR CD = PD TO ASMSym
			IF ASMSymbols(PD, 1) = ASMSymbols(CD, 1) AND CD <> PD THEN
				IF ASMSymbols(PD, 2) <> ASMSymbols(CD, 2) THEN
					Temp = Message("DupDef")
					Replace Temp, "%var%", ASMSymbols(PD, 1)
					LogError "GCASM: " + Temp
					ASMSymbols(PD, 1) = ""
					GOTO CheckNextSymbol
				End IF
			END IF
		NEXT
		CheckNextSymbol:
	NEXT
	
	'Convert asm > binary > hex
	IF VBS = 1 Then Print SPC(5); Message("MachineCode");
	PercOld = 0
	CurrPerc = 0.5
	PercAdd = 1 / (ASPC) * 100
	
	'Open list file
	Open ReplaceToolVariables("%filename%", "lst") For Output As #1
	Print #1, "GCASM List File (GCBASIC " + Version + ")"
	Print #1, ""
	Print #1, "Symbols:"
	For PD = 1 To ASMSym
		Print #1, ASMSymbols(PD, 1) + " " + Chr(9) + "EQU " + Chr(9) + AsmSymbols(PD, 2)
	Next
	Print #1, ""
	Print #1, "Code:"
	Print #1, "Loc" + Chr(9) + "Obj Code" + Chr(9) + "Original Assembly"
	
	'Having saved to list file, sort symbol table by length
	Dim As Integer StartPos, SearchPos, FirstPos
	For StartPos = 1 To AsmSym
		'Find first symbol name in unsorted part of list
		FirstPos = StartPos
		For SearchPos = StartPos + 1 To AsmSym
			If Len(AsmSymbols(SearchPos, 1)) > Len(AsmSymbols(FirstPos, 1)) Then
				FirstPos = SearchPos	
			End If
		Next
		'Move first unsorted item into first place of unsorted list
		'(Swap with first unsorted location)
		Swap AsmSymbols(StartPos, 1), AsmSymbols(FirstPos, 1)
		Swap AsmSymbols(StartPos, 2), AsmSymbols(FirstPos, 2)
	Next
	
	'Convert line at a time
	CurrentLine = 0
	FOR PD = 1 TO ASPC
		
		'Show percentage assembled
		IF VBS = 1 THEN
			CurrPerc += PercAdd
			If Int(CurrPerc) > Int(PercOld) Then
				PercOld = CurrPerc
				LOCATE , 60
				Print Int(CurrPerc); "%";
			End If
		End If
		
		'Get the line
		DataSource = ASMProg(PD)
		RepeatBanksel = 0
		IF INSTR(DataSource, ":") <> 0 THEN
			CurrentLine = Val(Left(DataSource, INSTR(DataSource, ":") - 1))
			DataSource = Mid(DataSource, INSTR(DataSource, ":") + 1)
		END IF
		IF DataSource <> "" THEN
			'Get the index of the assembly command
			CurrCmd = IsASM(DataSource)
			
			'Get current line for list file
			Dim As String DebugInput, DebugOutput, DebugLoc
			DebugInput = Mid(AsmProg(PD), InStr(AsmProg(PD), ":") + 1)
			DebugOutput = ""
			
			'Special Cases
			If CurrCmd = 0 Then
				IF Left(DataSource, 8) = "BANKSEL " THEN
					B = 0
					Temp = UCase(Trim(Mid(DataSource, 8)))
					FOR FB = 1 TO ASMSym
						IF Temp = ASMSymbols(FB, 1) THEN B = VAL(ASMSymbols(FB, 2)): Exit For
					NEXT
					IF ChipFamily = 12 Or ChipFamily = 14 THEN
						B = (B AND 384) / 128
						RepeatBanksel = 0
						AddMoreBanksel:
						RepeatBanksel = RepeatBanksel + 1
						If RepeatBanksel = 2 THEN
							CurrentLine = CurrentLine + 1
							DataSource = "BCF STATUS,RP1"
							IF (B AND 2) > 0 THEN DataSource = "BSF STATUS,RP1"
							ASMProg(PD) = DataSource
							CurrCmd = IsASM(DataSource)
						END IF
						If RepeatBanksel = 1 THEN
							IF RP1 = 0 THEN RepeatBanksel = 2
							DataSource = "BCF STATUS,RP0"
							IF (B AND 1) > 0 THEN DataSource = "BSF STATUS,RP0"
							ASMProg(PD) = DataSource
							CurrCmd = IsASM(DataSource)
						END IF
					
					ElseIf ChipFamily = 15 Then
						'Print DataSource + ": Location " + Hex(B) + " Bank ";
						B = (B AND 3968) / 128
						'Print B
						DataSource = "MOVLB " + Trim(Str(B))
						ASMProg(PD) = DataSource
						CurrCmd = IsASM(DataSource)
						
					ElseIf ChipFamily = 16 THEN
						B = (B AND 65280) / 256
						DataSource = "MOVLB " + Str(B)
						ASMProg(PD) = DataSource
						CurrCmd = IsASM(DataSource)
					END IF
					
				ElseIF Left(DataSource, 8) = "PAGESEL " THEN
					B = 0
					Temp = Trim(Mid(DataSource, 8))
					IF Temp = "$" THEN B = CurrentLine
					IF B = 0 THEN
						FOR FB = 1 TO ASMSym
							IF Temp = ASMSymbols(FB, 1) THEN B = VAL(ASMSymbols(FB, 2)): Exit FOR
						NEXT
					END IF
					
					If ChipFamily <> 15 Then
						B = (B AND 6144) / 2048
						RepeatPagesel = 0
						AddMorePagesel:
						RepeatPagesel = RepeatPagesel + 1
						If RepeatPagesel = 2 THEN
							CurrentLine = CurrentLine + 1
							DataSource = "BCF PCLATH,4"
							IF (B AND 2) > 0 THEN DataSource = "BSF PCLATH,4"
							ASMProg(PD) = DataSource
							CurrCmd = IsASM(DataSource)
						END IF
						If RepeatPagesel = 1 THEN
							If PCUpper = 1 THEN RepeatPagesel = 2
							DataSource = "BCF PCLATH,3"
							IF (B AND 1) > 0 THEN DataSource = "BSF PCLATH,3"
							ASMProg(PD) = DataSource
							CurrCmd = IsASM(DataSource)
						END If
					
					Else
						B = (B And 65280) / 256
						DataSource = "MOVLP " + Str(B)
						ASMProg(PD) = DataSource
						CurrCmd = IsASM(DataSource)
						
					End If
					
				ElseIF Left(DataSource, 9) = "BANKISEL " THEN
					B = 0
					Temp = Trim(Mid(DataSource, 9))
					IF Temp = "$" THEN B = CurrentLine
					IF B = 0 THEN
						FOR FB = 1 TO ASMSym
							IF Temp = ASMSymbols(FB, 1) THEN B = VAL(ASMSymbols(FB, 2)): Exit FOR
						NEXT
					END IF
					
					IF B < 256 Then DataSource = " bcf STATUS,IRP"
					IF B > 255 Then DataSource = " bsf STATUS,IRP"
					CurrCmd = IsASM(DataSource)
					
				'Raw hex code
				ElseIF Left(DataSource, 4) = "RAW " THEN
					DataSource = Trim(Mid(DataSource, 4))
					
					DebugLoc = Hex(CurrentLine)
					DebugOutput = ""
					Do While Len(DebugLoc) < 6
						DebugLoc = "0" + DebugLoc
					Loop 
					
					DO WHILE INSTR(DataSource, ",") <> 0 
						
						APC = APC + 1: Prog(APC) = Str(CurrentLine) + ":" + Left(DataSource, INSTR(DataSource, ",") - 1)
						DebugOutput += (Left(DataSource, INSTR(DataSource, ",") - 1) + " ")
						If ChipFamily = 16 Then
							CurrentLine = CurrentLine + 2
						Else
							CurrentLine = CurrentLine + 1
						End If
						DataSource = Mid(DataSource, INSTR(DataSource, ",") + 1)
					LOOP
					APC = APC + 1: Prog(APC) = Str(CurrentLine) + ":" + DataSource
					
					DebugOutput += DataSource
					
					Print #1, DebugLoc + Chr(9) + Trim(DebugOutput) + Chr(9) + DebugInput
					
					CurrCmd = 0
					
				'Label
				Else
					Print #1, DebugLoc + Chr(9) + Trim(DebugOutput) + Chr(9) + DebugInput
					
				END IF
			End If
			
			'Standard assembly command
			IF CurrCmd <> 0 THEN
				
				'If AVR, convert CLR r to EOR r,r and TST r to AND r,r
				If ModeAVR Then
					If Left(DataSource, 4) = "CLR " Then
						Reg = Trim(Mid(DataSource, 5))
						DataSource = "EOR " + Reg + "," + Reg
						CurrCmd = IsASM(DataSource)
						
					ElseIf Left(DataSource, 4) = "TST " Then
						Reg = Trim(Mid(DataSource, 5))
						DataSource = "AND " + Reg + "," + Reg
						CurrCmd = IsASM(DataSource)
						
					ElseIf Left(DataSource, 4) = "LSL " Then
						Reg = Trim(Mid(DataSource, 5))
						DataSource = "ADD " + Reg + "," + Reg
						CurrCmd = IsASM(DataSource)
						
					ElseIf Left(DataSource, 4) = "ROL " Then
						Reg = Trim(Mid(DataSource, 5))
						DataSource = "ADC " + Reg + "," + Reg
						CurrCmd = IsASM(DataSource)
						
					End If
				End If
				
				'Get command
				ParamValue = Trim(DataSource)
				IF INSTR(ParamValue, " ") = 0 Then
					ParamValue = ""
				Else
					ParamValue = Trim(Mid(ParamValue, INSTR(ParamValue, " ") + 1))
				End If
				
				'Get parameters
				GetTokens (ParamValue, ParamValues(), PValueCount, ",")
				PRC = ASMCommands(CurrCmd).Params
				If ASMCommands(CurrCmd).Params > PValueCount Then
					For CD = PValueCount + 1 To ASMCommands(CurrCmd).Params
						ParamValues(CD) = "0"
					Next
				End If
				'TD = Name, CT = Value
				
				'Replace symbols in parameters
				FOR CD = 1 TO PRC
					ParamWasLabel(CD) = 0
					CTO = ParamValues(CD)
					'Replace current address
					If ModePIC Then
						Do While InStr(ParamValues(CD), "$") <> 0: Replace ParamValues(CD), "$", Str(CurrentLine): Loop
					ElseIf ModeAVR Then
						WholeReplace ParamValues(CD), "PC", Str(CurrentLine)
					End If
					'Replace symbols
					Do
						KeepReplacing = 0
						FOR FB = 1 TO ASMSym
							Do While InStr(UCase(ParamValues(CD)), UCase(Trim(ASMSymbols(FB, 1)))) <> 0
								If WholeINSTR(ParamValues(CD), ASMSymbols(FB, 1)) <> 2 Then
									Replace ParamValues(CD), ASMSymbols(FB, 1), Chr(27)
								Else
									If IsConst(ASMSymbols(FB, 2)) Then
										WholeReplace ParamValues(CD), ASMSymbols(FB, 1), Str(MakeDec(ASMSymbols(FB, 2)))
										ParamWasLabel(CD) = -1
									Else
										WholeReplace ParamValues(CD), ASMSymbols(FB, 1), ASMSymbols(FB, 2)
										KeepReplacing = -1
										ParamWasLabel(CD) = -1
									End If
								End If
							Loop
							Do While INSTR(ParamValues(CD), Chr(27)) <> 0: Replace ParamValues(CD), Chr(27), ASMSymbols(FB, 1): Loop
							
						Next
					Loop While KeepReplacing
					
					'Some AVR only operations
					If ModeAVR Then
						'Correct address for br* and rjmp/rcall instructions
						If UCase(Left(DataSource, 2)) = "BR" Or UCase(Left(DataSource, 4)) = "RJMP" OR UCase(Left(DataSource, 5)) = "RCALL" Then
							If LCase(AsmCommands(CurrCmd).Param(CD)) = "k" Then
								'Calculate value
								If IsCalc(ParamValues(CD)) Then Calculate ParamValues(CD)
								ParamValues(CD) = Trim(ParamValues(CD))
								ParamValues(CD) = Str(MakeDec(ParamValues(CD)) - CurrentLine - 1)
							End If
						End If
						'Replace register names
						ParamValues(CD) = Trim(ParamValues(CD))
						If LCASE(LEFT(ParamValues(CD), 1)) = "r" And IsConst(MID(ParamValues(CD), 2)) Then
							ParamValues(CD) = MID(ParamValues(CD), 2)
						End If
						
					'PIC only operations
					ElseIf ModePIC Then
						If ChipFamily = 16 Then
							'Fix branches
							If UCase(Left(DataSource, 2)) = "BN" Or UCase(Left(DataSource, 3)) = "BRA" Or UCase(Left(DataSource, 6)) = "RCALL " Then
								ParamValues(CD) = Str((MakeDec(ParamValues(CD)) - CurrentLine) / 2 - 1)
							End If
						End If
					End If
					
					'Calculate value
					If IsCalc(ParamValues(CD)) Then Calculate ParamValues(CD)
					ParamValues(CD) = Trim(ParamValues(CD))
				NEXT
				
				'Get command binary, and add in parameters
				Cmd = ASMCommands(CurrCmd).Word(1)
				'IF VAL(ASMCommanDataSource(T, 2)) = 2 THEN Cmd = Cmd + ":" + ASMCommanDataSource(T, 4)
				If ChipFamily = 16 And Left(UCase(DataSource), 5) <> "LFSR " THEN
					 With ASMCommands(CurrCmd)
						For AW = 2 To .Words
							Cmd = .Word(AW) + ":" + Cmd
						Next
					End With
				END IF
				If ChipFamily <> 16 Or Left(UCase(DataSource), 5) = "LFSR " THEN
					With ASMCommands(CurrCmd)
						For AW = 2 To .Words
							Cmd = Cmd + ":" + .Word(AW)
						Next
					End With
				END IF
				
				FOR CD = 1 TO PRC
					
					'Count the number of bits available to store the parameter
					PB = 0
					CurrentParam = ASMCommands(CurrCmd).Param(CD)
					FOR FP = 1 TO LEN(Cmd)
						IF Mid(Cmd, FP, 1) = CurrentParam THEN
							If FP > 1 And Cmd = "'" Then Exit For
							PB = PB + 1
						End If
						NEXT
					
					'Handle negative parameter
					If LEFT(ParamValues(CD), 1) = "-" Then
						ParamValues(CD) = STR(VAL(ParamValues(CD)) And (2 ^ PB - 1))
					End If
					
					'Check parameter is not an undefined symbol
					If NOT IsConst(ParamValues(CD)) AND IsASM(ParamValues(CD)) = 0 THEN
						Temp = Message("SymbolNotDefined")
						Replace Temp, "%symbol%", ParamValues(CD)
						'Print Temp
						LogError "GCASM: " + Temp
					END If
					
					'Get data
					ParamStart = ""
					T = 1
					PVT = MakeDec(ParamValues(CD))
					
					'Halve address for call, goto on 18F
					If ChipFamily = 16 AND (Left(UCase(DataSource), 5) = "CALL " OR Left(UCase(DataSource), 5) = "GOTO ") AND CurrentParam = "K" Then
						PVT = PVT / 2
					End If
					
					'Convert parameter from decimal to binary
					DO WHILE PVT > 0
						TBT = PVT AND T
						IF TBT = 0 THEN ParamStart = "0" + ParamStart
						IF TBT <> 0 THEN ParamStart = "1" + ParamStart
						PVT = PVT - TBT
						T = T * 2
					LOOP
					'Adjust length
					DO WHILE LEN(ParamStart) < PB
						ParamStart = "0" + ParamStart
					LOOP
					'PRINT ParamStart, CurrentParam, DataSource, Cmd
					IF LEN(ParamStart) > PB AND CurrentParam = "K" AND INSTR(UCase(DataSource), "GOTO") = 0 AND INSTR(UCase(DataSource), "CALL") = 0 THEN						
						'PRINT ParamStart, CurrentParam, DataSource, Cmd
						LogWarning "GCASM: " + Message("ValueOutOfRange")
						Print #1, Message("ValueOutOfRange")
						ParamStart = Right(ParamStart, PB)
					END IF
					ParamStart = Right(ParamStart, PB)
					
					'Add in binary parameter to binary command
					PSO = ParamStart 'Save original parameter, may be needed
					FP = 0
					'Print #1, Chr(9) + CurrentParam + Chr(9) + Cmd + Chr(9) + ParamValues(CD)
					Do While FP <= LEN(Cmd)
						FP += 1
						IF Mid(Cmd, FP, 1) = CurrentParam THEN
							CurrentBit = "": IF FP > 1 THEN CurrentBit = Mid(Cmd, FP - 1, 1)
							'Inverted parameter bit
							If CurrentBit = "_" Then
								Temp = Left(ParamStart, 1)
								If Temp = "1" Then Temp = "0" Else Temp = "1"
								Cmd = Left(Cmd, FP - 2) + Temp + Mid(Cmd, FP + 1)
								FP -= 1
								
							'Start new copy of parameter
							ElseIf CurrentBit = "'" Then
								ParamStart = PSO
								Mid(Cmd, FP) = Left(ParamStart, 1)
								
							'Normal parameter bit
							Else
								Mid(Cmd, FP) = Left(ParamStart, 1)
								
							End If
							ParamStart = Mid(ParamStart, 2)
						END IF
					'NEXT
					Loop
					'Print #1, Chr(9) + Chr(9) + Cmd
				NEXT
				
				'Convert binary > hex
				DO WHILE INSTR(Cmd, " ") <> 0: Replace Cmd, " ", "": LOOP
				DO WHILE INSTR(Cmd, ":") <> 0
					IF ChipFamily <> 16 Or Left(UCase(DataSource), 5) = "LFSR " THEN
						NewCmd = Mid(Cmd, INSTR(Cmd, ":") + 1)
						Cmd = Left(Cmd, INSTR(Cmd, ":") - 1)
					END IF
					IF ChipFamily = 16 And Left(UCase(DataSource), 5) <> "LFSR " THEN
						NewCmd = Left(Cmd, INSTR(Cmd, ":") - 1)
						Cmd = Mid(Cmd, INSTR(Cmd, ":") + 1)
					END IF
					
					HT = 0
					PV = 1
					FOR T = LEN(Cmd) TO 1 STEP -1
						HT = HT + VAL(Mid(Cmd, T, 1)) * PV
						PV = PV * 2
					NEXT
					HTemp = Hex(HT)
					DO WHILE LEN(HTemp) < 4
						HTemp = "0" + HTemp
					LOOP
					DebugOutput = DebugOutput + HTemp + " "
					HTemp = Str(CurrentLine) + ":" + HTemp
					APC = APC + 1: Prog(APC) = HTemp
					If ChipFamily = 16 Then
						CurrentLine += 2
					Else
						CurrentLine += 1	
					End If
					Cmd = NewCmd
				LOOP
				
				HT = 0
				PV = 1
				FOR T = LEN(Cmd) TO 1 STEP -1
					HT = HT + VAL(Mid(Cmd, T, 1)) * PV
					PV = PV * 2
				NEXT
				HTemp = Hex(HT)
				DO WHILE LEN(HTemp) < 4
					HTemp = "0" + HTemp
				Loop
				DebugOutput = DebugOutput + HTemp + " "
				DebugLoc = Hex(Val(AsmProg(PD)))
				Do While Len(DebugLoc) < 6
					DebugLoc = "0" + DebugLoc
				Loop 
				Print #1, DebugLoc + Chr(9) + Trim(DebugOutput) + Chr(9) + DebugInput
				HTemp = Str(CurrentLine) + ":" + HTemp
				APC = APC + 1: Prog(APC) = HTemp
				
				'PRINT DataSource, Cmd, HTemp
				
				If RepeatBanksel = 1 THEN GOTO AddMoreBanksel
				If RepeatPagesel = 1 THEN GOTO AddMorePagesel
				
			END IF
		
		'Blank line
		Else
			Print #1, ""
		END If
		
	NEXT
	PRINT
	
	'Close list file
	Close #1
	
	'Add config
	Dim As Integer CurrConfWord, ConfWord(ConfWords)
	If ModePIC Then 'No config needed for AVR
		If ChipFamily <> 16 THEN
			
			For CurrConfWord = 1 To ConfWords
				'Get default word value
				If ChipFamily = 12 Then
					ConfWord(CurrConfWord) = 2 ^ 12 - 1
				Else
					ConfWord(CurrConfWord) = 2 ^ 14 - 1
				End If
				
				'Apply settings
				DataSource = OutConfig(CurrConfWord)
				DO WHILE INSTR(DataSource, "&") <> 0
					Temp = UCase(Trim(Left(DataSource, INSTR(DataSource, "&") - 1)))
					IF Left(Temp, 1) = "_" THEN Temp = Mid(Temp, 2)
					DataSource = Trim(Mid(DataSource, INSTR(DataSource, "&") + 1))
					FOR FCO = 1 TO COC
						With ConfigOps(FCO)
							IF Temp = UCase(.Op) THEN
								ConfWord(CurrConfWord) = ConfWord(CurrConfWord) AND .Val
								EXIT FOR
							END IF
						End With
					NEXT
				LOOP
				IF DataSource <> "" THEN
					Temp = UCase(Trim(DataSource))
					IF Left(Temp, 1) = "_" THEN Temp = Mid(Temp, 2)
					FOR FCO = 1 TO COC
						With ConfigOps(FCO)
							IF Temp = UCase(.Op) THEN
								ConfWord(CurrConfWord) = ConfWord(CurrConfWord) AND .Val
								EXIT FOR
							END IF
						End With
					NEXT
				END If
				
				'Store to hex file
				If ChipFamily = 12 Then
					APC = APC + 1: Prog(APC) = "&H0FFF:0" + HEX(ConfWord(CurrConfWord))
				ElseIf ChipFamily = 14 Then
					'APC = APC + 1: Prog(APC) = "&H2007:" + HEX(ConfWord(CurrConfWord))
					APC = APC + 1: Prog(APC) = "&H" + Hex(8198 + CurrConfWord) + ":" + HEX(ConfWord(CurrConfWord))
					'APC = APC + 1: Prog(APC) = "&H2008:" + HEX(ConfWord2)
				ElseIf ChipFamily = 15 Then
					APC = APC + 1: Prog(APC) = "&H" + Hex(32774 + CurrConfWord) + ":" + HEX(ConfWord(CurrConfWord))
				End If
			Next
			
		End IF
		
		If ChipFamily = 16 THEN
			
			'Use TempData to store config words
			Dim ConfWordCount As Integer
			ConfWordCount = ConfWords 'was 20
			
			'Initialise TempData()
			FOR CD = 1 to ConfWordCount
				TempData(CD) = Str(ConfigMask(CD))
			NEXT
			
			'Add default settings where needed
			FOR CD = 1 TO DCOC
				ConfName = Left(DefCONFIG(CD), INSTR(DefCONFIG(CD), "=") - 1)
				If WholeINSTR(OutConfig(1), ConfName) <> 2 THEN
					OutConfig(1) = OutConfig(1) + ", " + DefCONFIG(CD)
				END IF
			NEXT
			
			'Split up OutConfig(1), store in CheckTemp()
			DataSource = OutConfig(1)
			T = 0
			DO WHILE INSTR(DataSource, ",") <> 0
				T += 1
				CheckTemp(T) = Trim(Left(DataSource, INSTR(DataSource, ",") - 1))
				DataSource = Trim(Mid(DataSource, INSTR(DataSource, ",") + 1))
			LOOP
			IF DataSource <> "" THEN
				T += 1
				CheckTemp(T) = DataSource
			END IF
			
			'Process values
			FOR PD = 1 to T
				DataSource = CheckTemp(PD)
				COI = 0
				
				'Find exact match
				Temp = UCase(DataSource)
				DO WHILE INSTR(Temp, "=") <> 0: Replace Temp, "=", "_": LOOP
				FOR FCO = 1 TO COC
					IF Temp = UCase(ConfigOps(FCO).Op) THEN
						COI = FCO
						EXIT FOR
					END IF
				NEXT
				
				'If no exact match, find close match
				IF COI = 0 THEN
					ConfName = UCase(Trim(Left(DataSource, INSTR(DataSource, "=") - 1)))
					ValueTemp = UCase(Trim(Mid(DataSource, INSTR(DataSource, "=") + 1)))
					FOR FCO = 1 TO COC
						With ConfigOps(FCO)
							IF INSTR(UCase(.Op), ConfName) <> 0 AND INSTR(UCase(.Op), ValueTemp) <> 0 THEN
								COI = FCO
								EXIT FOR
							END IF
						End With
					NEXT
				END IF
				
				'If still nothing, then we have a problem
				IF COI = 0 THEN
					Temp = Message("BadConfig")
					Replace Temp, "%option%", DataSource
					LogError "GCASM: " + Temp
				End IF
				
				'Apply setting
				With ConfigOps(COI)
					TempData(.Loc) = Str(VAL(TempData(.Loc)) AND .Val)
				End With
				
			NEXT
			
			'Store settings
			CCI = 0
			Do
				CCI += 1
				Byte1 = Hex(VAL(TempData(CCI)))
				Do While LEN(Byte1) < 2: Byte1 = "0" + Byte1: LOOP
				IF Byte1 <> "FF" THEN
					CCI += 1
					Byte2 = Hex(VAL(TempData(CCI)))
					Do While LEN(Byte2) < 2: Byte2 = "0" + Byte2: LOOP
					APC = APC + 1: Prog(APC) = "&H" + HEX(3145726 + CCI) + ":" + Byte2 + Byte1
				END IF
				
			Loop While CCI < ConfWordCount
			
		END IF
	End If
	
	'Prepare hex records
	Dim As Single OA, FRA
	'Dim As Long OA, FRA
	HRC = 0
	OA = 0
	OIR = 0
	RCC = 0
	HRTemp = ""
	FRA = -1
	CHA = 0 'High address word
	FOR PD = 1 TO APC
		
		CL = VAL(Left(Prog(PD), INSTR(Prog(PD), ":") - 1))
		IF ChipFamily <> 16 THEN CL = CL * 2
		If PD = 1 Then OA = CL
		NewRecordData = Trim(Mid(Prog(PD), INSTR(Prog(PD), ":") + 1))
		
		IF OIR >= 16 OR OA < (CL - 2) Or OA > CL THEN
			'Save record, add new one
			
			'Format: :SSAAAATTDDDDDDDDCC
			'Size
			RecSize = Right(Hex(OIR), 2)
			DO WHILE LEN(RecSize) < 2: RecSize = "0" + RecSize: LOOP
			RCC = RCC + OIR
			'Start Address
			Address = Right(Hex(FRA), 4)
			DO WHILE LEN(Address) < 4: Address = "0" + Address: LOOP
			RCC = RCC + (FRA AND 255) + (FRA AND 65280) / 256
			'Type
			RecType = "00"
			'Checksum
			RecCheck = Right(Hex(256 - (RCC AND 255)), 2)
			DO WHILE LEN(RecCheck) < 2: RecCheck = "0" + RecCheck: LOOP
			
			'Add high address?
			IF ((FRA / 65536) AND 65535) <> CHA THEN
				CHA = ((FRA / 65536) AND 65535)
				':02 0000 04 0030 CA
				HexAddress = Right(HEX(CHA), 4)
				DO WHILE LEN(HexAddress) < 4: HexAddress = "0" + HexAddress: LOOP
				If ModePIC Then
					HexContents = Hex(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 6) AND 255))
				ElseIf ModeAVR Then
					HexContents = Hex(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 4) AND 255))
				End If
				DO WHILE LEN(HexContents) < 2: HexContents = "0" + HexContents: LOOP
				If ModePIC Then 
					HRC = HRC + 1: ASMProg(HRC) = ":02000004" + HexAddress + HexContents
				ElseIf ModeAVR Then
					HRC = HRC + 1: ASMProg(HRC) = ":02000002" + HexAddress + HexContents
				End IF
			END IF
			
			HRC = HRC + 1: ASMProg(HRC) = ":" + RecSize + Address + RecType + HRTemp + RecCheck
			
			HRTemp = Mid(NewRecordData, 3, 2) + Mid(NewRecordData, 1, 2)
			RCC = VAL("&H" + Mid(NewRecordData, 3, 2)) + VAL("&H" + Mid(NewRecordData, 1, 2))
			OIR = 2
			OA = CL
			FRA = CL
			
		ELSE
			'Add to current record
			HRTemp = HRTemp + Mid(NewRecordData, 3, 2) + Mid(NewRecordData, 1, 2)
			RCC = RCC + VAL("&H" + Mid(NewRecordData, 3, 2)) + VAL("&H" + Mid(NewRecordData, 1, 2))
			OIR += 2
			OA = CL
			IF FRA = -1 THEN FRA = CL
		END IF
		
	NEXT
	
	'Save last record
			
	'Format: :SSAAAATTDDDDDDDDCC
	'Size
	RecSize = Right(Hex(OIR), 2)
	DO WHILE LEN(RecSize) < 2: RecSize = "0" + RecSize: LOOP
	RCC = RCC + OIR
	'Start Address
	Address = Right(Hex(FRA), 4)
	DO WHILE LEN(Address) < 4: Address = "0" + Address: LOOP
	RCC = RCC + (FRA AND 255) + (FRA AND 65280) / 256
	'Type
	RecType = "00"
	'Checksum
	RecCheck = Right(Hex(256 - (RCC AND 255)), 2)
	DO WHILE LEN(RecCheck) < 2: RecCheck = "0" + RecCheck: LOOP
	
	'Add high address?
	IF ((FRA / 65536) AND 65535) <> CHA THEN
		CHA = ((FRA / 65536) AND 65535)
		':02 0000 04 0030 CA
		HexAddress = Right(HEX(CHA), 4)
		DO WHILE LEN(HexAddress) < 4: HexAddress = "0" + HexAddress: LOOP
		If ModePIC Then
			HexContents = HEX(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 6) AND 255))
		ElseIf ModeAVR Then
			HexContents = HEX(256 - ((VAL("&H" + Mid(HexAddress, 3, 2)) + VAL("&H" + Mid(HexAddress, 1, 2)) + 4) AND 255))
		End If
		DO WHILE LEN(HexContents) < 2: HexContents = "0" + HexContents: LOOP
		If ModePIC Then 
			HRC = HRC + 1: ASMProg(HRC) = ":02000004" + HexAddress + HexContents
		ElseIf ModeAVR Then
			HRC = HRC + 1: ASMProg(HRC) = ":02000002" + HexAddress + HexContents
		End IF
	END IF
	
	HRC = HRC + 1: ASMProg(HRC) = ":" + RecSize + Address + RecType + HRTemp + RecCheck
	
	'Write hex file	
	'HFI = OFI
	'Replace HFI, ".asm", ".hex"
	HFI = ReplaceToolVariables("%filename%", "hex")
	OPEN HFI FOR OUTPUT AS #1
	If ModePIC Then
		PRINT #1, ":020000040000FA"
	ElseIf ModeAVR Then
		PRINT #1, ":020000020000FC"
	End If
	FOR PD = 1 TO HRC
		PRINT #1, ASMProg(PD)
	NEXT
	PRINT #1, ":00000001FF"
	Close
	
END SUB

Sub BuildAsmSymbolTable
	'Scan through program, build symbol table
	
	Dim As String Temp, RomData, NewData, NDTemp
	Dim As Integer PD, CSB, RP1, CurrentLocation, DT, OrgLocation, SS, CS
	Dim As Integer DataBlockSize, DataSize, DWC, RSC, CurrCmd, DWIC, T
	
	If ModePIC Then
		ASMSym = 6
		ASMSymbols(1, 1) = "W": ASMSymbols(1, 2) = "0"
		ASMSymbols(2, 1) = "F": ASMSymbols(2, 2) = "1"
		ASMSymbols(3, 1) = "A": ASMSymbols(3, 2) = "0"
		ASMSymbols(4, 1) = "B": ASMSymbols(4, 2) = "1"
		ASMSymbols(5, 1) = "ACCESS": ASMSymbols(5, 2) = "0"
		ASMSymbols(6, 1) = "BANKED": ASMSymbols(6, 2) = "1"
	ElseIf ModeAVR Then
		ASMSym = 10
		'Need to do X after X+ and -X
		ASMSymbols(1, 1) = "X+": ASMSymbols(1, 2) = "29"
		ASMSymbols(2, 1) = "-X": ASMSymbols(2, 2) = "30"
		ASMSymbols(3, 1) = "X": ASMSymbols(3, 2) = "28"
		
		ASMSymbols(4, 1) = "Y+": ASMSymbols(4, 2) = "25"
		ASMSymbols(5, 1) = "-Y": ASMSymbols(5, 2) = "26"
		ASMSymbols(6, 1) = "Y": ASMSymbols(6, 2) = "8"
		
		ASMSymbols(7, 1) = "rZ": ASMSymbols(7, 2) = "0"
		ASMSymbols(8, 1) = "Z+": ASMSymbols(8, 2) = "17"
		ASMSymbols(9, 1) = "-Z": ASMSymbols(9, 2) = "18"
		
		'ASMSymbols(10, 1) = "RAMEND": ASMSymbols(10, 2) = STR(ChipRam + 95)
		Temp = MemRanges(MRC)
		Temp = "&H" + Trim(Mid(Temp, InStr(Temp, ":") + 1))
		ASMSymbols(10, 1) = "RAMEND": ASMSymbols(10, 2) = STR(Val(Temp) - 1)
	End If
		
	'SFRs
	FOR PD = 1 to SVC
		ASMSym = ASMSym + 1
		ASMSymbols(ASMSym, 1) = Trim(UCase(SysVars(PD).Name))
		ASMSymbols(ASMSym, 2) = Trim(Str(SysVars(PD).Location))
	NEXT
		
	'SFR bits
	FOR PD = 1 to SVBC
		ASMSym = ASMSym + 1
		ASMSymbols(ASMSym, 1) = Trim(UCase(SysVarBits(PD).Name))
		ASMSymbols(ASMSym, 2) = Trim(Str(SysVarBits(PD).Location))
	NEXT
	
	'User variables
	FOR PD = 1 to FVLC
		With FinalVarList(PD)
			ASMSym = ASMSym + 1
			ASMSymbols(ASMSym, 1) = UCase(.Name)
			ASMSymbols(ASMSym, 2) = .Value
		End With
	NEXT
	'User register variables (AVR only)
	If ModeAVR Then
		FOR PD = 1 to FRLC
			With FinalRegList(PD)
				ASMSym = ASMSym + 1
				ASMSymbols(ASMSym, 1) = .Name
				ASMSymbols(ASMSym, 2) = .Value
			End With
		NEXT
	End If
	'Alias variables
	FOR PD = 1 to FALC
		With FinalAliasList(PD)
			If Not HasSFR(.Value) Then
				ASMSym += 1
				ASMSymbols(ASMSym, 1) = UCase(.Name)
				ASMSymbols(ASMSym, 2) = .Value
			End If
		End With
	NEXT
	
	'Symbols from compiler
	For PD = 1 To ToAsmSymbols
		ASMSym += 1
		ASMSymbols(ASMSym, 1) = ToAsmSymbol(PD, 1)
		ASMSymbols(ASMSym, 2) = ToAsmSymbol(PD, 2) 
	Next
	
	'Labels
	'Check for RP1 bit
	If ModePIC Then
		'Should banksel set 1 or 2 STATUS bits?
		If ChipFamily = 15 Or ChipFamily = 16 Then
			RP1 = 0
		Else
			RP1 = HasSFRBit("RP1")
			If MemSize <= 256 Then RP1 = 0 'If 256 or less addresses, there is no RP1
		End If
	End If
	CurrentLocation = 0
	FOR PD = 1 to ASPC
		CurrCmd = IsASM(ASMProg(PD))
		'Blank line
		If AsmProg(PD) = "" Then
		
		'Assembly instruction
		ElseIf CurrCmd <> 0 THEN 
			DT = ASMCommands(CurrCmd).Words
			If ChipFamily = 16 THEN DT = DT * 2
			ASMProg(PD) = Str(CurrentLocation) + ":" + ASMProg(PD)
			CurrentLocation = CurrentLocation + DT
		
		'Not instruction
		Else
			'ORG directive
			IF Left(ASMProg(PD), 4) = "ORG " Then
				OrgLocation = MakeDec(Mid(ASMProg(PD), 4))
				If OrgLocation >= CurrentLocation Then
					CurrentLocation = OrgLocation
				Else
					Temp = Message("BadORG")
					Replace Temp, "%loc%", Str(OrgLocation)
					LogError("GCASM:" + Temp, "")
				End If
				CurrCmd = 999
				ASMProg(PD) = ""
				
			'BANKSEL/PAGESEL directives
			ElseIF Left(ASMProg(PD), 8) = "BANKSEL " THEN
				ASMProg(PD) = Str(CurrentLocation) + ":" + ASMProg(PD)
				If ChipFamily = 16 Then
					CurrentLocation += 2
				ElseIf ChipFamily = 15 Then
					CurrentLocation += 1
				Else
					IF RP1 Then
						CurrentLocation += 2
					Else
						CurrentLocation += 1
					End If
				End If
				CurrCmd = 999
				
			ElseIF Left(ASMProg(PD), 9) = "BANKISEL " THEN
				ASMProg(PD) = Str(CurrentLocation) + ":" + ASMProg(PD)
				CurrentLocation += 1
				CurrCmd = 999
				
			ElseIF Left(ASMProg(PD), 8) = "PAGESEL " THEN
				If ChipFamily = 15 Then
					ASMProg(PD) = Str(CurrentLocation) + ":" + ASMProg(PD)
					CurrentLocation += 1
					CurrCmd = 999
					
				Else
					ASMProg(PD) = Str(CurrentLocation) + ":" + ASMProg(PD)
					CurrentLocation += PCUpper
					CurrCmd = 999
					
				End If
				
			'Data embedding instructions
			ElseIf Left(ASMProg(PD), 3) = "DW " OR Left(ASMProg(PD), 3) = "DB " Or Left(ASMProg(PD), 3) = "DE " THEN
				
				DataBlockSize = 1
				If (ChipFamily = 14 Or ChipFamily = 15) And Left(ASMProg(PD), 3) = "DE " Then DataBlockSize = 2
				ROMData = Trim(Mid(ASMProg(PD), 4))
				DataSize = 0
				DWC = 0
				RSC = 0
				DO WHILE INSTR(ROMData, Chr(34)) <> 0
					Temp = Mid(ROMData, INSTR(ROMData, Chr(34)) + 1)
					Temp = Chr(34) + Left(Temp, INSTR(Temp, Chr(34)))
					RSC = RSC + 1
					Replace ROMData, Temp, "%S" + Str(RSC) + "S"
					Temp = Mid(Temp, 2)
					Temp = Left(Temp, LEN(Temp) - 1)
					TempData(RSC) = Temp
				LOOP
				DWIC = 0
				DO WHILE INSTR(ROMData, ",") <> 0
					DWIC += 1: CheckTemp(DWIC) = Trim(Left(ROMData, INSTR(ROMData, ",") - 1))
					ROMData = Trim(Mid(ROMData, INSTR(ROMDATA, ",") + 1))
				LOOP
				IF ROMData <> "" Then
					DWIC += 1: CheckTemp(DWIC) = ROMData
				End If
				
				NewData = ""
				
				SS = 0
				Do While SS < DWIC
					SS += 1
					
					Temp = CheckTemp(SS)
					IF Temp <> "" THEN
						'Print Temp,
						
						'Temp contains integer
						IF IsConst(Temp) Then
							'Print "Int", MakeDec(Temp)
							If ModePIC Then
								DataSize += 2
							ElseIf ModeAVR Then
								DataSize += 1
							End If
							NDTemp = Hex(MakeDec(Temp))
							'Combine with next byte?
							If DataBlockSize = 1 Then
								IF LEN(NDTemp) = 1 THEN NDTemp = "0" + NDTemp
								IF LEN(NDTemp) <= 2 THEN
									'CS = 0: IF MakeDec(CheckTemp(SS + 1)) <> 0 THEN CS = 1
									CS = IsConst(CheckTemp(SS + 1))
									
									IF CS Then
										NDTemp = HEX(MakeDec(CheckTemp(SS + 1))) + NDTemp
										SS += 1
									Else
										NDTemp = "00" + NDTemp
									END IF
								END IF
								IF LEN(NDTemp) = 3 THEN NDTemp = "0" + NDTemp
							ElseIf DataBlockSize = 2 Then
								Select Case Len(NDTemp)
									Case 1: NDTemp = "000" + NDTemp
									Case 2: NDTemp = "00" + NDTemp
									Case 3: NDTemp = "0" + NDTemp
								End Select
							End If
							'Print "Output as "; NDTemp
							NewData = NewData + "," + NDTemp 
							
						'Temp contains string
						Else
							IF INSTR(Temp, "%S") <> 0 THEN
								T = VAL(Mid(Temp, INSTR(Temp, "%S") + 2))
								Temp = TempData(T)
							END If
							'Print "String:"; Temp
							
							DataSize += LEN(Temp)
							IF (LEN(Temp) / 2) <> INT(LEN(Temp) / 2) Then DataSize += 1: Temp = Temp + Chr(0)
							FOR CS = 1 to LEN(Temp) Step 2
								NDTemp = HEX(ASC(Mid(Temp, CS + 1, 1))) + HEX(ASC(Mid(Temp, CS, 1)))
								IF LEN(NDTemp) = 3 THEN NDTemp = "0" + NDTemp
								NewData = NewData + "," + NDTemp 
							NEXT
						END If
						
					END IF
				Loop
				
				ASMProg(PD) = Str(CurrentLocation) + ":RAW " + Mid(NewData, 2)
				CurrentLocation += DataSize
				CurrCmd = 999
			END IF
			
			'EQU directive
			IF INSTR(ASMProg(PD), " EQU ") <> 0 THEN CurrCmd = 999: ASMProg(PD) = ""
			
			'If nothing else, then line is label
			If CurrCmd <> 999 THEN
				IF Right(ASMProg(PD), 1) = ":" Then ASMProg(PD) = Left(ASMProg(PD), LEN(ASMProg(PD)) - 1)
				ASMSym = ASMSym + 1
				ASMSymbols(ASMSym, 1) = UCase(ASMProg(PD))
				ASMSymbols(ASMSym, 2) = Str(CurrentLocation)
				IF INSTR(ASMProg(PD), " ") <> 0 THEN
					Temp = Message("BadSymbol")
					Replace Temp, "%symbol%", ASMProg(PD)
					LogError Temp
				END IF
			END IF
		END IF
	NEXT
	
	'Sort symbol table alphabetically
	'Use selection sort
	Dim As Integer StartPos, SearchPos, FirstPos
	For StartPos = 1 To AsmSym
		'Find first symbol name in unsorted part of list
		FirstPos = StartPos
		For SearchPos = StartPos + 1 To AsmSym
			If AsmSymbols(SearchPos, 1) < AsmSymbols(FirstPos, 1) Then
				FirstPos = SearchPos	
			End If
		Next
		'Move first unsorted item into first place of unsorted list
		'(Swap with first unsorted location)
		Swap AsmSymbols(StartPos, 1), AsmSymbols(FirstPos, 1)
		Swap AsmSymbols(StartPos, 2), AsmSymbols(FirstPos, 2)
	Next
	
End Sub

FUNCTION IsASM (DataSource As String, ParamCount As Integer = -1) As Integer
	
	'Returns 0 if instruction is not assembly
	'Returns instruction list index if it is asm
	
	Dim As String Temp, Params
	Dim As Integer T, CloseMatch
	
	Temp = Trim(LCase(DataSource))
	If INSTR(Temp, " ") <> 0 Then
		Params = Trim(Mid(Temp, InStr(Temp, " ") + 1))
		If ParamCount = -1 Then
			ParamCount = 1 + CountOccur(Params, ",")
		End If
		Temp = Trim(Left(Temp, INSTR(Temp, " ") - 1))
	End If
	If ParamCount = -1 Then ParamCount = 0
	
	CloseMatch = 0
	FOR T = 1 to ASMCC
		With ASMCommands(T)
			If .Cmd = Temp Then
				If .Params = ParamCount Then
					Return T
				Else
					CloseMatch = T
				End If
			End If
		End With
	NEXT
	
	'PRINT DataSource, "Not ASM"
	
	Return CloseMatch
END FUNCTION

Function IsASMConst (DataSource As String) As Integer
	
	If IsConst(DataSource) Then Return -1
	IF Left(Trim(DataSource), 1) = "@" Then Return -1
	Return 0
	
End Function
