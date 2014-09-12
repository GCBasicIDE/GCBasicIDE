'	GCBASIC - A BASIC Compiler for microcontrollers
'	 Preprocessor
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

Function CheckSysVarDef(ConditionIn As String) As String
	'Checks a condition from #ifdef or If in script
	'Returns result
	
	Dim As String Condition, Temp, Original
	Dim As Integer FV, FC, ConstFound
	
	Condition = ConditionIn
	
	'Test for SFR bit?
	Do While INSTR(Condition, "BIT(") <> 0
		'Get name of bit, and whether checking for presence or absence
		FV = 0: If INSTR(Condition, "NOBIT(") = INSTR(Condition, "BIT(") - 2 Then FV = 1
		Temp = Mid(Condition, INSTR(Condition, "(") + 1)
		Temp = Left(Temp, INSTR(Temp, ")") - 1)
		If FV = 0 Then
			Original = "BIT(" + Temp + ")"
		Else
			Original = "NOBIT(" + Temp + ")"
		End If
		
		'Search for bit in list
		ConstFound = HasSFRBit(Temp)
		
		'Replace result
		If FV = 1 Then
			'If ConstFound = 0 Then DelMode = 1 Else DelMode = 2
			If ConstFound = 0 Then
				ConstFound = 1
			Else
				ConstFound = 0
			End If
		Else
			If ConstFound <> 0 Then ConstFound = 1
		End If
		
		Replace Condition, Original, Str(ConstFound) + "=1"
	Loop
	
	'Test for SFR?
	Do While INSTR(Condition, "VAR(") <> 0
		'Get name of SFR, and checking mode
		FV = 0: IF INSTR(Condition, "NOVAR(") = INSTR(Condition, "VAR(") - 2 THEN FV = 1
		Temp = Mid(Condition, INSTR(Condition, "(") + 1)
		Temp = Left(Temp, INSTR(Temp, ")") - 1)
		If FV = 0 Then
			Original = "VAR(" + Temp + ")"
		Else
			Original = "NOVAR(" + Temp + ")"
		End If
		
		ConstFound = HasSFR(Temp)
		
		'Replace result
		If FV = 1 Then
			'If ConstFound = 0 Then DelMode = 1 Else DelMode = 2
			If ConstFound = 0 Then
				ConstFound = 1
			Else
				ConstFound = 0
			End If
		Else
			If ConstFound <> 0 Then ConstFound = 1
		End If
		
		Replace Condition, Original, Str(ConstFound) + "=1"
	Loop
	
	Return Condition
End Function

Sub PrepareBuiltIn
	
	'Read built-in subs and constants
	Dim As String InnerLoop, OuterLoop
	Dim As Integer CD, T, T2
	Dim As Double L
	Dim As LinkedListElement Pointer CurrPos
	
	'Label Ending
	LabelEnd = ""
	If ModeAVR Then LabelEnd = ":"
	If ModeZ8 Then LabelEnd = ":"
	
	'Equivalent config settings (PIC)
	GetEqConfig
	
	'Constants set by compiler
	'Set chip config defines for #IFDEF and #SCRIPT use
	DFC = DFC + 1: gcDEF(DFC, 1) = "CHIPNAME": gcDEF(DFC, 2) = ChipName
	DFC = DFC + 1: gcDEF(DFC, 1) = "CHIP_" + ChipName: gcDEF(DFC, 2) = ""
	DFC = DFC + 1: gcDEF(DFC, 1) = "CHIPMHZ": gcDEF(DFC, 2) = Str(ChipMhz)
	DFC = DFC + 1: gcDEF(DFC, 1) = "CHIPFAMILY": gcDEF(DFC, 2) = Str(ChipFamily)
	DFC = DFC + 1: gcDEF(DFC, 1) = "OSC": gcDEF(DFC, 2) = OSCType
	DFC = DFC + 1: gcDEF(DFC, 1) = "CHIPPINS": gcDEF(DFC, 2) = Str(ChipPins)
	If ModePIC Then DFC = DFC + 1: gcDEF(DFC, 1) = "PIC": gcDEF(DFC, 2) = ""
	If ModeAVR Then
		DFC = DFC + 1: gcDEF(DFC, 1) = "AVR": gcDEF(DFC, 2) = ""
		If HMult Then DFC = DFC + 1: gcDEF(DFC, 1) = "HARDWAREMULT": gcDEF(DFC, 2) = ""
	End If
	If ModeZ8 Then DFC = DFC + 1: gcDEF(DFC, 1) = "Z8": gcDEF(DFC, 2) = ""
	
	'Constants to provide information on RAM banks
	Dim As String TempData
	Dim As Integer Range, Min, Max
	For Range = 1 to MRC
		TempData = MemRanges(Range)
		Min = VAL("&h" + Left(TempData, INSTR(TempData, ":") - 1))
		Max = VAL("&h" + Mid(TempData, INSTR(TempData, ":") + 1))
		'Print "Bank " + Str(Min \ 128) + " starts 0x" + Hex(Min) + " ends 0x" + Hex(Max)
		DFC += 1: gcDEF(DFC, 1) = "CHIPBANK_" + Str(Min \ 128) + "_START": gcDEF(DFC, 2) = Str(Min)
		DFC += 1: gcDEF(DFC, 1) = "CHIPBANK_" + Str(Min \ 128) + "_END": gcDEF(DFC, 2) = Str(Max)
	Next
	
	'Delay subs
	'Time Intervals: us, 10us, ms, 10ms, sec, min, hour

	'Delay_US
	SBC += 1
	Subroutine(SBC) = NewSubroutine("Delay_US")
	CurrPos = Subroutine(SBC)->CodeStart
	
	If ModePIC Then
		'Each nop takes .2 us on 20 MHz chip
		L = ChipMhz / 4 - 3
		'Make sure L is positive integer
		'If not, us delays will be inaccurate
		If L < 0 Then
			L = 0
			USDelaysInaccurate = -1
		End If
		If L <> Int(L) Then
			USDelaysInaccurate = -1
		End If
		CurrPos = LinkedListInsert(CurrPos, " incf SysWaitTempUS_H, F")
		CurrPos = LinkedListInsert(CurrPos, " movf SysWaitTempUS, F")
		CurrPos = LinkedListInsert(CurrPos, " btfsc STATUS,Z")
		CurrPos = LinkedListInsert(CurrPos, " goto DUS_END")
		CurrPos = LinkedListInsert(CurrPos, "DUS_START")
		For CD = 1 TO L
			CurrPos = LinkedListInsert(CurrPos, " nop")
		Next
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempUS, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DUS_START")
		CurrPos = LinkedListInsert(CurrPos, "DUS_END")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempUS_H, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DUS_START")
	ElseIf ModeAVR Then
		
	ElseIf ModeZ8 Then
		
	End If
	
	'Delay_10US
	SBC = SBC + 1
	Subroutine(SBC) = NewSubroutine("Delay_10US")
	CurrPos = Subroutine(SBC)->CodeStart
	If ModePIC Then
		
		CurrPos = LinkedListInsert(CurrPos, "D10US_START")
		'Need to delay for 10 us
		'L = number of cycles to waste
		'  = cycles in 10 us, minus cycles at start and end of loop
		'3 cycles at end of loop
		L = 10 * ChipMhz \ 4 - 3
		If L < 0 Then L = 0
		CurrPos = LinkedListInsertList(CurrPos, GenerateExactDelay(L))
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTemp10US, F")
		CurrPos = LinkedListInsert(CurrPos, " goto D10US_START")
		
	ElseIf ModeAVR Then
		L = 10 * ChipMhz - 3
		If L < 0 Then L = 0
		CurrPos = LinkedListInsert(CurrPos, "D10US_START:")
		CurrPos = LinkedListInsertList(CurrPos, GenerateExactDelay(L))
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTemp10US")
		CurrPos = LinkedListInsert(CurrPos, " brne D10US_START")
	End If
	
	'Delay_MS
	'Repeat 20(wait 50)
	SBC += 1
	Subroutine(SBC) = NewSubroutine("Delay_MS")
	CurrPos = Subroutine(SBC)->CodeStart
	
	Dim As Integer D1, D2, BestD1, BestD2, ThisTime, ReqCycles, DiffFromReq, BestDiff
	
	If ModePIC Then
		'Cycles for code below:
		'1 + (2 + (2 + Inner * 3 - 1) * (Outer * 3) - 1) + Time * 3
		
		'Calculate required number of wasted cycles
		ReqCycles = 1000 * ChipMhz / 4
		BestDiff = ReqCycles
		
		'Find best values for delay
		For D1 = 1 To 255
			For D2 = 1 To 255
				'Calc how long current D1, D2 values will give
				ThisTime = 5 + D2 * (3 * D1 + 4)
				'Check to see how close it is to the required delay
				If ThisTime < ReqCycles Then
					DiffFromReq = ReqCycles - ThisTime
				ElseIf ThisTime > ReqCycles Then
					DiffFromReq = ThisTime - ReqCycles
				End If
				'If it's the best, record
				If DiffFromReq < BestDiff Then
					BestD1 = D1
					BestD2 = D2
					BestDiff = DiffFromReq
				End If
			Next
		Next
		OuterLoop = Str(BestD2)
		InnerLoop = Str(BestD1)
		
		CurrPos = LinkedListInsert(CurrPos, " incf SysWaitTempMS_H, F")
		CurrPos = LinkedListInsert(CurrPos, "DMS_START")
		CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP2 = " + OuterLoop)
		CurrPos = LinkedListInsert(CurrPos, "DMS_OUTER")
		CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = " + InnerLoop)
		CurrPos = LinkedListInsert(CurrPos, "DMS_INNER")
		CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DMS_INNER")
		CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP2, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DMS_OUTER")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempMS, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DMS_START")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempMS_H, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DMS_START")
	ElseIf ModeAVR Then
		
		'Calculate required number of wasted cycles
		ReqCycles = 1000 * ChipMhz
		BestDiff = ReqCycles
		
		'Find best values for delay
		For D1 = 1 To 255
			For D2 = 1 To 255
				'Calc how long current D1, D2 values will give
				ThisTime = D2 * (3 + 3 * D1)
				'Check to see how close it is to the required delay
				If ThisTime < ReqCycles Then
					DiffFromReq = ReqCycles - ThisTime
				ElseIf ThisTime > ReqCycles Then
					DiffFromReq = ThisTime - ReqCycles
				End If
				'If it's the best, record
				If DiffFromReq < BestDiff Then
					BestD1 = D1
					BestD2 = D2
					BestDiff = DiffFromReq
				End If
			Next
		Next
		OuterLoop = Str(BestD2)
		InnerLoop = Str(BestD1)
		
		CurrPos = LinkedListInsert(CurrPos, " inc SysWaitTempMS_H")
		CurrPos = LinkedListInsert(CurrPos, "DMS_START:")
		
		CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP2 = " + OuterLoop)
		CurrPos = LinkedListInsert(CurrPos, "DMS_OUTER:")
		CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = " + InnerLoop)
		CurrPos = LinkedListInsert(CurrPos, "DMS_INNER:")
		CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP")
		CurrPos = LinkedListInsert(CurrPos, " brne DMS_INNER")
		CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP2")
		CurrPos = LinkedListInsert(CurrPos, " brne DMS_OUTER")
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTempMS")
		CurrPos = LinkedListInsert(CurrPos, " brne DMS_START")
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTempMS_H")
		CurrPos = LinkedListInsert(CurrPos, " brne DMS_START")
	End If
	
	'Delay_10MS
	SBC += 1
	Subroutine(SBC) = NewSubroutine("Delay_10MS")
	CurrPos = Subroutine(SBC)->CodeStart
	If ModePIC Then
		CurrPos = LinkedListInsert(CurrPos, "D10MS_START")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempMS = 10")
		CurrPos = LinkedListInsert(CurrPos, "Delay_MS")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTemp10MS, F")
		CurrPos = LinkedListInsert(CurrPos, " goto D10MS_START")
	ElseIf ModeAVR Then
		CurrPos = LinkedListInsert(CurrPos, "D10MS_START:")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempMS = 10")
		CurrPos = LinkedListInsert(CurrPos, "Delay_MS")
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTemp10MS")
		CurrPos = LinkedListInsert(CurrPos, " brne D10MS_START")
	End If
	
	'Delay_Sec
	SBC += 1
	Subroutine(SBC) = NewSubroutine("Delay_S")
	CurrPos = Subroutine(SBC)->CodeStart
	If ModePIC Then
		CurrPos = LinkedListInsert(CurrPos, "DS_START")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempMS = 1000")
		CurrPos = LinkedListInsert(CurrPos, "Delay_MS")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempS, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DS_START")
	ElseIf ModeAVR Then
		CurrPos = LinkedListInsert(CurrPos, "DS_START:")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempMS = 1000")
		CurrPos = LinkedListInsert(CurrPos, "Delay_MS")
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTempS")
		CurrPos = LinkedListInsert(CurrPos, " brne DS_START")
	End If
	
	'Delay_Min
	SBC += 1
	Subroutine(SBC) = NewSubroutine("Delay_M")
	CurrPos = Subroutine(SBC)->CodeStart
	If ModePIC Then
		CurrPos = LinkedListInsert(CurrPos, "DMIN_START")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempS = 60")
		CurrPos = LinkedListInsert(CurrPos, "Delay_S")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempM, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DMIN_START")
	ElseIf ModeAVR Then
		CurrPos = LinkedListInsert(CurrPos, "DMIN_START:")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempS = 60")
		CurrPos = LinkedListInsert(CurrPos, "Delay_S")
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTempM")
		CurrPos = LinkedListInsert(CurrPos, " brne DMIN_START")
	End If
	
	'Delay_Hour
	SBC += 1
	Subroutine(SBC) = NewSubroutine("Delay_H")
	CurrPos = Subroutine(SBC)->CodeStart
	If ModePIC Then
		CurrPos = LinkedListInsert(CurrPos, "DHOUR_START")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempM = 60")
		CurrPos = LinkedListInsert(CurrPos, "Delay_M")
		CurrPos = LinkedListInsert(CurrPos, " decfsz SysWaitTempH, F")
		CurrPos = LinkedListInsert(CurrPos, " goto DHOUR_START")
	ElseIf ModeAVR Then
		CurrPos = LinkedListInsert(CurrPos, "DHOUR_START:")
		CurrPos = LinkedListInsert(CurrPos, "SysWaitTempM = 60")
		CurrPos = LinkedListInsert(CurrPos, "Delay_M")
		CurrPos = LinkedListInsert(CurrPos, " dec SysWaitTempH")
		CurrPos = LinkedListInsert(CurrPos, " brne DHOUR_START")
	End If
	
	'FSR alias on 18F
	If ChipFamily = 15 Or ChipFamily = 16 Then
		AddVar "AFSR0", "WORD", 1, 0, "ALIAS:FSR0H, FSR0L", ""
		AddVar "AFSR1", "WORD", 1, 0, "ALIAS:FSR1H, FSR1L", ""
		If ChipFamily = 16 Then AddVar "AFSR2", "WORD", 1, 0, "ALIAS:FSR2H, FSR2L", ""
	End If
	
	'Enable "GPIO" on chips that don't have it
	'This affects PIC 12F1822 and possibly others
	'Are we dealing with an 8 pin PIC?
	If ModePIC And ChipPins <= 8 Then
		'Is there a GPIO port?
		If Not HasSFR("GPIO") Then
			'If there's a PORTA, set GPIO to PORTA
			If HasSFR("PORTA") Then
				DFC = DFC + 1
				gcDEF(DFC, 1) = "GPIO"
				gcDEF(DFC, 2) = "PORTA"
				
			'If there's a PORTB, set GPIO to PORTB
			ElseIf HasSFR("PORTB") Then
				DFC = DFC + 1
				gcDEF(DFC, 1) = "GPIO"
				gcDEF(DFC, 2) = "PORTB"
				
			'If there's a PORTC, set GPIO to PORTC
			ElseIf HasSFR("PORTC") Then
				DFC = DFC + 1
				gcDEF(DFC, 1) = "GPIO"
				gcDEF(DFC, 2) = "PORTB"
				
			'Let's hope none have PORTD as their only port!
			End If
			
		End If
	End If

END SUB

SUB PreProcessor

	Dim As String Origin, Temp, DataSource, PreserveIn, DSOld, CurrentSub, StringTemp, SubName
	Dim As String Value, RTemp, LTemp, Ty, SubInType, ParamType, RestOfLine, VarName, FName, ConstName
	Dim As String TempFile, LastTableOrigin, NewFNType
	Dim As LinkedListElement Pointer CurrPos, MainCurrPos
	
	Dim As String LineToken(100)
	
	Dim As Integer T, T2, ICCO, CE, PD, RF, S, LC, LCS, SID, CD, SL, NR
	Dim As Integer ForceMain, LineTokens, FoundFunction, FoundMacro
	Dim As Single CurrPerc, PercAdd, PercOld
	
	CurrentSub = ""
	
	'Find required files
	IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("FindSource")
	ICC = 1: gcINC(1, 1) = ShortName(FI)
	T = 1
	
	'FindIncludeFiles:
	Do
		T2 = T
		ICCO = ICC
		FOR T = T2 TO ICC
			
			IF VBS = 1 THEN PRINT SPC(10); gcINC(T, 1);
			IF Dir(gcINC(T, 1)) = "" THEN
				IF VBS = 0 THEN 
					Temp = Message("NoFile")
					Replace Temp, FI, gcINC(T, 1)
					PRINT Temp
				Else
					PRINT ": " + Message("NotFound")   
				End If
			
			Else
				IF VBS = 1 THEN PRINT ": " + Message("found")
				OPEN gcINC(T, 1) FOR INPUT AS #1
				DO WHILE NOT EOF(1)
					LINE INPUT #1, Temp
					Temp = Trim(Temp)
					IF Left(UCase(Temp), 8) = "#INCLUDE" THEN
						IF INSTR(Temp, Chr(34)) <> 0 THEN
							Temp = Mid(Temp, INSTR(Temp, Chr(34)) + 1)
							Temp = Trim(Left(Temp, INSTR(Temp, Chr(34)) - 1))
							Temp = AddFullPath(Temp, ProgDir)
						ElseIF INSTR(Temp, "<") <> 0 THEN
							Temp = Mid(Temp, INSTR(Temp, "<") + 1)
							Temp = Left(Temp, INSTR(Temp, ">") - 1)
							Temp = AddFullPath(Temp, ID + "\include\") 
						END IF
						Temp = ShortName(Temp)
						'Check to see if include file already in list
						CE = 0
						FOR PD = 1 TO ICC
							IF UCase(gcINC(PD, 1)) = UCase(Temp) THEN CE = 1: EXIT FOR
						Next
						
						'If not, add it
						IF CE = 0 Then
							
							'May need to convert
							Temp = TranslateFile(Temp)
							
							ICC += 1
							gcINC(ICC, 1) = Temp
						End If
					END IF
				LOOP
				
				CLOSE #1
			END IF
		NEXT
	'IF ICCO < ICC THEN GOTO FindIncludeFiles
	Loop While ICCO < ICC
	ICCO = ICC
	
	'Add standard include files to list
	#IFDEF __FB_LINUX__
		OPEN ID + "/include/lowlevel.dat" FOR INPUT AS #1
	#ELSE
		OPEN ID + "\include\lowlevel.dat" FOR INPUT AS #1
	#ENDIF
	
	DO WHILE NOT EOF(1)
		LINE INPUT #1, DataSource
		DataSource = Trim(DataSource)
		IF Left(DataSource, 1) <> "'" THEN
			#IFDEF __FB_LINUX__
				DataSource = ID + "/include/lowlevel/" + DataSource
			#ELSE
				DataSource = ID + "\INCLUDE\LOWLEVEL\" + DataSource
			#ENDIF
			CE = 0
			FOR PD = 1 TO ICC
				IF UCase(gcINC(PD, 1)) = UCase(DataSource) THEN CE = 1: EXIT FOR
			NEXT
			IF CE = 0 THEN
				ICC = ICC + 1
				gcINC(ICC, 1) = DataSource
				Temp = Dir(DataSource)
				IF Temp <> "" THEN Temp = ": " + Message("found")
				IF Temp = "" THEN Temp = ": " + Message("NotFound")
				IF VBS = 1 THEN PRINT SPC(10); DataSource; Temp
			END IF
		END IF
	LOOP
	CLOSE
	
	'Create Main subroutine
	Subroutine(0) = NewSubroutine("Main")
	MainCurrPos = Subroutine(0)->CodeStart
	Subroutine(0)->Required = -1 'Mark as required so that it gets compiled
	
	'Read all required files
	IF VBS = 1 THEN PRINT SPC(5); Message("LoadSource");
	PercOld = 0
	CurrPerc = 0.5
	PercAdd = 1 / ICC * 100
	FOR RF = 1 TO ICC
		If OPEN(gcINC(RF, 1) FOR INPUT AS #1) <> 0 Then Goto LoadNextFile
		
		S = 0
		LC = 0
		LCS = 0
		
		DO WHILE NOT EOF(1)
		LoadFileData:
		
			LINE INPUT #1, DataSource
			LC = LC + 1
			LCS = LCS + 1
			
			'Save copy for Preserve mode
			PreserveIn = ""
			If (PreserveMode = 1 Or PreserveMode = 2) And (RF = 1 OR S = 1) Then
				PreserveIn = DataSource
				Do While Left(PreserveIn, 1) = Chr(9): PreserveIn = Mid(PreserveIn, 2): Loop
				PreserveIn = TRIM(PreserveIn)
				
				'Preserve Comments
				If Left(DataSource, 1) = "'" OR Left(DataSource, 1) = ";" OR Left(DataSource, 4) = "REM " Then
					IF Left(PreserveIn, 4) = "REM " Then PreserveIn = "'" + Trim(Mid(PreserveIn, 4))
					PreserveIn = Trim(Mid(PreserveIn, 2))
					PCC += 1
					PreserveCode(PCC) = PreserveIn
					IF S = 0 THEN MainCurrPos = LinkedListInsert(MainCurrPos, "PRESERVE " + Str(PCC))
					IF S = 1 THEN CurrPos = LinkedListInsert(CurrPos, "PRESERVE " + Str(PCC))
					PreserveIn = ""
				End If
				If PreserveMode = 1 Then PreserveIn = ""
			End If
			If PreserveMode = 3 Then
				PreserveIn = "Source:F" + Str(RF) + "L" + Str(LC) + "S" + Str(SLC * S) + "I" + Str(LCS)
			End If
			
			'Remove leading and trailing spaces, capitalise line
			DSOld = DataSource
			DO WHILE INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": LOOP
			DataSource = UCase(Trim(DataSource))
			If RF <= ICCO Then
				If DataSource <> "" Then
					MainProgramSize += 1
				End If
			End If
			
			'Remove comments
			IF INSTR(DataSource, "'") > INSTR(DataSource, "B'") Then
				DO WHILE INSTR(DataSource, "B'") <> 0
					Replace DataSource, "B'", "%B%"
					Replace DataSource, "'", "%S"
				LOOP
			End If
			
			IF INSTR(DataSource, "'") > INSTR(DataSource, "H'") Then
				DO WHILE INSTR(DataSource, "H'") <> 0
					Replace DataSource, "H'", "%H%"
					Replace DataSource, "'", "%S"
				LOOP
			End If
			
			'Trim line of comments when string is present
			If INSTR(DataSource, "'") <> 0 AND INSTR(DataSource, Chr(34)) <> 0 AND INSTR(DataSource, "'") < INSTR(DataSource, Chr(34)) Then
				DataSource = Trim(Left(DataSource, INSTR(DataSource, "'") - 1))
				DSOld = Trim(Left(DSOld, INSTR(DSOld, "'") - 1))
			End If
			'Remove comments started with ;
			IF INSTR(DataSource, ";") > 1 THEN DataSource = Left(DataSource, INSTR(DataSource, ";") - 1)
			
			'Only load line if it is valid
			T = 0
			IF Left(DataSource, 1) = ";" THEN T = 1
			IF Left(DataSource, 1) = "'" THEN T = 1
			IF Left(DataSource, 4) = "REM " THEN T = 1
			IF DataSource = "REM" THEN T = 1
			IF DataSource = "" THEN T = 1
			IF Left(DataSource, 8) = "#INCLUDE" THEN T = 1
			
			'Extract strings
			IF T <> 1 THEN
				DO WHILE INSTR(DSOld, Chr(34)) <> 0
					StringTemp = Mid(DSOld, INSTR(DSOld, Chr(34)) + 1)
					If InStr(StringTemp, Chr(34)) = 0 Then
						Temp = ";?F" + Str(RF) + "L" + Str(LC) + "S0" + "I" + Str(LCS) + "?"
						LogError(Message("NoClosingQuote"), Temp)
						
						Replace DataSource, Chr(34) + StringTemp, Chr(34) + StringTemp + Chr(34)
						Replace DSOld, Chr(34) + StringTemp, Chr(34) + StringTemp + Chr(34)
						
					Else
						StringTemp = Left(StringTemp, INSTR(StringTemp, Chr(34)) - 1)
					End If
					
					'Check for duplicates
					SID = 0
					FOR CD = 1 to SSC
						If StringStore(CD).Value = StringTemp THEN SID = CD: EXIT FOR
					NEXT
					
					IF SID = 0 THEN
						SSC = SSC + 1
						StringStore(SSC).Value = StringTemp
						StringStore(SSC).Used = 0
						Replace DataSource, Chr(34) + StringTemp + Chr(34), ";STRING" + Str(SSC) + ";"
						Replace DSOld, Chr(34) + StringTemp + Chr(34), ";STRING" + Str(SSC) + ";"
					Else
						Replace DataSource, Chr(34) + StringTemp + Chr(34), ";STRING" + Str(SID) + ";"
						Replace DSOld, Chr(34) + StringTemp + Chr(34), ";STRING" + Str(SID) + ";"
					END IF
				LOOP
			END IF
			
			'Trim line of all comments
			IF INSTR(DataSource, "'") > 1 THEN DataSource = Left(DataSource, INSTR(DataSource, "'") - 1)
			
			DO WHILE INSTR(DataSource, "%B%") <> 0
				Replace DataSource, "%B%", "B'"
				Replace DataSource, "%S", "'"
			LOOP
			DO WHILE INSTR(DataSource, "%H%") <> 0
				Replace DataSource, "%H%", "H'"
				Replace DataSource, "%S", "'"
			LOOP
			DataSource = RTrim(DataSource)
			
			'Remove any tabs and double spaces
			DO WHILE INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": LOOP
			DO WHILE INSTR(DataSource, "  ") <> 0: Replace DataSource, "  ", " ": LOOP
			
			'Load line
			IF T = 0 THEN
				
	MultiCommand:
	
				'Make adjustments to line if needed
				
				'Convert single-line IFs to multiple line
				IF INSTR(DataSource, "IF") <> 0 AND INSTR(DataSource, "THEN") <> 0 AND LEN(DataSource) > INSTR(DataSource, "THEN") + 3 THEN
					Replace DataSource, "THEN", "THEN:"
					If INSTR(DataSource, " ELSE ") <> 0 Then Replace DataSource, " ELSE ", " ELSE: "
					DataSource = DataSource + ": END IF"
				END IF
				
				'Check that the IF has a THEN
				IF Left(DataSource, 3) = "IF " AND INSTR(DataSource, "THEN") = 0 THEN
					LogError Message("NoThen"), ";?F" + Str(RF) + "L" + Str(LC) + "?"
				END IF
				
				'Replace <> with ~ (not equal)
				Do While INSTR(DataSource, "<>") <> 0: Replace DataSource, "<>", "~": Loop
				'Replace => with } (equal or greater)
				Do While INSTR(DataSource, "=>") <> 0: Replace DataSource, "=>", "}": Loop
				Do While INSTR(DataSource, ">=") <> 0: Replace DataSource, ">=", "}": Loop
				'Replace =< with { (less or equal)
				Do While INSTR(DataSource, "=<") <> 0: Replace DataSource, "=<", "{": Loop
				Do While INSTR(DataSource, "<=") <> 0: Replace DataSource, "<=", "{": Loop
				
				'Convert DIM Value AS BIT to #define Value BitTemp.BVC
				IF Left(DataSource, 4) = "DIM " Then
					'DIM, bit variable
					If INSTR(DataSource, "AS BIT") <> 0 THEN
						Value = Mid(DataSource, INSTR(DataSource, " ") + 1)
						Value = Trim(Left(Value, INSTR(Value, "AS BIT") - 1))
						
						'DataSource = "#DEFINE " + Value + " SYSBITVAR" + Str(INT(BVC / 8)) + "." + Str(BVC-INT(BVC/8)*8) 
						Do While Value <> ""
							ConstName = Value
							If Instr(ConstName, ",") <> 0 Then
								ConstName = Trim(Left(ConstName, Instr(ConstName, ",") - 1))
								Value = Trim(Mid(Value, Instr(Value, ",") + 1))
							Else
								Value = ""
							End If
							
							T = 0
							FOR CE = 1 TO DFC
								IF ConstName = gcDEF(CE, 1) THEN T = 1: EXIT FOR
							NEXT
							IF T = 0 THEN
								DFC = DFC + 1
								gcDEF(DFC, 1) = ConstName
								gcDEF(DFC, 2) = "SYSBITVAR" + Str(INT(BVC / 8)) + "." + Str(BVC MOD 8) 'Str(BVC-INT(BVC/8)*8)
								gcDEF(DFC, 3) = Str(RF)
								CheckConstName ConstName, Origin
								'Define the variable
								AddVar "SYSBITVAR" + Str(INT(BVC / 8)), "BYTE", 1, 0, "REAL", Origin
							END IF
							
							BVC = BVC + 1
						Loop
						
					END IF
				End If
				
				'Convert WORD FUNCTION x to FUNCTION x AS WORD
				IF Left(DataSource, 14) = "WORD FUNCTION " THEN
					FName = Trim(Mid(DataSource, 15))
					DataSource = "FUNCTION " + FName + " AS WORD"
				End If
				
				'Check/fix binary and hex notation
				'Convert H' ' to 0x
				IF INSTR(DataSource, "H'") <> 0 THEN
					Replace DataSource, "H'", "0x"
					Replace DataSource, "'", ""
				END IF
				'Convert 0b to B' '
				IF WholeINSTR(DataSource, "0B") > 0 Then
					T = InStr(DataSource, "0B")
					If IsDivider(Mid(DataSource, T - 1, 1)) Then
						Replace DataSource, "0B", "B'"
						T = LEN(DataSource) + 1
						For SL = INSTR(DataSource, "B'") + 2 TO LEN(DataSource)
							Temp = Mid(DataSource, SL, 1)   
							If IsDivider(Temp) Then T = SL: Exit For
						Next
						LTemp = Left(DataSource, T - 1)
						RTemp = ""
						IF T < LEN(DataSource) Then RTemp = Mid(DataSource, T)
						DataSource = LTemp + "'" + RTemp
					End If
				END IF
				
				'Remove any tabs and double spaces (again)
				DO WHILE INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": LOOP
				DO WHILE INSTR(DataSource, "  ") <> 0: Replace DataSource, "  ", " ": LOOP
				
				'Decide if the line read is part of a sub or not
				IF Left(DataSource, 4) = "SUB " Or Left(DataSource, 9) = "FUNCTION " Or Left(DataSource, 6) = "MACRO " Then
					S = 1
					
					'0 = Sub, 1 = Function, 2 = Macro
					FoundFunction = 0
					FoundMacro = 0
					Do While Left(DataSource, 4) = "SUB " Or Left(DataSource, 9) = "FUNCTION " Or Left(DataSource, 6) = "MACRO "
						If Left(DataSource, 4) = "SUB " Then
							DataSource = Trim(Mid(DataSource, 4))
						ElseIf Left(DataSource, 9) = "FUNCTION " Then
							DataSource = Trim(Mid(DataSource, 9))
							FoundFunction = -1
						ElseIf Left(DataSource, 6) = "MACRO " Then
							DataSource = Trim(Mid(DataSource, 6))
							FoundMacro = -1
						End If
					Loop
					
					LCS = 0
					'Check for function type
					If FoundFunction Then
						'Also need to remove type def from line
						NewFNType = "BYTE"
						For T = Len(DataSource) To 1 Step -1
							'Found a ), any ASes before this will be for params
							If Mid(DataSource, T, 1) = ")" Then Exit For
							'Found an AS, get the type
							If Mid(DataSource, T, 4) = " AS " Then
								NewFNType = Trim(Mid(DataSource, T + 4))
								DataSource = Trim(Left(DataSource, T))
								Exit For
							End If
						Next
					End If
					'Get sub/function name
					CurrentSub = DataSource
					If INSTR(CurrentSub, "(") <> 0 Then CurrentSub = Trim(Left(CurrentSub, INSTR(CurrentSub, "(") - 1))
					
					NR = 0
					If INSTR(DataSource, "#NR") <> 0 Then
						NR = -1
						Replace DataSource, "#NR", ""
					End If
					If FoundFunction Then NR = -1
					
					SBC += 1
					Subroutine(SBC) = NewSubroutine(CurrentSub)
					CurrPos = Subroutine(SBC)->CodeStart
					With *Subroutine(SBC)
						.SourceFile = RF	'Source file
						
						'Function or macro?
						.IsMacro = FoundMacro
						.IsFunction = FoundFunction
						If FoundFunction Then
							.ReturnType = NewFNType
							'If length specified in type name, remove from function type
							'(Length is only needed when adding the variable)
							If InStr(.ReturnType, "*") <> 0 Then
								.ReturnType = Trim(Left(.ReturnType, InStr(.ReturnType, "*") - 1))
							End If
							AddVar .Name, NewFNType, 1, Subroutine(SBC), "REAL", ";?F" + Str(RF) + "L" + Str(LC) + "S" + Str(SBC) + "?"
						End If
						
						'Is sub overloaded?
						.Overloaded = 0
						For T = 1 TO SBC - 1
							If Subroutine(T)->Name = .Name Then
								Subroutine(T)->Overloaded = -1
								.Overloaded = -1
								Exit For
							End If
						Next
						
						'Get parameters
						SubInType = ""
						Temp = Trim(Mid(DataSource, 4))
						IF INSTR(Temp, "(") <> 0 THEN
							Temp = Mid(Temp, INSTR(Temp, "(") + 1)	 
							FOR T = LEN(Temp) TO 1 STEP -1
								IF Mid(Temp, T, 1) = ")" THEN Temp = Left(Temp, T - 1): EXIT FOR
							NEXT
							
							Do While Instr(Temp, ",") <> 0
								.ParamCount += 1
								.Params(.ParamCount) = GetSubParam(Left(Temp, Instr(Temp, ",") - 1), NR)
								Temp = Mid(Temp, Instr(Temp, ",") + 1)
							Loop
							If Temp <> "" Then
								.ParamCount += 1
								.Params(.ParamCount) = GetSubParam(Temp, NR)
							End If
						End If
						
						'Find any large vars used as parameters
						For T = 1 To .ParamCount
							With .Params(T)
								If .Type = "STRING" Then
									AddVar .Name, .Type, 1, Subroutine(SBC), "POINTER", ";?F" + Str(RF) + "L" + Str(LC) + "S" + Str(SBC) + "?"
								ElseIf Instr(.Type, "()") <> 0 Then
									Temp = .Type
									Replace Temp, "()", ""
									AddVar .Name, Temp, 2, Subroutine(SBC), "POINTER", ";?F" + Str(RF) + "L" + Str(LC) + "S" + Str(SBC) + "?"
								Else
									AddVar .Name, .Type, 1, Subroutine(SBC), "REAL", ";?F" + Str(RF) + "L" + Str(LC) + "S" + Str(SBC) + "?"
								End If
							End With
						Next
						
					End With
					
					GOTO LoadNextLine
				END IF
				
				If Left(DataSource, 7) = "END SUB" OR Left(DataSource, 9) = "END MACRO" Or Left(DataSource, 12) = "END FUNCTION" THEN
					S = 0
					CurrentSub = ""
					GOTO LoadNextLine
				END IF
				
				'Decide if the line read is part of a data table or not
				IF Left(DataSource, 6) = "TABLE " THEN
					'Open new data table
					S = 2
					
					'Get data from line
					GetTokens DataSource, LineToken(), LineTokens
					
					'Create table
					DataTables += 1
					With DataTable(DataTables)
						.Name = LineToken(2)
						.Origin = ";?F" + Str(RF) + "L" + Str(LC) + "S0" + "I" + Str(LCS) + "?"
						.StoreLoc = 0
						.Type = "BYTE"
						For T = 2 To LineTokens
							'Get store location
							If LineToken(T) = "STORE" Then
								If LineToken(T + 1) = "PROGRAM" Then
									.StoreLoc = 0
								ElseIf LineToken(T + 1) = "DATA" Then
									.StoreLoc = 1
								Else
									Temp = Message("BadTableLocation")
									Replace Temp, "%loc%", LineToken(T + 1)
									LogError Temp, .Origin
								End If
							'Get type
							ElseIf LineToken(T) = "AS" Then
								.Type = LineToken(T + 1)
							End If
						Next
						.Items = 0
					End With
					
					GOTO LoadNextLine
				
				'End of table
				ElseIF Left(DataSource, 9) = "END TABLE" THEN
					S = 0
					GOTO LoadNextLine
				END IF
				
				'Does the command need to be inserted into the main routine, regardless of sub/not sub?
				ForceMain = 0
				Temp = ""
				
				If Left(DataSource, 1) = "#" Then
					'Automatic initialisation preparation
					IF Left(DataSource, 8) = "#STARTUP" THEN gcINC(RF, 2) = ";" + Trim(Mid(DataSource, 9)): GOTO LoadNextLine
					IF Left(DataSource, 7) = "#DEFINE" THEN DataSource = DataSource + "':" + Str(RF)
					
					If Left(DataSource, 8) = "#DEFINE " THEN ForceMain = 1
					IF Left(DataSource, 5) = "#OSC " THEN ForceMain = 1
					IF Left(DataSource, 8) = "#CONFIG " THEN ForceMain = 1
					IF Left(DataSource, 8) = "#OPTION " THEN ForceMain = 1
					IF Left(DataSource, 5) = "#MEM " THEN ForceMain = 1 'Not used
					IF Left(DataSource, 5) = "#RAM " THEN ForceMain = 1 'Not used
					IF Left(DataSource, 5) = "#INT " THEN ForceMain = 1 'Not used
					IF Left(DataSource, 6) = "#CHIP " THEN ForceMain = 1
				End If
				
				RestOfLine = ""
				IF INSTR(DataSource, ":") <> 0 AND Right(DataSource, 1) <> ":" AND Left(DataSource, 8) <> "#DEFINE " THEN
					'IF INSTR(DataSource, ":") > INSTR(DataSource, Chr(34)) AND INSTR(INSTR(DataSource, ":"), DataSource, Chr(34)) <> 0 THEN GOTO DontSplitLoad
					RestOfLine = LTrim(Mid(DataSource, INSTR(DataSource, ":") + 1))
					DataSource = RTrim(Left(DataSource, INSTR(DataSource, ":") - 1))
					
				END IF
				
				DontSplitLoad:
	
				'Remove LET commands
				IF Left(DataSource, 4) = "LET " THEN DataSource = Trim(Mid(DataSource, 5))
				
				'Remove SET if line contains =
				IF Left(DataSource, 4) = "SET " And INSTR(DataSource, "=") <> 0 THEN DataSource = Trim(Mid(DataSource, 5))
				
				IF DataSource = "EXIT FUNCTION" THEN DataSource = "EXIT SUB"
				
				'Convert WHILE and WEND to DO WHILE and LOOP
				IF Left(DataSource, 6) = "WHILE " THEN DataSource = "DO " + DataSource
				IF DataSource = "WEND" THEN DataSource = "LOOP"
				
				'Convert DO FOREVER To DO
				IF DataSource = "DO FOREVER" Then DataSource = "DO"
				
				'Convert LEFT and RIGHT in ROTATE command to LC or RC
				'Convert LEFT SIMPLE and RIGHT SIMPLE to L or R
				'(Prevents conflict with LEFT and RIGHT string functions
				If Left(DataSource, 7) = "ROTATE " Then
					If Right(DataSource, 6) = " RIGHT" Then
						DataSource = Left(DataSource, Len(DataSource) - 6) + " RC"
					ElseIf Right(DataSource, 5) = " LEFT" Then
						DataSource = Left(DataSource, Len(DataSource) - 5) + " LC"
					ElseIf Right(DataSource, 13) = " RIGHT SIMPLE" Then
						DataSource = Left(DataSource, Len(DataSource) - 13) + " R"
					ElseIf Right(DataSource, 12) = " LEFT SIMPLE" Then
						DataSource = Left(DataSource, Len(DataSource) - 12) + " L"
					End If
				End If
				
				'Replace ++, --, +=, -=
				IF INSTR(DataSource, "++") <> 0 THEN
					Value = Trim(Left(DataSource, INSTR(DataSource, "++") - 1))
					DataSource = Value + "=" + Value + "+1"
				END IF
				IF INSTR(DataSource, "--") <> 0 THEN
					Value = Left(DataSource, INSTR(DataSource, "--") - 1)  
					DataSource = Value + "=" + Value + "-1"
				END IF
				IF INSTR(DataSource, "+=") <> 0 THEN
					Value = Left(DataSource, INSTR(DataSource, "+=") - 1)
					Temp = Mid(DataSource, INSTR(DataSource, "+=") + 2)
					DataSource = Value + "=" + Value + "+" + Temp
				END IF
				IF INSTR(DataSource, "-=") <> 0 THEN
					Value = Left(DataSource, INSTR(DataSource, "-=") - 1)
					Temp = Mid(DataSource, INSTR(DataSource, "-=") + 2)
					DataSource = Value + "=" + Value + "-" + Temp
				END IF
				
				'Add tag to show origin of line, and make copy of line for preserve mode
				'Except for directives, ASM and labels
				IF (Left(DataSource, 1) <> "#" OR Left(DataSource, 8) = "#DEFINE ") AND Left(DataSource, 1) <> " " AND Right(DataSource, 1) <> ":" THEN
					'Add tag
					DataSource = DataSource + " ;?F" + Str(RF) + "L" + Str(LC) + "S" + Str(SBC * S) + "I" + Str(LCS) + "?"
					'Preserve
					If PreserveIn <> "" Then
						PCC += 1
						PreserveCode(PCC) = PreserveIn
						IF S = 0 OR ForceMain = 1 THEN MainCurrPos = LinkedListInsert(MainCurrPos, "PRESERVE " + Str(PCC))
						IF S = 1 AND ForceMain = 0 THEN CurrPos = LinkedListInsert(CurrPos, "PRESERVE " + Str(PCC))
						PreserveIn = ""
					End If
				END IF
				
				'If in sub and not forced to main, store line in sub
				IF S = 1 AND ForceMain = 0 Then
					CurrPos = LinkedListInsert(CurrPos, DataSource)
					Subroutine(SBC)->OriginalLOC += 1
					
				'We're in a data table, so add line to it
				ElseIf S = 2 THEN
					If INSTR(DataSource, ";") <> 0 Then DataSource = Trim(Left(DataSource, INSTR(DataSource, ";") - 1))
					
					With DataTable(DataTables) 
						
						'Check that data can be stored in table, upgrade table if it can't
						'Print .Type, DataSource, TypeOfValue(DataSource, 0)
						If CastOrder(TypeOfValue(DataSource, 0)) > CastOrder(.Type) Then
							.Type = TypeOfValue(DataSource, 0)
						End If
						
						.Items += 1
						.Item(.Items) = MakeDec(DataSource)
					End With
				
				'Not in data table, not in sub or forced to main, so store in main
				Else
					MainCurrPos = LinkedListInsert(MainCurrPos, DataSource)
					'Only count lines in main routine of first file
					If RF = 1 Then Subroutine(0)->OriginalLOC += 1
					
				End If
				
				CommandAdded:
				
				IF RestOfLine <> "" THEN DataSource = RestOfLine: GOTO MultiCommand
			END IF
	LoadNextLine:
		LOOP
		
		IF S = 1 THEN
			S = 0
		END IF
		CLOSE
		If VBS = 1 THEN
			CurrPerc += PercAdd
			If Int(CurrPerc) > Int(PercOld) Then
				PercOld = CurrPerc
				LOCATE , 60
				Print Int(CurrPerc);
				Print "%";
			End If
		End If

LoadNextFile:
 
	NEXT
	IF VBS = 1 THEN PRINT
	
	'Find compiler directives, except SCRIPT, ENDSCRIPT, IFDEF and ENDIF   
	IF VBS = 1 THEN
		PRINT SPC(5); Message("CompDirs");
	END IF
	
	PercOld = 0
	CurrPerc = 0.5
	PercAdd = 1 / APC * 100
	CurrPos = Subroutine(0)->CodeStart->Next
	Do While CurrPos <> 0
		
		Temp = CurrPos->Value
		
		IF Left(Temp, 1) = "#" AND INSTR(Temp, "IFDEF") = 0 AND INSTR(Temp, "IFNDEF") = 0 _
			                    AND InStr(Temp, "ENDIF") = 0 And Left(Temp, 4) <> "#IF " _
			                    AND InStr(Temp, "SCRIPT") = 0 AND INSTR(Temp, "ENDSCRIPT") = 0 THEN
			CurrPos = LinkedListDelete(CurrPos)
			
			IF Left(Temp, 7) = "#DEFINE" THEN
				Origin = ""
				IF INSTR(Temp, ";?F") <> 0 THEN 
					Origin = Mid(Temp, INSTR(Temp, ";?F"))
					Temp = RTrim(Left(Temp, INSTR(Temp, ";?F") - 1))
				END IF
				
				ConstName = Trim(Mid(Temp, INSTR(Temp, " ") + 1))
				Temp = RTrim(Left(ConstName, INSTR(ConstName, "'") - 1))
				
				IF INSTR(Temp, " ") = 0 THEN
					ConstName = Temp
					TempFile = ""
					Value = ""
				Else
					Value = Mid(ConstName, INSTR(ConstName, " ") + 1)
					TempFile = Trim(Mid(Value, INSTR(Value, "':") + 2))
					Value = Trim(Left(Value, INSTR(Value, "':") - 1))
					ConstName = Left(ConstName, INSTR(ConstName, " ") - 1)
				END IF
				
				'Calculate define value if = present
				IF Left(Value, 1) = "=" THEN
					Value = Mid(Value, 2)
					Calculate Value
					Value = Trim(Value)
				END IF
				
				'Check to see if define exists
				T = 0
				FOR CE = 1 TO DFC
					IF ConstName = gcDEF(CE, 1) THEN T = 1: EXIT FOR
				NEXT
				IF T = 0 THEN
					DFC = DFC + 1
					gcDEF(DFC, 1) = ConstName
					gcDEF(DFC, 2) = Value
					gcDEF(DFC, 3) = TempFile
					CheckConstName ConstName, Origin
				END IF
			
			ElseIF Left(Temp, 5) = "#CHIP" AND ChipName = "" THEN
				ChipName = Trim(Mid(Temp, 6))
				ChipMhz = 0
				If InStr(ChipName, ",") <> 0 Then
					ChipMhz = VAL(Mid(ChipName, INSTR(ChipName, ",") + 1))
					ChipName = Trim(Left(ChipName, INSTR(ChipName, ",") - 1))
				End If
				IF Left(UCase(ChipName), 3) = "PIC" THEN ChipName = Mid(ChipName, 4)
				IF Left(UCase(ChipName), 1) = "P" THEN ChipName = Mid(ChipName, 2)
			
			ElseIF Left(Temp, 4) = "#OSC" AND OSCType = "" THEN
				OSCType = Trim(Mid(Temp, 5))
			
			ElseIF Left(Temp, 7) = "#CONFIG" THEN
				Temp = Trim(Mid(Temp, 8))
				IF CONFIG <> "" THEN Temp = "," + Temp
				CONFIG = CONFIG + Temp
			
			ElseIF Left(Temp, 7) = "#OPTION" THEN
				Temp = Trim(Mid(Temp, 8))
				IF gcOPTION <> "" THEN Temp = "," + Temp
				gcOPTION = gcOPTION + Temp
				
			END IF
			
		END IF
		
		CurrPos = CurrPos->Next
	Loop
	
	'Check that a chip is specified, and exit if it is not
	IF ChipName = "" THEN
		LogError Message("NoChip")
		WriteErrorLog
		END
	END IF
	
	'Get chip data
	IF VBS = 1 THEN PRINT: PRINT SPC(5); Message("ReadChipData")
	ReadChipData
	
	'Correct clock speed
	CheckClockSpeed
	
	'Process #option
	ReadOptions(gcOPTION)
	
	'Prepare program memory page data
	PreparePageData
	
	'Display chip data
	IF VBS = 1 THEN
		PRINT
		PRINT SPC(10); Message("ChipS")
		PRINT SPC(15); Message("ChipM") + ChipName
		If ModePIC Then PRINT SPC(15); Message("ChipF") + "PIC (" + Trim(Str(ChipFamily)) + ")"
		If ModeAVR Then PRINT SPC(15); Message("ChipF") + "AVR"
		If ModeZ8 Then PRINT SPC(15); Message("ChipF") + "Z8"
		PRINT SPC(15); Message("ChipC") + Trim(Str(ChipMhz))
		PRINT SPC(15); Message("ChipR") + Trim(Str(ChipRam))
	END IF
	
	'Initialise built-in data, and prepare built-in subs
	PrepareBuiltIn
	
	'Find and run compiler scripts
	IF VBS = 1 THEN PRINT SPC(5); Message("RunScripts")
	RunScripts
	
	IF VBS = 1 THEN PRINT: PRINT SPC(5); Message("BuildMemoryMap")
	BuildMemoryMap
	
	'Remove any #IFDEFs that do not apply to the program
	IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("RemIfDefs")
	RemIfDefs
	
	'Replace constants with their values
	IF VBS = 1 THEN PRINT SPC(5); Message("RepDefs");
	ReplaceConstants
	IF VBS = 1 THEN PRINT
	
End SUB

SUB RemIfDefs
	'Remove IFDEFs for which the condition is false
	Dim As String Temp, TVar, Value, Cmd, OldCmd, UTemp
	Dim As Integer ForceMain, IL, DelMode, PMode, SV, FV, ConstFound, RecDetect
	Dim As Integer FC, DC, VF, SD, CheckValue, VC, TV, CD, EV, CurrSub
	
	Dim As LinkedListElement Pointer CurrLine, StartDel, EndDel, CurrPos
	
	'Need to scan through main program and all subs
	For CurrSub = 0 To SBC
		CurrLine = Subroutine(CurrSub)->CodeStart->Next
		
		IL = 0
		Do While CurrLine <> 0
			
	RemIfDef:
				Temp = CurrLine->Value
				UTemp = UCase(Temp)
				DelMode = 0 '1 if condition tests false, 2 if true
				PMode = 0 '0 if IFDEF, 1 if IFNDEF, 2 if IF
				
				IF (Left(UTemp, 7) = "#IFDEF " OR Left(UTemp, 8) = "#IFNDEF " Or Left(UTemp, 4) = "#IF ") AND IL = 0 THEN
					
					IF INSTR(Temp, ";") <> 0 THEN Temp = Left(Temp, INSTR(Temp, ";") - 1)
					StartDel = CurrLine
	  
					'Read condition
					If Left(UTemp, 4) = "#IF " Then
						Cmd = Trim(Mid(Temp, 5))
						PMode = 2
					ElseIf Left(UTemp, 7) = "#IFDEF " Then
						Cmd = Trim(Mid(Temp, 8))
						PMode = 0
					Else
						Cmd = Trim(Mid(Temp, 9))
						PMode = 1
					End If
					
					'IF
					If PMode = 2 Then
						OldCmd = ""
						RecDetect = 0
						'Print "Start:", Cmd
						Do While OldCmd <> Cmd
							OldCmd = Cmd
							Cmd = ReplaceConstantsLine(CheckSysVarDef(Cmd), 0)
							'Print "Now:", Cmd
							RecDetect += 1
							'If have looped too many times, there is probably a recursive define
							If RecDetect > 100 Then Exit Do
						Loop
						Calculate Cmd
						'Print "Result:", Cmd
						If Val(Cmd) = 0 Then
							DelMode = 1
						Else
							DelMode = 2
						End If
						
					'IFDEF/IFNDEF	
					Else
						'Test for SFR bit?
						IF INSTR(Cmd, "BIT(") <> 0 THEN
							'Get name of bit, and whether checking for presence or absence
							FV = 0: IF INSTR(Cmd, "NOBIT(") <> 0 THEN FV = 1
							Temp = Mid(Cmd, INSTR(Cmd, "(") + 1)
							Temp = Left(Temp, INSTR(Temp, ")") - 1)
							
							'Search for bit in list
							ConstFound = 0
							FOR FC = 1 TO SVBC
								IF SysVarBits(FC).Name = Temp THEN ConstFound = 1: EXIT FOR
							NEXT
							
							'Set DelMode appropriately
							If FV = 0 Then
								If ConstFound = 0 Then DelMode = 1 Else DelMode = 2
							Else
								If ConstFound = 0 Then DelMode = 2 Else DelMode = 1
							End If
							GOTO IfDefProcessed
						END IF
						
						'Test for SFR?
						IF INSTR(Cmd, "VAR(") <> 0 THEN
							'Get name of SFR, and checking mode
							FV = 0: IF INSTR(Cmd, "NOVAR(") <> 0 THEN FV = 1
							Temp = Mid(Cmd, INSTR(Cmd, "(") + 1)
							Temp = Left(Temp, INSTR(Temp, ")") - 1)
							
							'ConstFound = 0
							'FOR FC = 1 TO SVC
							'	IF SysVars(FC, 1) = Temp THEN ConstFound = 1: EXIT FOR
							'NEXT
							ConstFound = HasSFR(Temp)
							
							'Set DelMode appropriately
							If FV = 0 Then
								If ConstFound = 0 Then DelMode = 1 Else DelMode = 2
							Else
								If ConstFound = 0 Then DelMode = 2 Else DelMode = 1
							End If
							GOTO IfDefProcessed
						END IF
						
						'Test to see if one or all of several constants have been defined
						IF INSTR(Cmd, "ONEOF(") <> 0 OR INSTR(Cmd, "ALLOF(") <> 0 THEN
							FV = 0: IF INSTR(Cmd, "ALLOF(") <> 0 THEN FV = 1
							
							'Get list of defines to search for
							Temp = Mid(Cmd, INSTR(Cmd, "(") + 1)
							Temp = Left(Temp, INSTR(Temp, ")") - 1)
							DC = 0
							DO WHILE INSTR(Temp, ",")
								DC = DC + 1
								TempData(DC) = Trim(Left(Temp, INSTR(Temp, ",") - 1))
								Temp = Mid(Temp, INSTR(Temp, ",") + 1)
							LOOP
							DC = DC + 1
							TempData(DC) = Trim(Temp)
							
							'Search
							VF = 0
							FOR SD = 1 to DC
								FOR FC = 1 TO DFC
									IF gcDEF(FC, 1) = TempData(SD) THEN VF = VF + 1: EXIT FOR
								NEXT
							NEXT
		   
							'Decide outcome
							DelMode = 1 'Default, condition false
							IF FV = 0 AND VF > 0 THEN DelMode = 2 'Needed at least 1, 1 found
							IF FV = 1 AND VF = DC THEN DelMode = 2 'Needed all, found all
							GOTO IfDefProcessed
						END IF
						
						'All functions have been checked, so must now check plain constants
						'Does the value of the constant need to be checked?
						CheckValue = 0
						IF INSTR(Cmd, " ") <> 0 THEN
							CheckValue = 1
							Value = Mid(Cmd, INSTR(Cmd, " ") + 1)
							Cmd = Left(Cmd, INSTR(Cmd, " ") - 1)
						END IF
						
						'Don't check value, just see if constant exists
						IF CheckValue = 0 THEN
							DelMode = 1
							FOR FC = 1 TO DFC
								IF gcDEF(FC, 1) = Cmd THEN DelMode = 2: EXIT FOR
							NEXT
							GOTO IfDefProcessed
						END IF
						
						'Check to see if value matches desired value
						'Get list of values
						VC = 0
						DO WHILE INSTR(Value, ",") <> 0
							VC = VC + 1
							TempData(VC) = Trim(Left(Value, INSTR(Value, ",") - 1))
							Value = Mid(Value, INSTR(Value, ",") + 1)
						LOOP
						VC = VC + 1
						TempData(VC) = Trim(Value)
						
						'Replace names of test constants with values
						FOR SD = 1 TO VC
							TV = 0
							FOR FV = 1 TO DFC
								IF UCase(TempData(SD)) = UCase(gcDEF(FV, 1)) THEN TV = FV: EXIT FOR
							NEXT
							IF TV <> 0 THEN TempData(SD) = gcDEF(FV, 2)
						NEXT
						
						'TVar = Cmd
						'ReplaceConstantsLine TVar
						'IF INSTR(TVar, ";") <> 0 THEN TVar = Left(TVar, INSTR(TVar, ";") - 1)
						'TVar = UCase(TVar)
						TVar = UCase(ReplaceConstantsLine(Cmd, 0))
						
						'Compare real and test values
						DelMode = 1
						FOR SD = 1 TO VC
							IF UCase(TempData(SD)) = TVar THEN DelMode = 2: EXIT FOR
						Next
					End If
					
					'Remove whatever needs to be removed
					'DelMode = 1 > condition is false > remove all
					'DelMode = 2 > condition is true > remove just #ifdef and #endif
	IfDefProcessed:
					'Swap DelMode when mode is #IFNDEF (1)
					If PMode = 1 Then
						If DelMode = 1 Then
							DelMode = 2
						ElseIf DelMode = 2 Then
							DelMode = 1
						End If
					End If
					
					'Find End Location
					CurrPos = StartDel
					EndDel = StartDel
					IL = 0
					Do While CurrPos <> 0
						Temp = UCase(CurrPos->Value)
						IF Left(Temp, 7) = "#IFDEF " Or Left(Temp, 8) = "#IFNDEF " Or Left(Temp, 4) = "#IF " Then
							 IL = IL + 1
						End If
						IF Left(Temp, 6) = "#ENDIF" THEN IL = IL - 1
						If IL = 0 Then EndDel = CurrPos: Exit Do
						CurrPos = CurrPos->Next
					Loop
					
					'Remove everything up to the #ENDIF
					IF DelMode = 1 THEN
						'DelLines SV, EV
						'DS = DS - 1
						CurrLine = StartDel->Prev
						LinkedListDeleteList(StartDel, EndDel)
					END IF
					
					'Remove the IFDEF and corresponding #ENDIF
					IF DelMode = 2 THEN
						'DelLine EV
						'DelLine SV
						'DS = DS - 1
						CurrLine = StartDel->Prev
						LinkedListDelete(StartDel)
						LinkedListDelete(EndDel)
					END IF
				
				END IF
			
			If CurrLine <> 0 Then CurrLine = CurrLine->Next
		Loop
	Next
		
END SUB

SUB ReplaceConstants
	
	Dim As String Origin, SourceData, TempData, LeftSection
	Dim As Integer RepCount, CurrSub
	Dim As LinkedListElement Pointer CurrLine
	Dim As Single CurrPerc, PercAdd, PercOld
	PercOld = 0
	CurrPerc = 0.5
	PercAdd = 1 / (SBC + 1) * 100
	
	'Replace constants with their values
	FOR CurrSub = 0 To SBC
		
		IF VBS = 1 Then
			CurrPerc += PercAdd
			If Int(CurrPerc) > Int(PercOld) Then
				PercOld = CurrPerc
				LOCATE , 60
				Print Int(CurrPerc);
				Print "%";
			End If
		End If
		
		CurrLine = Subroutine(CurrSub)->CodeStart->Next
		Do While CurrLine <> 0
			'Print "Getting Line"
			SourceData = CurrLine->Value
			'Print "Got Line: " + SourceData
			
			LeftSection = ""
			IF Left(SourceData, 3) = "ON " THEN LeftSection = "ON ": SourceData = Mid(SourceData, 4)
			
			RepCount = 0
			Do
				
				'Attempt to replace constants in line
				TempData = ReplaceConstantsLine(SourceData, -1)
				
				'Check if constants were replaced
				ConstReplaced = 0
				If TempData <> SourceData Then
					ConstReplaced = -1
					SourceData = TempData
					
					RepCount += 1
					'Prevent recursion from crashing compiler
					If RepCount > 100 Then
						Origin = ""
						IF INSTR(SourceData, ";?F") <> 0 THEN Origin = Mid(SourceData, INSTR(SourceData, ";?F"))
						If INSTR(Origin, ";STARTUP") <> 0 Then Origin = Left(Origin, INSTR(Origin, ";STARTUP") - 1)
						LogError Message("RecursiveDefine"), Origin
						TempData = SourceData
					End If
				End If
				
			Loop While ConstReplaced
			
			CurrLine->Value = LeftSection + SourceData
			CurrLine = CurrLine->Next
		Loop
	Next
	
End SUB

Function ReplaceConstantsLine (ByRef DataSourceIn As String, IncludeStartup As Integer) As String
	
	Dim As String ConstName, RCmd, DSUppercase, ConstFile, DataSource, Startup
	Dim As Integer CL, SCC, SearchStart
	
	ConstReplaced = 0
	DataSource = DataSourceIn
	DSUppercase = UCase(DataSource)
	Startup = ""
	
	For CL = 1 TO DFC
		ConstName = gcDEF(CL, 1)
		IF InStr(DSUppercase, ConstName) <> 0 THEN
			RCmd = gcDEF(CL, 2)
			WholeReplace DataSource, ConstName, RCmd
			
			If DSUppercase <> UCase(DataSource) Then
				ConstReplaced = -1
				If gcDEF(CL, 3) <> "" AndAlso InStr(Startup, gcDEF(CL, 3)) = 0 THEN
					If IncludeStartup Then
						Startup = Startup + ";STARTUP" + gcDEF(CL, 3)
					End If
				End If
				DSUppercase = UCase(DataSource)
			END IF
		END IF
	NEXT
	
	Return DataSource + Startup
End Function

SUB RunScripts
	
	Dim As String V1, Act, V2, Condition
	Dim As String CO, COCR, OCO, TempData, OtherData, MoreData
	Dim As String OutVar, Value, Origin
	Dim As Integer PD, ReadScript, CondFalse, TL, FC, CD
	Dim As Integer CurrSub
	Dim As LinkedListElement Pointer CurrLine
	
	Dim As LinkedListElement Pointer ScriptCode, ScriptCodePos
	
	'Read Scripts
	ScriptCode = LinkedListCreate
	ScriptCodePos = ScriptCode
	For CurrSub = 0 To SBC
		CurrLine = Subroutine(CurrSub)->CodeStart->Next
		Do While CurrLine <> 0
			
			IF CurrLine->Value = "#SCRIPT" THEN
				ReadScript = -1
				CurrLine = LinkedListDelete(CurrLine)
			ElseIf CurrLine->Value = "#ENDSCRIPT" Then
				ReadScript = 0
				CurrLine = LinkedListDelete(CurrLine)
			ElseIf ReadScript Then
				ScriptCodePos = LinkedListInsert(ScriptCodePos, CurrLine->Value)
				CurrLine = LinkedListDelete(CurrLine)
			End If
			
			If CurrLine <> 0 Then CurrLine = CurrLine->Next
		Loop
	Next
	
	'Execute Script
	ScriptCodePos = ScriptCode->Next
	Do While ScriptCodePos <> 0
		CO = ScriptCodePos->Value
		
		'Get origin
		Origin = ""
		IF InStr(CO, ";?F") <> 0 Then
			Origin = Trim(Mid(CO, InStr(CO, ";?F")))
			CO = Trim(Left(CO, INSTR(CO, ";?F") - 1))
		End If
		
		'Prepare a version of the command with constants replaced by values
		COCR = CO
		Do
			OCO = COCR
			COCR = ReplaceConstantsLine(COCR, 0)
		Loop While OCO <> COCR
		IF INSTR(COCR, ";?F") <> 0 THEN COCR = Left(COCR, INSTR(COCR, ";?F") - 1)
		
		'IF
		IF Left(CO, 3) = "IF " THEN
			Condition = Mid(COCR, 4)
			IF INSTR(Condition, "THEN") <> 0 THEN Condition = Left(Condition, INSTR(Condition, "THEN") - 1)
			Condition = Trim(Condition)
			CondFalse = -1
			Condition = CheckSysVarDef(Condition)
			If IsCalc(Condition) Then
				Calculate Condition
				If Val(Condition) <> 0 Then CondFalse = 0 
				
			'If no action, check for constant defined
			Else
				Condition = Mid(CO, 4)
				IF INSTR(Condition, "THEN") <> 0 THEN Condition = Left(Condition, INSTR(Condition, "THEN") - 1)
				Condition = Trim(Condition)
				FOR FC = 1 TO DFC
					IF gcDEF(FC, 1) = Condition THEN CondFalse = 0: EXIT FOR
				Next
			End If
			
			If CondFalse Then
				TL = 1
				ScriptCodePos = ScriptCodePos->Next
				DO WHILE TL > 0 And ScriptCodePos <> 0
					TempData = ScriptCodePos->Value
					IF Left(TempData, 3) = "IF " THEN TL = TL + 1
					IF Left(TempData, 6) = "END IF" THEN TL = TL - 1
					ScriptCodePos = ScriptCodePos->Next
				LOOP
				'GCBASIC has come to end of script without closing end if, display error!
				IF TL > 0 Then
					TempData = Message("NoENDIF")
					LogError(TempData, Origin)
				Else
					ScriptCodePos = ScriptCodePos->Prev
				End If
			END IF
		END IF
		
		'ERROR
		IF Left(CO, 6) = "ERROR " THEN
			TempData = Mid(COCR, 7)
			Do While INSTR(TempData, ";STRING") <> 0
				OtherData = Mid(TempData, INSTR(TempData, ";") + 1)
				OtherData = ";" + Left(OtherData, INSTR(OtherData, ";"))
				Replace TempData, OtherData, StringStore(VAL(Mid(OtherData, 8))).Value
			Loop
			Do While INSTR(TempData, "MSG(") <> 0
				OtherData = Mid(TempData, INSTR(TempData, "MSG("))
				OtherData = Left(OtherData, INSTR(OtherData, ")"))
				MoreData = MID(OtherData, 5, LEN(OtherData) - 5)
				Replace TempData, OtherData, Message(MoreData)
			Loop
			
			LogError TempData
		END IF
		
		'CALCULATE
		IF INSTR(CO, "=") <> 0 AND Left(CO, 3) <> "IF " THEN
			'Get data and output name
			OutVar = Trim(Left(CO, INSTR(CO, "=") - 1))
			Value = Trim(Mid(COCR, INSTR(COCR, "=") + 1))
			
			'Check if the data is a sum, and calculate if it is
			DO WHILE INSTR(Value, "&") <> 0: Replace Value, "&", "AND": LOOP
			
			If IsCalc(Value) THEN
				Calculate Value
				Value = Trim(UCase(Value))
			End If
			
			'Retrieve string/s
			Do While INSTR(Value, ";STRING") <> 0
				TempData = Mid(Value, INSTR(Value, ";") + 1)
				TempData = ";" + Left(TempData, INSTR(TempData, ";"))
				Replace Value, TempData, StringStore(VAL(Mid(TempData, 8))).Value
			Loop
			
			'Write the data to the output
			FC = 0
			For CD = 1 TO DFC
				If UCase(gcDEF(CD, 1)) = UCase(OutVar) Then FC = CD: Exit For
			Next
			If FC = 0 Then
				DFC += 1
				gcDEF(DFC, 1) = OutVar
				gcDEF(DFC, 2) = Trim(Value)
			Else
				gcDEF(FC, 2) = Trim(Value)
			End If
		End If
		
		'May have been forced to 0 by missing end if
		If ScriptCodePos <> 0 Then
			ScriptCodePos = ScriptCodePos->Next
		End If
	Loop
	
END SUB
