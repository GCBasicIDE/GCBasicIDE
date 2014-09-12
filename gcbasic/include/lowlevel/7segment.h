'    7 Segment LED/LCD display routines for Great Cow BASIC
'    Copyright (C) 2006-2009 Hugh Considine

'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.

'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.

'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

'********************************************************************************
'IMPORTANT:
'THIS FILE IS ESSENTIAL FOR SOME OF THE COMMANDS IN GCBASIC. DO NOT ALTER THIS FILE
'UNLESS YOU KNOW WHAT YOU ARE DOING. CHANGING THIS FILE COULD RENDER SOME GCBASIC
'COMMANDS UNUSABLE!
'********************************************************************************

'Changes:
' 16/6/2009: New connection modes, and now uses data tables to store codes

'When using DisplayPortx constants, the following setup is assumed:
'	PIC Pin		Display Segment
'	PORTname.0	A
'	PORTname.1	B
'	PORTname.2	C
'	 ...
'	PORTname.5	F
'	PORTname.6	G
'	PORTname.7	Decimal
'The constants will need to be set up as follows:
'#define DisplayPortA PORTx
'#define DisplayPortD PORTx
'If several displays are connected to the same port, then the corresponding
'DisplayPortx constants should be set to the same value.
'
'If any other setup has been used, then DISP_SEG_A, etc constants must be set:
'#define DISP_SEG_A PORTa.z
'#define DISP_SEG_B PORTa.x
'...
'#define DISP_SEG_G PORTa.x
'#define DISP_SEG_DOT PORTa.y
'Using DisplayPortx constants will give more efficient code, but is harder to
'arrange for. When using DISP_SEG_x constants, it is assumed that if multiple
'displays are used, they will all be connected to the same port (ie, seg A on
'display 1 connects to the same pin as seg A on display 2. DisplayPortx and 
'DISP_SEG_x constants should not both be set, as this will cause strange
'display results.

'Commands to enable individual displays
'Only used when several displays are multiplexed on the same port
'Can specify the commands, like so:
'#define DispSelectA nop
'#define DispSelectB nop
'#define DispSelectC nop
'#define DispSelectD nop
'Or the enable pins:
'#define DISP_SEL_1 PORTa.b
'...
'#define DISP_SEL_4 PORTa.b

#startup InitSevenSeg

Sub InitSevenSeg
	
	'Whole port mode
	#ifdef DisplayPortA
		Dir DisplayPortA Out
	#endif
	#ifdef DisplayPortB
		Dir DisplayPortB Out
	#endif
	#ifdef DisplayPortC
		Dir DisplayPortC Out
	#endif
	#ifdef DisplayPortD
		Dir DisplayPortD Out
	#endif
	
	'Individual bits
	#ifdef DISP_SEG_A
		Dir DISP_SEG_A Out
	#endif
	#ifdef DISP_SEG_B
		Dir DISP_SEG_B Out
	#endif
	#ifdef DISP_SEG_C
		Dir DISP_SEG_C Out
	#endif
	#ifdef DISP_SEG_D
		Dir DISP_SEG_D Out
	#endif
	#ifdef DISP_SEG_E
		Dir DISP_SEG_E Out
	#endif
	#ifdef DISP_SEG_F
		Dir DISP_SEG_F Out
	#endif
	#ifdef DISP_SEG_G
		Dir DISP_SEG_G Out
	#endif
	#ifdef DISP_SEG_DOT
		Dir DISP_SEG_DOT Out
	#endif
	
	'Selection pins
	#ifdef DISP_SEL_1
		Dir DISP_SEL_1 Out
	#endif
	#ifdef DISP_SEL_2
		Dir DISP_SEL_2 Out
	#endif
	#ifdef DISP_SEL_3
		Dir DISP_SEL_3 Out
	#endif
	#ifdef DISP_SEL_4
		Dir DISP_SEL_4 Out
	#endif
	
End Sub

'Numbers for display
Table SevenSegDispDigit
	63 '0
	6
	91
	79
	102
	109
	125
	7
	127
	111 '9
End Table

'Letters for display
'Letter is at ASC - 64, so A at 1, B at 2, etc
Table SevenSegDispLetter
	119 'A
	124 'B
	57 'C
	94 'D
	121 'E
	113 'F
	61 'G
	118 'H
	6 'I
	14 'J
	118 'K
	56 'L
	55 'M
	55 'N
	63 'O
	115 'P
	103 'Q
	80 'R
	109 'S
	7 'T
	62 'U
	62 'V
	62 'W
	118 'X
	110 'Y
	27 'Z
End Table

'Write integer between 0 and 9 inclusive
Sub DisplayValue(In DispPort, In DispChar)
	
	'Convert to code for output
	ReadTable SevenSegDispDigit, DispChar + 1, DispTemp
	
	'Select display and show integer
	DisplaySevenSeg
	
End Sub

'Write ASCII character
Sub DisplayChar(In DispPort, In DispChar)
	
	'Only accept letters A-Z
	'Space
	If DispChar = 32 Then DispTemp = 0: Goto ShowChar
	'Numbers
	If DispChar >= 48 And DispChar <= 57 Then
		ReadTable SevenSegDispDigit, DispChar - 48, DispTemp
		Goto ShowChar
	End If
	If DispChar < 65 Then Exit Sub
	'Convert to upper case
	If DispChar > 96 Then DispChar = DispChar - 32
	'Exit if not a letter
	If DispChar > 90 Then Exit Sub
	
	'Convert to code for output
	ReadTable SevenSegDispLetter, DispChar - 64, DispTemp
	
ShowChar:
	DisplaySevenSeg
End Sub

'Display the value on the seven segment display
Macro DisplaySevenSeg
	
	'Individual segment code
	#ifdef DISP_SEG_A
		
		'Clear current display
		Set DISP_SEG_A Off
		Set DISP_SEG_B Off
		Set DISP_SEG_C Off
		Set DISP_SEG_D Off
		Set DISP_SEG_E Off
		Set DISP_SEG_F Off
		Set DISP_SEG_G Off
		#ifdef DISP_SEG_DOT
			Set DISP_SEG_DOT Off
		#endif
		
		'Select new display
		'Commands to select
		#ifdef DispSelectA
		If DispPort = 1 Then DispSelectA
		#endif
		#ifdef DispSelectB
		If DispPort = 2 Then DispSelectB
		#endif
		#ifdef DispSelectC
		If DispPort = 3 Then DispSelectC
		#endif
		#ifdef DispSelectD
		If DispPort = 4 Then DispSelectD
		#endif
		
		'Pins to select
		'Turn all off
		#ifdef DISP_SEL_1
			Set DISP_SEL_1 Off
		#endif
		#ifdef DISP_SEL_2
			Set DISP_SEL_2 Off
		#endif
		#ifdef DISP_SEL_3
			Set DISP_SEL_3 Off
		#endif
		#ifdef DISP_SEL_4
			Set DISP_SEL_4 Off
		#endif
		'Turn appropriate pin back on
		#ifdef DISP_SEL_1
			If DispPort = 1 Then Set DISP_SEL_1 On
		#endif
		#ifdef DISP_SEL_2
			If DispPort = 2 Then Set DISP_SEL_2 On
		#endif
		#ifdef DISP_SEL_3
			If DispPort = 3 Then Set DISP_SEL_3 On
		#endif
		#ifdef DISP_SEL_4
			If DispPort = 4 Then Set DISP_SEL_4 On
		#endif
		
		'Show number on new display
		If DispTemp.0 = On Then DISP_SEG_A = On
		If DispTemp.1 = On Then DISP_SEG_B = On
		If DispTemp.2 = On Then DISP_SEG_C = On
		If DispTemp.3 = On Then DISP_SEG_D = On
		If DispTemp.4 = On Then DISP_SEG_E = On
		If DispTemp.5 = On Then DISP_SEG_F = On
		If DispTemp.6 = On Then DISP_SEG_G = On
		#ifdef DISP_SEG_DOT
			If DispTemp.7 = On Then DISP_SEG_DOT = On
		#endif
		
	#endif
	
	'Whole port code
	#ifndef DISP_SEG_A
		
		'Pins to select
		'Turn all off
		#ifdef DISP_SEL_1
			Set DISP_SEL_1 Off
		#endif
		#ifdef DISP_SEL_2
			Set DISP_SEL_2 Off
		#endif
		#ifdef DISP_SEL_3
			Set DISP_SEL_3 Off
		#endif
		#ifdef DISP_SEL_4
			Set DISP_SEL_4 Off
		#endif
		
		#ifdef DisplayPortA
			#ifdef OneOf(DisplayPortB, DisplayPortC, DisplayPortD)
				If DispPort = 1 Then
			#endif
				#ifdef DispSelectA
					DispSelectA
				#endif
				#ifdef DISP_SEL_1
					Set DISP_SEL_1 On
				#endif
				DisplayPortA = DispTemp
			#ifdef OneOf(DisplayPortB, DisplayPortC, DisplayPortD)
				End If
			#endif
		#endif
		#ifdef DisplayPortB
			If DispPort = 2 Then
				#ifdef DispSelectB
					DispSelectB
				#endif
				#ifdef DISP_SEL_2
					Set DISP_SEL_2 On
				#endif
				DisplayPortB = DispTemp
			End If
		#endif
		#ifdef DisplayPortC
			If DispPort = 3 Then
				#ifdef DispSelectC
					DispSelectC
				#endif
				#ifdef DISP_SEL_3
					Set DISP_SEL_3 On
				#endif
				DisplayPortC = DispTemp
			End If
		#endif
		#ifdef DisplayPortD
			If DispPort = 4 Then
				#ifdef DispSelectD
					DispSelectD
				#endif
				#ifdef DISP_SEL_4
					Set DISP_SEL_4 On
				#endif
				DisplayPortD = DispTemp
			End If
		#endif
	#endif
End Macro
