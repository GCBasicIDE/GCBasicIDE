'    Keypad routines for Great Cow BASIC
'    Copyright (C) 2006 Hugh Considine

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

'Note: This is designed for a 4x4 keypad. If using a 3x3 keypad, make sure that all
'      unused pins are pulled high.
'
'When KeypadPort is set, the following wiring configuration is expected:
' Col 1: Port.0
' Col 2: Port.1
' Col 3: Port.2
' Col 4: Port.3
' Row 1: Port.4
' Row 2: Port.5
' Row 3: Port.6
' Row 4: Port.7
'Alternately, the following can be set:
'KEYPAD_ROW_1
'KEYPAD_ROW_2
'KEYPAD_ROW_3
'KEYPAD_ROW_4
'KEYPAD_COL_1
'KEYPAD_COL_2
'KEYPAD_COL_3
'KEYPAD_COL_4
'Where the first 4 correspond to rows 1 through 4, and the next 4 to columns
'1 through 4.
'GCBASIC will set the rows, then read the columns

'Functions
'KeypadRaw	Returns 16 bit var, where each bit represents the status of a key
'KeypadData	Returns the value of the currently pressed key
'KeypadASCII	Returns the ASCII value of the selected letter

'Configuration
'#define KeypadPort SysKeypadTemp
#define KeypadReadDelay 10 us

'Pull up/pull down resistors used?
'Default is pull up, unless KEYPAD_PULLDOWN defined
'#define KEYPAD_PULLDOWN

#define KEY_A 10
#define KEY_B 11
#define KEY_C 12
#define KEY_D 13
#define KEY_STAR 14
#define KEY_HASH 15
#define KEY_NONE 255

#startup InitKeypad

Sub InitKeypad
	'Set direction when using KeypadPort
	#ifdef KeypadPort
		#ifdef PIC
			Dir KeypadPort b'00001111'
		#endif
		#ifdef AVR
			Dir KeypadPort b'11110000'
		#endif
	#endif
	
	'Set direction when using KEYPAD_xxx_y
	#ifdef KEYPAD_ROW_1
		Dir KEYPAD_ROW_1 Out
	#endif
	#ifdef KEYPAD_ROW_2
		Dir KEYPAD_ROW_2 Out
	#endif
	#ifdef KEYPAD_ROW_3
		Dir KEYPAD_ROW_3 Out
	#endif
	#ifdef KEYPAD_ROW_4
		Dir KEYPAD_ROW_4 Out
	#endif
	#ifdef KEYPAD_COL_1
		Dir KEYPAD_COL_1 In
	#endif
	#ifdef KEYPAD_COL_2
		Dir KEYPAD_COL_2 In
	#endif
	#ifdef KEYPAD_COL_3
		Dir KEYPAD_COL_3 In
	#endif
	#ifdef KEYPAD_COL_4
		Dir KEYPAD_COL_4 In
	#endif
End Sub

'Functions

Function KeypadRaw As Word
	
	KeypadRaw = 0
	#ifdef KeypadPort
		#ifdef KEYPAD_PULLDOWN
			KeypadPort = b'10000000'
			for KeyReadColumn = 1 to 4
				Wait KeypadReadDelay
				set C off
				For KeypadTemp = 1 to 4
					Rotate KeypadRaw Left
				next
				if KeypadPort.3 On then Set KeypadRaw.3 On
				if KeypadPort.2 On then Set KeypadRaw.2 On
				if KeypadPort.1 On then Set KeypadRaw.1 On
				if KeypadPort.0 On then Set KeypadRaw.0 On
				set C off
				Rotate KeypadPort right
			next
			KeypadPort = 0
			Wait KeypadReadDelay
		#endif
		#ifndef KEYPAD_PULLDOWN
			KeypadPort = b'01110000'
			for KeyReadColumn = 1 to 4
				Wait KeypadReadDelay
				set C off
				For KeypadTemp = 1 to 4
					Rotate KeypadRaw Left
				next
				if KeypadPort.3 Off then Set KeypadRaw.3 On
				if KeypadPort.2 Off then Set KeypadRaw.2 On
				if KeypadPort.1 Off then Set KeypadRaw.1 On
				if KeypadPort.0 Off then Set KeypadRaw.0 On
				set C on
				Rotate KeypadPort right
			next
			KeypadPort = 0
			Wait KeypadReadDelay
			
		#endif
	#endif
	
	#ifndef KeypadPort
		#ifdef KEYPAD_ROW_1
			#ifdef KEYPAD_PULLDOWN
				for KeyReadColumn = 1 to 4
					'Set all rows off
					#ifdef KEYPAD_ROW_1
						Set KEYPAD_ROW_1 Off
					#endif
					#ifdef KEYPAD_ROW_2
						Set KEYPAD_ROW_2 Off
					#endif
					#ifdef KEYPAD_ROW_3
						Set KEYPAD_ROW_3 Off
					#endif
					#ifdef KEYPAD_ROW_4
						Set KEYPAD_ROW_4 Off
					#endif
					'Set appropriate row on
					#ifdef KEYPAD_ROW_1
						If KeyReadColumn = 1 Then Set KEYPAD_ROW_1 On
					#endif
					#ifdef KEYPAD_ROW_2
						If KeyReadColumn = 2 Then Set KEYPAD_ROW_2 On
					#endif
					#ifdef KEYPAD_ROW_3
						If KeyReadColumn = 3 Then Set KEYPAD_ROW_3 On
					#endif
					#ifdef KEYPAD_ROW_4
						If KeyReadColumn = 4 Then Set KEYPAD_ROW_4 On
					#endif
					Wait KeypadReadDelay
					For KeypadTemp = 1 to 4
						Set C off
						Rotate KeypadRaw Left
					Next
					#ifdef KEYPAD_COL_1
						if KEYPAD_COL_1 = On then Set KeypadRaw.3 On
					#endif
					#ifdef KEYPAD_COL_2
						if KEYPAD_COL_2 = On then Set KeypadRaw.2 On
					#endif
					#ifdef KEYPAD_COL_3
						if KEYPAD_COL_3 = On then Set KeypadRaw.1 On
					#endif
					#ifdef KEYPAD_COL_4
						if KEYPAD_COL_4 = On then Set KeypadRaw.0 On
					#endif
				Next

			#endif
			
			#ifndef KEYPAD_PULLDOWN
				for KeyReadColumn = 1 to 4
					'Set all rows off
					#ifdef KEYPAD_ROW_1
						Set KEYPAD_ROW_1 On
					#endif
					#ifdef KEYPAD_ROW_2
						Set KEYPAD_ROW_2 On
					#endif
					#ifdef KEYPAD_ROW_3
						Set KEYPAD_ROW_3 On
					#endif
					#ifdef KEYPAD_ROW_4
						Set KEYPAD_ROW_4 On
					#endif
					'Set appropriate row on
					#ifdef KEYPAD_ROW_1
						If KeyReadColumn = 1 Then Set KEYPAD_ROW_1 Off
					#endif
					#ifdef KEYPAD_ROW_2
						If KeyReadColumn = 2 Then Set KEYPAD_ROW_2 Off
					#endif
					#ifdef KEYPAD_ROW_3
						If KeyReadColumn = 3 Then Set KEYPAD_ROW_3 Off
					#endif
					#ifdef KEYPAD_ROW_4
						If KeyReadColumn = 4 Then Set KEYPAD_ROW_4 Off
					#endif
					Wait KeypadReadDelay
					For KeypadTemp = 1 to 4
						Set C off
						Rotate KeypadRaw Left
					Next
					#ifdef KEYPAD_COL_1
						if KEYPAD_COL_1 = Off then Set KeypadRaw.3 On
					#endif
					#ifdef KEYPAD_COL_2
						if KEYPAD_COL_2 = Off then Set KeypadRaw.2 On
					#endif
					#ifdef KEYPAD_COL_3
						if KEYPAD_COL_3 = Off then Set KeypadRaw.1 On
					#endif
					#ifdef KEYPAD_COL_4
						if KEYPAD_COL_4 = Off then Set KeypadRaw.0 On
					#endif
				Next
			#endif
		#endif
	#endif

end function

function KeypadData
	Dim KeypadDataTemp As Word
	
	'Get raw key data
	KeypadDataTemp = KeypadRaw
	
	KeypadData = 255
	For KeypadTemp = 1 to 16
		if KeypadDataTemp.15 = On then
			'KeypadTemp = KeypadData
			ReadTable KeypadBitToCodeTable, KeypadTemp, KeypadData
		End If
		rotate KeyPadDataTemp left
	Next
	
' select case KeyPadTemp
'
'  case 1: KeypadData = 1
'  case 2: KeypadData = 2
'  case 3: KeypadData = 3
'  case 4: KeypadData = KEY_A
'
'  case 5: KeypadData = 4
'  case 6: KeypadData = 5
'  case 7: KeypadData = 6
'  case 8: KeypadData = KEY_B
'
'  case 9: KeypadData = 7
'  case 10: KeypadData = 8
'  case 11: KeypadData = 9
'  case 12: KeypadData = KEY_C
'
'  case 13: KeypadData = KEY_STAR
'  case 14: KeypadData = 0
'  case 15: KeypadData = KEY_HASH
'  case 16: KeypadData = KEY_D
'
'  case else: KeypadData = 255
' end select

end function

Table KeypadBitToCodeTable
	1
	2
	3
	10
	4
	5
	6
	11
	7
	8
	9
	12
	14
	0
	15
	13
End Table
