'    PS/2 keyboard/mouse routines for Great Cow BASIC
'    Copyright (C) 2006 - 2010 Hugh Considine

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
' 31/1/2010: Added write support, and bus inhibit when not ready for data

'#define PS2Data SysTemp.0 'Set to data pin
'#define PS2Clock SysTemp.0 'Set to clock pin

#startup InitPS2

'Flags
#define PS2KeyShift PS2Flags.0
#define PS2KeyCtrl PS2Flags.1
#define PS2KeyAlt PS2Flags.2
#define PS2NumLock PS2Flags.3
#define PS2CapsLock PS2Flags.4
#define PS2ShiftLock PS2Flags.5
#define PS2Extended PS2Flags.6
#define PS2Release PS2Flags.7

'Initialise
sub InitPS2
	'Inhibit device
	DIR PS2Clock Out
	DIR PS2Data IN
	Set PS2Clock Off
	
end sub

'Return ASCII value of input key
function INKEY
	'Initialise
	INKEY = 0
	
	PS2GetAnotherKey:
	
	'Get key scan code
	ScanCode = PS2ReadByte
	if ScanCode = 0 then exit function
	
	'Set flags
	PS2Extended = 0
	PS2Release = 0
	
	'2-byte key?
	if ScanCode = 0xE0 then
		PS2Extended = 1
		Do
			ScanCode = PS2ReadByte
		Loop While ScanCode = 0
	end if
	
	'Key released?
	If ScanCode = 0xF0 Then
		PS2Release = 1
		Do
			ScanCode = PS2ReadByte
		Loop While ScanCode = 0
	End If
	
	'Extended chars
	If PS2Extended Then
		
		
	'Non-extended chars
	Else
		'Shift pressed/released
		If ScanCode = 0x12 or ScanCode = 0x59 then
			If PS2Release Then
				PS2KeyShift = 0
			Else
				PS2KeyShift = 1
			End If
			Goto PS2GetAnotherKey
		End If
		
		'Ctrl pressed/released
		If ScanCode = 0x14 then
			If PS2Release Then
				PS2KeyCtrl = 0
			Else
				PS2KeyCtrl = 1
			End If
			Goto PS2GetAnotherKey
		End If
		
		'Alt pressed/released
		If ScanCode = 0x11 then
			If PS2Release Then
				PS2KeyAlt = 0
			Else
				PS2KeyAlt = 1
			End If
			Goto PS2GetAnotherKey
		End If
		
		If PS2Release = 0 Then
			'Caps Lock pressed
			if ScanCode = 0x58 then 
				If PS2CapsLock Then
					PS2CapsLock = 0
				Else
					PS2CapsLock = 1
				End If
				PS2SyncKBLeds
				Goto PS2GetAnotherKey
			end if
			
			'Num lock pressed
			if ScanCode = 0x77 then 
				If PS2NumLock Then
					PS2NumLock = 0
				Else
					PS2NumLock = 1
				End If
				PS2SyncKBLeds
				Goto PS2GetAnotherKey
			end if
			
			'Scroll lock pressed
			if ScanCode = 0x7E then 
				If PS2ScrollLock Then
					PS2ScrollLock = 0
				Else
					PS2ScrollLock = 1
				End If
				PS2SyncKBLeds
				Goto PS2GetAnotherKey
			end if
			
			'Translate scan code to ASCII
			'Control Chars
			if ScanCode = 0x5A then INKEY = 13	'Enter
			if ScanCode = 0x66 then INKEY = 8	'Backspace
			if ScanCode = 0x76 then INKEY = 27	'Esc
			if ScanCode = 0x29 then INKEY = 32	'Space
			
			'Arrows
			
			
			'Numbers
			If PS2KeyShift Then
				if ScanCode = 0x16 then INKEY = "!"
				if ScanCode = 0x1E then INKEY = "@"
				if ScanCode = 0x26 then INKEY = "#"
				if ScanCode = 0x25 then INKEY = "$"
				if ScanCode = 0x2E then INKEY = "%"
				if ScanCode = 0x36 then INKEY = "^"
				if ScanCode = 0x3D then INKEY = "&"
				if ScanCode = 0x3E then INKEY = "*"
				if ScanCode = 0x46 then INKEY = "("
				if ScanCode = 0x45 then INKEY = ")"
			Else
				if ScanCode = 0x16 then INKEY = 49
				if ScanCode = 0x1E then INKEY = 50
				if ScanCode = 0x26 then INKEY = 51
				if ScanCode = 0x25 then INKEY = 52
				if ScanCode = 0x2E then INKEY = 53
				if ScanCode = 0x36 then INKEY = 54
				if ScanCode = 0x3D then INKEY = 55
				if ScanCode = 0x3E then INKEY = 56
				if ScanCode = 0x46 then INKEY = 57
				if ScanCode = 0x45 then INKEY = 48
			End If
			
			'Letters
			if ScanCode = 0x1C then INKEY = 65 'A
			if ScanCode = 0x32 then INKEY = 66
			if ScanCode = 0x21 then INKEY = 67 'C
			if ScanCode = 0x23 then INKEY = 68
			if ScanCode = 0x24 then INKEY = 69 'E
			if ScanCode = 0x2B then INKEY = 70
			if ScanCode = 0x34 then INKEY = 71 'G
			if ScanCode = 0x33 then INKEY = 72
			if ScanCode = 0x43 then INKEY = 73 'I
			if ScanCode = 0x3B then INKEY = 74
			if ScanCode = 0x42 then INKEY = 75 'K
			if ScanCode = 0x4B then INKEY = 76
			if ScanCode = 0x3A then INKEY = 77 'M
			if ScanCode = 0x31 then INKEY = 78
			if ScanCode = 0x44 then INKEY = 79 'O
			if ScanCode = 0x4D then INKEY = 80
			if ScanCode = 0x15 then INKEY = 81 'Q
			if ScanCode = 0x2D then INKEY = 82
			if ScanCode = 0x1B then INKEY = 83 'S
			if ScanCode = 0x2C then INKEY = 84
			if ScanCode = 0x3C then INKEY = 85 'U
			if ScanCode = 0x2A then INKEY = 86
			if ScanCode = 0x1D then INKEY = 87 'W
			if ScanCode = 0x22 then INKEY = 88
			if ScanCode = 0x35 then INKEY = 89 'Y
			if ScanCode = 0x1A then INKEY = 90 'Z
			
			'If shift key and caps lock in same state, make lower case
			If PS2KeyShift = PS2CapsLock Then
				If INKEY >= 65 and INKEY <= 90 Then INKEY += 32
			End If
			
			'Symbols
			If PS2KeyShift Then
				if ScanCode = 0x0E then INKEY = "~"
				if ScanCode = 0x4E then INKEY = "_"
				if ScanCode = 0x55 then INKEY = "+"
				if ScanCode = 0x5D then INKEY = "|"
				if ScanCode = 0x4c then INKEY = ":"
				if ScanCode = 0x52 then INKEY = 34 '"
				if ScanCode = 0x41 then INKEY = "<"
				if ScanCode = 0x49 then INKEY = ">"
				if ScanCode = 0x4A then INKEY = "?"
				if ScanCode = 0x54 then INKEY = "{"
				if ScanCode = 0x5B then INKEY = "}"
			Else
				if ScanCode = 0x0E then INKEY = 96 '`
				if ScanCode = 0x4E then INKEY = 45 '-
				if ScanCode = 0x55 then INKEY = 61 '=
				if ScanCode = 0x5D then INKEY = 92 '\
				if ScanCode = 0x4c then INKEY = 59 ';
				if ScanCode = 0x52 then INKEY = 39 ''
				if ScanCode = 0x41 then INKEY = 44 ',
				if ScanCode = 0x49 then INKEY = 46 '.
				if ScanCode = 0x4A then INKEY = 47 '/
				if ScanCode = 0x54 then INKEY = "["
				if ScanCode = 0x5B then INKEY = "]"
			End If
			
		End If
	End If
	
end function

'''Synchronise LEDs on PS/2 keyboard with key status flags
Sub PS2SyncKBLeds
	'Get value
	PS2Value = 0
	If PS2NumLock Then PS2Value.1 = On
	If PS2CapsLock Then PS2Value.2 = On
	If PS2ScrollLock Then PS2Value.0 = On
	
	'Send LED command
	PS2WriteByte (0xED)
	PS2Parity = PS2ReadByte
	'Send value
	PS2WriteByte (PS2Value)
	PS2Parity = PS2ReadByte
	
End Sub

'''Keyboard LED setting routine
'''@param PS2Value Value to send. Bits 0-2 control LEDs
Sub PS2SetKBLeds (In PS2Value)
	'Send LED command
	PS2WriteByte (0xED)
	PS2Parity = PS2ReadByte
	'Send value
	PS2WriteByte (PS2Value)
	PS2Parity = PS2ReadByte
End Sub

function PS2ReadByte
	
	PS2ReadByte = 0
	
	'Release bus
	Dir PS2Clock In
	
	'Give device time to respond
	Wait 25 us
	
	'If no response after 200 us, exit
	PS2Bit = 0
	Do While PS2Clock = On
		If PS2Bit > 200 Then
			Goto PS2ReadByteDone
		End If
		Wait 10 us
		PS2Bit += 10
	Loop
	
	'Start Bit
	If PS2Data = ON Then Goto PS2ReadByteDone
	Wait Until PS2Clock = ON
	
	'8 data bits
	For PS2Bit = 1 to 8
		wait until PS2Clock = OFF 
		ROTATE PS2ReadByte RIGHT
		PS2ReadByte.7 = PS2Data
		wait until PS2Clock = ON
	Next
	
	'Parity bit
	wait until PS2Clock = OFF
	wait until PS2Clock = ON
	
	'End bit
	wait until PS2Clock = OFF
	wait until PS2Clock = ON
	
	'Inhibit bus
	PS2ReadByteDone:
	DIR PS2Clock Out
	Set PS2Clock Off
	Wait 100 us
	
	#ifdef PS2_DELAY
		Wait PS2_DELAY
	#endif
	
end function

sub PS2WriteByte (In PS2Byte)
	
	'Pull data down, then release clock
	Dir PS2Data Out
	Set PS2Data Off
	Wait 10 us
	Dir PS2Clock In
	
	'Wait up to 20 ms for device to pull clock down
	PS2Bit = 0
	Do While PS2Clock = On
		'Re-use PS2Parity to count 100 us
		For PS2Parity = 1 to 10
			Wait 10 us
			If PS2Clock = Off Then Exit Do
		Next
		PS2Bit += 1
		If PS2Bit > 200 Then Goto PS2WriteByteDone
	Loop
	
	'8 data bits
	PS2Parity = 0
	For PS2Bit = 1 to 8
		If PS2Byte.0 = Off then
			Set PS2Data off
		Else
			Set PS2Data on
			PS2Parity += 1
		End If
		ROTATE PS2Byte RIGHT
		Wait until PS2Clock = On
		wait until PS2Clock = Off
	Next
	
	'Parity
	If PS2Parity.0 = off then Set PS2Data on
	If PS2Parity.0 = on then Set PS2Data off
	wait until PS2Clock = On
	wait until PS2Clock = Off
	
	'Stop
	Dir PS2Data In
	
	'Ack
	Wait Until PS2Data = Off
	wait until PS2Clock = OFF
	wait until PS2Clock = ON
	
	'Inhibit bus
	PS2WriteByteDone:
	Dir PS2Clock Out
	Set PS2Clock Off
	Wait 100 us
	
	#ifdef PS2_DELAY
		Wait PS2_DELAY
	#endif
	
End Sub
