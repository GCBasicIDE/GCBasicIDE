'    Graphical LCD routines for the GCBASIC compiler
'    Copyright (C) 2012 Hugh Considine

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

'Notes:
' At present, only supports KS0108 controllers.

'Changes:
' 14/10/2012: First version

'Initialisation routine
#startup InitGLCD

'Hardware settings
'''@hardware All; Data Bus 0; GLCD_DB0; IO_Pin
'''@hardware All; Data Bus 1; GLCD_DB1; IO_Pin
'''@hardware All; Data Bus 2; GLCD_DB2; IO_Pin
'''@hardware All; Data Bus 3; GLCD_DB3; IO_Pin
'''@hardware All; Data Bus 4; GLCD_DB4; IO_Pin
'''@hardware All; Data Bus 5; GLCD_DB5; IO_Pin
'''@hardware All; Data Bus 6; GLCD_DB6; IO_Pin
'''@hardware All; Data Bus 7; GLCD_DB7; IO_Pin

'''@hardware All; Chip Select 1; GLCD_CS1; IO_Pin
'''@hardware All; Chip Select 2; GLCD_CS2; IO_Pin
'''@hardware All; Reset; GLCD_RESET; IO_Pin
'''@hardware All; Register Select; GLCD_RS; IO_Pin
'''@hardware All; Read/Write; GLCD_RW; IO_Pin
'''@hardware All; Enable; GLCD_ENABLE; IO_Pin

'Constants

'Subs
'''Clears the GLCD screen
Sub GLCDCLS
	'Clear screen
	For CurrPage = 0 to 7
		'Set page
		Set GLCD_RS Off
		Set GLCD_CS1 On
		Set GLCD_CS2 On
		GLCDWriteByte b'10111000' Or CurrPage
		
		'Clear columns
		For CurrCol = 0 to 63
			'Select column
			Set GLCD_RS Off
			GLCDWriteByte 64 Or CurrCol
			'Clear
			Set GLCD_RS On
			GLCDWriteByte 0
		Next
	Next
End Sub

'''Displays a message
'''@param PrintLocX X coordinate for message
'''@param PrintLocY Y coordinate for message
'''@param PrintData Message to display
Sub GLCDPrint(In PrintLocX, In PrintLocY, PrintData As String)
	PrintLen = PrintData(0)
	If PrintLen = 0 Then Exit Sub
	GLCDPrintLoc = PrintLocX
	
	'Write Data
	For SysPrintTemp = 1 To PrintLen
		GLCDDrawChar GLCDPrintLoc, PrintLocY, PrintData(SysPrintTemp)
		GLCDPrintLoc += 6
	Next
End Sub

'''Draws a character at the specified location
'''@hide
Sub GLCDDrawChar(In CharLocX, In CharLocY, In CharCode)
	
	'CharCode needs to have 16 subtracted, table starts at char 16 not char 0
	CharCode -= 15
	
	'Need to read characters from CharColn (n = 0:7) tables
	'(First 3, ie 0:2 are blank, so can ignore)
	For CurrCharCol = 1 to 5
		Select Case CurrCharCol
			Case 1: ReadTable GLCDCharCol3, CharCode, CurrCharVal
			Case 2: ReadTable GLCDCharCol4, CharCode, CurrCharVal
			Case 3: ReadTable GLCDCharCol5, CharCode, CurrCharVal
			Case 4: ReadTable GLCDCharCol6, CharCode, CurrCharVal
			Case 5: ReadTable GLCDCharCol7, CharCode, CurrCharVal
		End Select
		For CurrCharRow = 1 to 8
			PSet CharLocX + CurrCharCol, CharLocY + CurrCharRow, CurrCharVal.0
			Rotate CurrCharVal Right
		Next
	Next
End Sub

'''Draws a box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box border (0 = erase, 1 = draw, default is 1)
Sub Box(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour = 1)
	'Make sure that starting point (1) is always less than end point (2)
	If LineX1 > LineX2 Then
		GLCDTemp = LineX1
		LineX1 = LineX2
		LineX2 = GLCDTemp
	End If
	If LineY1 > LineY2 Then
		GLCDTemp = LineY1
		LineY1 = LineY2
		LineY2 = GLCDTemp
	End If
	
	'Draw lines going across
	For DrawLine = LineX1 To LineX2
		PSet DrawLine, LineY1, LineColour
		PSet DrawLine, LineY2, LineColour
	Next
	
	'Draw lines going down
	For DrawLine = LineY1 To LineY2
		PSet LineX1, DrawLine, LineColour
		PSet LineX2, DrawLine, LineColour
	Next
End Sub

'''Draws a filled box on the GLCD screen
'''@param LineX1 Top left corner X location
'''@param LineY1 Top left corner Y location
'''@param LineX2 Bottom right corner X location
'''@param LineY2 Bottom right corner Y location
'''@param LineColour Colour of box (0 = erase, 1 = draw, default is 1)
Sub FilledBox(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour = 1)
	'Make sure that starting point (1) is always less than end point (2)
	If LineX1 > LineX2 Then
		GLCDTemp = LineX1
		LineX1 = LineX2
		LineX2 = GLCDTemp
	End If
	If LineY1 > LineY2 Then
		GLCDTemp = LineY1
		LineY1 = LineY2
		LineY2 = GLCDTemp
	End If
	
	'Draw lines going across
	For DrawLine = LineX1 To LineX2
		For GLCDTemp = LineY1 To LineY2
			PSet DrawLine, GLCDTemp, LineColour
		Next
	Next
End Sub

'''Draws a line on the GLCD screen
'''@param LineX1 Starting X point of line
'''@param LineY1 Starting Y point of line
'''@param LineX2 Ending X point of line
'''@param LineY2 Ending Y point of line
'''@param LineColour Colour of line (0 = blank, 1 = show, default is 1)
Sub Line(In LineX1, In LineY1, In LineX2, In LineY2, Optional In LineColour = 1)
	'Draw a line using Bresenham's algorithm and calls to PSet
	Dim LineErr, LineErr2 As Integer
	
	'Calculate step sizes and differences between start and end points
	If LineX1 < LineX2 Then
		LineDiffX = LineX2 - LineX1
		LineStepX = 1
	Else
		LineDiffX = LineX1 - LineX2
		LineStepX = 255
	End If
	If LineY1 < LineY2 Then
		LineDiffY = LineY2 - LineY1
		LineStepY = 1
	Else
		LineDiffY = LineY1 - LineY2
		LineStepY = 255
	End If
	'Calculate initial error
	LineErr = LineDiffX - LineDiffY
	
	'Drawing loop
	Do
		'Draw point at current location
		PSet LineX1, LineY1, LineColour
		
		'If at ending point, quit
		If LineX1 = LineX2 Then
			If LineY1 = LineY2 Then Exit Sub
		End If
		
		'Calculate new point to draw
		LineErr2 = LineErr * 2
		If LineErr2 >= LineDiffY Then
			LineErr += LineDiffY
			LineX1 += LineStepX
		End If
		If LineErr2 <= LineDiffX Then
			LineErr += LineDiffX
			LineY1 += LineStepY
		End If
	Loop
	
End Sub

'''Draws a pixel on the GLCD
'''@param GLCDX X coordinate of pixel
'''@param GLCDY Y coordinate of pixel
'''@param GLCDState State of pixel (0 = erase, 1 = display)
Sub PSet(In GLCDX, In GLCDY, In GLCDState)
	'Set pixel at X, Y on LCD to State
	'X is 0 to 127
	'Y is 0 to 63
	'Origin in top left
	
	'Select screen half
	If GLCDX.6 = Off Then Set GLCD_CS2 On
	If GLCDX.6 = On Then Set GLCD_CS1 On: GLCDX -= 64
	
	'Select page
	CurrPage = GLCDY / 8
	Set GLCD_RS Off
	GLCDWriteByte b'10111000' Or CurrPage
	
	'Select column
	Set GLCD_RS Off
	GLCDWriteByte 64 Or GLCDX
	'Dummy read first
	Set GLCD_RS On
	GLCDDataTemp = GLCDReadByte
	'Read current data
	Set GLCD_RS On
	GLCDDataTemp = GLCDReadByte
	
	'Change data to set/clear pixel
	GLCDBitNo = GLCDY And 7
	If GLCDState.0 = 0 Then
		GLCDChange = 254
		Set C On
	Else
		GLCDChange = 1
		Set C Off
	End If
	Repeat GLCDBitNo
		Rotate GLCDChange Left
	End Repeat
	If GLCDState.0 = 0 Then
		GLCDDataTemp = GLCDDataTemp And GLCDChange
	Else
		GLCDDataTemp = GLCDDataTemp Or GLCDChange
	End If
	
	'Select correct column again
	Set GLCD_RS Off
	GLCDWriteByte 64 Or GLCDX
	'Write data back
	Set GLCD_RS On
	GLCDWriteByte GLCDDataTemp
	
	Set GLCD_CS1 Off
	Set GLCD_CS2 Off
	
End Sub

'''Write byte to LCD
'''@hide
Sub GLCDWriteByte (In LCDByte)
	
	Dim GLCDSaveRS As Bit
	Dim GLCDSaveCS2 As Bit
	
	'Wait until LCD is available
	GLCDSaveRS = GLCD_RS
	GLCDSaveCS2 = GLCD_CS2
	If GLCD_CS1 = 1 Then
		GLCD_CS2 = 0
	End If
	Set GLCD_RS Off
	Wait Until GLCDReadByte.7 = Off
	GLCD_RS = GLCDSaveRS
	GLCD_CS2 = GLCDSaveCS2
	
	'Set LCD data direction
	Set GLCD_RW Off
	
	'Set data pin directions
	Dir GLCD_DB7 Out
	Dir GLCD_DB6 Out
	Dir GLCD_DB5 Out
	Dir GLCD_DB4 Out
	Dir GLCD_DB3 Out
	Dir GLCD_DB2 Out
	Dir GLCD_DB1 Out
	Dir GLCD_DB0 Out
	
	'Set output data
	GLCD_DB7 = LCDByte.7
	GLCD_DB6 = LCDByte.6
	GLCD_DB5 = LCDByte.5
	GLCD_DB4 = LCDByte.4
	GLCD_DB3 = LCDByte.3
	GLCD_DB2 = LCDByte.2
	GLCD_DB1 = LCDByte.1
	GLCD_DB0 = LCDByte.0
	
	'Write
	Wait 2 us
	Set GLCD_ENABLE On
	Wait 2 us
	Set GLCD_ENABLE Off
	Wait 2 us
End Sub

'''Read byte from LCD
'''@hide
Function GLCDReadByte
	
	'Set data pin directions
	Dir GLCD_DB7 In
	Dir GLCD_DB6 In
	Dir GLCD_DB5 In
	Dir GLCD_DB4 In
	Dir GLCD_DB3 In
	Dir GLCD_DB2 In
	Dir GLCD_DB1 In
	Dir GLCD_DB0 In
	
	'Set LCD data direction
	Set GLCD_RW On
	
	'Read
	Set GLCD_ENABLE On
	Wait 2 us
	Set GLCD_ENABLE Off
	Wait 2 us
	
	'Get input data
	GLCDReadByte.7 = GLCD_DB7
	GLCDReadByte.6 = GLCD_DB6
	GLCDReadByte.5 = GLCD_DB5
	GLCDReadByte.4 = GLCD_DB4
	GLCDReadByte.3 = GLCD_DB3
	GLCDReadByte.2 = GLCD_DB2
	GLCDReadByte.1 = GLCD_DB1
	GLCDReadByte.0 = GLCD_DB0
	
End Function

'''Reset GLCD
'''@hide
Sub InitGLCD
	'Set pin directions
	Dir GLCD_RS Out
	Dir GLCD_RW Out
	Dir GLCD_ENABLE Out
	Dir GLCD_CS1 Out
	Dir GLCD_CS2 Out
	Dir GLCD_RESET Out
	
	'Reset
	Set GLCD_RESET Off
	Wait 1 ms
	Set GLCD_RESET On
	Wait 1 ms
	
	'Select both chips
	Set GLCD_CS1 On
	Set GLCD_CS2 On
	
	'Set on
	Set GLCD_RS Off
	GLCDWriteByte 63
	
	'Set Z to 0
	GLCDWriteByte 192
	
	'Deselect chips
	Set GLCD_CS1 Off
	Set GLCD_CS2 Off
	
	'Clear screen
	GLCDCLS
	
End Sub

'Character bitmaps for print routines
Table GLCDCharCol3
0
16
12
10
136
34
56
32
8
32
16
16
128
128
64
4
0
0
0
40
72
70
108
0
0
0
40
16
0
16
0
64
124
0
132
130
48
78
120
6
108
12
0
0
16
40
0
4
100
248
254
124
254
254
254
124
254
0
64
254
254
254
254
124
254
124
254
76
2
126
62
126
198
14
194
0
4
0
8
128
0
64
254
112
112
112
16
16
254
0
64
254
0
248
248
112
248
16
248
144
16
120
56
120
136
24
136
0
0
0
32
120
End Table

Table GLCDCharCol4
254
56
10
6
204
102
124
112
4
64
16
56
136
162
112
28
0
0
14
254
84
38
146
10
56
130
16
16
160
16
192
32
162
132
194
130
40
138
148
2
146
146
108
172
40
40
130
2
146
36
146
130
130
146
18
130
16
130
128
16
128
4
8
130
18
130
18
146
2
128
64
128
40
16
162
254
8
130
4
128
2
168
144
136
136
168
252
168
16
144
128
32
130
8
16
136
40
40
16
168
124
128
64
128
80
160
200
16
0
130
16
68
End Table

Table GLCDCharCol5
124
124
0
0
238
238
124
168
254
254
84
84
148
148
124
124
0
158
0
40
254
16
170
6
68
68
124
124
96
16
192
16
146
254
162
138
36
138
146
226
146
146
108
108
68
40
68
162
242
34
146
130
130
146
18
146
16
254
130
40
128
24
16
130
18
162
50
146
254
128
128
112
16
224
146
130
16
130
2
128
4
168
136
136
136
168
18
168
8
250
136
80
254
240
8
136
40
40
8
168
144
128
128
96
32
160
168
108
254
108
16
66
End Table

Table GLCDCharCol6
56
254
12
10
204
102
124
32
4
64
56
16
162
136
112
28
0
0
14
254
84
200
68
0
130
56
16
16
0
16
0
8
138
128
146
150
254
138
146
18
146
82
0
0
130
40
40
18
130
36
146
130
68
146
18
146
16
130
126
68
128
4
32
130
18
66
82
146
2
128
64
128
40
16
138
130
32
254
4
128
8
168
136
136
144
168
2
168
8
128
122
136
128
8
8
136
40
48
8
168
128
64
64
128
80
160
152
130
0
16
32
68
End Table

Table GLCDCharCol7
16
0
10
6
136
34
56
62
8
32
16
16
128
128
64
4
0
0
0
40
36
196
160
0
0
0
40
16
0
16
0
4
124
0
140
98
32
114
96
14
108
60
0
0
0
40
16
12
124
248
108
68
56
130
2
244
254
0
2
130
128
254
254
124
12
188
140
100
2
126
62
126
198
14
134
0
64
0
8
128
0
240
112
64
254
48
4
120
240
0
0
0
0
240
240
112
16
248
16
64
64
248
56
120
136
120
136
0
0
0
16
120
End Table

