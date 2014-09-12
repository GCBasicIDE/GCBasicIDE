'    Software I2C routines for the GCBASIC compiler
'    Copyright (C) 2009 Hugh Considine

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
'

'Initialisation routine
#startup InitI2C

'Constants (can change to suit application)
#define I2C_BIT_DELAY 2 us
#define I2C_CLOCK_DELAY 1 us
#define I2C_END_DELAY 1 us
#define I2C_MODE Master
'Uncomment to enable:
'#define I2C_USE_TIMEOUT

'Constants (shouldn't change)
#define I2C_DATA_HIGH Dir I2C_DATA In
#define I2C_DATA_LOW Dir I2C_DATA Out: Set I2C_DATA Off
#define I2C_CLOCK_HIGH Dir I2C_CLOCK In
#define I2C_CLOCK_LOW Dir I2C_CLOCK Out: Set I2C_CLOCK Off

#define I2CStopped I2COldState

'Subs
Sub InitI2C
	'Initialisation routine
	'Release data and clock lines
	I2C_DATA_HIGH
	I2C_CLOCK_HIGH
	
	'Set old state flag (slave mode)
	#if I2C_MODE = Slave
		I2COldState = 255
	#endif
	
End Sub

Function I2CStartOccurred
	'Check if a start condition has occurred since the last run of this function
	'Only used in slave mode
	
	#if I2C_MODE = Master
		I2CStartOccurred = TRUE
	#endif
	
	#if I2C_MODE = Slave
		I2CStartOccurred = FALSE
		
		If I2C_CLOCK = 1 Then
			'State 0, CK 1, DA 1
			If I2C_DATA = 1 Then
				I2CState = 0
			'State 1, CK 1, DA 0
			Else
				'Start happens when data drops while clock high, so:
				'start occurs on transition from CK 1 DA 1 (state 0) to CK 1 DA 0 (state 1)
				If I2COldState = 0 Then
					I2CStartOccurred = TRUE
				End If
				I2CState = 1
			End If
		Else
			'State 2 when CK 0
			I2CState = 2
		End If
		I2COldState = I2CState
		
	#endif
	
End Function

Sub I2CStart
	'Send I2C start condition
	'Only needed in master mode
	#if I2C_MODE = Master
		'Disable interrupts. Make sure to call I2CStop to re-enable
		IntOff
		
		'For start, need to drop data while clock high
		I2C_DATA_HIGH
		I2C_CLOCK_HIGH
		Wait I2C_END_DELAY
		
		I2C_DATA_LOW
		Wait I2C_CLOCK_DELAY
		I2C_CLOCK_LOW
	#endif
	
	'Wait for I2C start condition
	'Only needed in slave mode
	#if I2C_MODE = Slave
		I2CState = 255
		Do
			I2COldState = I2CState
			If I2C_CLOCK = 1 Then
				'State 0, CK 1, DA 1
				If I2C_DATA = 1 Then
					I2CState = 0
				'State 1, CK 1, DA 0
				Else
					'Start happens when data drops while clock high, so:
					'start occurs on transition from CK 1 DA 1 (state 0) to CK 1 DA 0 (state 1)
					If I2COldState = 0 Then
						Goto I2CStartReceived
					End If
					I2CState = 1
				End If
			Else
				'State 2 when CK 0
				I2CState = 2
			End If
			
		Loop
		
		I2CStartReceived:
		
		'Disable interrupts
		IntOff
	#endif
	
	
End Sub

Sub I2CStop
	
	'In master mode, send stop
	#if I2C_MODE = Master
		'For stop, need to raise data while clock high
		I2C_CLOCK_LOW
		I2C_DATA_LOW
		Wait I2C_END_DELAY
		
		I2C_CLOCK_HIGH
		Wait I2C_CLOCK_DELAY
		
		I2C_DATA_HIGH
		Wait I2C_END_DELAY
		
		'Have finished transfer, can re-enable interrupts
		IntOn
	#endif
	
	'In slave mode, just need to re-enable interrupt
	#if I2C_MODE = Slave
		
		IntOn
	#endif
	
End Sub

Sub I2CSend (In I2CByte, In I2CGetAck = TRUE)
	
	#if I2C_MODE = Master
		I2C_CLOCK_LOW
		Wait I2C_END_DELAY
		
		'Send highest bit first
		For I2CCurrByte = 1 to 8
			If I2CByte.7 = On Then
				I2C_DATA_HIGH
			Else
				I2C_DATA_LOW
			End If
			Rotate I2CByte Left
			
			Wait I2C_END_DELAY
			I2C_CLOCK_HIGH
			Wait While I2C_CLOCK = Off
			Wait I2C_CLOCK_DELAY
			I2C_CLOCK_LOW
			Wait I2C_END_DELAY
		Next
		
		If I2CGetAck Then
			
			I2C_DATA_HIGH
			Wait I2C_END_DELAY
			
			I2C_CLOCK_HIGH
			Wait While I2C_CLOCK = Off
			For I2CCurrByte = 0 to 100
				If I2C_DATA = 0 Then
					I2CGetAck = False
					'If data line goes low, ack has been received
					Goto I2CAckEnd
				End If
				Wait I2C_END_DELAY
			Next
			
			'No ack received
			I2CAckEnd:
			I2C_CLOCK_LOW
			Wait I2C_END_DELAY
			I2C_DATA_LOW
			Wait I2C_END_DELAY
		End If
		
		'Wait one bit length
		Wait I2C_BIT_DELAY
	#endif
	
	#if I2C_MODE = Slave
		
		'Wait for clock to drop
		#ifndef I2C_USE_TIMEOUT
			Wait Until I2C_CLOCK = Off
		#endif
		#ifdef I2C_USE_TIMEOUT
			I2CState = 0
			Do Until I2C_CLOCK = Off
				I2CState += 1
				If I2CState = 0 Then
					I2CStopped = True
					Exit Sub
				End If
			Loop
		#endif
		
		'Get data line
		I2C_DATA_LOW
		
		'Write bits
		I2CByte = 0
		For I2CCurrByte = 1 To 8
			'Clock will be low
			
			'Write bit
			If I2CByte.7 = On Then
				I2C_DATA_HIGH
			Else
				I2C_DATA_LOW
			End If
			Rotate I2CByte Left
			
			'Wait for clock pulse
			'Wait for clock to rise
			Wait Until I2C_CLOCK = On
			'Wait for clock to drop (end of bit)
			#ifndef I2C_USE_TIMEOUT
				Wait Until I2C_CLOCK = Off
			#endif
			#ifdef I2C_USE_TIMEOUT
				I2CState = 0
				Do Until I2C_CLOCK = Off
					I2CState += 1
					If I2CState = 0 Then
						I2CStopped = True
						Exit Sub
					End If
				Loop
			#endif
		Next
		
		'Release data line
		I2C_DATA_HIGH
		
		'Read Ack
		I2CStopped = FALSE
		If I2CGetAck Then
			
			'Wait for clock to go high
			Wait Until I2C_CLOCK = On
			'Read ack
			If I2C_DATA = On Then
				'If data on, have received NACK
				I2CStopped = TRUE
			End If
			'Wait for clock to go low
			#ifndef I2C_USE_TIMEOUT
				Wait Until I2C_CLOCK = Off
			#endif
			#ifdef I2C_USE_TIMEOUT
				I2CState = 0
				Do Until I2C_CLOCK = Off
					I2CState += 1
					If I2CState = 0 Then
						Exit Do
					End If
				Loop
			#endif
		End If
		
	#endif
End Sub

Sub I2CReceive (Out I2CByte, In I2CGetAck = TRUE)
	
	'Code for master mode
	#if I2C_MODE = Master
		'Need to generate clock while checking input
		I2C_CLOCK_LOW
		'Setting data high will make data pin input
		I2C_DATA_HIGH
		
		I2CByte = 0
		For I2CCurrByte = 1 To 8
			Wait I2C_END_DELAY
			I2C_CLOCK_HIGH
			
			'Clock stretching
			Wait While I2C_CLOCK = Off
			
			Wait I2C_END_DELAY
			Rotate I2CByte Left
			Set I2CByte.0 Off
			If I2C_DATA = On Then
				Set I2CByte.0 On
			End If
			Wait I2C_END_DELAY
			
			I2C_CLOCK_LOW
			Wait I2C_END_DELAY
			
		Next
		
		'Send Ack
		If I2CGetAck Then
			I2C_DATA_LOW
			Wait I2C_END_DELAY
			I2C_CLOCK_HIGH
			Wait While I2C_CLOCK = Off
			Wait I2C_CLOCK_DELAY
			I2C_CLOCK_LOW
			Wait I2C_END_DELAY
		End If
	#endif
	
	'Slave mode
	#if I2C_MODE = Slave
		'Assumes that start has just been received, so CK and DA high
		
		'Wait for clock to drop
		#ifndef I2C_USE_TIMEOUT
			Wait Until I2C_CLOCK = Off
		#endif
		#ifdef I2C_USE_TIMEOUT
			I2CState = 0
			Do Until I2C_CLOCK = Off
				I2CState += 1
				If I2CState = 0 Then
					I2CStopped = True
					Exit Sub
				End If
			Loop
		#endif
		
		'Read bits
		I2CByte = 0
		For I2CCurrByte = 1 To 8
			
			'Wait for clock to rise
			Wait Until I2C_CLOCK = On
			
			'Get bit from data pin
			Rotate I2CByte Left
			Set I2CByte.0 Off
			If I2C_DATA = On Then
				Set I2CByte.0 On
			End If
			
			'Wait for clock to drop (end of bit)
			#ifndef I2C_USE_TIMEOUT
				Wait Until I2C_CLOCK = Off
			#endif
			#ifdef I2C_USE_TIMEOUT
				I2CState = 0
				Do Until I2C_CLOCK = Off
					I2CState += 1
					If I2CState = 0 Then
						I2CStopped = True
						Exit Sub
					End If
				Loop
			#endif
		Next
		
		'Send Ack
		If I2CGetAck Then
			
			'Send ack by pulling data down for 1 clock pulse
			I2C_DATA_LOW
			Wait Until I2C_CLOCK = On
			#ifndef I2C_USE_TIMEOUT
				Wait Until I2C_CLOCK = Off
			#endif
			#ifdef I2C_USE_TIMEOUT
				I2CState = 0
				Do Until I2C_CLOCK = Off
					I2CState += 1
					If I2CState = 0 Then
						Exit Do
					End If
				Loop
			#endif
			I2C_DATA_HIGH
		End If
		
		I2CStopped = FALSE
		
	#endif
End Sub

Sub I2CWriteReg (I2CAdr, I2CReg, I2CValue)
	'Write to an I2C register
	
	'Ensure low bit clear (write)
	I2CAdr.0 = 0
	
	'Start
	I2CStart
	
	'Address
	I2CSend(I2CAdr)
	
	'Register
	I2CSend(I2CReg)
	
	'Data
	I2CSend(I2CValue)
	
	'Stop
	I2CStop
	
End Sub

Function I2CReadReg(I2CAdr, I2CReg)
	I2CReadReg = 0
	
	'Ensure low bit set (read)
	I2CAdr.0 = 1
	
	'Start
	I2CStart
	
	'Address
	I2CSend(I2CAdr)
	
	'Register
	I2CSend(I2CReg)
	
	'Data
	I2CReceive(I2CReadReg)
	
	'Stop
	I2CStop
	
End Function
