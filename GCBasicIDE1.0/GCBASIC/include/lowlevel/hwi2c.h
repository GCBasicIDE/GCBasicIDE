'    Hardware I2C routines for Great Cow BASIC
'    Copyright (C) 2010 Hugh Considine

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

'To make the PIC pause until it receives an SPI message while in slave mode, set the
'constant "WaitForSPI" at the start of the program. The value does not matter.

'SPI mode constants also used by hardware I2C:
'#define MasterFast 13
'#define Master 12
'#define MasterSlow 11
'#define SlaveSS 1
'#define Slave 0

'I2C Mode constants
#define Slave10 2
'use Slave for 7-bit slave mode and Master for master mode

'Baud rate in KHz (Change to desired frequency)
#define HI2C_BAUD_RATE 100

#script
	HI2C_BAUD_TEMP = int((ChipMhz * 1000000)/(4000 * HI2C_BAUD_RATE)) - 1
	'error I2CBaudTemp
	
	If PIC Then
		HI2CHasData = "BF = On"
	End If
	
#endscript

Sub HI2CMode (In HI2CCurrentMode)
	
	#ifdef PIC
		#ifndef Var(SSPCON1)
			#ifdef Var(SSPCON)
				Dim SSPCON1 Alias SSPCON
			#endif
		#endif
		
		set SSPSTAT.SMP on
		set SSPCON1.CKP on
		set SSPCON1.WCOL Off
		
		'Select mode and clock
		if HI2CCurrentMode = Master then
			set SSPCON1.SSPM3 on
			set SSPCON1.SSPM2 off
			set SSPCON1.SSPM1 off
			set SSPCON1.SSPM0 off
			SSPADD = HI2C_BAUD_TEMP And 127
		end if
		
		if HI2CCurrentMode = Slave then
			set SSPCON1.SSPM3 off
			set SSPCON1.SSPM2 on
			set SSPCON1.SSPM1 on
			set SSPCON1.SSPM0 off
		end if
		
		if HI2CCurrentMode = Slave10 then
			set SSPCON1.SSPM3 off
			set SSPCON1.SSPM2 on
			set SSPCON1.SSPM1 on
			set SSPCON1.SSPM0 on
		end if
		
		'Enable I2C
		set SSPCON1.SSPEN on
	#ENDIF
	
End Sub

Sub HI2CSetAddress(In I2CAddress)
	#ifdef PIC
		'Slave mode only
		If HI2CCurrentMode <= 10 Then
			SSPADD = I2CAddress
		End If
	#endif
End Sub

Sub HI2CStart
	
	'Master mode
	If HI2CCurrentMode > 10 Then
		#ifdef PIC
			#ifdef Var(SSPCON2)
				Set SSPCON2.SEN On
			#endif
		#endif
		
	'Slave mode
	Else
		#ifdef PIC
			Wait Until SSPSTAT.S = On
		#endif
		
	End If
	
End Sub

Sub HI2CStop
	'Master mode
	If HI2CCurrentMode > 10 Then
		#ifdef PIC
			#ifdef Var(SSPCON2)
				Set SSPCON2.PEN On
			#endif
		#endif
		
	'Slave mode
	Else
		#ifdef PIC
			Wait Until SSPSTAT.P = On
		#endif
		
	End If
	
End Sub

Function HI2CStartOccurred
	'Master mode
	'Always return true
	If HI2CCurrentMode > 10 Then
		HI2CStartOccurred = TRUE
		Exit Function
		
	'Slave mode, check if start condition received last
	Else
		HI2CStartOccurred = FALSE
		#ifdef PIC
			#ifdef Var(SSPSTAT)
				If SSPSTAT.S = On Then HI2CStartOccurred = TRUE
			#endif
		#endif
		
	End If
End Function

Function HI2CStopped
	'Master mode
	'Always return false
	If HI2CCurrentMode > 10 Then
		HI2CStopped = FALSE
		Exit Function
		
	'Slave mode, check if start condition received last
	Else
		HI2CStopped = FALSE
		#ifdef PIC
			#ifdef Var(SSPSTAT)
				If SSPSTAT.P = On Then HI2CStopped = TRUE
			#endif
		#endif
		
	End If
End Function

Sub HI2CSend(In I2CByte)
	
	#ifdef PIC
		#ifndef Var(SSPCON1)
			#ifdef Var(SSPCON)
				Dim SSPCON1 Alias SSPCON
			#endif
		#endif
		
		RetryHI2CSend:
			'Clear WCOL
			SET SSPCON1.WCOL OFF
			'Load data to send
			SSPBUF = I2CByte
			
		If SSPCON1.WCOL = On Then
			If HI2CCurrentMode <= 10 Then Goto RetryHI2CSend
		End If
		
		'Release clock (only needed by slave)
		If HI2CCurrentMode <= 10 Then Set SSPCON1.CKP On
		
	#endif
	
End Sub

Sub HI2CReceive (Out I2CByte)
	
	#ifdef PIC
		#ifndef Var(SSPCON1)
			#ifdef Var(SSPCON)
				Dim SSPCON1 Alias SSPCON
			#endif
		#endif
		
		'Enable receive
		#ifdef SSPCON2
			'Master mode
			If HI2CCurrentMode > 10 Then
				Set SSPCON2.RCEN On
			'Slave mode
			Else
				SET SSPSTAT.R ON
			End If
		#endif
		
		'Clear WCOL
		SET SSPCON1.WCOL OFF
		SET SSPCON1.SSPOV Off
		
		'Wait for receive
		Wait Until SSPSTAT.BF = On
		I2CByte = SSPBUF
		
		'Disable receive (master mode)
		#ifdef SSPCON2
			'Master mode
			If HI2CCurrentMode > 10 Then
				Set SSPCON2.RCEN Off
			'Slave mode
			Else
				SET SSPSTAT.R Off
			End If
		#endif
	#endif
	
End Sub
