'    Subroutines to allow the Polulu 3pi robot to work with Great Cow BASIC
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

#chip mega168, 20
#startup InitBot

'Button states
#define Pressed 0
#define Released 1

'Input devices
#define PortButtonA PORTB.1
#define PortButtonB PORTB.4
#define PortButtonC PORTB.5
#define DigLine1 PORTC.0
#define DigLine2 PORTC.1
#define DigLine3 PORTC.2
#define DigLine4 PORTC.3
#define DigLine5 PORTC.4
#define Line1 ReadAD(ADC0)
#define Line2 ReadAD(ADC1)
#define Line3 ReadAD(ADC2)
#define Line4 ReadAD(ADC3)
#define Line5 ReadAD(ADC4)

'Output devices
#define Red PORTD.1
#define Green PORTD.7
#define FloorLight PORTC.5
#define SoundOut PORTB.2
#define NotBuzzer

'Motors
#define LF PORTD.6
#define LR PORTD.5
#define RF PORTB.3
#define RR PORTD.3

'LCD settings
#define LCD_IO 4
#define LCD_RS PORTD.2
#define LCD_RW PORTB.0
#define LCD_Enable PORTD.4
#define LCD_DB4 PORTB.1
#define LCD_DB5 PORTB.4
#define LCD_DB6 PORTB.5
#define LCD_DB7 PORTD.7

'''@hide
Sub InitBot
	
	'Enable pullups
	Set PUD Off
	
	'Set up line sensor ports with pull up
	Dir DigLine1 In
	Dir DigLine2 In
	Dir DigLine3 In
	Dir DigLine4 In
	Dir DigLine5 In
	Set DigLine1 On
	Set DigLine2 On
	Set DigLine3 On
	Set DigLine4 On
	Set DigLine5 On
	
	'Need to slow down A/D conversion a bit
	Set ADPS2 On
	
	'Set up output devices
	Dir Red Out
	Dir Green Out
	Dir FloorLight Out
	Dir SoundOut Out
	
	'Set up motors
	Dir LF Out
	Dir LR Out
	Dir RF Out
	Dir RR Out
	LeftSpeed = 0
	RightSpeed = 0
	
	'Set up PWM interrupt
	InitTimer0 Osc, PS_8
	On Interrupt Timer0Overflow Call PWMHandler
	StartTimer 0
	
	'Startup delay, LCD goes silly without it
	Wait 50 ms
	
End Sub

'Button reading
'''Checks the value of button A
Function ButtonA
	Dir PortButtonA In
	Set PortButtonA On
	Wait 2 us
	ButtonA = PortButtonA
	Set PortButtonA Off
	Dir PortButtonA Out
End Function

'''Checks the value of button B
Function ButtonB
	Dir PortButtonB In
	Set PortButtonB On
	Wait 2 us
	ButtonB = PortButtonB
	Set PortButtonB Off
	Dir PortButtonB Out
End Function

'''Checks the value of button C
Function ButtonC
	Dir PortButtonC In
	Set PortButtonC On
	Wait 2 us
	ButtonC = PortButtonC
	Set PortButtonC Off
	Dir PortButtonC Out
End Function

'''Sets the motor speed
'''@param LeftSpeed Left motor speed (0 to 100 for forward, 255 to 155 for reverse)
'''@param RightSpeed Right motor speed (0 to 100 for forward, 255 to 155 for reverse)
Sub SetMotors(In LeftSpeed, In RightSpeed)
	'No body needed, just set parameters
	'Forward: LeftSpeed and RightSpeed should be between 0 (stop) and 100 (full)
	'Reverse: LeftSpeed and RightSpeed should be between 255 (stop) and 155 (full)
End Sub

'''@hide
Sub PWMHandler
	'Sub to do PWM work on motors
	'Left forward
	If LeftSpeed < 120 Then
		Set LR Off
		If PWMCounter < LeftSpeed Then
			Set LF On
		Else
			Set LF Off
		End If
	'Left reverse
	Else
		Set LF Off
		If PWMCounter < 255 - LeftSpeed Then
			Set LR On
		Else
			Set LR Off
		End If
	End If
	'Right forward
	If RightSpeed < 120 Then
		Set RR Off
		If PWMCounter < RightSpeed Then
			Set RF On
		Else
			Set RF Off
		End If
	'Right reverse
	Else
		Set RF Off
		If PWMCounter < 255 - RightSpeed Then
			Set RR On
		Else
			Set RR Off
		End If
	End If
	PWMCounter += 1
	If PWMCounter >= 100 Then PWMCounter = 0
End Sub

'Driving
'''Drive forward
SUB Forward
	SetMotors 100, 100
END SUB

'''Drive backward
SUB Reverse
	SetMotors 155, 155
END SUB

'''Spin left
SUB SpinLeft
	SetMotors 155, 100
END SUB

'''Spin right
SUB SpinRight
	SetMotors 100, 155
END SUB

'''Turn left
SUB TurnLeft
	SetMotors 0, 100
END SUB

'''Turn right
SUB TurnRight
	SetMotors 100, 0
END SUB

'''Stop both motors
SUB Stop
	SetMotors 0, 0
	'Make motors stop NOW, not in next call of PWMHandler
	Set LR Off
	Set LF Off
	Set RR Off
	Set RF Off
END SUB
