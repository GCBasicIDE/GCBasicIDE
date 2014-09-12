'    Timer control routines for Great Cow BASIC
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
' 10/7/2009: Added AVR support

'Subroutines:
' InitTimer0 (Source, Prescaler)
' InitTimer1 (Source, Prescaler)
' InitTimer2 (Prescaler, Postscaler)
' InitTimer3 (Source, Prescaler)
' ClearTimer(TimerNumber)
' StartTimer(TimerNumber)
' StopTimer(TimerNumber)

'Some simpler names for the timers (use to read)
#ifdef PIC
	'#define Timer0 TMR0
	Dim Timer0 Alias TMR0
	'#define Timer1 TMR1L
	'Is now a function, need to ensure read happens in certain order
	'#define Timer2 TMR2
	Dim Timer2 Alias TMR2
#endif
#ifdef AVR
	Dim Timer0 Alias TCNT0
	Dim Timer2 Alias TCNT2
#endif

'Sources
#define Osc 1
#define Ext 2
#define ExtOsc 3

'AVR prescaler settings
#define PS_1 1
#define PS_8 2
#define PS_64 3
#define PS_256 4
#define PS_1024 5
'Also worth noting here,
'0: stop timer
'6: external, clock on falling
'7: external, clock on rising
#define AVR_EXT_TMR 7

'Timer 0 prescales
#define PS0_1/2 0
#define PS0_1/4 1
#define PS0_1/8 2
#define PS0_1/16 3
#define PS0_1/32 4
#define PS0_1/64 5
#define PS0_1/128 6
#define PS0_1/256 7
#define PS0_2 0
#define PS0_4 1
#define PS0_8 2
#define PS0_16 3
#define PS0_32 4
#define PS0_64 5
#define PS0_128 6
#define PS0_256 7

'Timer 1 prescales
#define PS1_1/1 0
#define PS1_1/2 16
#define PS1_1/4 32
#define PS1_1/8 48
#define PS1_1 0
#define PS1_2 16
#define PS1_4 32
#define PS1_8 48

'Timer 2 prescales
#define PS2_1/1 0
#define PS2_1/4 1
#define PS2_1/16 2
#define PS2_1 0
#define PS2_4 1
#define PS2_16 2

'Timer 3 prescales
#define PS3_1/1 0
#define PS3_1/2 16
#define PS3_1/4 32
#define PS3_1/8 48
#define PS3_1 0
#define PS3_2 16
#define PS3_4 32
#define PS3_8 48


Function Timer1 As Word
	#ifdef PIC
		[byte]Timer1 = TMR1L
		Timer1_H = TMR1H
	#endif
	#ifdef AVR
		[byte]Timer1 = TCNT1L
		Timer1_H = TCNT1H
	#endif
End Function

Function Timer3 As Word
	#ifdef PIC
		[byte]Timer3 = TMR3L
		Timer3_H = TMR3H
	#endif
	#ifdef AVR
		[byte]Timer3 = TCNT3L
		Timer3_H = TCNT3H
	#endif
End Function

Function Timer4 As Word
	#ifdef AVR
		[byte]Timer4 = TCNT4L
		Timer4_H = TCNT4H
	#endif
End Function

Function Timer5 As Word
	#ifdef AVR
		[byte]Timer5 = TCNT5L
		Timer5_H = TCNT5H
	#endif
End Function

'Start/Clear/Stop subs
Sub StartTimer(In TMRNumber)
	#ifdef PIC
		'Timer 0 always runs
		#ifdef Var(T1CON)
			If TMRNumber = 1 Then
				Set TMR1ON On
			End If
		#endif
		#ifdef Var(T2CON)
			If TMRNumber = 2 Then
				T2CON = TMR2Pres
				Set TMR2ON On
			End If
		#endif
		#ifdef Var(T3CON)
			If TMRNumber = 3 Then
				Set TMR3ON On
			End If
		#endif
	#endif
	
	#ifdef AVR
		'Need to set clock select bits to 0
		#ifndef Var(TCCR0B)
			#ifdef Var(TCCR0)
			If TMRNumber = 0 Then
				TCCR0 = TMR0Pres
			End If
			#endif
		#endif
		#ifdef Var(TCCR0B)
			If TMRNumber = 0 Then
				TCCR0B = TCCR0B And 248 Or TMR0Pres
			End If
		#endif
		#ifdef Var(TCCR1B)
			If TMRNumber = 1 Then
				TCCR1B = TCCR1B And 248 Or TMR1Pres
			End If
		#endif
		#ifdef Var(TCCR2B)
			If TMRNumber = 2 Then
				TCCR2B = TCCR2B And 248 Or TMR2Post
			End If
		#endif
		#ifdef Var(TCCR3B)
			If TMRNumber = 3 Then
				TCCR3B = TCCR3B And 248 Or TMR3Pres
			End If
		#endif
		#ifdef Var(TCCR4B)
			If TMRNumber = 4 Then
				TCCR4B = TCCR4B And 248 Or TMR4Pres
			End If
		#endif
		#ifdef Var(TCCR5B)
			If TMRNumber = 5 Then
				TCCR5B = TCCR5B And 248 Or TMR5Pres
			End If
		#endif
	#endif
End Sub

Sub ClearTimer (In TMRNumber)
	#ifdef PIC
		If TMRNumber = 0 Then
			TMR0 = 0
			SInitTimer0
		End If
		#ifdef Var(T1CON)
			If TMRNumber = 1 then
				TMR1H = 0
				TMR1L = 0
				SInitTimer1
			End If
		#endif
		#ifdef Var(T2CON)
			If TMRNumber = 2 Then
				TMR2Pres = T2CON
				TMR2 = 0
				T2CON = TMR2Pres
			End If
		#endif
		#ifdef Var(T3CON)
			If TMRNumber = 3 then
				TMR3H = 0
				TMR3L = 0
				SInitTimer3
			End If
		#endif
	#endif
	#ifdef AVR
		#ifdef Var(TCNT0)
			If TMRNumber = 0 Then
				TCNT0 = 0
			End If
		#endif
		#ifdef Var(TCNT1L)
			If TMRNumber = 1 Then
				TCNT1H = 0
				TCNT1L = 0
			End If
		#endif
		#ifdef Var(TCNT2)
			If TMRNumber = 2 Then
				TCNT2 = 0
			End If
		#endif
		#ifdef Var(TCNT3L)
			If TMRNumber = 3 Then
				TCNT3H = 0
				TCNT3L = 0
			End If
		#endif
		#ifdef Var(TCNT4L)
			If TMRNumber = 4 Then
				TCNT4H = 0
				TCNT4L = 0
			End If
		#endif
		#ifdef Var(TCNT5L)
			If TMRNumber = 5 Then
				TCNT5H = 0
				TCNT5L = 0
			End If
		#endif
	#endif
End Sub

Sub StopTimer (In TMRNumber)
	#ifdef PIC
		'Timer 0 always runs
		If TMRNumber = 1 Then
			Set TMR1ON OFF
		End If
		#ifdef Var(T2CON)
			If TMRNumber = 2 Then
				Set TMR2ON OFF
			End If
		#endif
		#ifdef Var(T3CON)
			If TMRNumber = 3 Then
				Set TMR3ON OFF
			End If
		#endif
	#endif
	#ifdef AVR
		'Need to set clock select bits to 0
		#ifdef Var(TCCR0B)
			If TMRNumber = 0 Then
				TCCR0B = TCCR0B And 248
			End If
		#endif
		#ifdef Var(TCCR1B)
			If TMRNumber = 1 Then
				TCCR1B = TCCR1B And 248
			End If
		#endif
		#ifdef Var(TCCR2B)
			If TMRNumber = 2 Then
				TCCR2B = TCCR2B And 248
			End If
		#endif
		#ifdef Var(TCCR3B)
			If TMRNumber = 3 Then
				TCCR3B = TCCR3B And 248
			End If
		#endif
		#ifdef Var(TCCR4B)
			If TMRNumber = 4 Then
				TCCR4B = TCCR4B And 248
			End If
		#endif
		#ifdef Var(TCCR5B)
			If TMRNumber = 5 Then
				TCCR5B = TCCR5B And 248
			End If
		#endif
	#endif
End Sub

'Initialise subs for individual timers
Sub InitTimer0(In TMR0Source, In TMR0Pres)
	#ifdef PIC
		SInitTimer0
	#endif
	#ifdef AVR
		'Just need to buffer TMR0Pres
		'(And change it for external clock)
		If TMR0Source = Ext Then
			TMR0Pres = AVR_EXT_TMR
		End If
	#endif
End Sub

Sub SInitTimer0
	OPTION_REG = OPTION_REG AND 192
	SET OPTION_REG.PSA OFF
	if TMR0Source = Osc THEN SET OPTION_REG.T0SE OFF
	if TMR0Source = Ext THEN SET OPTION_REG.T0SE ON
	clrwdt
	OPTION_REG = OPTION_REG OR TMR0Pres
End Sub

Sub InitTimer1(In TMR1Source, In TMR1Pres)
	#ifdef PIC
		SInitTimer1
	#endif
	#ifdef AVR
		If TMR1Source = Ext Then
			TMR1Pres = AVR_EXT_TMR
		End If
	#endif
End Sub

Sub SInitTimer1
	T1CON = TMR1Pres
	#ifndef Bit(TMR1CS1)
		If TMR1Source = Ext Then
			Set TMR1CS On
		End If
		If TMR1Source = ExtOsc Then
			Set TMR1CS On
			Set T1OSCEN On
		End If
	#endif
	#ifdef Bit(TMR1CS1)
		If TMR1Source = Ext Then
			Set TMR1CS1 On
		End If
		If TMR1Source = ExtOsc Then
			Set TMR1CS1 On
			Set T1OSCEN On
		End If
	#endif
End Sub

Sub InitTimer2 (In TMR2Pres, In TMR2Post)
	#ifdef PIC
		swapf TMR2Post,F
		rrf TMR2Post,W
		andlw 120
		iorwf TMR2Pres,F
	#endif
	#ifdef AVR
		'Some wacky swapping of variable names here
		If TMR2Pres = Ext Then
			TMR2Post = AVR_EXT_TMR
		End If
	#endif
End Sub

Sub InitTimer3(In TMR3Source, In TMR3Pres)
	#ifdef PIC
		SInitTimer3
	#endif
	#ifdef AVR
		If TMR3Source = Ext Then
			TMR3Pres = AVR_EXT_TMR
		End If
	#endif
End Sub

Sub SInitTimer3
	T3CON = TMR3Pres
	#ifndef Bit(TMR3CS1)
		If TMR3Source = Ext Then
			Set TMR3CS On
		End If
		If TMR3Source = ExtOsc Then
			Set TMR3CS On
			Set T3OSCEN On
		End If
	#endif
	#ifdef Bit(TMR3CS1)
		If TMR3Source = Ext Then
			Set TMR3CS1 On
		End If
		If TMR3Source = ExtOsc Then
			Set TMR3CS1 On
			Set T3OSCEN On
		End If
	#endif
	
End Sub

Sub InitTimer4(In TMR4Source, In TMR4Pres)
	#ifdef AVR
		If TMR4Source = Ext Then
			TMR4Pres = AVR_EXT_TMR
		End If
	#endif
End Sub

Sub InitTimer5(In TMR5Source, In TMR5Pres)
	#ifdef AVR
		If TMR5Source = Ext Then
			TMR5Pres = AVR_EXT_TMR
		End If
	#endif
End Sub
