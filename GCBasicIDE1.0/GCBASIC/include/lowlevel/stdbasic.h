'    Some common BASIC commands/functions for Great Cow BASIC
'    Copyright (C) 2006 - 2013 Hugh Considine

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
' 17/6/2009: AVR support added
' 10/2/2013: Indirect call added

'Misc settings

'Bit rate delays
#define r300 64
#define r600 32
#define r1200 16
#define r2400 8
#define r4800 4
#define r9600 2
#define r19200 1
'Number to multiply above by to get correct delay length (us)
#define rBitRateUnit 52

'Indirect call
Sub IndCall(In MemAdr As Word)
	'Jump to a subroutine located at MemAdr
	#ifdef PIC
		#ifdef Var(PCLATU)
			PCLATU = 0
		#endif
		PCLATH = MemAdr_H
		'Use inline assembly, or movff will be generated for 18F and this does not work
		movf MemAdr, W
		movwf PCL
	#endif
	#ifdef AVR
		Dim MemAdr As Word Alias SysReadA_H, SysReadA
		ijmp
	#endif
End Sub

'Direct memory access
Sub Poke (In MemAdr As Word, In MemData)
	#ifdef PIC
		#IFDEF ChipFamily 12,14
			SET STATUS.IRP OFF
			if MemAdr.8 ON then SET STATUS.IRP ON
			FSR = MemAdr
			INDF = MemData
		#ENDIF
		#IFDEF ChipFamily 15,16
			Dim MemAdr As Word Alias FSR0H, FSR0L
			'FSR0H = MemAdr_H
			'FSR0L = MemAdr
			INDF0 = MemData
		#ENDIF
	#endif
	#ifdef AVR
		Dim MemAdr As Word Alias SysStringA_H, SysStringA
		Dim MemData Alias SysValueCopy
		st X, MemData
	#endif
End Sub

Function Peek (MemAdr As Word)
	#ifdef PIC
		#IFDEF ChipFamily 12,14
			SET STATUS.IRP OFF
			if MemAdr.8 ON then SET STATUS.IRP ON
			FSR = MemAdr
			PEEK = INDF
		#ENDIF
		#IFDEF ChipFamily 15,16
			FSR0H = MemAdr_H
			FSR0L = MemAdr
			PEEK = INDF0
		#ENDIF
	#endif
	#ifdef AVR
		Dim MemAdr As Word Alias SysStringA_H, SysStringA
		Dim Peek Alias SysValueCopy
		ld Peek, X
	#endif
End Function

'Software PWM
'Cannot be put in pwm.h as then the InitPWM subroutine will be called even though it is not needed

'Duty is /255, Dur is ms
sub PWMOut(PWMChannel, SoftPWMDuty, SoftPWMCycles) #NR
	For PWMDur = 1 to SoftPWMCycles
		For DOPWM = 1 to 255
			
			if SoftPWMDuty > DOPWM then
				#IFDEF PWM_Out1
					if PWMChannel = 1 then set PWM_Out1 ON
				#ENDIF
				#IFDEF PWM_Out2
					if PWMChannel = 2 then set PWM_Out2 ON
				#ENDIF
				#IFDEF PWM_Out3
					if PWMChannel = 3 then set PWM_Out3 ON
				#ENDIF
				#IFDEF PWM_Out4
					if PWMChannel = 4 then set PWM_Out4 ON
				#ENDIF
			Else
				#IFDEF PWM_Out1
					if PWMChannel = 1 then set PWM_Out1 OFF
				#ENDIF
				#IFDEF PWM_Out2
					if PWMChannel = 2 then set PWM_Out2 OFF
				#ENDIF
				#IFDEF PWM_Out3
					if PWMChannel = 3 then set PWM_Out3 OFF
				#ENDIF
				#IFDEF PWM_Out4
					if PWMChannel = 4 then set PWM_Out4 OFF
				#ENDIF
			end if
			#IFDEF PWM_Delay
				Wait PWM_Delay
			#ENDIF
		next
	next
end sub

'PulseOut
macro Pulseout (Pin, Time)
	Set Pin On
	Wait Time
	Set Pin Off
end macro

'PulseIn
macro PulseIn (Pin, Variable, Units)
	Variable = 0
	Do While Pin = On
		Wait 1 Units
		Variable += 1
		If Variable = 0 Then Exit Do
	Loop
end macro

'Delay
'macro Sleep (time)
' Wait Time s
'end macro

macro Pause (time)
 Wait Time ms
end macro

'Provides a fast way to calculate the average of two 8 bit numbers
function Average(SysCalcTempA, SysCalcTempB)
	SET C OFF
	Average = SysCalcTempA + SysCalcTempB
	ROTATE Average RIGHT
end function

'Miscellaneous Variable handling subs

'Swap SysTempA and SysTempB
Sub Swap(SysCalcTempA, SysCalcTempB)
	SysCalcTempX = SysCalcTempA
	SysCalcTempA = SysCalcTempB
	SysCalcTempB = SysCalcTempX
End Sub

Sub Swap(SysCalcTempA As Word, SysCalcTempB As Word)
	Dim SysCalcTempX As Word
	SysCalcTempX = SysCalcTempA
	SysCalcTempA = SysCalcTempB
	SysCalcTempB = SysCalcTempX
End Sub

Function Abs(SysCalcTempA As Integer) As Integer
	If SysCalcTempA.15 Then
		Abs = -SysCalcTempA
	Else
		Abs = SysCalcTempA
	End If
End Function

'Swap nibbles (4-byte blocks)
Function Swap4(Swap4In)
	#ifdef PIC
		swapf Swap4In, W
		movwf swap4
	#endif
	#ifdef AVR
		Dim Swap4In Alias SysCalcTempA
		Dim Swap4 Alias SysCalcTempX
		mov Swap4, Swap4In
		swap Swap4
	#endif
End Function
