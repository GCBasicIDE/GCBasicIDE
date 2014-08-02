'    Pulse Width Modulation routines for Great Cow BASIC
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

'Defaults:
#define PWM_Freq 38 'Frequency of PWM in KHz
#define PWM_Duty 50 'Duty cycle of PWM (%)

#startup InitPWM

Sub InitPWM
	'Script to calculate constants required for given Frequency and Duty Cycle
	#script
		PR2Temp = int((1/PWM_Freq)/(4*(1/(ChipMHz*1000))))
		T2PR = 1
		if PR2Temp > 255 then
			PR2Temp = int((1/PWM_Freq)/(16*(1/(ChipMHz*1000))))
			T2PR = 4
			if PR2Temp > 255 then
				PR2Temp = int((1/PWM_Freq)/(64*(1/(ChipMHz*1000))))
				T2PR = 16
				if PR2Temp > 255 then
					'error T2PR, PR2Temp
					error msg(BadPWMFreq)
				end if
			end if
		end if
		
		'DutyCycle = (PWM_Duty*10.24)*PR2Temp/1024
		DutyCycle = (PWM_Duty / 25) * (PR2Temp + 1)
		DutyCycleH = (DutyCycle AND 1020)/4
		DutyCycleL = DutyCycle AND 3
		
		PWMOsc1 = int(60000/(240/ChipMHz))
		PWMOsc4 = int(60000/(960/ChipMHz))
		PWMOsc16 = int(60000/(3840/ChipMHz))
	#endscript

	#ifdef PIC
		'Set PWM Period
		PR2 = PR2Temp
		#ifdef T2PR 1
			SET T2CON.T2CKPS0 OFF
			SET T2CON.T2CKPS1 OFF
		#endif
		#ifdef T2PR 4
			SET T2CON.T2CKPS0 ON
			SET T2CON.T2CKPS1 OFF
		#endif
		#ifdef T2PR 16
			SET T2CON.T2CKPS0 OFF
			SET T2CON.T2CKPS1 ON
		#endif
		
		'Set Duty cycle
		CCPR1L = DutyCycleH
		#ifdef DutyCycleL 0
			#ifdef Bit(CCP1X)
				SET CCPCONCache.CCP1Y OFF
				SET CCPCONCache.CCP1X OFF
			#endif
			#ifdef Bit(DC1B1)
				SET CCPCONCache.DC1B1 OFF
				SET CCPCONCache.DC1B0 OFF
			#endif
		#endif
		#ifdef DutyCycleL 1
			#ifdef Bit(CCP1X)
				SET CCPCONCache.CCP1Y ON
				SET CCPCONCache.CCP1X OFF
			#endif
			#ifdef Bit(DC1B1)
				SET CCPCONCache.DC1B1 OFF
				SET CCPCONCache.DC1B0 ON
			#endif
		#endif
		#ifdef DutyCycleL 2
			#ifdef Bit(CCP1X)
				SET CCPCONCache.CCP1Y OFF
				SET CCPCONCache.CCP1X ON
			#endif
			#ifdef Bit(DC1B1)
				SET CCPCONCache.DC1B1 ON
				SET CCPCONCache.DC1B0 OFF
			#endif
		#endif
		#ifdef DutyCycleL 3
			#ifdef Bit(CCP1X)
				SET CCPCONCache.CCP1Y ON
				SET CCPCONCache.CCP1X ON
			#endif
			#ifdef Bit(DC1B1)
				SET CCPCONCache.DC1B1 ON
				SET CCPCONCache.DC1B0 ON
			#endif
		#endif
		
		'Finish preparing CCP*CON
		SET CCPCONCache.CCP1M3 ON
		SET CCPCONCache.CCP1M2 ON
		SET CCPCONCache.CCP1M1 OFF
		SET CCPCONCache.CCP1M0 OFF
		
		'Enable Timer 2
		SET T2CON.TMR2ON ON
	#endif
	
End Sub

sub PWMOn
	CCP1CON = CCPCONCache
end sub

sub PWMOff
	CCP1CON = 0
end sub

sub HPWM (In PWMChannel, In PWMFreq, In PWMDuty)
	dim PR2_Temp as word
	
	T2_PR = 1
	PR2_Temp = PWMOsc1/PWMFreq
	IF PR2_Temp_H > 0 then 
		T2_PR = 4
		'Divide by 4
		set STATUS.C off
		rotate PR2_Temp right
		set STATUS.C off
		rotate PR2_Temp right
	end if 
	IF PR2_Temp_H > 0 then
		'PR2_Temp = PWMOsc16/PWMFreq: T2_PR = 16
		T2_PR = 16
		'Divide by 4
		set STATUS.C off
		rotate PR2_Temp right
		set STATUS.C off
		rotate PR2_Temp right
	end if
	
	PR2 = PR2_Temp

	SET T2CON.T2CKPS0 OFF
	SET T2CON.T2CKPS1 OFF
	if T2_PR = 4 then SET T2CON.T2CKPS0 ON
	if T2_PR = 16 then SET T2CON.T2CKPS1 ON
	SET T2CON.TMR2ON ON
	
	#ifdef NoVar(CCP2CON)
		CCPR1L = ([word]PWMDuty * PR2) / 255
		SET CCP1CON.CCP1M3 ON
		SET CCP1CON.CCP1M2 ON
		SET CCP1CON.CCP1M1 OFF
		SET CCP1CON.CCP1M0 OFF
	#endif
	#ifdef Var(CCP2CON)
		if PWMChannel = 1 then 
			CCPR1L = ([word]PWMDuty * PR2) / 255 
			SET CCP1CON.CCP1M3 ON
			SET CCP1CON.CCP1M2 ON
			SET CCP1CON.CCP1M1 OFF
			SET CCP1CON.CCP1M0 OFF
		end if 
		if PWMChannel = 2 then
			CCPR2L = ([word]PWMDuty * PR2) / 255
			SET CCP2CON.CCP1M3 ON
			SET CCP2CON.CCP1M2 ON
			SET CCP2CON.CCP1M1 OFF
			SET CCP2CON.CCP1M0 OFF
		end if
		#ifdef Var(CCP3CON)
			if PWMChannel = 3 then
				CCPR3L = ([word]PWMDuty * PR2) / 255
				SET CCP3CON.CCP1M3 ON
				SET CCP3CON.CCP1M2 ON
				SET CCP3CON.CCP1M1 OFF
				SET CCP3CON.CCP1M0 OFF
			end if
		#endif
	#endif
	
end sub
