'    Analog to Digital conversion routines for Great Cow BASIC
'    Copyright (C) 2006 - 2013 Hugh Considine, Kent Schafer

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

'Original code by Hugh Considine
'18Fxx31 code by Kent Schafer

'Commands:
'var = ReadAD(port)	Reads port, and returns value.
'ADFormat(type)		Choose Left or Right justified
'ADOff			Set A/D converter off. Use if trouble is experienced when
'			attempting to use ports in digital mode

#define Format_Left 0
#define Format_Right 255

'Acquisition time. Can be reduced in some circumstances - see PIC manual for details
#define AD_Delay 2 10us

'Optimisation
#define ADSpeed MediumSpeed

#define HighSpeed 255
#define MediumSpeed 128
#define LowSpeed 0
#define InternalClock 192

'Port names
'PIC style
#define AN0 0
#define AN1 1
#define AN2 2
#define AN3 3
#define AN4 4
#define AN5 5
#define AN6 6
#define AN7 7
#define AN8 8
#define AN9 9
#define AN10 10
#define AN11 11
#define AN12 12
#define AN13 13
#define AN14 14
#define AN15 15
#define AN16 16
#define AN17 17
#define AN18 18
#define AN19 19
#define AN20 20
#define AN21 21
#define AN22 22
#define AN23 23
#define AN24 24
#define AN25 25
#define AN26 26
#define AN27 27

'AVR style
#define ADC0 0
#define ADC1 1
#define ADC2 2
#define ADC3 3
#define ADC4 4
#define ADC5 5
#define ADC6 6
#define ADC7 7
#define ADC8 8
#define ADC9 9
#define ADC10 10

macro LLReadAD

	#IFDEF PIC
		
		'Set up A/D
		'Make necessary ports analog
		'Code for PICs with older A/D (No ANSEL register)
		#IFDEF NoVar(ANSEL)
			#IFDEF NoVar(ANSEL0)
				#IFDEF NoVar(ANSELA)
					#IFDEF NoVar(ANSELB)
				
						#IFDEF NoBit(PCFG4)
							#IFDEF NoVar(ADCON2)
								#IFDEF NoBit(ANS0)
									'Example: 16F877A
									#IFDEF Bit(PCFG3)
										SET PCFG3 OFF
									#ENDIF
									SET PCFG2 OFF
									SET PCFG1 OFF
									SET PCFG0 OFF
								#ENDIF
								'Example: 10F220
								#IFDEF Bit(ANS0)
									SET ANS0 OFF
									SET ANS1 OFF
								#ENDIF
							#ENDIF
							
							#IFDEF Var(ADCON2)
								'Example: 18F4620
								#IFDEF BIT(PCFG3)
									SET PCFG3 OFF
									SET PCFG2 OFF
									SET PCFG1 OFF
									SET PCFG0 OFF
								#ENDIF
							#ENDIF
						#ENDIF
						
						'PICs with PCFG4 and higher use ADCON1 as an ANSEL type register
						'Example: 18F1320
						#IFDEF Bit(PCFG4)
							'Some 18F8xxxx chips have error in chip definition
							'They claim to have PCFG4, but actually don't, can spot them by presence of ADCON2
							Dim AllANSEL As Byte Alias ADCON1
							AllANSEL = 0
							ADTemp = ADReadPort + 1
							Set C On
							Do
								Rotate AllANSEL Left
								decfsz ADTemp,F
							Loop
						#ENDIF
					
					'ANSELB/A
					#ENDIF
				#ENDIF
				
				'Code for 16F193x chips (and others?) with ANSELA/ANSELB/ANSELE registers
				#IFDEF Var(ANSELA)
					Select Case ADReadPort
						#IF ChipPins = 18
							Case 0: Set ANSELA.0 On
							Case 1: Set ANSELA.1 On
							Case 2: Set ANSELA.2 On
							Case 3: Set ANSELA.3 On
							Case 4: Set ANSELA.4 On
							
							Case 11: Set ANSELB.1 On
							Case 10: Set ANSELB.2 On
							Case 9: Set ANSELB.3 On
							Case 8: Set ANSELB.4 On
							Case 7: Set ANSELB.5 On
							Case 5: Set ANSELB.6 On
							Case 6: Set ANSELB.7 On
						#ENDIF
						
						#IF ChipPins = 28 Or ChipPins = 40
							Case 0: Set ANSELA.0 On
							Case 1: Set ANSELA.1 On
							Case 2: Set ANSELA.2 On
							Case 3: Set ANSELA.3 On
							Case 4: Set ANSELA.5 On
							
							#IFDEF Var(ANSELB)
								Case 12: Set ANSELB.0 On
								Case 10: Set ANSELB.1 On
								Case 8: Set ANSELB.2 On
								Case 9: Set ANSELB.3 On
								Case 11: Set ANSELB.4 On
								Case 13: Set ANSELB.5 On
							#ENDIF
							
							#IFDEF Var(ANSELC)
								Case 14: Set ANSELC.2 On
								Case 15: Set ANSELC.3 On
								Case 16: Set ANSELC.4 On
								Case 17: Set ANSELC.5 On
								Case 18: Set ANSELC.6 On
								Case 19: Set ANSELC.7 On
							#ENDIF
							
							#IFDEF Var(ANSELD)
								Case 20: Set ANSELD.0 On
								Case 21: Set ANSELD.1 On
								Case 22: Set ANSELD.2 On
								Case 23: Set ANSELD.3 On
								Case 24: Set ANSELD.4 On
								Case 25: Set ANSELD.5 On
								Case 26: Set ANSELD.6 On
								Case 27: Set ANSELD.7 On
							#ENDIF
							
							#IFDEF Var(ANSELE)
								Case 5: Set ANSELE.0 On
								Case 6: Set ANSELE.1 On
								Case 7: Set ANSELE.2 On
							#ENDIF
						#ENDIF
						
					End Select
				#ENDIF
				
				'ANSEL0/ANSEL
			#ENDIF
		#ENDIF
		
		'Code for PICs with newer A/D (with ANSEL register)
		#IFDEF Var(ANSEL)
			#IFDEF Var(ANSELH)
				Dim AllANSEL As Word Alias ANSELH, ANSEL
			#ENDIF
			#IFDEF NoVar(ANSELH)
				Dim AllANSEL As Byte Alias ANSEL
			#ENDIF
			AllANSEL = 0
			ADTemp = ADReadPort + 1
			Set C On
			Do
				Rotate AllANSEL Left
				decfsz ADTemp,F
			Loop
			
		#ENDIF
		'Code for 18F4431, uses ANSEL0 and ANSEL1
		#IFDEF Var(ANSEL0)
			#IFDEF Var(ANSEL1)
				Dim AllANSEL As Word Alias ANSEL1, ANSEL0
			#ENDIF
			#IFDEF NoVar(ANSEL1)
				Dim AllANSEL As Byte Alias ANSEL0
			#ENDIF
			AllANSEL = 0
			ADTemp = ADReadPort + 1
			Set C On
			Do
				Rotate AllANSEL Left
				decfsz ADTemp,F
			Loop
			
		#ENDIF
		
		'Set Auto or Single Convert Mode
		#IFDEF Bit(ACONV)
			SET ACONV OFF  'Single shot mode 
			SET ACSCH OFF  'Single channel CONVERSION
			'GroupA
			IF ADReadPort = 0 OR ADReadPort = 4 OR ADReadPort = 8 Then
				SET ACMOD1 OFF
				SET ACMOD0 OFF
			END IF
			'GroupB
			IF ADReadPort = 1 OR ADReadPort = 5 Then
				SET ACMOD1 OFF
				SET ACMOD0 ON
			END IF
			'GroupC
			IF ADReadPort = 2 OR ADReadPort = 6 Then
				SET ACMOD1 ON
				SET ACMOD0 OFF
			END IF
			'GroupD
			IF ADReadPort = 3 OR ADReadPort = 7 Then
				SET ACMOD1 ON
				SET ACMOD0 ON
			END IF
			
		#ENDIF
		
		'Set conversion clock
		#IFDEF Bit(ADCS0)
			#IFDEF ADSpeed HighSpeed
				SET ADCS1 OFF
				SET ADCS0 OFF
			#ENDIF
			#IFDEF ADSpeed MediumSpeed
				SET ADCS1 OFF
				SET ADCS0 ON
			#ENDIF
			#IFDEF ADSpeed LowSpeed
				SET ADCS1 ON
				SET ADCS0 ON
			#ENDIF
			#IFDEF ADSpeed InternalClock
				SET ADCS1 ON
				SET ADCS0 ON
			#ENDIF
		#ENDIF
		
		'Choose port
		#IFDEF Bit(CHS0)
			SET ADCON0.CHS0 OFF
			SET ADCON0.CHS1 OFF
			#IFDEF Bit(CHS2)
				SET ADCON0.CHS2 OFF
				#IFDEF Bit(CHS3)
					SET ADCON0.CHS3 OFF
					#IFDEF Bit(CHS4)
						SET ADCON0.CHS4 OFF
					#ENDIF
				#ENDIF
			#ENDIF
			
			IF ADReadPort.0 On Then Set ADCON0.CHS0 On
			IF ADReadPort.1 On Then Set ADCON0.CHS1 On
			#IFDEF Bit(CHS2)
				IF ADReadPort.2 On Then Set ADCON0.CHS2 On
				#IFDEF Bit(CHS3)
					If ADReadPort.3 On Then Set ADCON0.CHS3 On
					#IFDEF Bit(CHS4)
						If ADReadPort.4 On Then Set ADCON0.CHS4 On
					#ENDIF
				#ENDIF
			#ENDIF
		#ENDIF
		#IFDEF BIT(GASEL0)
			'GROUP A SELECT BITS
			IF ADReadPort = 0 THEN
				SET GASEL1 OFF
				SET GASEL0 OFF
			END IF
			IF ADReadPort = 4 THEN
				SET GASEL1 OFF
				SET GASEL0 ON
			END IF
			IF ADReadPort = 8 THEN
				SET GASEL1 ON
				SET GASEL0 OFF
			END IF
			'GROUP C SELECT BITS		
			IF ADReadPort = 2 THEN
				SET GCSEL1 OFF
				SET GCSEL0 OFF
			END IF
			IF ADReadPort = 6 THEN
				SET GCSEL1 OFF
				SET GCSEL0 ON
			END IF
			'GROUP B SELECT BITS
			IF ADReadPort = 1 THEN
				SET GBSEL1 OFF
				SET GBSEL0 OFF
			END IF
			IF ADReadPort = 5 THEN
				SET GBSEL1 OFF
				SET GBSEL0 ON
			END IF
			'GROUP D SELECT BITS
			IF ADReadPort = 3 THEN
				SET GDSEL1 OFF
				SET GDSEL0 OFF
			END IF
			IF ADReadPort = 7 THEN
				SET GDSEL1 OFF
				SET GDSEL0 ON
			END IF
		#ENDIF
		
		'Enable A/D
		SET ADCON0.ADON ON

		'Acquisition Delay
		Wait AD_Delay
		 
		'Read A/D
		#ifdef bit(GO_NOT_DONE)
			SET ADCON0.GO_NOT_DONE ON
			Wait While ADCON0.GO_NOT_DONE ON
		#endif
		#ifndef bit(GO_NOT_DONE)
			#IFDEF Bit(GO_DONE)
				SET ADCON0.GO_DONE ON
				Wait While ADCON0.GO_DONE ON
			#ENDIF
			#IFNDEF Bit(GO_DONE)
				#IFDEF Bit(GO)
					SET ADCON0.GO ON
					Wait While ADCON0.GO ON
				#ENDIF
			#ENDIF
		#endif
		
		'Switch off A/D
		#IFDEF Var(ADCON0)
			SET ADCON0.ADON OFF
			#IFDEF NoVar(ANSEL)
				#IFDEF NoVar(ANSELA)
					
					#IFDEF NoBit(PCFG4)
						#IFDEF NoVar(ADCON2)
							#IFDEF NoBit(ANS0)
								#IFDEF Bit(PCFG3)
									SET PCFG3 OFF
								#ENDIF
								SET PCFG2 ON
								SET PCFG1 ON
								SET PCFG0 OFF
							#ENDIF
							#IFDEF Bit(ANS0)
								SET ANS0 OFF
								SET ANS1 OFF
							#ENDIF
						#ENDIF
						
						#IFDEF Var(ADCON2)
							#IFDEF BIT(PCFG3)
								SET PCFG3 ON
								SET PCFG2 ON
								SET PCFG1 ON
								SET PCFG0 ON
							#ENDIF
						#ENDIF
					#ENDIF
					
					'For 18F1320, which uses ADCON1 as an ANSEL register
					#IFDEF Bit(PCFG4)
						ADCON1 = 0
					#ENDIF
				#ENDIF
			#ENDIF
		#ENDIF
		
		'Clear whatever ANSEL variants the chip has
		#IFDEF Var(ANSEL)
			ANSEL = 0
		#ENDIF
		#IFDEF Var(ANSELH)
			ANSELH = 0
		#ENDIF
		#IFDEF Var(ANSEL0)
			ANSEL0 = 0
		#ENDIF
		#IFDEF Var(ANSEL1)
			ANSEL1 = 0
		#ENDIF
		#IFDEF Var(ANSELA)
			ANSELA = 0
		#ENDIF
		#IFDEF Var(ANSELB)
			ANSELB = 0
		#ENDIF
		#IFDEF Var(ANSELC)
			ANSELC = 0
		#ENDIF
		#IFDEF Var(ANSELD)
			ANSELD = 0
		#ENDIF
		#IFDEF Var(ANSELE)
			ANSELE = 0
		#ENDIF
		
	#ENDIF

	#IFDEF AVR
	
		'Select channel
		ADMUX = ADReadPort
		
		'Set conversion clock
		#IFDEF Bit(ADPS2)
			#IFDEF ADSpeed HighSpeed
				SET ADPS2 Off
				SET ADPS1 Off
			#ENDIF
			#IFDEF ADSpeed MediumSpeed
				SET ADPS2 On
				SET ADPS1 Off
			#ENDIF
			#IFDEF ADSpeed LowSpeed
				SET ADPS2 On
				SET ADPS1 On
			#ENDIF
		#ENDIF
		
		'Acquisition Delay
		Wait AD_Delay
		
		'Take reading
		Set ADEN On
		Set ADSC On
		Wait While ADSC On
		Set ADEN Off
		
	#ENDIF
	
end macro

function ReadAD(ADReadPort)

'Set up for 8 bit
#IFDEF AVR
	#IFDEF Bit(ADLAR)
		Set ADReadPort.ADLAR On
	#endif
#ENDIF

'Perform conversion
LLReadAD

'Write output
#IFDEF PIC
	#IFDEF Var(ADRESH)
		ReadAD = ADRESH
	#ENDIF
	#IFDEF NoVar(ADRESH)
		ReadAD = ADRES
	#ENDIF
#ENDIF

#IFDEF AVR
	ReadAD = ADCH
#ENDIF

end function

'Large ReadAD
function ReadAD10(ADReadPort) As Word

#IFDEF PIC
	'Set up A/D format
	#IFDEF Bit(ADFM)
		SET ADFM ON
	#ENDIF
#ENDIF
#IFDEF AVR
	Dim LLADResult As Word Alias ADCH, ADCL
	#IFDEF Bit(ADLAR)
		Set ADReadPort.ADLAR Off
	#EndIf
#ENDIF

'Do conversion
LLReadAD

#IFDEF PIC
	'Write output
	#IFDEF NoVar(ADRESL)
		ReadAD10 = ADRES
	#ENDIF
	#IFDEF Var(ADRESL)
		ReadAD10 = ADRESL
	#ENDIF
	#IFDEF Var(ADRESH)
		ReadAD10_H = ADRESH
	#ENDIF
	
	'Put A/D format back to normal
	#IFDEF Bit(ADFM)
		SET ADFM OFF
	#ENDIF
#ENDIF

#IFDEF AVR
	ReadAD10 = LLADResult
#ENDIF

end function

'This sub is deprecated
sub ADFormat(ADReadFormat)
 SET ADFM OFF
 IF ADReadFormat.1 ON THEN SET ADFM ON  
end sub

'This sub is deprecated
sub ADOff
'Disable the A/D converter, and set all ports to digital.
'This sub is deprecated, InitSys automatically turns off A/D 

 SET ADCON0.ADON OFF
#IFDEF NoBit(PCFG4)
 #IFDEF NoVar(ANSEL)
  #IFDEF NoVar(ADCON2)
   #IFDEF Bit(PCFG3)
    SET PCFG3 OFF
   #ENDIF
   SET PCFG2 ON
   SET PCFG1 ON
   SET PCFG0 OFF
  #ENDIF
  #IFDEF Var(ADCON2)
   SET PCFG3 ON
   SET PCFG2 ON
   SET PCFG1 ON
   SET PCFG0 ON
  #ENDIF
 #ENDIF
#ENDIF

 #IFDEF Bit(PCFG4)
  #IFDEF Bit(PCFG6)
   SET PCFG6 ON
  #ENDIF
  #IFDEF Bit(PCFG5)
   SET PCFG5 ON
  #ENDIF
  SET PCFG4 ON
  SET PCFG3 ON
  SET PCFG2 ON
  SET PCFG1 ON
  SET PCFG0 ON
 #ENDIF

 #IFDEF Var(ANSEL)
  ANSEL = 0
 #ENDIF
 #IFDEF Var(ANSELH)
  ANSELH = 0
 #ENDIF

end sub
