'    EEPROM routines for Great Cow BASIC
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
' 3/5/2006: Program EEPROM read/write added
' 16/5/2006: Write mode disabled at end of EPWrite
' 23/5/2006: EPWrite and EPRead changed to stop use of temporary vars
' 19/8/2006: Program EEPROM altered to take input as word
' 9/2/2007: Bug fix: EEPGD bit not set if not present
' 5/8/2007: Altered to use alias to access address, data vars
' 4/9/2007: Data EEPROM code altered to support AVR
' 15/10/2007: Bug fix: FREE bit not set if not present, altered to use IntOn/IntOff to control interrupt
' 4/12/2007: Added In/Out to parameters
' 17/6/2008: Altered to make EPRead a system sub, to allow use in ReadTable
' 19/4/2009: Bugfixes for AVR, allow EEPE instead of EEWE
' 4/3/2013: Corrections for PIC16F1847 (EEDAT instead of EEDATA)

sub EPWrite(In EEAddress, In EEDataValue)

#IFDEF PIC
	'Variable alias
	#IFNDEF Var(EEADRH)
		Dim EEAddress Alias EEADR
	#ENDIF
	#IFDEF Var(EEADRH)
		Dim EEAddress As Word Alias EEADRH, EEADR
	#ENDIF
	#IFNDEF Var(EEDAT)
		Dim EEDataValue Alias EEDATA
	#endif
	#ifdef Var(EEDAT)
		Dim EEDataValue Alias EEDAT
	#endif
	
	'Disable interrupt
	IntOff
	
	'Select data memory
	#IFDEF Bit(EEPGD)
		SET EECON1.EEPGD OFF
	#ENDIF
	#IFDEF Bit(CFGS)
		Set EECON1.CFGS OFF
	#ENDIF
	
	'Start write
	SET EECON1.WREN ON
	EECON2 = 0x55
	EECON2 = 0xAA
	SET EECON1.WR ON
	SET EECON1.WREN OFF
	
	'Wait for write to complete
	WAIT WHILE EECON1.WR ON
	SET EECON1.WREN OFF
	
	'Restore interrupt
	IntOn
 
#ENDIF

#IFDEF AVR
	'Variable alias
	#IFDEF Var(EEARH)
		Dim EEAddress As Word Alias EEARH, EEARL
	#ENDIF
	#IFNDEF Var(EEARH)
		#IFDEF Var(EEAR)
			Dim EEAddress Alias EEAR
		#ENDIF
	#ENDIF
	Dim EEDataValue Alias EEDR
	
	'Enable write
	#IFDEF Bit(EEMWE)
		Set EECR.EEMWE On
	#ENDIF
	#IFNDEF Bit(EEMWE)
		#IFDEF Bit(EEMPE)
			Set EECR.EEMPE On
		#ENDIF
	#ENDIF
	'Start write, wait for it to complete
	#IFDEF Bit(EEWE)
		Set EECR.EEWE On
		Wait Until EECR.EEWE Off
	#ENDIF
	#IFNDEF Bit(EEWE)
		#IFDEF Bit(EEPE)
			Set EECR.EEPE On
			Wait Until EECR.EEPE Off
		#ENDIF
	#ENDIF
	
#ENDIF
 
end sub

'EPRead made into system subroutine
'Constant added to allow it to still be referred to as EPRead
#define EPRead SysEPRead
sub SysEPRead(In EEAddress, Out EEDataValue)
 
#IFDEF PIC
	'Variable alias
	#IFNDEF Var(EEADRH)
		Dim EEAddress Alias EEADR
	#ENDIF
	#IFDEF Var(EEADRH)
		Dim EEAddress As Word Alias EEADRH, EEADR
	#ENDIF
	#IFNDEF Var(EEDAT)
		Dim EEDataValue Alias EEDATA
	#endif
	#ifdef Var(EEDAT)
		Dim EEDataValue Alias EEDAT
	#endif
	
	'Disable interrupt
	IntOff
	
	'Select data memory
	#IFDEF Bit(EEPGD)
		SET EECON1.EEPGD OFF
	#ENDIF
	#IFDEF Bit(CFGS)
		Set EECON1.CFGS OFF
	#ENDIF
	
	'Read
	SET EECON1.RD ON
	
	'Restore interrupt
	IntOn
#ENDIF

#IFDEF AVR
	'Variable alias
	#IFDEF Var(EEARH)
		Dim EEAddress As Word Alias EEARH, EEARL
	#ENDIF
	#IFNDEF Var(EEARH)
		#IFDEF Var(EEAR)
			Dim EEAddress Alias EEAR
		#ENDIF
	#ENDIF
	Dim EEDataValue Alias EEDR
	
	'Start read
	Set EECR.EERE On
	
#ENDIF	

end sub

function ReadEP(EEAddress)

#IFDEF PIC
	'Variable alias
	#IFNDEF Var(EEADRH)
		Dim EEAddress Alias EEADR
	#ENDIF
	#IFDEF Var(EEADRH)
		Dim EEAddress As Word Alias EEADRH, EEADR
	#ENDIF
	Dim EEDataValue Alias EEDATA
	
	'Disable interrupt
	IntOff
	
	'Select data memory
	#IFDEF Bit(EEPGD)
		SET EECON1.EEPGD OFF
	#ENDIF
	#IFDEF Bit(CFGS)
		Set EECON1.CFGS OFF
	#ENDIF
	
	'Read
	SET EECON1.RD ON
	
	'Restore interrupt
	IntOn
#ENDIF

#IFDEF AVR
	'Variable alias
	#IFDEF Var(EEARH)
		Dim EEAddress As Word Alias EEARH, EEARL
	#ENDIF
	#IFNDEF Var(EEARH)
		#IFDEF Var(EEAR)
			Dim EEAddress Alias EEAR
		#ENDIF
	#ENDIF
	Dim EEDataValue Alias EEDR
	
	'Start read
	Set EECR.EERE On
#ENDIF
	
end sub

sub ProgramWrite(In EEAddress, In EEDataWord)

#IFDEF PIC
	Dim EEAddress As Word Alias EEADRH, EEADR
	Dim EEDataWord As Word Alias EEDATH, EEDATA
	
	'Disable Interrupt
	IntOff
	
	'Select program memory
	SET EECON1.EEPGD ON
	#IFDEF Bit(CFGS)
		Set EECON1.CFGS OFF
	#ENDIF
	
	'Enable write
	SET EECON1.WREN ON
	#ifdef bit(FREE)
		SET EECON1.FREE OFF
	#endif
	
	'Write enable code
	EECON2 = 0x55
	EECON2 = 0xAA
	
	'Start write, wait for it to finish
	SET EECON1.WR ON
	NOP
	NOP
	SET EECON1.WREN OFF
	
	'Enable Interrupt
	IntOn
#ENDIF

end sub

sub ProgramRead(In EEAddress, Out EEDataWord)
	Dim EEAddress As Word Alias EEADRH, EEADR
	Dim EEDataWord As Word Alias EEDATH, EEDATA
	
	'Disable Interrupt
	IntOff
	
	'Select program memory
	SET EECON1.EEPGD ON
	#IFDEF Bit(CFGS)
		Set EECON1.CFGS OFF
	#ENDIF
	
	'Start read, wait for it to finish
	SET EECON1.RD ON
	NOP
	NOP
	
	'Enable interrupt
	IntOn
end sub

sub ProgramErase(In EEAddress)
	Dim EEAddress As Word Alias EEADRH, EEADR
	
	'Disable Interrupt
	IntOff
	
	'Select program memory
	SET EECON1.EEPGD ON
	#IFDEF Bit(CFGS)
		Set EECON1.CFGS OFF
	#ENDIF
	
	SET EECON1.WREN ON
	#ifdef bit(FREE)
		SET EECON1.FREE ON
	#endif
	EECON2 = 0x55
	EECON2 = 0xAA
	SET EECON1.WR ON
	NOP
	NOP
	#ifdef bit(FREE)
		SET EECON1.FREE OFF
	#endif
	SET EECON1.WREN OFF
	
	'Enable interrupt
	IntOn
end sub
