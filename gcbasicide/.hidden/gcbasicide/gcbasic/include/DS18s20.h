'    DS18s20 routines for the Great Cow BASIC compiler
'    Copyright (C) 2007 Kent Schafer

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
'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA

'Notes:
' This is a Dallas DS18s20 temperature device device program for Great Cow Basic.
' This is the minimal code to get a positive temperature reading in degrees
' Celsius.  Has not been tested in Parasitic mode or with long lengths of wire.
' Kent Schafer Feb 12, 2007                           
' Rev0

'To read a value, use the ReadTemp function. For example,
' LCDInt ReadTemp: Print " deg. C"

' Port settings
#define DQ PortA.0

'Dim RxData As Word (will need this for neg. no.)

'=========ROM Commands for 1-Wire DS18S20======================================
#define SearchRom 240 	'0xF0 (240)
			'Command for identifying Slave Rom codes, use as many
			'times as needed
#define ReadRom 51 	'0x33 (051)
			'Command for single slave
#define	MatchRom 85 	'0x55 (085)
			'Command for master to identify a specific slave code
#define	SkipRom 204 	'0xCC (204)
			'Command for addressing all devices simultaneously
#define	AlarmSearch 236 '0xEC (236)
			'Command is same as Search Rom to identify any alarm flags

'==========Function Commands for for 1-Wire DS18S20=============================
#define	ConvertT 68 	'0x44 (68)
			'Protocol for Single Temp Conversion
#define	WriteScratch 78 '0x4E (78)
			'Protocol for Write Scratchpad
#define	ReadScratch 190 '0xBE (190)
			'Protocol for Read Scratchpad
#define CopyScratch  72 '0x48 (72)
			'Protocol for copying Scratchpad TH and TL
			'registers to EEPROM
#define RecallE2 187 	'0xB8 (187)
			'Protocol for recalling alarm trigger values from EEPROM 

function ReadTemp
 wait 1 sec '*** Is this needed? ***
 MasterRST
 PPulse
 wait 1 ms
 OWout SkipRom
 wait 1 ms
 OWout ConvertT
 wait 1 s  'Need at least 1/2 sec for conversion?

 MasterRST
 PPulse
 wait 1 ms
 OWout SkipRom
 wait 1 ms
 OWout ReadScratch
 wait 1 ms
 Owin
 wait 1 ms

 ReadTemp = RxData / 2

 'cls
 'LCDInt (RxData/2)
 'If RxData.0 1 Then Print ".5"
 'If RxData.0 0 Then Print ".0"
 'Print " Deg C"
 'Wait 5 sec
 'Goto Main
end function

'-----SUBS-----------------------------------------

Sub MasterRST  'Master reset low for minimum 480 us
 Dir DQ In
 Dir DQ Out
 Set DQ Off
 wait 50 10us
 Dir DQ In  'HiZ
end sub

Sub PPulse  'Master receive DS18s20 presence pulse
  wait 70 us
  'If DQ 0 Then
  'cls
  'Print "OW Device ACK"  
  'End if 
  wait 43 10us
  Dir DQ In
end sub

Sub OWout (Command) #NR
 Clocks = 0
 For Clocks = 1 to 8 
  Dir DQ Out
  Set DQ Off
  wait 3 us  'Need to release bus within 15 us
  If Command.0 On then
  Dir DQ In
  End if
  wait 60 us  '60 us per AN2420
  Dir DQ In  'HiZ the bus if still pulled low
  wait 3 us
  ROTATE Command Right  'The DS18s20 wants data LSB first
 Next
end sub

Sub OWin
 For Clocks = 1 to 8
  Rotate RxData Right 'The DS18s20 xmits data LSB first
  Dir DQ Out
  Set DQ Off  'Read time slot
  wait 6 us  
  Dir DQ In   'Release bus for one wire Reception
  wait 4 us  
  If DQ On  Then Set RxData.7 1
  If DQ Off Then Set RxData.7 0
  wait 50 us
 Next
end sub  
