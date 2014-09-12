'    Remote Control routines for Great Cow BASIC
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

'Port settings
'RC5Out		Pin to output RC5 codes on

sub NECSend (NECAdd, NECData) #NR

 'Start
 For NECTemp = 1 to 2
  For RC5Pulse = 1 to 172
   set RC5Out on
   wait 8 us
   set RC5Out off
   wait 18 us
  next
 next

 'Sync
 wait 4 ms
 wait 50 10us
 
 'Address
 NECByte NECAdd

 'Address Complement
 NECByte !NECAdd

 'Command
 NECByte NECData

 'Command Complement
 NECByte !NECData

end sub

sub NECByte (NECSendData) #NR
 NECTemp = 8
 NECSendByte:
 
  'Send High
  if NECSendData.0 on then 
  'Emit high
   For RC5Pulse = 1 to 21
    set RC5Out on
    wait 8 us
    set RC5Out off
    wait 18 us
   next 
   'Emit low
   wait 56 10us
  end if

  'Send Low
  if NECSendData.0 off then
   'Emit high
   For RC5Pulse = 1 to 21
    set RC5Out on
    wait 8 us
    set RC5Out off
    wait 18 us
   next
   'Emit low
   wait 168 10us
  end if

  rotate NECSendData right
 decfsz NECTemp, F
 goto NECSendByte
end sub


sub RC5Send (RC5Add, RC5Data) #NR

 'AGC
 RC5High
 RC5High
 
 'Check bit
 if RC5Toggle.0 off then RC5Low
 if RC5Toggle.0 on then RC5High
 RC5Toggle += 1 

 'Address
 RC5Temp = 5
 RC5SendAdd:
  if RC5Add.4 on then RC5High
  if RC5Add.4 off then RC5Low
  rotate RC5Add left
 decfsz RC5Temp, F
 goto RC5SendAdd

 'Command
 RC5Temp = 6
 RC5SendCom:
  if RC5Data.5 on then RC5High
  if RC5Data.5 off then RC5Low
  rotate RC5Data left
 decfsz RC5Temp, F
 goto RC5SendCom 
end sub

sub RC5High
 'Emit low
 wait 88 10us

 'Emit high
 For RC5Pulse = 1 to 64
  set RC5Out on
  wait 7 us
  set RC5Out off
  wait 20 us
 next
end sub

sub RC5Low
 'Emit high
 For RC5Pulse = 1 to 64
  set RC5Out on
  wait 7 us
  set RC5Out off
  wait 20 us
 next

 'Emit low
 wait 88 10us
end sub