'    X10 control routines for Great Cow BASIC
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

'Subroutines/Macros:
X10_High	'Send high bit
X10_Low		'Send low bit
WaitForX10Zero	'Wait for powerline zero crossing

'Configuration
#define X10_In X10Temp.0
#define X10_Out X10Temp.0
#define X10_Sense X10Temp.0

'Macros
#define X10_Low WaitForX10Zero
#define WaitForX10Zero Wait until X10_Sense off

'Subs
sub X10_High
 WaitForX10Zero
 'Send 120 KHz pulse for 1 ms
 For X10Temp = 1 to 125
  pulseout X10_Out, 4 us
  wait 4 us 
 next
end sub

sub X10Send (X10_House, X10_Key) #NR

 'Start code
 X10_High
 X10_High
 X10_High
 X10_Low 

 'House code
 For X10_Bit = 1 to 4
  If X10_House.0 on then X10_High
  If X10_House.0 off then X10_Low
  rotate X10_House right
 next
 
 'Key code
 For X10_Bit = 1 to 5
  If X10_Key.0 on then X10_High
  If X10_Key.0 off then X10_Low
  rotate X10_Key right
 next

end sub