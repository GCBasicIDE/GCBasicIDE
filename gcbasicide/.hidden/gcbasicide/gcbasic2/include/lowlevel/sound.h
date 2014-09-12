'    Sound/Tone generation routines for Great Cow BASIC
'    Copyright (C) 2006-2007 Hugh Considine

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

#define SoundOut SysTemp.0 'Set to port for sound output
#define SoundOut2 SysTemp.0 'Second port, used by MultiTone only

#startup InitSound

'Frequency is Hz, Duration is in 10ms units
sub Tone (In ToneFrequency as word, In ToneDuration as word)
	
	dim ToneLoop as word
	dim ToneLoop2 as word
	
	If ToneFrequency = 0 Then Exit Sub
	
	'TonePeriod = 20000 / ToneFrequency
	TonePeriod = 50000 / ToneFrequency
	'ToneFrequency = ToneFrequency / 200
	ToneFrequency = ToneFrequency / 100
	
	For ToneLoop = 1 to ToneDuration
		For ToneLoop2 = 1 to ToneFrequency
			Set SoundOut ON
			SysToneDelay
			Set SoundOut OFF
			SysToneDelay
		Next
	Next
End Sub

'#define SysToneDelay Wait TonePeriod 10us: Wait TonePeriod 10us
#define SysToneDelay Wait TonePeriod 10us

'Frequency is Hz/10, Duration is in 1 ms units
sub ShortTone (ToneFrequency, ToneDuration) #NR

 TonePeriod = 250/ToneFrequency
 ToneFrequency = ToneFrequency / 10
 if ToneDuration =< 4 then ToneFrequency = ToneFrequency/4
 if ToneDuration > 4 then ToneDuration = ToneDuration/4

 for ToneLoop = 1 to ToneDuration
  for ToneLoop2 = 1 to ToneFrequency
   SET SoundOut ON
   ShortToneDelay
   SET SoundOut OFF
   ShortToneDelay
  next
 next
end sub

sub ShortToneDelay
 for ToneDelayLoop = 1 to Period
  Wait 9 10us
 next
end sub

sub InitSound
 dir SoundOut out
end sub
