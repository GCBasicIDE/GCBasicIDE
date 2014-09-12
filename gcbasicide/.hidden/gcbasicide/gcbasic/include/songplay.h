'    Song playing routines for the GCBASIC compiler
'    This provides a Play subroutine that is compatible with that in QBASIC
'    Copyright (C) 2012 Hugh Considine

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

'Notes:
' Constant SOUNDOUT must be defined. This is the pin that sound will be
' produced from. No hardware setting needed for GCGB, SOUNDOUT is set with
' Hardware Settings for the standard Tone command already.

'Changes:
' 3/6/2012: Initial version

'Initialisation routine
#startup InitSoundPlay

'Constants
#define SOUND_CMD_NONE 0

#define SOUND_NOTE_NORMAL 0
#define SOUND_NOTE_SHARP 1
#define SOUND_NOTE_FLAT 2

''' Set up for sound playing
''' @hide
Sub InitSoundPlay
	'Default octave and note length
	SoundPlayOctave = 4
	SoundPlayLength = 4
	SoundPlayTempo = 120
	SoundPlayDots = 0
	
	SoundPlayCommand = SOUND_CMD_NONE
	
	'Set pin direction
	Dir SOUNDOUT Out
	
End Sub

''' Plays a QBASIC sequence of notes
''' @param SoundPlayData Musical note or notes to play. (Notes are A to G)
Sub Play(SoundPlayData As String)
	
	'Parse and play the song in the string SoundPlayData
	'Commands:
	' A - G: Notes. May be followed by length: 2 = half note, 4 = quarter, etc.
	'               May also be followed by # or + (sharp) or - (flat)
	' On: Sets current octave. n is octave from 0 to 6
	' Pn: Pause playing. n is length of rest
	' Ln: Set default note length. n = 1 to 8.
	' < or >: Change down or up an octave
	' Tn: Sets tempo in L4s/minute. n = 32 to 255, default 120.
	' Nn: Play note n. n = 0 to 84, 0 = rest.
	
	'Unsupported: M (play mode), . (changes note length)
	
	'Parsing method
	'For each char:
	'	Check if it is a command
	'	If command or null, play last note
	'	Else, buffer byte:
	'		Can have number (length), +/#/- (sharp/flat)
	
	'Parse commands
	SoundPlayNumber = 0
	SoundPlayBytes = SoundPlayData(0) + 1
	For SoundPlayCurrNote = 1 To SoundPlayBytes
		
		'Get command
		If SoundPlayCurrNote = SoundPlayBytes Then
			SoundPlayByte = SOUND_CMD_NONE
		Else
			SoundPlayByte = SoundPlayData(SoundPlayCurrNote)
		End If
		
		'Convert to all upper case
		If SoundPlayByte >= "a" Then
			If SoundPlayByte <= "z" Then
				SoundPlayByte -= 32
			End If
		End If
		
		'If new command starting, process any command already buffered
		If SoundPlayCheckCommand(SoundPlayByte) Then
			'Execute buffered command
			If SoundPlayCommand <> SOUND_CMD_NONE Then
				'Command is note (A - G)
				If SoundPlayCommand >= "A" Then
					If SoundPlayCommand <= "G" Then
						
						'Translate to note number
						'Need to use SoundPlayCommand for letter and SoundPlayNoteOctave for octave
						'Also need to note flat/sharp
						ReadTable SoundPlayNoteNumbers, SoundPlayCommand - 64, SoundPlayNote
						
						'In QB, Note 1 = C0
						' C0 = 65 Hz = Note 1
						' A1 = 220 Hz = Note 22 (wikipedia: A3/key 37)
						' A2 = 440 Hz = Note 34
						' A3 = 880 Hz = Note 46
						' B6 = 7902 Hz = Note 84
						'Get note number
						SoundPlayNote = SoundPlayOctave * 12 + SoundPlayNote
						'Get note duration
						If SoundPlayNumber = 0 Then
							SoundPlayNoteLen = SoundPlayLength
						Else
							SoundPlayNoteLen = SoundPlayNumber
						End If
						'Is note sharp or flat?
						If SoundPlayNoteType = SOUND_NOTE_FLAT Then
							SoundPlayNote -= 1
							If SoundPlayNote = 0 Then
								SoundPlayNote = 1
							End If
						End If
						If SoundPlayNoteType = SOUND_NOTE_SHARP Then
							SoundPlayNote += 1
							If SoundPlayNote = 85 Then
								SoundPlayNote = 84
							End If
						End If
						
						SoundPlayTone SoundPlayNote, SoundPlayNoteLen
					End If
				End If
				
				'Command is pause (P)
				If SoundPlayCommand = "P" Then
					'Treat as N0 command, but set length
					If SoundPlayNumber = 0 Then
						SoundPlayNoteLen = SoundPlayLength
					Else
						SoundPlayNoteLen = SoundPlayNumber
					End If
					SoundPlayTone 0, SoundPlayNoteLen
				End If
				
				'Command is note (N)
				If SoundPlayCommand = "N" Then
					SoundPlayTone SoundPlayNumber, SoundPlayLength
					'SoundPlayToneEnd
		
				End If
				
				'Command is length set (L)
				If SoundPlayCommand = "L" Then
					'Note length must be 1 to 8
					If SoundPlayNumber >= 1 Then
						If SoundPlayNumber <= 8 Then
							SoundPlayLength = SoundPlayNumber
						End If
					End If
				End If
				
				'Command is tempo set (T)
				If SoundPlayCommand = "T" Then
					If SoundPlayNumber >= 32 Then
						SoundPlayTempo = SoundPlayNumber
					End If
				End If
				
				'Command is octave set (O)
				If SoundPlayCommand = "O" Then
					SoundPlayOctave = SoundPlayNumber
				End If
				'Command is octave increment/decrement
				If SoundPlayCommand = "<" Then
					If SoundPlayOctave > 0 Then SoundPlayOctave -= 1
				End If
				If SoundPlayCommand = ">" Then
					SoundPlayOctave += 1
				End If
				'Verify that octave is still valid, must be 0 to 6
				If SoundPlayOctave > 6 Then SoundPlayOctave = 6
				
			End If
			
			'Store new command
			SoundPlayCommand = SoundPlayByte
			SoundPlayNumber = 0
			SoundPlayNoteType = SOUND_NOTE_NORMAL
			SoundPlayDots = 0
		
		Else
			'Buffer parameters
			'Param is number - what does it belong to?
			If SoundPlayByte >= "0" Then
				If SoundPlayByte <= "9" Then
					SoundPlayNumber = SoundPlayNumber * 10 + SoundPlayByte - 48
					
					Goto SoundPlayNextNote
				End If
			End If
			'Param is sharp (# or +) or flat (-)
			If SoundPlayByte = "#" Then SoundPlayNoteType = SOUND_NOTE_SHARP
			If SoundPlayByte = "+" Then SoundPlayNoteType = SOUND_NOTE_SHARP
			If SoundPlayByte = "-" Then SoundPlayNoteType = SOUND_NOTE_FLAT
			If SoundPlayByte = "." Then SoundPlayDots += 1
		End If
		
		SoundPlayNextNote:
		
	Next
	
End Sub

''' Plays a sequence of RTTTL notes
''' @param SoundPlayData RTTTL sequence to play
Sub PlayRTTTL(SoundPlayData As String)
	'Count colons, decide what mode
	'Due to RAM limitations, this sub must accept partial songs
	'If 2 colons present, assume name:params:song
	'If 1 colon, assume params:song
	'If no colon, assume song only (probably continuation of previous)
	SoundPlayMode = 2
	SoundPlayBytes = SoundPlayData(0)
	For SoundPlayCurrNote = 1 to SoundPlayBytes
		If SoundPlayData(SoundPlayCurrNote) = ":" Then
			SoundPlayMode -= 1
		End If
	Next
	
	'Parse commands
	SoundPlayNumber = 0
	SoundPlayBytes += 1
	For SoundPlayCurrNote = 1 To SoundPlayBytes
		
		'Get command
		If SoundPlayCurrNote = SoundPlayBytes Then
			SoundPlayByte = ","
		Else
			SoundPlayByte = SoundPlayData(SoundPlayCurrNote)
		End If
		
		'Convert to all upper case
		If SoundPlayByte <= "Z" Then
			If SoundPlayByte >= "A" Then
				SoundPlayByte += 32
			End If
		End If
		
		'Parse byte
		
		'Command
		If SoundPlayByte >= "a" Then
			If SoundPlayByte <= "z" Then
				SoundPlayCommand = SoundPlayByte
				SoundPlayNumber2 = SoundPlayNumber
				SoundPlayNumber = 0
				Goto SoundPlayRTTTLNextNote
			End If
		End If
		
		'Number
		If SoundPlayByte >= "0" Then
			If SoundPlayByte <= "9" Then
				SoundPlayNumber = SoundPlayNumber * 10 + SoundPlayByte - 48
				Goto SoundPlayRTTTLNextNote
			End If
		End If
		
		'What parsing mode?
		Select Case SoundPlayMode
			'Parsing name
			Case 0
				If SoundPlayByte = ":" Then
					SoundPlayMode += 1
					SoundPlayCommand = SOUND_CMD_NONE
					SoundPlayNumber = 0
				End If
				
			'Parsing default values
			Case 1
				'Colon, save setting, change to song mode
				If SoundPlayByte = ":" Then
					SoundPlayMode += 1
					Goto SoundPlayRTTTLComma
				End If
				'Comma, save setting
				If SoundPlayByte = "," Then
					SoundPlayRTTTLComma:
					
					If SoundPlayCommand = "d" Then SoundPlayLength = SoundPlayNumber
					'RTTTL octave 4 = QBASIC octave 2
					If SoundPlayCommand = "o" Then SoundPlayOctave = SoundPlayNumber - 2
					If SoundPlayCommand = "b" Then SoundPlayTempo = SoundPlayNumber
					SoundPlayNumber = 0
					SoundPlayCommand = SOUND_CMD_NONE
				End If
				
			'Parsing song
			Case 2
				'Sharp note
				If SoundPlayByte = "#" Then SoundPlayNoteType = SOUND_NOTE_SHARP
				'Increase length
				If SoundPlayByte = "." Then SoundPlayDots += 1
				
				'Play note
				If SoundPlayByte = "," Then
					'SoundPlayNumber2: Duration
					'SoundPlayCommand: Letter of note
					'SoundPlayNumber: Octave
					
					If SoundPlayCommand <> SOUND_CMD_NONE Then
						'Pause
						If SoundPlayCommand = "p" Then
							'Treat as N0 command, but set length
							If SoundPlayNumber2 = 0 Then
								SoundPlayNoteLen = SoundPlayLength
							Else
								SoundPlayNoteLen = SoundPlayNumber2
							End If
							SoundPlayTone 0, SoundPlayNoteLen
							
						'Play note
						Else
							'Get note octave and length
							If SoundPlayNumber = 0 Then
								SoundPlayNoteOctave = SoundPlayOctave
							Else
								SoundPlayNoteOctave = SoundPlayNumber - 2
							End If
							If SoundPlayNumber2 = 0 Then
								SoundPlayNoteLen = SoundPlayLength
							Else
								SoundPlayNoteLen = SoundPlayNumber2
							End If
							'Play note
							'Translate to note number
							ReadTable SoundPlayNoteNumbers, SoundPlayCommand - 96, SoundPlayNote
							
							'Get note number
							SoundPlayNote = SoundPlayNoteOctave * 12 + SoundPlayNote
							'SoundPlayToneNumber SoundPlayCommand, SoundPlayNoteType, SoundPlayNoteLen
							'Get note duration
							If SoundPlayNoteLen = 0 Then
								SoundPlayNoteLen = SoundPlayLength
							End If
							'Is note sharp or flat?
							If SoundPlayNoteType = SOUND_NOTE_SHARP Then
								SoundPlayNote += 1
								If SoundPlayNote = 85 Then
									SoundPlayNote = 84
								End If
							End If
							
							SoundPlayTone SoundPlayNote, SoundPlayNoteLen
							
							If SoundPlayDots <> 0 Then
								SoundPlayTone SoundPlayNote, SoundPlayNoteLen * 2
							End If
							
							'Eighth note silence at end of note
							'SoundPlayToneEnd
							
						End If
						
						SoundPlayNumber = 0
						SoundPlayNumber2 = 0
						SoundPlayNoteType = SOUND_NOTE_NORMAL
						SoundPlayCommand = SOUND_CMD_NONE
						SoundPlayDots = 0
					End If
					
				End If
				
		End Select
		SoundPlayRTTTLNextNote:
		
	Next
End Sub

'''Play a note
'''@param SoundPlayNote Note to play (0 to 84, 0 = rest, 1 = C0)
'''@param SoundPlayNoteLen Length of note (1 = whole, 2 = half, 4 = quarter, etc)
'''@hide
Sub SoundPlayTone(In SoundPlayNote, In SoundPlayNoteLen)
	
	'Rest?
	If SoundPlayNote = 0 Then
		'Rest for SoundPlayNoteLen
		' Tempo is number of L4s/min
		' Each L4 is 60000 / tempo long
		' Delay in ms = 60000 / tempo / 4 / length
		' 120 L4s/min = 2 L4s/sec, so 1 L4 = 0.5 s. 1 L2 = 1 s, 1 L1 = 2 s
		' Delay = 2000 / NoteLen (for tempo 120)
		' Delay = 2000 * 120 / Tempo / NoteLen
		' Delay = 60000 / Tempo * 4 / NoteLen
		Wait 60000 / SoundPlayTempo * 4 / SoundPlayNoteLen ms
		
	Else
		'Get half period of tone
		'First 24 notes have same half period in table as next 24
		SoundPlayTemp = SoundPlayNote
		If SoundPlayTemp > 24 Then SoundPlayTemp -= 24
		ReadTable SoundPlayNotePeriods, SoundPlayTemp, SoundPlayPeriod
		
		'Calculate number of cycles
		' Example: T120, L4, 1 kHz
		' 120 L4/min, so total time len = 60 / 120 = 0.5 s
		' Cycles = L / T = 0.5 / 0.001 = 500 cycles
		' Cycles = 60 / Tempo / Period = 60 / (4 / NoteLen * Tempo) / Period
		' Cycles = 240 / NoteLen / Tempo / Period (Ex: 240 / 4 / 120 / 0.001)
		' Cycles = 240 / NoteLen / Tempo * Freq (Ex: 240 / 4 / 120 * 1000 = 500)
		' Cycles = 240 * Freq / NoteLen / Tempo
		
		' Ranges:
		'   NoteLen: 1 to 16
		'   Tempo: 32 to 255
		'   Period: 6 to 191
		
		' Or, use period
		' Cycles = 240 / NoteLen / Tempo / Period (Ex: 240 / 4 / 120 / 0.001)
		' Period is multiplied by 12500 for notes <= 24, 5000 for notes >= 25
		' For note 34 (440 Hz), need 880 cycles (Whole note, 120 L4/min, note len = 2 s):
		' Calculated (half) period for 440 Hz = 114 (20 us units)
		'     Cycles = 240 / NoteLen / Tempo / Period
		'            = 240 * 250 / NoteLen / Tempo * 200 / Period
		'            = 60000 / Tempo * 10 / NoteLen * 20 / Period
		'            = 60000 / Tempo * 10 / NoteLen * 2 / Period * 10
		Dim SoundPlayCycles As Word
		SoundPlayCycles = 60000 / SoundPlayTempo
		
		If SoundPlayNote > 24 Then
			'Each loop takes 2 * SoundPlayPeriod * 10 us.
			'Each note should take 60000 / Tempo * 4 / NoteLen
			'Loops needed: time needed / loop time
			'Cycles = (60000 / Tempo * 4 / NoteLen) / (2 * SoundPlayPeriod * 0.01)
			'Cycles = 60000 / Tempo * 4 / NoteLen / 2 / SoundPlayPeriod / 0.01
			'Cycles = 60000 / Tempo * 4 / NoteLen / 2 / SoundPlayPeriod * 100
			SoundPlayCycles = SoundPlayCycles * 20 / SoundPlayNoteLen / SoundPlayPeriod * 10
		Else
			SoundPlayCycles = SoundPlayCycles * 5 / SoundPlayNoteLen / SoundPlayPeriod * 10
		End If
		
		'Use high or low frequency?
		If SoundPlayNote <= 24 Then
			'Low when tone <= 24 (T = 40 us units)
			Repeat SoundPlayCycles
				Set SOUNDOUT On
				Repeat SoundPlayPeriod
					Wait 40 us
				End Repeat
				Set SOUNDOUT Off
				Repeat SoundPlayPeriod
					Wait 40 us
				End Repeat
			End Repeat
		Else
			'High when tone >= 25 (T = 10 us units)
			Repeat SoundPlayCycles
				PulseOut SOUNDOUT, SoundPlayPeriod 10us
				Wait SoundPlayPeriod 10us
			End Repeat
		End If
	End If
End Sub

'''Silence at the end of a tone
'''@hide
Sub SoundPlayToneEnd
	'Eighth note silence at end of note
	'If tempo = 120, there are 120 L4s/minute
	'So 2 L4/s a second, or 4 L8/s second
	'With tempo = 120, need to wait for 125 ms
	'Wait 15000 / SoundPlayTempo ms
End Sub

'Table to translate from note letter to note number
Table SoundPlayNoteNumbers
	10 'A
	12 'B
	1 'C
	3 'D
	5 'E
	6 'F
	8 'G
End Table

'Only includes notes 25 to 94
'Notes 1 to 24 have identical values to notes 25 to 49
'Notes 1 to 24 use 40 us units, notes 25 to 84 use 10 us units.
Table SoundPlayNotePeriods
   191
   180
   170
   161
   152
   143
   135
   128
   120
   114
   107
   101
    96
    90
    85
    80
    76
    72
    68
    64
    60
    57
    54
    51
    48
    45
    43
    40
    38
    36
    34
    32
    30
    28
    27
    25
    24
    23
    21
    20
    19
    18
    17
    16
    15
    14
    13
    13
    12
    11
    11
    10
     9
     9
     8
     8
     8
     7
     7
     6
End Table

'''Check if byte is a command
'''@hide
Function SoundPlayCheckCommand(SoundPlayByte)
	SoundPlayCheckCommand = TRUE
	If SoundPlayByte = SOUND_CMD_NONE Then Exit Function
	If SoundPlayByte >= "A" Then
		If SoundPlayByte <= "G" Then Exit Function
		If SoundPlayByte = "L" Then Exit Function
		If SoundPlayByte = "N" Then Exit Function
		If SoundPlayByte = "O" Then Exit Function
		If SoundPlayByte = "P" Then Exit Function
		If SoundPlayByte = "T" Then Exit Function
	End If
	If SoundPlayByte = "<" Then Exit Function
	If SoundPlayByte = ">" Then Exit Function
	
	SoundPlayCheckCommand = FALSE
End Function
