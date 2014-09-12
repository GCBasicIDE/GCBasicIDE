'    Lego Mindstorms interface routines for the GCBASIC compiler
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

'    For more info on Lego Mindstorms, please see http://www.legomindstorms.com.
'    This file is included for the convenience of the user, and does not represent any
'    form of endorsement of or association with GCBASIC by Lego.

'Notes:
' Requires that IR_In is defined as a constant

'Subroutines
'Standard firmware compatible
' SendMessage (message)			Send a message using standard Lego Mindstorms protocol
' SendOpcode (data(), size)		Send the array data() using the standard Lego protocol
' ReceiveOpcode (data(), size)		Read a standard-protocol opcode into the array data()
'BrickOS/LeJOS compatible
' SendLNP (data(), size)		Send the array data() using the LNP protocol
' SendLNPAdd (address, data(), size)	Send the array data() to the given address, using LNP
' ReceiveLNP (data(), size)		Read from the LNP protocol, storing the received data
' SetLNPAddress (port, address)		Set own LNP address to selected value between 0 and 15. Port is usually 1

'Functions
' Message				Receive a message using standard Lego Mindstorms protocol

'Specify the initialisation routine for this file
#startup InitLego

'Serial Port Settings
#define RecALow IR_In On
#define RecAHigh IR_In Off
#define SendAHigh PWMOn 
#define SendALow PWMOff

'Misc Settings
#define PWM_Freq 38
#define PWM_Duty 50
'#define Ser_Links 1

sub InitLego
 InitSer(1,r2400,WaitForStart+1,8,1,odd,invert)
 OwnLNPPort = 1
end sub

sub SendMessage(message)
 SerSend(1, 0x55)		'Header
 SerSend(1, 0xff)
 SerSend(1, 0x00)
 SerSend(1, 0xf7)		'Message Command
 SerSend(1, 0x08)
 SerSend(1, message)		'Message Value
 SerSend(1, !message)
 SerSend(1, 0xf7+message)   	'Checksum
 SerSend(1, 0x08+(!message))
end sub

function Message
 Message = 0
 if RecALow then exit function
 'Header
 SerReceive(1, LegoTemp)
 if LegoTemp = 0x55 then SerReceive(1, LegoTemp)
 SerReceive(1, LegoTemp)
 'Command
 SerReceive(1, LegoTemp)
 if LegoTemp <> 0xf7 then exit function
 SerReceive(1, LegoTemp)
 'Message
 SerReceive(1, Message)
 SerReceive(1, LegoTemp)
 'Checksum
 SerReceive(1, LegoTemp)
 SerReceive(1, LegoTemp)
end sub

'Standard protocol send
sub SendOpcode(LegoArray(), LegoByteCount) #NR
 LegoChecksum = 0

 'Header
 SerSend(1, 0x55)
 SerSend(1, 0xff)
 SerSend(1, 0x00)

 'Opcodes
 for LegoByte = 1 to LegoByteCount
  LegoTemp = LegoArray(LegoByte)
  SerSend(1, LegoTemp)
  SerSend(1, !LegoTemp)
  LegoChecksum = LegoChecksum + LegoTemp
 next

 'Checksum
 SerSend(1, LegoChecksum)
 SerSend(1, !LegoChecksum)
end sub

'Standard protocol receive
sub ReceiveOpcode(LegoArray(), LegoByteCount)
 LegoByteCount = 0
 LegoChecksum = 0
 if RecALow then exit function
 
 'Header
 SerReceive(1, LegoTemp)
 if LegoTemp = 0x55 then SerReceive(1, LegoTemp)
 SerReceive(1, LegoTemp)
 
 'Command
 SerReceive(1, LegoCommand)
 SerReceive(1, LegoTemp)
 LegoArray(1) = LegoCommand
 LegoParams = LegoCommand AND 7
 LegoByteCount = LegoParams + 1
 if LegoParams = 0 then goto ReceiveLegoChecksum

 'Parameters
 for LegoByte = 1 to LegoParams
  SerReceive(1, LegoTemp)
  LegoArray(LegoByte+1) = LegoTemp
  LegoChecksum = LegoChecksum + LegoTemp
  SerReceive(1, LegoTemp)
 next

 'Checksum
ReceiveLegoChecksum:
 SerReceive(1, LegoTemp)
 LegoChecksum = LegoChecksum - LegoTemp
 SerReceive(1, LegoTemp)
end sub

'LNP Integrity send
sub SendLNP(LNPArray(), LNPByteCount) #NR
 LNPChecksum = 0xef + LNPByteCount '0xf0 + byte count - 1

 'Send type
 SerSend(1, 0xf0)
  
 'Byte Count
 SerSend(1, LNPByteCount)
 
 'Data
 for LNPByte = 1 to LNPByteCount
  LNPTemp = LNPArray(LNPByte)
  LNPChecksum = LNPTemp + LNPChecksum
  SerSend(1, LNPTemp)
 next
 
 'Checksum
 SerSend(1, LNPChecksum)
end sub

'LNP Addressing send
sub SendLNPAdd(LNPPort, LNPAddress, LNPArray(), LNPByteCount)
 LNPChecksum = 0xf2 + LNPByteCount '0xf1 + byte count + 2 - 1
 
 'Send type
 SerSend(1, 0xf1)
  
 'Byte Count
 SerSend(1, LNPByteCount+2)
 
 'Destination
 LNPTemp = LNPPort
 'rotate LNPTemp left
 'rotate LNPTemp left
 'rotate LNPTemp left
 'rotate LNPTemp left
 LNPTemp = (Swap4(LNPTemp) and 0xf0)+(LNPAddress and 0x0f)
 SerSend(1, LNPTemp)
 LNPChecksum = LNPTemp + LNPChecksum

 'Source 
 LNPTemp = OwnLNPPort
 'rotate LNPTemp left
 'rotate LNPTemp left
 'rotate LNPTemp left
 'rotate LNPTemp left
 LNPTemp = (Swap4(LNPTemp) and 0xf0)+(OwnLNPAddress and 0x0f)
 SerSend(1, LNPTemp)
 LNPChecksum = LNPTemp + LNP_Checksum
 
 'Data
 for LNPByte = 1 to LNPByteCount
  LNPTemp = LNPArray(LNPByte)
  LNPChecksum = LNPTemp + LNPChecksum
  SerSend(1, LNPTemp)
 next
 
 'Checksum
 SerSend(1, LNPChecksum)
end sub

'LNP Receive
sub ReceiveLNP(LNPArray(), LNPByteCount)
 LNPByteCount = 0
 LNPChecksum = 0
 if RecALow then exit sub
 
 SerReceive(1, LNPType) 'Header
 SerReceive(1, LNPByteCount) 'Byte Count
 LNPChecksum = LNPType + LNPByteCount - 1
 if LNPType = 0xf0 then goto LNPIntReceive
 if LNPType = 0xf1 then goto LNPAddReceive
 LNPChecksum = FALSE
 exit function

LNPAddReceive:
 SerReceive(1, LNP_Receiver)
 SerReceive(1, LNP_Sender)
 LNPByteCount = LNPByteCount - 2
 LNPChecksum = LNP_Checksum + LNP_Receiver + LNP_Sender

LNPIntReceive:
 for LNPByte = 1 to LNPByteCount
  SerReceive(1, LNPTemp)
  LNPArray(LNPByte) = LNPTemp
  LNPChecksum = LNPChecksum + LNPTemp
 next

 SerReceive(1, LegoTemp)
 if LegoTemp = LNPChecksum then LNPChecksum = TRUE: exit function
 LNPChecksum = FALSE
end sub

sub SetLNPAddress(OwnLNPPort, OwnLNPAddress) #NR
end sub