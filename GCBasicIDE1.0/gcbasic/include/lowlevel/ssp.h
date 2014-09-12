'    SPI/I2C routines for Great Cow BASIC
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

'To make the PIC pause until it receives an SPI message while in slave mode, set the
'constant "WaitForSPI" at the start of the program. The value does not matter.

'Subs
' SPIMode(Mode)			Set the SPI mode. See the list of constants below
' SPITransfer(Tx, Rx)		Simultaneously send and receive an SPI byte
' I2CSend(value)		Sends given value using I2C
' I2CReceive			Function to receive from I2C

'SPI mode constants
#define MasterFast 13
#define Master 12
#define MasterSlow 11
#define SlaveSS 1
#define Slave 0

#define I2CBaud 100 'Change to desired frequency

#script
I2CBaudTemp = int((ChipMhz * 1000000)/(4000*I2CBaud)) - 1
'error I2CBaudTemp
#endscript

'I2C Mode constants
#define Slave10 2
'use Slave for 7-bit slave mode and Master for master mode

'Turn off SSP
sub SSPOFF
 #IFDEF Var(SSPCON1)
 set SSPCON1.SSPEN off
 #ENDIF
 #IFDEF Var(SSPCON)
 set SSPCON.SSPEN off
 #ENDIF
end sub

sub I2CMode(I2CCurrentMode) #NR

#IFDEF Var(SSPCON1)
 'Turn off I2C function on pins
 set SSPCON1.SSPEN off

 set SSPSTAT.SMP on
 set SSPSTAT.CKE off
 set SSPCON1.CKP on
 set SSPCON1.WCOL Off

 'Select mode and clock

 if I2CCurrentMode = Master then
  set SSPCON1.SSPM3 on
  set SSPCON1.SSPM2 off
  set SSPCON1.SSPM1 off
  set SSPCON1.SSPM0 off
  SSPADD = I2CBaudTemp
 end if

 if I2CCurrentMode = Slave then
  set SSPCON1.SSPM3 off
  set SSPCON1.SSPM2 on
  set SSPCON1.SSPM1 on
  set SSPCON1.SSPM0 off
 end if

 if I2CCurrentMode = Slave10 then
  set SSPCON1.SSPM3 off
  set SSPCON1.SSPM2 on
  set SSPCON1.SSPM1 on
  set SSPCON1.SSPM0 on
 end if

 'Enable I2C function on pins
 set SSPCON1.SSPEN on
#ENDIF

end sub

sub I2CSend(I2C_Tx) #NR

 'Clear WCOL
 SET SSPCON1.WCOL OFF


end sub

function I2CReceive

 'Clear WCOL
#ifdef Var(SSPCON)
 SET SSPCON.WCOL OFF
#endif
#ifdef Var(SSPCON1)
 SET SSPCON1.WCOL OFF
#endif

 'Wait for receive
 SET SSPSTAT.R ON
 Wait while SSPSTAT.R On
 I2CReceive = SSPBUF
 SET SSPSTAT.BF OFF
end function

sub SPITransfer(SPI_Tx, SPI_Rx)

 'Master mode
 if SPICurrentMode > 10 then

  'Clear WCOL
#ifdef Var(SSPCON)
   SET SSPCON.WCOL OFF
#endif
#ifdef Var(SSPCON1)
   SET SSPCON1.WCOL OFF
#endif

  'Put byte to send into buffer
  SSPBUF = SPI_Tx

  'Wait for transfer to complete
  Wait While SSPSTAT.BF off

  'Copy received byte out of buffer
  SPI_Rx = SSPBUF
  SET SSPSTAT.BF off
  
 end if

 'Slave mode
 if SPICurrentMode < 10 then

  'Attempt to send
#ifdef Var(SSPCON)
  SPISlaveTx:
   SET SSPCON.WCOL OFF
   SSPBUF = SPI_Tx
  if SSPCON.WCOL on then goto SPISlaveTx
#endif
#ifdef Var(SSPCON1)
  SPISlaveTx:
   SET SSPCON1.WCOL OFF
   SSPBUF = SPI_Tx
  if SSPCON1.WCOL on then goto SPISlaveTx
#endif

  'Read buffer
  Wait While SSPSTAT.BF off
  SPI_Rx = SSPBUF
  SET SSPSTAT.BF OFF
  
 end if

end sub

sub SPIMode(SPICurrentMode) #NR

#IFDEF Var(SSPCON1)
 'Turn off SPI function on pins
 set SSPCON1.SSPEN off

 set SSPSTAT.SMP off
 set SSPSTAT.CKE off
 set SSPCON1.CKP off

 'Select mode and clock
 set SSPCON1.SSPM3 off
 if SPICurrentMode = MasterFast then
  set SSPCON1.SSPM2 off
  set SSPCON1.SSPM1 off
  set SSPCON1.SSPM0 off
 end if

 if SPICurrentMode = Master then
  set SSPCON1.SSPM2 off
  set SSPCON1.SSPM1 off
  set SSPCON1.SSPM0 on
 end if

 if SPICurrentMode = MasterSlow then
  set SSPCON1.SSPM2 off
  set SSPCON1.SSPM1 on
  set SSPCON1.SSPM0 off
 end if

 if SPICurrentMode = Slave then
  set SSPCON1.SSPM2 on
  set SSPCON1.SSPM1 off
  set SSPCON1.SSPM0 on
 end if

 if SPICurrentMode = SlaveSS then
  set SSPCON1.SSPM2 on
  set SSPCON1.SSPM1 off
  set SSPCON1.SSPM0 off
 end if

 'Enable SPI function on pins
 set SSPCON1.SSPEN on
#ENDIF

#IFDEF Var(SSPCON)

 'Turn off SPI function on pins
 set SSPCON.SSPEN off

 set SSPSTAT.SMP off
 set SSPSTAT.CKE off
 set SSPCON.CKP off

 'Select mode and clock
 set SSPCON.SSPM3 off
 if SPICurrentMode = MasterFast then
  set SSPCON.SSPM2 off
  set SSPCON.SSPM1 off
  set SSPCON.SSPM0 off
 end if

 if SPICurrentMode = Master then
  set SSPCON.SSPM2 off
  set SSPCON.SSPM1 off
  set SSPCON.SSPM0 on
 end if

 if SPICurrentMode = MasterSlow then
  set SSPCON.SSPM2 off
  set SSPCON.SSPM1 on
  set SSPCON.SSPM0 off
 end if

 if SPICurrentMode = Slave then
  set SSPCON.SSPM2 on
  set SSPCON.SSPM1 off
  set SSPCON.SSPM0 on
 end if

 if SPICurrentMode = SlaveSS then
  set SSPCON.SSPM2 on
  set SSPCON.SSPM1 off
  set SSPCON.SSPM0 off
 end if

 'Enable SPI
 set SSPCON.SSPEN on
#ENDIF

end sub