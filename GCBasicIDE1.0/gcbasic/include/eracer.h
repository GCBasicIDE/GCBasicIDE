'    Subroutines to allow the eLabtronics eRacer robot to work with Great Cow BASIC
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

'    For more info on the eRacer robot, please visit http://www.elabtronics.com
'    This file is included for the convenience of the user, and does not represent any
'    form of endorsement of GCBASIC by eLabtronics.

'Hardware settings
#chip 16F819, 20
#config MCLR_OFF, CCP1_RB3
#startup InitBot

'Outputs
#define TL_LED PORTA.0
#define TR_LED PORTA.1
#define RED PORTB.0
#define GREEN PORTB.1
#define BUZZER PORTA.4
#define IR_LED PORTB.3
#define SoundOut PORTA.4

'Inputs
#define IR_In PORTB.2
#define LDR_Left PORTA.2
#define LDR_Right PORTA.3
#define BUTTON PORTA.5
#define Analog_LDR_Left 2
#define Analog_LDR_Right 3
#define LeftLDR ReadAD(2)
#define RightLDR ReadAD(3)

'Sensor readings
#define PRESSED 0
#define RELEASED 1
#define DARK 0
#define LIGHT 1

'Motors
#define LR PORTB.4
#define LF PORTB.5
#define RR PORTB.7
#define RF PORTB.6

'Initialisation
SUB InitBot
 'Configure Port B latches
 SET OPTION_REG.NOT_RBPU ON

 'Set port directions
 DIR A b'11101100'
 DIR B b'00000100'

 'Turn off all ports
 PORTA = 0
 PORTB = 0
 
 'Switch off A/D on pins A0-A4
 ADOff
END SUB

'Driving
SUB Forward
 SET LR 0
 SET RR 0
 SET LF 1
 SET RF 1
END SUB

SUB Reverse
 SET LF 0
 SET RF 0
 SET LR 1
 SET RR 1
END SUB

SUB SpinLeft
 SET RR 0
 SET LF 0
 SET RF 1
 SET LR 1
END SUB

SUB SpinRight
 SET RF 0
 SET LR 0
 SET RR 1
 SET LF 1
END SUB

SUB TurnLeft
 SET LR 0
 SET LF 0
 SET RR 0
 SET RF 1
END SUB

SUB TurnRight
 SET LR 0
 SET RR 0
 SET RF 0
 SET LF 1
END SUB

SUB Stop
 SET LR 0
 SET LF 0
 SET RR 0
 SET RF 0
END SUB