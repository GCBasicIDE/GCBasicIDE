'    Subroutines to allow the Great Cow Alpha robot to work with the GCBASIC compiler
'    Copyright (C) 2006  Hugh Considine

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

'THIS FILE IS BASED ON THE PRELIMINARY PORT ALLOCATIONS AS OF 7/6/2006

#chip 16F877A, 20
#startup InitBot

'Standard Outputs
#define RED PORTC.6
#define GREEN PORTC.7
#define BLUE PORTB.5

'Standard Inputs
#define LDR_Left PORTE.0
#define LDR_Right PORTE.1
#define BUTTON PORTB.0
#define Bump_Left PORTB.6
#define Bump_Right PORTB.7
#define Analog_LDR_Left 5
#define Analog_LDR_Right 6

'Expansion Ports

'Analogue Sensor
#define AS_1a PORTA.0
#define AS_1b PORTA.1
#define AS_1Out PORTB.5
#define AS_2a PORTA.2
#define AS_2b PORTA.3
#define AS_2Out PORTD.6
#define AS_3a PORTA.5
#define AS_3b PORTE.2
#define AS_3Out PORTD.7

'Communication Modules
#define CM_1a PORTC.1
#define CM_1b PORTC.2
#define CM_2a PORTC.0
#define CM_2b PORTC.5

'Extra Microcontroller
#define EM_1 PORTD.0
#define EM_2 PORTD.1

'Actuator
#define MO_1a PORTD.2
#define MO_1b PORTD.3
#define MO_2a PORTD.4
#define MO_2b PORTD.5

'Sensor readings
#define PRESSED 0
#define RELEASED 1
#define DARK 0
#define LIGHT 1

'Motors
#define LF PORTB.1
#define LR PORTB.2
#define RF PORTB.3
#define RR PORTB.4

'Initialisation
SUB InitBot

 'Configure Port B latches
 SET OPTION_REG.NOT_RBPU ON

 'Set Port Directions
 DIR A b'00101111'
 DIR B b'11000001'
 DIR C b'00011011'
 DIR D b'00000000'
 DIR E b'00000111'

 'Turn off all ports
 PORTA = 0
 PORTB = 0
 PORTC = 0
 PORTD = 0
 PORTE = 0

 'Disable A/D on port A0-A3, A5, E0-E2
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
 SET LR 1
 SET RR 1
 SET LF 0
 SET RF 0
END SUB

SUB SpinLeft
 SET LR 1
 SET RR 0
 SET LF 0
 SET RF 1
END SUB

SUB SpinRight
 SET LR 0
 SET RR 1
 SET LF 1
 SET RF 0
END SUB

SUB TurnLeft
 SET LR 0
 SET LF 0
 SET RR 0
 SET RF 1
END SUB

SUB TurnRight
 SET LR 0
 SET LF 1
 SET RR 0
 SET RF 0
END SUB

SUB Stop
 SET LR 0
 SET LF 0
 SET RR 0
 SET RF 0
END SUB