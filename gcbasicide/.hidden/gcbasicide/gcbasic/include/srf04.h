'    Ultrasonic distance sensor routines for the GCBASIC compiler
'    Copyright (C) 2006-2008 Hugh Considine

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

'These routines were designed and tested with the SRF04 ultrasonic sensors. They have not been tried with any
'other models of ultrasonic sensor, and may require some modification to work.

'To read a sensor:
'	1. Use #define to set USUnits to m, cm, feet or inch (Optional, defaults to cm)
'	2. For each sensor, set USxPing and USxEcho to the trigger and echo ports
'	   respectively, where x is the number of the sensor. If there is only a single
'      pin (SRF05), set both USxPing and USxEcho to the pin
'	3. Call the function USDistance(US_Sensor), where US_Sensor is the number
'	   of the sensor to read. For example, to take a measurement with sensor 2
'	   and store the reading in the variable Distance:
'		Distance = USDistance(2)

'If a sensor that does not exist/is faulty is read, or if the reading is out of range,
'then the USDistance function will return 0.

'Updates:
' 19/5/2009: Added support for using same pin as pin and echo
' 25/1/2010: Added documentation for GCGB

'Initialisation (Prevents crash if a sensor is not attached)
#startup InitUSSensor

'Deprecated, don't bother setting
''Number of Sensors (Default 1, max 4)
'#define USSensors 1

'Time for 1 unit
#define cm 52 us
#define metre 5 ms
#define inch 130 us
#define feet 44 10us

'Units (default cm) 
#define USUnits cm

'''Read the distance to the nearest object
'''@param US_Sensor Sensor to read (1 to 4)
'''@return Distance in cm
Function USDistance(US_Sensor) As Word
	USDistance = 0
	
	'Send ping, measure time for echo
	#ifdef US1Ping
		#ifdef OneOf(US2Ping, US3Ping, US4Ping)
			If US_Sensor = 1 Then
		#endif
				#if US1Ping = US1Echo
					Dir US1Ping Out
				#endif
				Pulseout US1Ping, 1 10us
				#if US1Ping = US1Echo
					Dir US1Echo In
				#endif
				
				Wait Until US1Echo ON
				Do While US1Echo ON
					USDistance = USDistance + 1
					If USDistance = 0 Then Exit Function
					Wait USUnits
				Loop
 				
		#ifdef OneOf(US2Ping, US3Ping, US4Ping)
			End If
		#endif
	#endif
	
	#ifdef US2Ping
		#ifdef OneOf(US1Ping, US3Ping, US4Ping)
			If US_Sensor = 2 Then
		#endif
				#if US2Ping = US2Echo
					Dir US2Ping Out
				#endif
				Pulseout US2Ping, 1 10us
				#if US2Ping = US2Echo
					Dir US2Ping In
				#endif
				
				Wait Until US2Echo ON
				Do While US2Echo ON
					USDistance = USDistance + 1
					If USDistance = 0 Then Exit Function
					Wait USUnits
				Loop
				
		#ifdef OneOf(US1Ping, US3Ping, US4Ping)
			End If
		#endif
	#endif
	
	#ifdef US3Ping
		#ifdef OneOf(US1Ping, US2Ping, US4Ping)
			If US_Sensor = 3 Then
		#endif
				#if US3Ping = US3Echo
					Dir US3Ping Out
				#endif
				Pulseout US3Ping, 1 10us
				#if US3Ping = US3Echo
					Dir US3Ping In
				#endif
				
				Wait Until US3Echo ON
				Do While US3Echo ON
					USDistance = USDistance + 1
					If USDistance = 0 Then Exit Function
					Wait USUnits
				Loop
				
		#ifdef OneOf(US1Ping, US2Ping, US4Ping)
			End If
		#endif
	#endif
	
	#ifdef US4Ping
		#ifdef OneOf(US1Ping, US2Ping, US3Ping)
			If US_Sensor = 4 Then
		#endif
				#if US4Ping = US4Echo
					Dir US4Ping Out
				#endif
				Pulseout US4Ping, 1 10us
				#if US4Ping = US4Echo
					Dir US4Ping In
				#endif
				
				Wait Until US4Echo ON
				Do While US4Echo ON
					USDistance = USDistance + 1
					If USDistance = 0 Then Exit Function
					Wait USUnits
				Loop
				
		#ifdef OneOf(US1Ping, US2Ping, US3Ping)
			End If
		#endif
	#endif
	
End Function

'''@hide
Sub InitUSSensor
	'Set pin directions
	'This doesn't need to be done with a single ping/echo line
	#ifdef US1Echo
		#if US1Ping <> US1Echo
			Dir US1Ping Out
			Dir US1Echo In
		#endif
	#endif
	#ifdef US2Echo
		#if US2Ping <> US2Echo
			Dir US2Ping Out
			Dir US2Echo In
		#endif
	#endif
	#ifdef US3Echo
		#if US3Ping <> US3Echo
			Dir US3Ping Out
			Dir US3Echo In
		#endif
	#endif
	#ifdef US4Echo
		#if US4Ping <> US4Echo
			Dir US4Ping Out
			Dir US4Echo In
		#endif
	#endif
End Sub
