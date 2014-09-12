;Program compiled by Great Cow BASIC (0.9 22/9/2013)
;Need help? See the GCBASIC forums at http://sourceforge.net/projects/gcbasic/forums,
;check the documentation or email w_cholmondeley at users dot sourceforge dot net.

;********************************************************************************

;Set up the assembler options (Chip type, clock source, other bits and pieces)
 LIST p=16F628A, r=DEC
#include <P16F628A.inc>
 __CONFIG _INTOSC_OSC_NOCLKOUT & _MCLRE_OFF & _LVP_OFF & _WDT_OFF

;********************************************************************************

;Set aside memory locations for variables
SYSBITVAR0	EQU	32

;********************************************************************************

;Vectors
	ORG	0
	goto	BASPROGRAMSTART
	ORG	4
	retfie

;********************************************************************************

;Start of program memory page 0
	ORG	5
BASPROGRAMSTART
;Call initialisation routines
	call	INITSYS

;Start of the main program
	banksel	TRISB
	bcf	TRISB,0
	bsf	TRISB,1
SysDoLoop_S1
	banksel	PORTB
	btfss	PORTB,1
	goto	ENDIF1
	bsf	PORTB,0
	bsf	PORTB,0
ENDIF1
	goto	SysDoLoop_S1
SysDoLoop_E1
BASPROGRAMEND
	sleep
	goto	BASPROGRAMEND

;********************************************************************************

INITSYS
	movlw	7
	movwf	CMCON
	clrf	PORTA
	clrf	PORTB
	return

;********************************************************************************


 END
