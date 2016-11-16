;**************************************************************************
;*   Whirly-GIG (Graphical Information Gyro)                              *
;*                                                                        *
;*   Copyright (C) 2002 Craig Shelley                                     *
;*   This program is free software; you can redistribute it and/or modify *
;*   it under the terms of the GNU General Public License as published by *
;*   the Free Software Foundation; either version 2, or (at your option)  *
;*   any later version.                                                   *
;*                                                                        *
;*   This program is distributed in the hope that it will be useful,      *
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of       *
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
;*   GNU General Public License for more details.                         *
;*                                                                        *
;*   You should have received a copy of the GNU General Public License    *
;*   along with this program; if not, write to the Free Software          *
;*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.            *
;*                                                                        *
;**************************************************************************



	PROCESSOR 16c84
	__CONFIG B'11111111110101'
	RADIX dec



#DEFINE TEXT "TRW AERONAUTICAL SYSTEMS         YEAR IN INDUSTRY EXHIBITION        HELEN DAWSON, BRETT WALTON, MALCOLM WRIGHT, JAMES BARRETT, NNEKA OKOYE, EMMA BROADLEY, BEN SELLERS, DAVE WILLIAMS, ALEX BAINES, TOBY O'SULLIVAN, JAMES MASHHOURI AND CRAIG SHELLEY       "

#DEFINE TEXTSPEED 2
#DEFINE TEXTWIDTH 150

#DEFINE PAGE0 BCF STATUS,5
#DEFINE PAGE1 BSF STATUS,5
INDF		EQU H'00'
TMR0		EQU H'01'
PCL		EQU H'02'
STATUS		EQU H'03'
FSR		EQU H'04'
PORTA		EQU H'05'
PORTB		EQU H'06'
NIL		EQU H'07'
EEDATA		EQU H'08'
EEADR		EQU H'09'
PCLATH		EQU H'0A'
INTCON		EQU H'0B'
W		EQU 0
F		EQU 1
DELAY1		EQU H'20'
DELAY2		EQU H'21'
DELAY3		EQU H'22'
LED		EQU H'23'
SCANPOS		EQU H'29'	;position left-right 0-136 ish
PARTSHIFT	EQU H'2C'
CHARSELECT	EQU H'2D'
PARTSELECT	EQU H'2E' 
CHARSHIFT	EQU H'2F'
CHARSHIFTW	EQU H'30'
PARTSHIFTW	EQU H'31'
CHARINDEX	EQU H'32'
ASCIICODE	EQU H'33'

#DEFINE TEXTLENGTH  TEXTEND - TEXTSTART -1



	ORG 0	
	GOTO INIT
	ORG 4
	GOTO CYCLE


;startup sequence, and initialisation


INIT
	CLRWDT
	CLRF PORTA
	CLRF PORTB
	CLRF LED
	PAGE1
	MOVLW B'11110000'
	MOVWF PORTA
	MOVLW B'11000001'
	MOVWF PORTB
	MOVLW B'11001100'
	MOVWF TMR0
	PAGE0
	


SPIN
	MOVLW 2
	XORWF PORTA, F
	MOVF LED, W
	MOVWF DELAY2

SPIN2
	DECFSZ DELAY1, F
	GOTO SPIN2
	MOVLW 40
	MOVWF DELAY1
	DECFSZ DELAY2, F
	GOTO SPIN2

	CLRWDT
	CALL LEDUPDATE
	DECFSZ LED, F
	GOTO SPIN

	


;each time the gig completes a cycle, give it another pulse of juce
;after 8 cycles, it is fair to assume that there are no obstructions, give fullpower

	COMF LED, F
FREESPIN
	BTFSS PORTB, 0
	GOTO FREESPIN
FREESPIN2
	BTFSC PORTB, 0
	GOTO FREESPIN2

	CLRWDT

	CALL LEDUPDATE
	BCF STATUS, 0
	RRF LED, F
	MOVF LED, F
	BTFSS STATUS, 2
	GOTO FREESPIN	


;clear memory registers and enable interrupts

	MOVLW B'10010000'
	MOVWF INTCON
	CLRF PARTSHIFT
	CLRF CHARSHIFT
	BSF PORTA, 1




;this routine is called on each cycle of the gig
;record the time taken for the cycle in ROTTIME
;adjust the shift position PARTSHIFT, depending on the TEXTSPEED
;reset the led position SCANPOS to the current shift position 
;clear any timers




CYCLE
	CLRWDT
	
	MOVLW TEXTSPEED
	MOVWF SCANPOS
	
	MOVF CHARSHIFT, W   ;create the working copies of CHARSHIFT and PARTSHIFT
	MOVWF CHARSHIFTW
	MOVF PARTSHIFT, W
	MOVWF PARTSHIFTW	
	
	CALL SELECTDATA

	MOVF CHARSELECT, W
	MOVWF CHARSHIFT
	MOVWF CHARSHIFTW
	MOVF PARTSELECT, W
	MOVWF PARTSHIFT
	MOVWF PARTSHIFTW

	CLRF SCANPOS


;enter main loop
;traverse TEXTTABLE for the correct character code
;traverse CHARTABLE and return the correct line of the correct character
;update the display with correct delays

MAINLOOP
	CALL SELECTDATA
	CALL GETCHAR
	MOVWF ASCIICODE
	CALL ASCIIDECODE
	CALL RENDERFONT


	MOVWF LED
	CALL LEDUPDATE
	CALL PAUSE
	CLRF LED
	CALL LEDUPDATE
	CALL PAUSE


	BCF INTCON, 1
	BSF INTCON, 7

;every 8 x pixels, select the next character

	INCF SCANPOS, F
	GOTO MAINLOOP


SELECTDATA
	MOVF PARTSHIFTW, W
	ADDWF SCANPOS, W
	ANDLW B'00000111'
	MOVWF PARTSELECT

	MOVF PARTSHIFTW, W
	ADDWF SCANPOS, W
	ANDLW B'11111000'
	MOVWF CHARSELECT
	BCF STATUS, 0
	RRF CHARSELECT, F
	RRF CHARSELECT, F
	RRF CHARSELECT, W
	ADDWF CHARSHIFTW, W
	MOVWF CHARSELECT

	
	SUBLW TEXTLENGTH
	BTFSC STATUS, 0		;oops, ran out of text, seemlessly carry on from the start of the string 
	RETURN
	CLRF SCANPOS		;pretend to be at position 0
	CLRF PARTSHIFTW		;clear the shifts and set the output values PARTSELECT and CHARSELECT to 0 too
	CLRF CHARSHIFTW
	CLRF CHARSELECT           ;no need to clear PARTSELECT since it should be 0 anyway
	RETURN



GETCHAR
	MOVLW HIGH TEXTSTART
	MOVWF PCLATH
	MOVF CHARSELECT, W
	ADDLW LOW TEXTSTART
	MOVWF PCL



RENDERFONT
	BCF STATUS, 0
	RLF CHARINDEX, F
	RLF PCLATH, F
	RLF CHARINDEX, F
	RLF PCLATH, F
	RLF CHARINDEX, F
	RLF PCLATH, W
	ADDLW HIGH CHARSET
	MOVWF PCLATH


	MOVF PARTSELECT, W
	IORWF CHARINDEX, W
	ADDLW LOW CHARSET
	BTFSC STATUS, 0
	INCF PCLATH, F
	MOVWF PCL





;this routine will block wait 
;this is used as an led delay, purly to time the text width

PAUSE
	MOVLW TEXTWIDTH
	MOVWF DELAY1
PAUSE2
	DECFSZ DELAY1, F
	GOTO PAUSE2
	RETURN


LEDUPDATE
	BCF PORTA, 2
	BTFSC LED, 0
	BSF PORTA, 2
	BCF PORTA, 0
	BTFSC LED, 1
	BSF PORTA, 0
	BCF PORTA, 3
	BTFSC LED, 2
	BSF PORTA, 3
	BCF PORTB, 1
	BTFSC LED, 3
	BSF PORTB, 1
	BCF PORTB, 2
	BTFSC LED, 4
	BSF PORTB, 2
	BCF PORTB, 3
	BTFSC LED, 5
	BSF PORTB, 3
	BCF PORTB, 4
	BTFSC LED, 6
	BSF PORTB, 4
	BCF PORTB, 5
	BTFSC LED, 7
	BSF PORTB, 5
	RETURN

include Courier-Font.asm

	

	ORG H'0300'
TEXTSTART
	DT TEXT
TEXTEND

END
