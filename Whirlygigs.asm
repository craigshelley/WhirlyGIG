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

;CODE FOR WHIRLY-GIG (GRAPHICAL INFORMATION GYRO)

#DEFINE TEXT "GRAPHICAL INFORMATION GYRO    -- Craig Shelley"

#DEFINE TEXTSPEED 	3	;3   [1-7]	The speed that the text scrolls
#DEFINE NEWTEXTSPEED	6	;6   [1-7]	The speed for new text entering the display
#DEFINE TEXTWIDTH 	200	;200 [1-255]	The width of the pixels on the characters
#DEFINE BLANKPOS	120	;120 [0-255]	The X pixel number that new text scrolls in from
#DEFINE STARTBLANK	200	;200 [0-255]	The X pixel number that the default string scrolls in from
#DEFINE DATDELAY 	210	;210 [1-255]	Time delay for half a bit of incoming RS232 data (baud rate dependent)



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
S_TEMP 		EQU H'0C'
W_TEMP 		EQU H'0D'
DELAY1		EQU H'0E'
DELAY2		EQU H'0F'
DELAY3		EQU H'10'
LED		EQU H'11'
SCANPOS		EQU H'12'	;position left-right 0-136 ish
PARTSHIFT	EQU H'13'
CHARSELECT	EQU H'14'
PARTSELECT	EQU H'15' 
CHARSHIFT	EQU H'16'
CHARINDEX	EQU H'17'
DAT		EQU H'18'
BUFFPTR		EQU H'19'
ASCIICODE	EQU H'1A'
DISPBLANK	EQU H'1B'
DISPBLANKW	EQU H'1C'

MEMLOW		EQU H'1D'
MEMHIGH  	EQU H'4F'
MEMSIZE 	EQU MEMHIGH - MEMLOW + 1

#DEFINE TEXTLENGTH  TEXTEND - TEXTSTART -1

	ORG 0	
	GOTO INIT
	ORG 4

;Interrupt handling routine
;Determine source of interrupt
;If it is a cycle interrupt start next cycle
;If it is incoming data, decode the data then resuume to normal program execution

	BTFSS INTCON, 2		;Check the intcon register to see what caused the interupt		
	GOTO CYCLE		;If a cycle was detected, start a new cycle of the gig 

        MOVWF W_TEMP		;Save context (working and status registers) using swapf
        SWAPF STATUS, W
        MOVWF S_TEMP


;Data receiving routine
;Input a byte of data from the IO port
;Store it to the next free byte of the buffer
;If the end of the buffer is reached, reset the buffer pointer to the beginning

;This routine works by having a data reception register DAT, rotate right, then the next
;bit is inputted from the port, and stored in to bit 0 of DAT. The routine stops when a marker bit 
;is rotated off the end of the register and into Status:Carry. The marker bit therefore has to be set
;in the DAT register before data is received.
;The PAUSEDAT routine is a half bit delay 

	CLRWDT			;Make sure watchdog does not time out during transfer
	CLRF DAT		;Clear the dat register ready to accept new data
	BSF DAT, 7		;Set the marker bit so that exactly 8 bits are received
	CALL PAUSEDAT		;Advance to the centre of the start bit

INPUTDAT2			;Start the main input loop
	CALL PAUSEDAT		;These two pauses advance
	CALL PAUSEDAT		;to the next bit
	BCF STATUS, 0		;Make sure that the carry is clear before rotating
	RRF DAT, F		;Rotate the data data reception register
	BTFSC PORTA, 4		;Load in the data from the port
	BSF DAT, 7		;Store it into bit 7 of the data reception register
	BTFSS STATUS, 0		;Check to see if the marker bit rotated off into carry
	GOTO INPUTDAT2		;if it didn't load in more data bits

;If the data stored in DAT was 255 then turn the gig off. 	

	INCF DAT, W		;If DAT was 255 then incrementing should make it 0
	BTFSC STATUS, 2		;If DAT=0
	GOTO TURNOFF		;Then goto TURNOFF

;At this point the data has been received, and is now stored in DAT
;It must now be stored into the buffer

	MOVF BUFFPTR, W		;Get the current buffer pointer 
	MOVWF FSR		;Set up the indirect memory addressing
	MOVF DAT, W		;Get the data value from DAT
	MOVWF INDF		;Store it into the destination at buffer pointer

;At this point the data has been stored into the buffer
;The new buffer pointer must now be calculated for next time

	INCF BUFFPTR, F		;Advance the buffer pointer
	MOVF BUFFPTR, W
	SUBLW MEMHIGH		;Check to see if the memory address is within range ie <= MEMHIGH
	MOVLW MEMLOW		
	BTFSS STATUS, 0		;If not, then reset the buffer pointer to the first memory address MEMLOW
	MOVWF BUFFPTR		 

;All buffer operations are now complete
;It is now time to set up interrupts again, restore context, then return to the program

	MOVLW 255		;The interrupts are provided from the internal timer overflowing when the data is received
	MOVWF TMR0		;The timer register must be set to the point just before it overflows
	BCF INTCON, 2		;Clear the interrupt bit flag to stop endless interrupts

        SWAPF S_TEMP, W		;Restore context using the swapf instruction
        MOVWF STATUS
        SWAPF W_TEMP, F
        SWAPF W_TEMP, W

	RETFIE




;Start of Program
;When the PIC is reset, execution starts here
;All IO ports must be configured
;The data buffer must be cleared
;The motor must be run at half power until it is going fast enough
;When the motor is going fast enough, let it free spin for 8 revolutions
;If it is still going fast enough, clear some memory registers, set up interrupts, and start the main loop



INIT	
	CLRWDT			;Clear the watchdog timer to allow time to do the init sequence
	CLRF PORTA		;Turn all outputs off on both ports	
	CLRF PORTB
	BSF STATUS, 5		;Set bank select flag to enable access into bank 1 for the TRIS registers
	MOVLW B'11110000'
	MOVWF PORTA		;Setup PORTA to have correct pins set to input and output
	MOVLW B'11000001'
	MOVWF PORTB		;Setup PORTB to have correct pins set to input and output
	MOVLW B'11111100'
	MOVWF TMR0		;Setup the option register, taking clock from RA4 falling 
				;edge and prescailer assigned to watchdog
	BCF STATUS, 5			;Back to bank 0
	

	


;This routine spins the gig at half power for a period of time
;This is done by rapidly turning on and off the power to the motor
;The frequency is designed to increase during the period by decreasing the pulse delay
;Since it is equal mark/space, the power stays the same
;Once the pulse delay reaches zero, the power is turned off

	CLRF LED		;The LED register is used to store the pulse delay but must start clear 
SPIN
	MOVLW 2
	XORWF PORTA, F		;Toggle the motor on/off
	MOVF LED, W		
	MOVWF DELAY2		;Load the delay counter register with the current pulse delay value

SPIN2
	DECFSZ DELAY1, F	;Start the internal delay loop, by decrementation and testing
	GOTO SPIN2		
	MOVLW 40		;Once the delay is complete, reset the internal delay counter 
	MOVWF DELAY1		;For the first itteration DELAY1 is not set up, the error is insignificant		
	DECFSZ DELAY2, F	;End of external delay loop, decrement and test 
	GOTO SPIN2		
				;Delay completed
	CLRWDT			;Make sure wdt does not time out
	CALL LEDUPDATE		;Refresh the LEDs to show the last delay ammmount
	DECFSZ LED, F		;Reduce the delay ammount, increasing the frequency
	GOTO SPIN		;Loop until the delay value hits zero

				;As LED started at zero, after 256 itterations,
				;An ther motor has been toggled an even number of times
				;Therefore it is now turned off				
	


;This routine determines if there are any obstructons
;It does this by letting the motor freespin for 8 revoulutions clearing the watchdog on each cycle
;If the rotor is going fast enough then all 8 cycles should complete without a wdt time out
;It is then fair to assume that there are no obstructions, give fullpower to the motor
;The 8 cycles are counted by shifting the LED register that starts at 255, until it ends up zero

	COMF LED, F		;Prepair to turn all bits of the LED register on as LED started clear
FREESPIN
	BTFSS PORTB, 0		;Wait until the position indicator flag on the rotor is optically detected
	GOTO FREESPIN
FREESPIN2
	BTFSC PORTB, 0		;After it has been detected, wait until the rotor has passed it
	GOTO FREESPIN2

	CLRWDT			;Clear the watchdog timer on each cycle

	CALL LEDUPDATE		;Refresh the LEDs
	BCF STATUS, 0		;Make sure that the carry flag is clear
	RRF LED, F		;Rotate the LEDs
	MOVF LED, F		
	BTFSS STATUS, 2		;Check to see if LED is now zero
	GOTO FREESPIN		;If not loop again






;This routine sets up and prepirrs the pic for interrupts
;The motor is then turned on full power

	MOVLW B'10110000'	
	MOVWF INTCON		;Set up interrupts
	MOVLW 255		;Set the timer to the point just before overflow to
	MOVWF TMR0		;cause an interrupt the moment it overflows by incomming data
	BSF PORTA, 1		;Turn on the motors



;The buffer must now be initialised
;This is loop repeats until the memory pointer has reached the last memory address
;On each reputition, it either loads chars from the text string in program memory, or if the end of the 
;string is reached, it will fill the rest of the buffer memory with space chars


	MOVLW MEMLOW		;Get the first buffer memory address
	MOVWF BUFFPTR		;Save it in BUFFPTR ready for the input data routine
	MOVWF FSR		;Set up indirect addressing to point to the first buffer memory address

INITBUFFER


	MOVLW ' '		
	MOVWF ASCIICODE		;Prepare to load a space character 		

	MOVLW MEMLOW		;Find the character number from the memory address by subtracting 
	SUBWF FSR, W		;the first memory address from the current memory address
	MOVWF CHARSELECT	;Load the charactter number into CHARSHIFT ready for GETCHARP routine
	
	SUBLW TEXTLENGTH	;Find out if the CHARSELECT is stored in the program memory
	BTFSC STATUS, 0		;Test to see if CHARSHIFT > TEXTLENGTH
	CALL GETCHARP		;If not get a char from the program memory else use the prepaired one (above)

	MOVF ASCIICODE, W	
	MOVWF INDF		;Load ASCIICODE into the buffer
 
	INCF FSR, F		;Move to the next location

	MOVF FSR, W		;Compare the buffer position with
	SUBLW MEMHIGH		;the last memory location
	BTFSC STATUS, 0		;Check to see if the buffer pointer is outside the last memory location
	GOTO INITBUFFER		;If not, continue looping


;Prepair the main variables before the main loop starts

	CLRF PARTSHIFT		;Clear variables to start
	CLRF CHARSHIFT		;at beginning of string
	MOVLW STARTBLANK
	MOVWF DISPBLANK		;Introduce the new text smoothly from the right


;This routine is called at the start of each cycle of the rotor
;Adjust the shift variables to rotate the display
;If the previous char in the buffer was a null, set PARTSHIFT
;to the start of the current char. This should be the start of the next
;string, and preventing the display from scroling. DISPBLANK must also be
;set up to push the text out of the display to the right.  


CYCLE
	CLRWDT			;Make sure watchdog does not time out

	MOVLW TEXTSPEED
	MOVWF SCANPOS		;Prepair to advance shifts by TEXTSPEED

	CALL SELECTDATA		;Perform calculations to advance shifts, storing results in CHARSELECT & PARTSELECT

	MOVF CHARSELECT, W
	MOVWF CHARSHIFT		;CHARSELECT needs to be saved in to CHARSHIFT
	MOVF PARTSELECT, W
	MOVWF PARTSHIFT		;PARTSELECT needs to be saved in to PARTSHIFT


	CLRF SCANPOS		;The scan position must be cleared ready for the start of the next scan
	
	
	
;Select the Previous char in the buffer
	MOVF CHARSELECT, F	;Affect the status bits coresponding to CHARSELECT
	MOVLW MEMSIZE - 1	;Load the working register with the last memory location
	BTFSS STATUS, 2		;Check to see if CHARSELECT = 0
	DECF CHARSELECT, W	;If not decrement CHARSELECT, storing result in W
	MOVWF CHARSELECT	;Save either the last memory location or the decremented 
				;value depending on last test
	
	CALL GETCHAR		;Find the ASCIICODE for the selected char

;If the char was a null then set DISPBLANK else leave it alone
	
	MOVF ASCIICODE, F	;Affect the status bits coresponding to ASCIICODE
	MOVLW BLANKPOS		;Load working with the new DISPBLANK 
	BTFSC STATUS, 2  	;Check to see if ASCIICODE = 0
	MOVWF DISPBLANK		;If it is, save new DISPBLANK value 

;If DISPBLANK != 0 then don't advance the shifts by clearing the PARTSHIFT 
	MOVF DISPBLANK, F	;Affect the status bits coresponding to DISPBLANK
	BTFSS STATUS, 2		;Check to see if DISPBLANK = 0
	CLRF PARTSHIFT		;If not clear PARTSHIFT

;Reduce Dispblank by an ammount until it reaches zero
	MOVLW NEWTEXTSPEED	;Load working with an ammount (NEWTEXTSPEED) 
	SUBWF DISPBLANK, F	;Subtract NEWTEXTSPEED from DISPBLANK
	BTFSS STATUS, 0		;Check to see if the subtraction resulted in a value <0
	CLRF DISPBLANK		;If it did, make DISPBLANK = 0

;Create the working values for use within the main loop	
	MOVF DISPBLANK, W
	MOVWF DISPBLANKW

;Clear the interrupt source, and enable interrupts	
	BCF INTCON, 1
	BSF INTCON, 7




;This routine manages the displaying of text on the display
;It loops once for each pixel in the x direction
;It keeps track of its horizontal position using a variable called SCANPOS
;This starts at 0, and increases as the rotor scans te display
;Firstly, it has to select data in the memory using SELECTDATA, then it can 
;get the data in ascii form using GETDATA This data must then be decoded
;to get a table index value for use in the charmap table using the ASCIIDECODE
;function. The data can then be rendered into the character strip to be displayed
;using the RENDERFONT function. This returns the vertical character strip in the working 
;register. This is then ready for displaying directly to the LEDs
;This routine must also take into account display blanking. The DISPBLANKW register
;initialy contains the x pixel number where the text should start. 
;Also, if a null char is in the display, no text should be displayed after it.

MAINLOOP

	CALL SELECTDATA		;Create the PARTSELECT and CHARSELECT values from CHARSHIFT, PARTSHIFT and SCANPOS
	CALL GETCHAR		;Find the ASCIICODE of the character pointed to by CHARSELECT

	CALL ASCIIDECODE	;Convert the ASCIICODE into a CHARINDEX for use on the CHARSET table
	CALL RENDERFONT		;Grab the vertical strip from the CHARSET table ready for the LEDs 

	MOVF DISPBLANKW, F	;Affect the status bits coresponding to DISPBLANK
	BTFSS STATUS, 2		;Check to see if DISPBLANKW = 0
	CLRF LED		;If not, don't display anything

	CALL LEDUPDATE		;Update the leds
	CALL PAUSE		;Let them shine for a while
	CLRF LED		;Set up the LEDs to turn off
	CALL LEDUPDATE		;Update the leds, turning them all off
	CALL PAUSE		;Wait for a while

	MOVF DISPBLANKW, F	;Affect the status bits coresponding to DISPBLANK	
	BTFSS STATUS, 2		;Check to see if DISPBLANK = 0
	DECF DISPBLANKW, F	;If not, decrement DISPBLANK
	BTFSS STATUS, 2		;Check again to see if DISPBLANK = 0
	CLRF ASCIICODE		;If not, Make the current char appear to be a null

	MOVF ASCIICODE, F	;Affect the status bits coresponding to ASCIICODE
	BTFSS STATUS, 2		;Check to see if ASCIICODE = 0 (A null char)
	INCF SCANPOS, F		;If not then advance the scan position

	GOTO MAINLOOP		;Continue the loop forever (unless interrupted)

;All functions / routines are below this point
;These routines are all conversion based, and therefore 
;take in one or more variables, process them, and give one 
;or more outputs. The working register is NEVER used to transfer
;data into and out of these routines. The working register contents
;are modified within the routines as part of calculation.
;All routines have a CALL - RETURN method of invocation.




;This routine calculates the character and sub character positions from
;the current position, and an ammount to advance by. The current position is 
;contained in  CHARSHIFT (the character number) and PARTSHIFT (the pixel 
;within that character). The ammount to advance by is contained in SCANPOS, 
;and can be anyware in the range 0-255 but only values of 1-8 are sensible.
;The new calculated values are stored in CHARSELECT and PARTSELECT.
;The calculation works by adding PARTSHIFT to SCANPOS and doing a modulo 8.
;This provides the new sub character position and it is stored into PARTSELECT.
;CHARSELECT is calculated by adding PARTSHIFT to SCANPOS and rounding down to
;the nearist multiple of 8. This is then divided by 8 and added onto CHARSHIFT.
;CHARSELECT must then be checked to make sure that it is within its valid range.

;<IO SUMMARY>
;Input  CHARSHIFT
;Input  PARTSHIFT
;Input  SCANPOS
;Output CHARSELECT
;Output PARTSELECT

SELECTDATA
	MOVF PARTSHIFT, W	;Start of with PARTSHIFT in W
	ADDWF SCANPOS, W	;Add on SCANPOS
	ANDLW B'00000111'	;Modulo 8
	MOVWF PARTSELECT	;Save to PARTSELECT

	MOVF PARTSHIFT, W	;Start with PARTSHIFT in W
	ADDWF SCANPOS, W	;Add on SCANPOS
	ANDLW B'11111000'	;Round to down to the nerist multiple of 8
	MOVWF CHARSELECT	;Store in CHARSELECT temporarly
	BCF STATUS, 0		;make sure carry is clear
	RRF CHARSELECT, F	;Divide 
	RRF CHARSELECT, F	;by
	RRF CHARSELECT, W	;8
	ADDWF CHARSHIFT, W	;Add to CHARSHIFT
	MOVWF CHARSELECT	;Save to CHARSELECT

	

;Make sure that CHARSELECT is within the valid memory range
	MOVLW MEMSIZE		;Start with the MEMSIZE in W
	SUBWF CHARSELECT, W	;Subtract this from CHARSELECT
	BTFSC STATUS, 0		;If the subtraction did not carry then 
	MOVWF CHARSELECT	;let CHARSELECT = CHARSELECT - MEMSIZE
	RETURN



;This routine retrieves the character pointed to by CHARSELECT from 
;the memory buffer and stores the ascii code in ASCIICODE. 

;<IO SUMMARY>
;Input	CHARSELECT
;Output	ASCIICODE

GETCHAR
	MOVF CHARSELECT, W	;Start with CHARSELECT in W
	ADDLW MEMLOW		;Add on the first buffer position
	MOVWF FSR		;Set up indirect addressing
	
	MOVF INDF, W		;Grab the contents of this address
	MOVWF ASCIICODE		;Save to ASCIICODE
	RETURN


;This routine retrieves the character pointed to by CHARSELECT from 
;the program memory default string  and stores the ascii code in ASCIICODE. 
;It works by calculating an absolute program adress and jumping to it.
;The absolute address is calculated using the HIGH and LOW functions
;The PCLATH is loaded with the value for the start of the string. 
;CHARSELECT is added to the string start offset. If this results in crossing
;a memory page boundary, the PCLATH is adjusted. The routine then issues a call
;to GETCHARP2 which skips to the string table. The returned value is 
;saved into ASCIICODE before returning.

;<IO SUMMARY>
;Input	CHARSELECT
;Output	ASCIICODE

GETCHARP
	MOVLW HIGH TEXTSTART	;Load PCLATH with the high bits
	MOVWF PCLATH		;for the start of the string
	MOVF CHARSELECT, W	;Add CHARSELECT to the low bits
	ADDLW LOW TEXTSTART	;for the start of the string.
	BTFSC STATUS, 0		;Did the addition cross a page boundary		ADDED 16/8/02
	INCF PCLATH, F		;If so increase the PCLATH			ADDED 16/8/02
	CALL GETCHARP2		;Get the asciicode into W
	MOVWF ASCIICODE		;Save it into ASCIICODE
	RETURN
GETCHARP2
	MOVWF PCL		;Jump to the string table

;This routine grabs a vertical character stripe from the CHARSET table. It takes
;in two variables CHARINDEX (the character to grab) and PARTSELECT (the vertical 
;stripe within the character). The data within CHARSET is a continuous list of
;stripes with no separation from one char to the next, but at the font is fixed
;width, the char positions can be calculated. 
;CHARINDEX contains the character number within the table, so it must be multiplied
;by 8 (the number of stripes per char) in order to get the position of the start of 
;the char within the table. PARTSELECT must also be added to this to get the absolute 
;position of the stripe within the table. 
;The absolute position within program memory can be calculated by adding the start position
;of the table to the relative position of the stripe within the table.
;Although the program counter is 14 bits wide, the whole thing is complicated by 
;the fact that the processor can only do 8 bit calculations.

;PCLATH is cleared and loaded with ((CHARINDEX * 8 (number of stripes per char)) / 256)
;to simplify this PCLATH is loaded with CHARINDEX divided by 32. This is achieved by rotating
;the 3 high bits of CHARINDEX into the 3 low bits of PCLATH. The high bits of the start
;of CHARSET are then added on to PCLATH.
;The rotation performed in the pevious step had the effect of multiplying CHARINDEX by 8. This
;can now be used to calculate the low bits of the computed goto.
;PARTSELECT is added to CHARINDEX (which was multiplied by 8 in prev step) this is then added to
;the low bits of the start of CHARSET. If in any of the previous two additions, the 
;calculation carries over, PCLATH must be incremented. But as CHARINDEX was multiplied by 8
;and PARTSELECT is less than 8, it is imposible for the addition of these two variables to result
;in a carry.

;<IO SUMMARY>
;Input	CHARINDEX
;Input	PARTSELECT
;Output	LED

RENDERFONT
	CLRF PCLATH		;Start with PCLATH clear
	BCF STATUS, 0		;Clear the carry
	RLF CHARINDEX, F	;Rotate the 3
	RLF PCLATH, F		;high bits of CHARINDEX
	RLF CHARINDEX, F	;into the 3 low bits
	RLF PCLATH, F		;of PCLATH
	RLF CHARINDEX, F	;multiplying CHARINDEX
	RLF PCLATH, W		;by 8 in the process
	ADDLW HIGH CHARSET	;Add on the start of the table
	MOVWF PCLATH		;Save to PCLATH


	MOVF CHARINDEX, W	;Start with CHARINDEX in (this has been multipled by 8 above) 
	ADDWF PARTSELECT, W	;Adding PARTSELECT can not cause a carry because PARTSELECT<8
	ADDLW LOW CHARSET	;Add on the start address of the CHARSET table
	BTFSC STATUS, 0		;Check if the previous addition crossed another page boundary
	INCF PCLATH, F		;If so increment PCLATH
	CALL RENDERFONT2	;Get the led data into W
	MOVWF LED		;Store it into LED
	RETURN
RENDERFONT2
	MOVWF PCL		;Jumping to the CHARSET table


;This is the display pause routine. It is a standard single drop out loop
;in which the number of iterations is dependent on a predifined
;constant called TEXTWIDTH. Altering the value of TEXTWIDTH effectivly 
;alters the ammount of time the led's are on, therefore the width of the text.

PAUSE
	MOVLW TEXTWIDTH		;Load the value of TEXTWIDTH
	MOVWF DELAY1		;into DELAY1
PAUSE2
	DECFSZ DELAY1, F	;Loop until the value of
	GOTO PAUSE2		;DELAY1 is equal to 0
	RETURN


;This is the serial data pause routine. It is an accuratly timed,
;standard single drop out loop in which the number of iterations is dependent 
;on a predifined constant called DATDELAY. This routine is used to accuratly
;time a half bit of serial data.

PAUSEDAT
	MOVLW DATDELAY		;Load the value of DATDELAY
	MOVWF DELAY3		;into DELAY3
PAUSEDAT2
	nop			;some nops for course timing adjustment
	nop
	nop
	DECFSZ DELAY3, F	;Loop until the value
	GOTO PAUSEDAT2		;of DELAY3 = 0
	RETURN


;This is the led display routine. It takes the data stored in LED and
;maps it to the display using the correct IO ports. The ports are not
;in a logical order because the PCB was designed to be as neat as
;possible.
;It works by turning the led off and then checking whether it should be
;turned back on.

LEDUPDATE
	BCF PORTA, 2		;LED:0 -> PORTA:2
	BTFSC LED, 0
	BSF PORTA, 2
	BCF PORTA, 0		;LED:1 -> PORTA:0
	BTFSC LED, 1
	BSF PORTA, 0
	BCF PORTA, 3		;LED:2 -> PORTA:3
	BTFSC LED, 2
	BSF PORTA, 3
	BCF PORTB, 1		;LED:3 -> PORTB:1
	BTFSC LED, 3
	BSF PORTB, 1
	BCF PORTB, 2		;LED:4 -> PORTB:2
	BTFSC LED, 4
	BSF PORTB, 2
	BCF PORTB, 3		;LED:5 -> PORTB:3
	BTFSC LED, 5
	BSF PORTB, 3
	BCF PORTB, 4		;LED:6 -> PORTB:4
	BTFSC LED, 6
	BSF PORTB, 4
	BCF PORTB, 5		;LED:7 -> PORTB:5
	BTFSC LED, 7
	BSF PORTB, 5
	RETURN

;This routine turns off the whirlygig by turning off the leds and motor.
;It prevents the watchdog from overflowing by continuously clearing it.
;It monitors the data pin for a change. It wakes up if a change occured.

TURNOFF
	CLRWDT			;Keep the watchdog clear
	CLRF PORTB		;All leds and
	CLRF PORTA		;motor are to be turned off
	BTFSC PORTA, 4		;Check for any change of condition
	GOTO TURNOFF		;If not stay off
	GOTO INIT		;Otherwise restart

include Courier-Font.asm



TEXTSTART
	DT TEXT
TEXTEND

END
