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



;This file contains the character decoder and font glyphs for the Courrier font.
;The ASCIIDECODE routine is called from the main program to convert ASCII character 
;values into a table index for the CHARSET table, thus allowing any character 
;encoding, language specific character encodings, and any glyph/font mappings 
;to be used. This also enables the displaying of images by replacing and 
;remapping normally unused glyphs.








;This routine takes the value of ASCIICODE, and converts it to the 
;appropriate index for the CHARSET table. As there is not enough memory
;on a PIC16F84 for a full ASCII character set. There is just enough to 
;do the upper case characters, the numbers and the symbols.
;...tell a lie, this is only the case for the non streaming version of the whirly 
;gig program where the text string was also contained within program memory.
;This routine works by firstly substituting a space in for null characters. 
;Then the ASCII value is decremented as to make A=64 and a=96, this makes
;it easier to convert lowercase characters into uppercase ones. The u/c->l/c conversion
;can be done by taking off 32 if the just decremented value is greater than 64. 
;But before the u/c->l/c conversion can take place, something must be done to prevent 
;the symbols in the ASCII range 123-126 from being lost, so a check is done first
;and if the ASCII value is greater than 122, 6 must be added (so that the chars within 
;this range can be tagged to the end of the character set.) But the actual
;adding of 6 can't take place until after the case has been connverted.
;This is done by storing 6 in the W if the addition is required and 0 if not.
;Then after the case has been converted, W can simply be added to CHARINDEX.
;The final stage just involves taking the 31 from CHARINDEX as special characters 
;are not being used.

ASCIIDECODE
	
	MOVF ASCIICODE, W	;Start with ASCIICODE in W
	BTFSC STATUS, 2		;Test to see if ASCIICODE was null
	MOVLW ' '		;If so make it into a space
	MOVWF CHARINDEX		;Save the result back to CHARINDEX
	
	DECF CHARINDEX, F	;Decrement so that A=64 and a=96

	SUBLW 122		;Allowing for chars 123-126
	CLRW			;The subtraction affected status, W can now be cleared
	BTFSS STATUS, 0		;If CHARCODE>123 then W=6 else W=0
	MOVLW 6			;But don't add W to CHARCODE yet because it bodges the case conversion

	BTFSC CHARINDEX, 6	;Case conversion, if CHARINDEX=>64
	BCF CHARINDEX, 5	;then let CHARINDEX=5

	ADDWF CHARINDEX, F	;Finally add W to CHARCODE to display chars 123-126 properly

	MOVLW 31		;Remove ascii special char offset
	SUBWF CHARINDEX, F	;Saving to CHARINDEX
	RETURN



;This is the glyph table for the fixed width font. It contains 68 characters,
;these include the uppercase letters, the numbers and symbols. It is formatted
;in blocks of 8 to make it more readable.
;This table can also freely cross the page boundarys as this is handled by the
;main program.
;The symbols included in this table are 
; !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`{|}~


CHARSET	
	RETLW B'00000000'	;	<SPACE>	
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	!	
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'10011111'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	"
	RETLW B'00000000'
	RETLW B'00001111'
	RETLW B'00000011'
	RETLW B'00000000'
	RETLW B'00001111'
	RETLW B'00000011'
	RETLW B'00000000'

	RETLW B'00000000'	;	#
	RETLW B'10100100'
	RETLW B'01111110'
	RETLW B'00100101'
	RETLW B'10100100'
	RETLW B'01111110'
	RETLW B'00100101'
	RETLW B'00000000'

	RETLW B'00000000'	;	$ 
	RETLW B'00000000'
	RETLW B'01101110'
	RETLW B'11001010'
	RETLW B'01010011'
	RETLW B'01110110'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	%
	RETLW B'00010010'
	RETLW B'00010101'
	RETLW B'01010010'
	RETLW B'10101000'
	RETLW B'01001000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	&
	RETLW B'00000000'
	RETLW B'01100000'
	RETLW B'10011100'
	RETLW B'10110010'
	RETLW B'11000010'
	RETLW B'10100010'
	RETLW B'00000000'

	RETLW B'00000000'	;	'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000111'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	(
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00111100'
	RETLW B'11000011'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	)
	RETLW B'00000000'
	RETLW B'11000011'
	RETLW B'00111100'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	*
	RETLW B'00000000'
	RETLW B'00000010'
	RETLW B'00011010'
	RETLW B'00000111'
	RETLW B'00011010'
	RETLW B'00000010'
	RETLW B'00000000'

	RETLW B'00000000'	;	+
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'11111110'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00010000'

	RETLW B'00000000'	;	,
	RETLW B'10000000'
	RETLW B'01000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	-
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00000000'

	RETLW B'00000000'	;	.
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'01100000'
	RETLW B'01100000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	/
	RETLW B'10000000'
	RETLW B'01100000'
	RETLW B'00011000'
	RETLW B'00000110'
	RETLW B'00000001'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	0
	RETLW B'01111110'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'01111110'
	RETLW B'00000000'

	RETLW B'00000000'	;	1
	RETLW B'00000000'
	RETLW B'10000010'
	RETLW B'10000010'
	RETLW B'11111111'
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	2
	RETLW B'11000010'
	RETLW B'10100001'
	RETLW B'10010001'
	RETLW B'10001001'
	RETLW B'11000110'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	3
	RETLW B'01000010'
	RETLW B'10000001'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'01110110'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	4
	RETLW B'00010000'
	RETLW B'00011100'
	RETLW B'00010010'
	RETLW B'10010001'
	RETLW B'11111111'
	RETLW B'10010000'
	RETLW B'00000000'

	RETLW B'00000000'	;	5
	RETLW B'01000000'
	RETLW B'10001111'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'01110001'
	RETLW B'00000000'

	RETLW B'00000000'	;	6
	RETLW B'00000000'
	RETLW B'01111100'
	RETLW B'10001010'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'01110001'
	RETLW B'00000000'

	RETLW B'00000000'	;	7
	RETLW B'00000011'
	RETLW B'00000001'
	RETLW B'00000001'
	RETLW B'11100001'
	RETLW B'00011001'
	RETLW B'00000111'
	RETLW B'00000000'

	RETLW B'00000000'	;	8
	RETLW B'01110110'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'01110110'
	RETLW B'00000000'

	RETLW B'00000000'	;	9
	RETLW B'10001110'
	RETLW B'10010001'
	RETLW B'10010001'
	RETLW B'10010001'
	RETLW B'01010001'
	RETLW B'00111110'
	RETLW B'00000000'

	RETLW B'00000000'	;	:
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'01100110'
	RETLW B'01100110'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	;
	RETLW B'00000000'
	RETLW B'10000000'
	RETLW B'11001100'
	RETLW B'01001100'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	<
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00101000'
	RETLW B'01000100'
	RETLW B'01000100'
	RETLW B'10000010'
	RETLW B'00000000'

	RETLW B'00000000'	;	=
	RETLW B'00010100'
	RETLW B'00010100'
	RETLW B'00010100'
	RETLW B'00010100'
	RETLW B'00010100'
	RETLW B'00010100'
	RETLW B'00000000'

	RETLW B'00000000'	;	>
	RETLW B'10000010'
	RETLW B'01000100'
	RETLW B'01000100'
	RETLW B'00101000'
	RETLW B'00010000'
	RETLW B'00010000'
	RETLW B'00000000'

	RETLW B'00000000'	;	?
	RETLW B'00000000'
	RETLW B'00000010'
	RETLW B'10000001'
	RETLW B'10100001'
	RETLW B'00010001'
	RETLW B'00001110'
	RETLW B'00000000'

	RETLW B'00000000'	;	@
	RETLW B'00000000'
	RETLW B'01111110'
	RETLW B'10000001'
	RETLW B'10011001'
	RETLW B'10100101'
	RETLW B'10111110'
	RETLW B'00000000'

	RETLW B'00000000'	;	A
	RETLW B'10000000'
	RETLW B'11000000'
	RETLW B'10111101'
	RETLW B'00100011'
	RETLW B'10111100'
	RETLW B'11000000'
	RETLW B'10000000'

	RETLW B'00000000'	;	B
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'01110110'
	RETLW B'00000000'

	RETLW B'00000000'	;	C
	RETLW B'00111100'
	RETLW B'01000010'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'01000011'
	RETLW B'00000000'

	RETLW B'00000000'	;	D
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'01000010'
	RETLW B'00111100'
	RETLW B'00000000'

	RETLW B'00000000'	;	E
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10001001'
	RETLW B'10011101'
	RETLW B'10000001'
	RETLW B'11000011'
	RETLW B'00000000'

	RETLW B'00000000'	;	F
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10001001'
	RETLW B'00011101'
	RETLW B'00000001'
	RETLW B'00000011'
	RETLW B'00000000'

	RETLW B'00000000'	;	G
	RETLW B'00111100'
	RETLW B'01000010'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'10010001'
	RETLW B'01110011'
	RETLW B'00010000'

	RETLW B'00000000'	;	H
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10001001'
	RETLW B'00001000'
	RETLW B'10001001'
	RETLW B'11111111'
	RETLW B'10000001'

	RETLW B'00000000'	;	I
	RETLW B'00000000'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'00000000'

	RETLW B'00000000'	;	J
	RETLW B'01110000'
	RETLW B'10000000'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'01111111'
	RETLW B'00000001'
	RETLW B'00000000'

	RETLW B'00000000'	;	K
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10010001'
	RETLW B'00011000'
	RETLW B'00100101'
	RETLW B'11000011'
	RETLW B'10000001'

	RETLW B'00000000'	;	L
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10000001'
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'11100000'
	RETLW B'00000000'

	RETLW B'00000000'	;	M
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10000111'
	RETLW B'00011000'
	RETLW B'00000111'
	RETLW B'11111111'
	RETLW B'10000001'

	RETLW B'10000001'	;	N
	RETLW B'11111111'
	RETLW B'10000011'
	RETLW B'00001100'
	RETLW B'00110000'
	RETLW B'11000001'
	RETLW B'11111111'
	RETLW B'00000001'

	RETLW B'00000000'	;	O
	RETLW B'00111100'
	RETLW B'01000010'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'01000010'
	RETLW B'00111100'

	RETLW B'00000000'	;	P
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10010001'
	RETLW B'00010001'
	RETLW B'00010001'
	RETLW B'00001110'
	RETLW B'00000000'

	RETLW B'00000000'	;	Q
	RETLW B'00111100'
	RETLW B'01000010'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'10100001'
	RETLW B'01000010'
	RETLW B'10111100'

	RETLW B'00000000'	;	R
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10010001'
	RETLW B'00010001'
	RETLW B'00110001'
	RETLW B'01001110'
	RETLW B'10000000'

	RETLW B'00000000'	;	S
	RETLW B'11000110'
	RETLW B'01001001'
	RETLW B'10001001'
	RETLW B'10001001'
	RETLW B'10001010'
	RETLW B'01110011'
	RETLW B'00000000'

	RETLW B'00000000'	;	T
	RETLW B'00000011'
	RETLW B'00000001'
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'10000001'
	RETLW B'00000001'
	RETLW B'00000011'

	RETLW B'00000000'	;	U
	RETLW B'00000001'
	RETLW B'01111111'
	RETLW B'10000001'
	RETLW B'10000000'
	RETLW B'10000001'
	RETLW B'01111111'
	RETLW B'00000001'

	RETLW B'00000001'	;	V
	RETLW B'00000111'
	RETLW B'00111001'
	RETLW B'11000000'
	RETLW B'11000000'
	RETLW B'00111001'
	RETLW B'00000111'
	RETLW B'00000001'

	RETLW B'00000000'	;	W
	RETLW B'00000001'
	RETLW B'01111111'
	RETLW B'10000001'
	RETLW B'01111000'
	RETLW B'10000001'
	RETLW B'01111111'
	RETLW B'00000001'

	RETLW B'00000000'	;	X
	RETLW B'10000001'
	RETLW B'11000011'
	RETLW B'10100101'
	RETLW B'00011000'
	RETLW B'10100101'
	RETLW B'11000011'
	RETLW B'10000001'

	RETLW B'00000000'	;	Y
	RETLW B'00000001'
	RETLW B'00000011'
	RETLW B'10001101'
	RETLW B'11110000'
	RETLW B'10001101'
	RETLW B'00000011'
	RETLW B'00000001'

	RETLW B'00000000'	;	Z
	RETLW B'00000000'
	RETLW B'11000011'
	RETLW B'10100001'
	RETLW B'10011001'
	RETLW B'10000101'
	RETLW B'11000011'
	RETLW B'00000000'

	RETLW B'00000000'	;	[
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'11111111'
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'00000000'

	RETLW B'00000000'	;	\
	RETLW B'00000000'
	RETLW B'00000001'
	RETLW B'00000110'
	RETLW B'00011000'
	RETLW B'01100000'
	RETLW B'10000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	]
	RETLW B'10000001'
	RETLW B'10000001'
	RETLW B'11111111'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	^
	RETLW B'00000000'
	RETLW B'00001000'
	RETLW B'00000100'
	RETLW B'00000010'
	RETLW B'00000100'
	RETLW B'00001000'
	RETLW B'00000000'

	RETLW B'10000000'	;	_
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'10000000'
	RETLW B'10000000'

	RETLW B'00000000'	;	`
	RETLW B'00000001'
	RETLW B'00000010'
	RETLW B'00000100'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	{
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00010000'
	RETLW B'01101110'
	RETLW B'10000001'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	|
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'11111111'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	}
	RETLW B'00000000'
	RETLW B'10000001'
	RETLW B'01101110'
	RETLW B'00010000'
	RETLW B'00000000'
	RETLW B'00000000'
	RETLW B'00000000'

	RETLW B'00000000'	;	~
	RETLW B'00001000'
	RETLW B'00000100'
	RETLW B'00000100'
	RETLW B'00001000'
	RETLW B'00001000'
	RETLW B'00000100'
	RETLW B'00000000'
