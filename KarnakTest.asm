;-----------------------------------------------------------------------------
;
;  WonderSwan Karnak Test
;         by Fredrik Ahlström, 2025
;         https://github.com/FluBBaOfWard/KarnakTest
;
;  UP/DOWN    - Choose option
;  A          - Start
;
;  Assemble with: 
;                   nasm -f bin -o KarnakTest.wsc KarnakTest.asm
;
;-----------------------------------------------------------------------------

	ORG 0x6800
	CPU 186
	BITS 16

	%include "WonderSwan.inc"

	MYSEGMENT equ 0x0000
	foregroundMap equ WS_TILE_BANK - MAP_SIZE
	backgroundMap equ foregroundMap - MAP_SIZE
	spriteTable equ backgroundMap - SPR_TABLE_SIZE

	PSR_S equ 0x80
	PSR_Z equ 0x40
	PSR_P equ 0x04

SECTION .text start=0x6800

	db 0x62,0x46
	dw initialize
initialize:
	cli
	cld

;-----------------------------------------------------------------------------
; Initialize registers and RAM
;-----------------------------------------------------------------------------
	mov ax, MYSEGMENT
	mov ds, ax
	xor ax, ax
	mov es, ax			; Set ES segment to 0x0000 (RAM).

	; Setup stack
	mov bp, ax
	mov ss, ax
	mov sp, WS_STACK

	; Clear Ram
	mov di, 0x0100
	mov cx, 0x1E80
	rep stosw

	out IO_SRAM_BANK,al

;-----------------------------------------------------------------------------
; Initialize variables
;-----------------------------------------------------------------------------
	mov word [es:globalFrameCounter], 0
	mov word [es:lfsr1], 0x0234
	mov word [es:lfsr2], 0x7321
	mov word [es:lfsr3], 0x0001
	mov word [es:lfsr3+2], 0x8420

;-----------------------------------------------------------------------------
; Initialize video
;-----------------------------------------------------------------------------
	in al, SYSTEM_CTRL2
;	or al, VMODE_4C | VMODE_CLEANINIT
	or al, VMODE_CLEANINIT
	out SYSTEM_CTRL2, al

	xor ax, ax
	mov al, BG_MAP( backgroundMap ) | FG_MAP( foregroundMap )
	out IO_SCR_AREA, al

	mov al, SPR_AREA( spriteTable )
	out IO_SPR_AREA, al

	in al, IO_LCD_IF_CTRL
	or al, LCD_ON
	out IO_LCD_IF_CTRL, al

	xor al, al
	out IO_LCD_SEG_DATA, al

;-----------------------------------------------------------------------------
; Register our interrupt handlers
;-----------------------------------------------------------------------------
	mov di, 0*4		; Division error vector
	mov word [es:di], divisionErrorHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 1*4		; Trap/Brk
	mov word [es:di], trapHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 2*4		; NMI
	mov word [es:di], nmiHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 3*4		; Int3
	mov word [es:di], int3InstructionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 4*4		; BRKV
	mov word [es:di], overflowExceptionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 5*4		; CHKIND
	mov word [es:di], boundsExceptionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 6*4		; Undefined instruction vector
	mov word [es:di], undefinedInstructionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 7*4		; POLL
	mov word [es:di], pollExceptionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 0x10*4	; output char vector
	mov word [es:di], outputCharHandler
	mov word [es:di + 2], MYSEGMENT

	mov ax, INT_BASE	; 0x20
	out IO_INT_VECTOR, al

	mov di, INTVEC_VBLANK_START
	add di, ax
	shl di, 2
	mov word [es:di], vblankInterruptHandler
	mov word [es:di + 2], MYSEGMENT

	; Clear HBL & Timer
	xor ax, ax
	out IOw_H_BLANK_TIMER, ax
	out IO_TIMER_CTRL, al

	; Acknowledge all interrupts
	dec al
	out INT_CAUSE_CLEAR, al

	; Enable VBL interrupt
	mov al, INT_VBLANK_START 
	out IO_INT_ENABLE, al

	; We have finished initializing, interrupts can now fire again
	sti

;-----------------------------------------------------------------------------
; Copy font tile data into WS's tile mem
;-----------------------------------------------------------------------------
	; Copy font tile data to tile bank 1
	xor ax,ax
	mov si, MonoFont
	mov di, WS_TILE_BANK + 16*16*2
	mov cx, 8*16*6
monoFontLoop:
	lodsb
	stosw
	loop monoFontLoop

;-----------------------------------------------------------------------------
; Copy font palette into WSC's palette area
;-----------------------------------------------------------------------------

	; Copy 2-colour (2 bytes per colour) font palette to 
	; beginning of palettes area (becoming palette 0)
	mov si, FontTilePalette
	mov di, WSC_PALETTES
	mov cx, 2
	rep movsw

	mov ax, 0x7f0
	out IO_LCD_GRAY_01, ax
	mov ax, 0x0010
	out IOw_SCR_LUT_0, ax
	mov ax, 0x0020
	out IOw_SCR_LUT_1, ax

;-----------------------------------------------------------------------------
; Make background map point to our tiles, essentially "painting" the
; background layer with our tiles, coloured as per our palettes
;-----------------------------------------------------------------------------
main:
	call clearScreen

	mov si, headLineStr
	call writeString

	mov si, menuDumpIOPortsStr
	call writeString
	mov si, menuTestAllStr
	call writeString
	mov si, menuTestKarnakStr3
	call writeString
	mov si, menuTestKarnakStr4
	call writeString
	mov si, menuTestKarnakStr5
	call writeString
	mov si, menuTestKarnakStr6
	call writeString
	mov si, menuTestKarnakStr7
	call writeString
	mov si, menuTestKarnakStr8
	call writeString
	mov si, menuTestKarnakStr9
	call writeString
	mov si, menuTestKarnakStr10
	call writeString
	mov si, menuTestKarnakStr11
	call writeString

	; Turn on display
	mov al, BG_ON
	out IO_DISPLAY_CTRL, al

;-----------------------------------------------------------------------------
;
; BEGIN main area
;
;-----------------------------------------------------------------------------
mainLoop:
	hlt					; Wait until next interrupt

	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	nop
	nop
	nop
	nop
	in al, IO_KEYPAD
	mov bl, al
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	nop
	nop
	nop
	nop
	in al, IO_KEYPAD
	and al, 0x0F
	shl bl, 4
	or al, bl
	mov bl, [es:keysHeld]
	mov [es:keysHeld], al
	xor bl, al
	and bl, al
	mov [es:keysDown], bl

	; Check player input
;	test al, PAD_RIGHT
;	jnz speed_up

;	test al, PAD_LEFT
;	jnz speed_down

	mov cl, [es:menuYPos]
	test bl, (PAD_UP<<4)
	jz dontMoveUp
	sub cl, 1
	jns dontMoveUp
	mov cl, 0
dontMoveUp:
	test bl, (PAD_DOWN<<4)
	jz dontMoveDown
	add cl, 1
	cmp cl, 10			; Index of last menu item
	js dontMoveDown
	mov cl, 10			; same
dontMoveDown:
	mov [es:menuYPos], cl

	mov ch, cl
	add ch, 1
	mov byte [es:cursorXPos], 0
	mov [es:cursorYPos], ch
	mov al, ' '
	int 0x10
	add ch, 1
	mov byte [es:cursorXPos], 0
	mov [es:cursorYPos], ch
	mov al, '>'
	int 0x10
	add ch, 1
	mov byte [es:cursorXPos], 0
	mov [es:cursorYPos], ch
	mov al, ' '
	int 0x10

	test bl, PAD_A
	jz mainLoop
	call clearScreen

	cmp cl, 0
	jz runDumpIOPorts
	cmp cl, 1
	jz testAll
	cmp cl, 2
	jz testKarnakPatternValues
	cmp cl, 3
	jz testKarnakSaturationValues
	cmp cl, 4
	jz testKarnakRandomValues
	cmp cl, 5
	jz testKarnakWriteOnceReadTwice
	cmp cl, 6
	jz testKarnakWriteTwiceReadOnce
	cmp cl, 7
	jz testKarnakEnabled
	cmp cl, 8
	jz testKarnakTiming
	cmp cl, 9
	jz testKarnakAllValues
	cmp cl, 10
	jz testKarnakAllValuesWithReset
	; No input, restart main loop
	jmp mainLoop
;-----------------------------------------------------------------------------
;
; END main area
;
;-----------------------------------------------------------------------------
runDumpIOPorts:
	call dumpIOPorts
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testAll:
	call runKarnakTestPatternValues
	call runKarnakTestSaturationValues
	call runKarnakTestRandomValues
	call runKarnakTestWriteOnceReadTwice
	call runKarnakTestWriteTwiceReadOnce
	call runKarnakTestTiming
	call runKarnakTestEnabled

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakPatternValues:
	call runKarnakTestPatternValues
	call checkKeyInput
	jmp main

testKarnakSaturationValues:
	call runKarnakTestSaturationValues
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakRandomValues:
	call runKarnakTestRandomValues
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakWriteOnceReadTwice:
	call runKarnakTestWriteOnceReadTwice
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakWriteTwiceReadOnce:
	call runKarnakTestWriteTwiceReadOnce
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakTiming:
	call runKarnakTestTiming
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakEnabled:
	call runKarnakTestEnabled
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakAllValues:
	call runKarnakTestAllValues
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testKarnakAllValuesWithReset:
	call runKarnakTestAllValuesWithReset
	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
runKarnakTestPatternValues:
	call resetADPCM
	call testADPCMPattern
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestSaturationValues:
	call resetADPCM
	call testADPCMSaturation
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestRandomValues:
	call resetADPCM
	call testADPCMRandom
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestWriteOnceReadTwice:
	call resetADPCM
	call testADPCMW1R2
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestWriteTwiceReadOnce:
	call resetADPCM
	call testADPCMW2R1
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestTiming:
	call resetADPCM
	call testADPCMTiming
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestEnabled:
	call resetADPCM
	call testADPCMEnabled
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestAllValues:
	call resetADPCM
	call testOnly7
	call testOnlyF
	call testOnly0
	call testOnly8
	call testOnly1
	call testOnly9
	call testOnly2
	call testOnlyA
	call testOnly3
	call testOnlyB
	call testOnly4
	call testOnlyC
	call testOnly5
	call testOnlyD
	call testOnly6
	call testOnlyE
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
runKarnakTestAllValuesWithReset:
	call resetADPCM
	call testOnly0
	call resetADPCM
	call testOnly1
	call resetADPCM
	call testOnly2
	call resetADPCM
	call testOnly3
	call resetADPCM
	call testOnly4
	call resetADPCM
	call testOnly5
	call resetADPCM
	call testOnly6
	call resetADPCM
	call testOnly7
	call resetADPCM
	call testOnly8
	call resetADPCM
	call testOnly9
	call resetADPCM
	call testOnlyA
	call resetADPCM
	call testOnlyB
	call resetADPCM
	call testOnlyC
	call resetADPCM
	call testOnlyD
	call resetADPCM
	call testOnlyE
	call resetADPCM
	call testOnlyF
	jmp endTestWriteOk

;-----------------------------------------------------------------------------
; Print values of all IO ports.
;-----------------------------------------------------------------------------
dumpIOPorts:
	mov bp, 0xC0
.b1:
	mov ax, bp
	call printHexB
	mov al, ':'
	int 0x10
.b0:
	mov al, ' '
	int 0x10
	mov dx, bp
	in al, dx
	call printHexB
	add bp, 1
	mov al, 0xAA
	out 0xC7, al
	test bp, 7
	jnz .b0
	mov al, 10
	int 0x10
	test bp, 0xFF
	jnz .b1

	ret

;-----------------------------------------------------------------------------
; Print result from only writing a specified nibble to the decoder.
;-----------------------------------------------------------------------------
testSingleNibbleOnly:
	mov dl, al

	mov cx, 80
testOnlyNibbleLoop:
	in al, IO_LCD_LINE
	mov bl, al
	mov al, dl
	out 0xD8, al
	call waitNextLine
	in al, 0xD9
	call printHexB
	mov al, ','
	int 0x10

	loop testOnlyNibbleLoop
	mov al, 0xA
	int 0x10
	call checkKeyInput

	ret
;-----------------------------------------------------------------------------
; Test a pattern of nibbles written to the decoder.
;-----------------------------------------------------------------------------
testADPCMPattern:
	mov si, testingTableStr
	call writeString
	mov byte [es:isTesting], 7
	mov si, adpcmTestValues
	mov cx, 16*24		; 24 rows.

testPatternLoop:
	mov [es:inputVal3], cx
	in al, IO_LCD_LINE
	mov dl, al
	mov al, [es:si]
	test cl, 1
	jz tpNoInc
	inc si
tpNoInc:
	out 0xD8, al
	call writeSoftADPCM
	mov bl, dl
	call waitNextLine
	in al, 0xD9
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jz tpAdpcmOk
	mov [es:expectedResult1], al
	mov [es:testedResult1], bl
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz tpAdpcmStop
tpAdpcmOk:

	loop testPatternLoop
tpAdpcmStop:
	hlt						; Wait for VBlank

	ret
;-----------------------------------------------------------------------------
; Test saturation of the decoder.
;-----------------------------------------------------------------------------
testADPCMSaturation:
	mov si, testingSaturationStr
	call writeString
	mov byte [es:isTesting], 7
	mov si, adpcmSaturationValues
	mov cx, 16*2		; 4 rows.
	jmp testPatternLoop

;-----------------------------------------------------------------------------
; Test a lot of random nibbles written to the decoder.
;-----------------------------------------------------------------------------
testADPCMRandom:
	mov si, testingValuesStr
	call writeString
	mov byte [es:isTesting], 7
	call getLFSR1Value
	mov si, ax
	mov cx, 0
rndAdpcmLoop:
	mov [es:inputVal3], cx
	in al, IO_LCD_LINE
	mov dl, al
	test cl, 1
	jz rndAdpcmNoInc
	call getLFSR1Value
	mov si, ax
rndAdpcmNoInc:
	mov ax, si
	out 0xD8, al
	call writeSoftADPCM
	mov bl, dl
	call waitNextLine
	in al, 0xD9
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jz rndAdpcmOk
	mov [es:expectedResult1], al
	mov [es:testedResult1], bl
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz rndAdpcmStop
rndAdpcmOk:

	loop rndAdpcmLoop
rndAdpcmStop:
	hlt						; Wait for VBlank

	ret
;-----------------------------------------------------------------------------
; Test a lot of random nibbles written to the decoder.
;-----------------------------------------------------------------------------
testADPCMEnabled:
	mov si, testingDisabledStr
	call writeString
	mov byte [es:isTesting], 7
	call getLFSR1Value
	mov si, ax
	mov cx, 0
enaAdpcmLoop:
	mov [es:inputVal3], cx
	in al, IO_LCD_LINE
	mov dl, al
	test cl, 1
	jz enaAdpcmNoInc
	call getLFSR1Value
	mov si, ax
enaAdpcmNoInc:
	mov ax, si
	out 0xD8, al
	call writeSoftADPCM
	mov bl, dl
	call waitNextLine
	in al, 0xD9
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jnz enaAdpcmFail
	mov ax, si
	mov bl, [es:adpcmInput]
	cmp al, bl
	jz enaAdpcmOk
enaAdpcmFail:
	mov [es:expectedResult1], al
	mov [es:testedResult1], bl
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz enaAdpcmStop
enaAdpcmOk:
	cmp cl, 0x80
	jnz enaNoDisable
	call disableADPCM
enaNoDisable:
	cmp cl, 0x00
	jnz enaNoEnable
	call enableADPCM
enaNoEnable:
	loop enaAdpcmLoop
enaAdpcmStop:
	hlt						; Wait for VBlank

	ret
;-----------------------------------------------------------------------------
; Test a lot of random nibbles written once, read twice.
;-----------------------------------------------------------------------------
testADPCMW1R2:
	mov si, testingWr1Rd2Str
	call writeString
	mov byte [es:isTesting], 7
	call getLFSR1Value
	mov si, ax
	mov cx, 0x1000
testW1R2Loop:
	mov [es:inputVal3], cx
	in al, IO_LCD_LINE
	mov dl, al
	test cl, 1
	jz w1r2NoInc
	call getLFSR1Value
	mov si, ax
w1r2NoInc:
	mov ax, si
	out 0xD8, al
	call writeSoftADPCM
	mov bl, dl
	call waitNextLine
	in al, 0xD9
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jnz w1r2AdpcmNotOk
	inc dl
	mov bl, dl
	call waitNextLine
	in al, 0xD9
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jz w1r2AdpcmOk
w1r2AdpcmNotOk:
	mov [es:expectedResult1], al
	mov [es:testedResult1], bl
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz w1r2AdpcmStop
w1r2AdpcmOk:

	loop testW1R2Loop
w1r2AdpcmStop:
	hlt						; Wait for VBlank

	ret
;-----------------------------------------------------------------------------
; Test a lot of random nibbles written twice, read once.
;-----------------------------------------------------------------------------
testADPCMW2R1:
	mov si, testingWr2Rd1Str
	call writeString
	mov byte [es:isTesting], 7
	call getLFSR1Value
	mov si, ax
	mov cx, 0x1000
testW2R1Loop:
	mov [es:inputVal3], cx
	in al, IO_LCD_LINE
	mov dl, al
	call getLFSR1Value
	mov si, ax
w2r1NoInc:
	mov ax, si
	out 0xD8, al
	call writeSoftADPCM
	mov bl, dl
	call waitNextLine
	mov ax, si
	out 0xD8, al
	call writeSoftADPCM
	inc dl
	mov bl, dl
	call waitNextLine
	in al, 0xD9
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jz w2r1AdpcmOk
	mov [es:expectedResult1], al
	mov [es:testedResult1], bl
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz w2r1AdpcmStop
w2r1AdpcmOk:

	loop testW2R1Loop
w2r1AdpcmStop:
	hlt						; Wait for VBlank

	ret
;-----------------------------------------------------------------------------
; Test a lot of random nibbles written to the decoder.
;-----------------------------------------------------------------------------
testADPCMTiming:
	mov si, testingTimingStr
	call writeString
	mov byte [es:isTesting], 7
	in al, 0xD9
	mov bp, ax
	call getLFSR1Value
	mov si, ax
	mov cx, 0
timAdpcmLoop:
	mov [es:inputVal3], cx
	test cl, 1
	jz timAdpcmNoInc
	call getLFSR1Value
	mov si, ax
timAdpcmNoInc:
	mov ax, si
	call writeSoftADPCM
	mov ax, si
	out 0xD8, al
	in al, 0xD9
	mov bx, bp
	cmp bl, al
	jnz timAdpcmEarly
	nop
	nop
	nop
	nop
	nop
	nop
	in al, 0xD9
timAdpcmEarly:
	mov bp, ax
	mov bl, al
	call readSoftADPCM
	cmp al, bl
	jz timAdpcmOk
	mov [es:expectedResult1], al
	mov [es:testedResult1], bl
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz timAdpcmStop
timAdpcmOk:

	loop timAdpcmLoop
timAdpcmStop:
	hlt						; Wait for VBlank

	ret
;-----------------------------------------------------------------------------
; Print result from only writing 0x0 to the decoder.
;-----------------------------------------------------------------------------
testOnly0:
	mov si, testingOnly0Str
	call writeString

	mov al, 0x00
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x1 to the decoder.
;-----------------------------------------------------------------------------
testOnly1:
	mov si, testingOnly1Str
	call writeString

	mov al, 0x11
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x2 to the decoder.
;-----------------------------------------------------------------------------
testOnly2:
	mov si, testingOnly2Str
	call writeString

	mov al, 0x22
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x3 to the decoder.
;-----------------------------------------------------------------------------
testOnly3:
	mov si, testingOnly3Str
	call writeString

	mov al, 0x33
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x4 to the decoder.
;-----------------------------------------------------------------------------
testOnly4:
	mov si, testingOnly4Str
	call writeString

	mov al, 0x44
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x5 to the decoder.
;-----------------------------------------------------------------------------
testOnly5:
	mov si, testingOnly5Str
	call writeString

	mov al, 0x55
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x6 to the decoder.
;-----------------------------------------------------------------------------
testOnly6:
	mov si, testingOnly6Str
	call writeString

	mov al, 0x66
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x7 to the decoder.
;-----------------------------------------------------------------------------
testOnly7:
	mov si, testingOnly7Str
	call writeString

	mov al, 0x77
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x8 to the decoder.
;-----------------------------------------------------------------------------
testOnly8:
	mov si, testingOnly8Str
	call writeString

	mov al, 0x88
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0x9 to the decoder.
;-----------------------------------------------------------------------------
testOnly9:
	mov si, testingOnly9Str
	call writeString

	mov al, 0x99
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0xA to the decoder.
;-----------------------------------------------------------------------------
testOnlyA:
	mov si, testingOnlyAStr
	call writeString

	mov al, 0xAA
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0xB to the decoder.
;-----------------------------------------------------------------------------
testOnlyB:
	mov si, testingOnlyBStr
	call writeString

	mov al, 0xBB
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0xC to the decoder.
;-----------------------------------------------------------------------------
testOnlyC:
	mov si, testingOnlyCStr
	call writeString

	mov al, 0xCC
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0xD to the decoder.
;-----------------------------------------------------------------------------
testOnlyD:
	mov si, testingOnlyDStr
	call writeString

	mov al, 0xDD
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0xE to the decoder.
;-----------------------------------------------------------------------------
testOnlyE:
	mov si, testingOnlyEStr
	call writeString

	mov al, 0xEE
	jmp testSingleNibbleOnly
;-----------------------------------------------------------------------------
; Print result from only writing 0xF to the decoder.
;-----------------------------------------------------------------------------
testOnlyF:
	mov si, testingOnlyFStr
	call writeString

	mov al, 0xFF
	jmp testSingleNibbleOnly

;-----------------------------------------------------------------------------
waitNextLine:
	in al, IO_LCD_LINE
	cmp bl, al
	jz waitNextLine
	ret

;-----------------------------------------------------------------------------
; Reset ADPCM chip
;-----------------------------------------------------------------------------
resetADPCM:
	call disableADPCM
	jmp enableADPCM
;-----------------------------------------------------------------------------
disableADPCM:
	xor al, al		; Reset timer/adpcm
	out 0xD6, al
disableSoftADPCM:
	xor al, al
	mov [es:adpcmEnable], al
	mov [es:adpcmIdx], al
	mov [es:adpcmOdd], al
	mov ax, 0x4000
	mov [es:adpcmAcc], ax
	ret
;-----------------------------------------------------------------------------
enableADPCM:
	mov al, 0x80
	out 0xD6, al
enableSoftADPCM:
	mov al, 0x80
	mov [es:adpcmEnable], al
	ret
;-----------------------------------------------------------------------------
; Write ADPCM value to software version.
;-----------------------------------------------------------------------------
writeSoftADPCM:
	mov [es:adpcmInput], al
	mov bl, [es:adpcmEnable]
	test bl, 0x80
	jnz adpcmIsOn
	ret
adpcmIsOn:
	push cx
	push si
	mov bl, [es:inputVal1]
	mov [es:inputVal2], bl
	mov bl, [es:adpcmIdx]
	mov [es:adpcmIdxOld], bl
	mov bl, [es:adpcmOdd]
	xor bl, 1
	mov [es:adpcmOdd], bl
	jz notOdd
	shr al, 4
notOdd:
	and al, 0xF
	mov [es:inputVal1], al
	xor bh, bh
	mov bl, [es:adpcmIdx]
	shl bl, 3
	mov si, bx
	mov bl, al
	and bl, 7
	xor cx, cx
	mov cl, [es:upd775xStep + bx + si]
	test al,8
	jz notSign
	neg cx
notSign:
	shl cx, 6
	mov bl, [es:upd775xIndexShift + bx]
	mov al, [es:adpcmIdx]
	add al, bl
	jns notUnder
	mov al, 0
notUnder:
	cmp al, 0xF
	jc notOver
	mov al, 0xF
notOver:
	mov [es:adpcmIdx], al

	mov ax, [es:adpcmAcc]
	add ax, cx
	mov [es:adpcmAcc], ax

	pop si
	pop cx
	ret
;-----------------------------------------------------------------------------
; Read PCM value from software version. Return val in al.
;-----------------------------------------------------------------------------
readSoftADPCM:
	mov ax, [es:adpcmAcc]
	sar ax, 7
	js saturateADPCM
	ret
saturateADPCM:
	xor al, 0x80
	sar al, 7
	ret
;-----------------------------------------------------------------------------
; Wait for input, A continue, B cancel.
;-----------------------------------------------------------------------------
checkKeyInput:
	hlt
	in al, IO_KEYPAD
	test al, PAD_A | PAD_B
	jnz checkKeyInput		; Make sure no input is held before.
keyLoop:
	hlt
	in al, IO_KEYPAD
	test al, PAD_A
	jnz keyContinue
	test al, PAD_B
	jnz keyCancel
	jmp keyLoop
keyContinue:
	mov al, 1
	ret
keyCancel:
	xor al, al
	ret
;-----------------------------------------------------------------------------
; Gets the next number from LFSR1 in AX
;-----------------------------------------------------------------------------
getLFSR1Value:
	mov ax, [es:lfsr1]
	shr ax, 1
	jnc noTaps1
	xor ax, 0x8016
noTaps1:
	mov [es:lfsr1], ax
	ret
;-----------------------------------------------------------------------------
; Gets the next number from LFSR2 in AX
;-----------------------------------------------------------------------------
getLFSR2Value:
	mov ax, [es:lfsr2]
	shr ax, 1
	jnc noTaps2
	xor ax, 0x8016
noTaps2:
	mov [es:lfsr2], ax
	ret
;-----------------------------------------------------------------------------
; Gets the next number from LFSR3 in AX & DX
;-----------------------------------------------------------------------------
getLFSR3Value:
	mov ax, [es:lfsr3]
	mov dx, [es:lfsr3+2]
	add ax, ax
	adc dx, dx
	jnc noTaps3
	xor ax, 0x0001
	xor dx, 0xEA00
noTaps3:
	mov [es:lfsr3], ax
	mov [es:lfsr3+2], dx
	ret
;-----------------------------------------------------------------------------
; Print expected result and tested result.
;-----------------------------------------------------------------------------
printFailedResult:
	push cx
	push si
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10

	mov si, inputStr
	call writeString
	mov ax, [es:inputVal1]
	call printHexB
	mov si, indexStr
	call writeString
	mov ax, [es:adpcmIdx]
	call printHexB
	mov al, 10
	int 0x10

	mov si, inputPrevStr
	call writeString
	mov ax, [es:inputVal2]
	call printHexB
	mov si, indexStr
	call writeString
	mov ax, [es:adpcmIdxOld]
	call printHexB
	mov al, 10
	int 0x10

	mov si, expectedStr
	call writeString
	mov ax, [es:expectedResult1]
	call printHexB
	mov al, 10
	int 0x10

	mov si, testedStr
	call writeString
	mov ax, [es:testedResult1]
	call printHexB
	mov al, 10
	int 0x10

	pop si
	pop cx
	ret
;-----------------------------------------------------------------------------
; Print expected result and flags plus tested result and flags.
;-----------------------------------------------------------------------------
printFailedResult8:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, inputStr
	call writeString

	mov ax, [es:inputVal2]
	call printHexB
	mov si, hexPrefixStr
	call writeString
	mov ax, [es:inputVal1]
	call printHexB
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:inputFlags]
	call printHexW
	mov al, 10
	int 0x10

	mov si, expectedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:expectedResult1]
	call printHexB
	mov si, flagsStr
	call writeString
	mov ax, [es:expectedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:expectedException]
	add al, '0'
	int 0x10

	mov si, testedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:testedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:testedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:testedException]
	add al, '0'
	int 0x10
	mov al, 10
	int 0x10

	ret
;-----------------------------------------------------------------------------
; Print expected result and flags plus tested result and flags.
;-----------------------------------------------------------------------------
printFailedResult16:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, inputStr
	call writeString

	mov ax, [es:inputVal2]
	call printHexW
	mov si, hexPrefixStr
	call writeString
	mov ax, [es:inputVal1]
	call printHexW
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:inputFlags]
	call printHexW
	mov al, 10
	int 0x10

	mov si, expectedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:expectedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:expectedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:expectedException]
	add al, '0'
	int 0x10

	mov si, testedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:testedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:testedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:testedException]
	add al, '0'
	int 0x10
	mov al, 10
	int 0x10

	ret
;-----------------------------------------------------------------------------
; Print expected result and flags plus tested result and flags.
;-----------------------------------------------------------------------------
printFailedResult32:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, input32Str
	call writeString

	mov ax, [es:inputVal3]
	call printHexW
	mov ax, [es:inputVal2]
	call printHexW
	mov si, hexPrefixStr
	call writeString
	mov ax, [es:inputVal1]
	call printHexW
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:inputFlags]
	call printHexW
	mov al, 10
	int 0x10

	mov si, expectedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:expectedResult2]
	call printHexW
	mov ax, [es:expectedResult1]
	call printHexW
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:expectedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:expectedException]
	add al, '0'
	int 0x10

	mov si, testedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:testedResult2]
	call printHexW
	mov ax, [es:testedResult1]
	call printHexW
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:testedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:testedException]
	add al, '0'
	int 0x10
	mov al, 10
	int 0x10

	ret
;-----------------------------------------------------------------------------
; New Line, write OK, set result to OK.
;-----------------------------------------------------------------------------
endTestWriteOk:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
writeTestOk:
	mov si, okStr
	call writeString
	xor ax, ax
	ret

;-----------------------------------------------------------------------------
; Clear tilemap line.
;-----------------------------------------------------------------------------
clearLine:
	mov bl, [es:cursorYPos]
	and bx, 0x1f
	shl bx, 6		; bx * MAP_TWIDTH
	mov di, backgroundMap
	add di, bx
	mov cx, MAP_TWIDTH
	mov ax, BG_CHR( ' ', 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	rep stosw
	ret
;-----------------------------------------------------------------------------
; Clear foreground tilemap.
;-----------------------------------------------------------------------------
clearForegroundMap:
	mov di, foregroundMap
	jmp clearTileMap
;-----------------------------------------------------------------------------
; Clear background tilemap.
;-----------------------------------------------------------------------------
clearScreen:
	push cx
	mov di, backgroundMap
clearTileMap:
	; Clear a tilemap by writing space (0x20) to all locations.
	mov ax, BG_CHR( ' ', 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov cx, MAP_TWIDTH * MAP_THEIGHT
	rep stosw
	xor ax, ax
	mov [es:cursorPos], ax
	mov [es:bgPos], ax
	pop cx
	ret
;-----------------------------------------------------------------------------
; Write text to background. si = source
;-----------------------------------------------------------------------------
writeString:
	mov cx, SCREEN_TWIDTH * SCREEN_THEIGHT
textLoop:
	lodsb
	int 0x10
	xor al, 0
	loopne textLoop
endString:
	ret

;-----------------------------------------------------------------------------
printHexW:
	push ax
	mov al, ah
	call printHexB
	pop ax
;-----------------------------------------------------------------------------
printHexB:
	push ax
	shr al, 0x04
	call printNibble
	pop ax
	and al, 0x0f
printNibble:
	cmp al, 0x09
	jg .letter
	add al, '0'
	int 0x10
	ret
.letter:
	add al, 'a' - 0xa
	int 0x10
	ret
;-----------------------------------------------------------------------------
; Our vblank interrupt handler
; It is called automatically whenever the vblank interrupt occurs, 
; that is, every time the screen is fully drawn.
;-----------------------------------------------------------------------------
vblankInterruptHandler:
	push ax
	push bx
	push di

	; globalFrameCounter++
	inc word [es:globalFrameCounter]

	mov ax, [es:bgPos]
	out IO_SCR1_SCRL_X, ax
	mov ax, [es:fgPos]
	out IO_SCR2_SCRL_X, ax

	mov al, [es:isTesting]
	xor al, 0
	jz skipValuePrint
	cmp al, 1
	jnz skipValue8x8Print
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal2]
	call printHexB
	mov byte [es:cursorXPos], 23
	mov al, [es:inputVal1]
	call printHexB
	cmp byte [es:inputCarry], 0
	jz skipValuePrint
	mov byte [es:cursorXPos], 26
	mov al, 'C'
	int 0x10
	jmp skipValuePrint
skipValue8x8Print:
	cmp al, 2
	jnz skipValue16x8Print
	mov byte [es:cursorXPos], 17
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 25
	mov al, [es:inputVal1]
	call printHexB
	jmp skipValuePrint
skipValue16x8Print:
	cmp al, 3
	jnz skipValue16x16Print
	mov byte [es:cursorXPos], 15
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 23
	mov ax, [es:inputVal1]
	call printHexW
	jmp skipValuePrint
skipValue16x16Print:
	cmp al, 4
	jnz skipValue8Print
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal1]
	call printHexB
	jmp skipValuePrint
skipValue8Print:
	cmp al, 5
	jnz skipValue16Print
	mov byte [es:cursorXPos], 17
	mov ax, [es:inputVal1]
	call printHexW
	jmp skipValuePrint
skipValue16Print:
	cmp al, 6
	jnz skipValue32x16Print
	mov byte [es:cursorXPos], 23
	mov ax, [es:inputVal1]
	call printHexW
	mov byte [es:cursorXPos], 15
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 11
	mov ax, [es:inputVal3]
	call printHexW
	jmp skipValuePrint
skipValue32x16Print:
	cmp al, 7
	jnz skipValuePrint
	mov byte [es:cursorXPos], 2
	mov ax, [es:inputVal3]
	call printHexW
skipValuePrint:
acknowledgeVBlankInterrupt:
	mov al, INT_VBLANK_START
	out INT_CAUSE_CLEAR, al

	pop di
	pop bx
	pop ax
	iret

;-----------------------------------------------------------------------------
; The division error handler
; It is called if a division error occurs.
;-----------------------------------------------------------------------------
divisionErrorHandler:
	mov byte [es:testedException], 1
	iret
;-----------------------------------------------------------------------------
; The Trap/Brk handler
; It is called on Trap/Brk flag being set.
;-----------------------------------------------------------------------------
trapHandler:
	adc al, al
	push ax
	add sp, 6
	pop ax				; Get original flags
	and ah, 0xFE		; Clear Trap
	push ax				; Set back flags
	sub sp, 6
	mov byte [es:testedException], 1
	pop ax
	iret
;-----------------------------------------------------------------------------
; The NMI handler
;-----------------------------------------------------------------------------
nmiHandler:
	mov byte [es:testedException], 2
	iret
;-----------------------------------------------------------------------------
; The Int3 handler
; It is called on INT3 (0xCC).
;-----------------------------------------------------------------------------
int3InstructionHandler:
	mov byte [es:testedException], 3
	iret
;-----------------------------------------------------------------------------
; The BRKV handler
; It is called on BRKV (0xCE).
;-----------------------------------------------------------------------------
overflowExceptionHandler:
	mov byte [es:testedException], 4
	iret
;-----------------------------------------------------------------------------
; The BOUND/CHKIND handler
; It is called on bounds exception for CHKIND (0x62).
;-----------------------------------------------------------------------------
boundsExceptionHandler:
	mov byte [es:testedException], 5
	iret

;-----------------------------------------------------------------------------
; The undefined instruction handler
; It is called if trying to execute an undefined instruction (not on V30MZ).
;-----------------------------------------------------------------------------
undefinedInstructionHandler:
	mov byte [es:testedException], 6
	iret

;-----------------------------------------------------------------------------
; The POLL exception handler
; It is called if POLL instruction gives an exception (not on V30MZ).
;-----------------------------------------------------------------------------
pollExceptionHandler:
	mov byte [es:testedException], 7
	iret
;-----------------------------------------------------------------------------
; Write a char to background. al = char
;-----------------------------------------------------------------------------
outputCharHandler:
	push bx
	push cx
	push di

	cmp al, 10
	jz newLine
	mov cl, [es:cursorXPos]
	xor al, 0
	jz endOutput
	xor bh, bh
	mov bl, [es:cursorYPos]
	and bl, 0x1F
	shl bx, 5		; bx * MAP_TWIDTH
	add bl, cl
	shl bx, 1
	mov di, backgroundMap
	add di, bx
	stosb
	inc cl
	cmp cl, 28
	jnz endOutput
newLine:
	mov bl, [es:cursorYPos]
	inc bl
	mov al, bl
	sub al, SCREEN_THEIGHT-1
	jle notAtEnd
	and bl, 0x1F
	or bl, 0x40
	shl al, 3
	mov [es:bgYPos], al
notAtEnd:
	mov [es:cursorYPos], bl
	call clearLine
	xor cl, cl
endOutput:
	mov [es:cursorXPos], cl
	pop di
	pop cx
	pop bx
	iret

;-----------------------------------------------------------------------------
; Constants area
;-----------------------------------------------------------------------------

	align 2

upd775xStep:
	db  0,  0,  1,  2,  3,   5,   7,  10
	db  0,  1,  2,  3,  4,   6,   8,  13
	db  0,  1,  2,  4,  5,   7,  10,  15
	db  0,  1,  3,  4,  6,   9,  13,  19
	db  0,  2,  3,  5,  8,  11,  15,  23
	db  0,  2,  4,  7, 10,  14,  19,  29
	db  0,  3,  5,  8, 12,  16,  22,  33
	db  1,  4,  7, 10, 15,  20,  29,  43
	db  1,  4,  8, 13, 18,  25,  35,  53
	db  1,  6, 10, 16, 22,  31,  43,  64
	db  2,  7, 12, 19, 27,  37,  51,  76
	db  2,  9, 16, 24, 34,  46,  64,  96
	db  3, 11, 19, 29, 41,  57,  79, 117
	db  4, 13, 24, 36, 50,  69,  96, 143
	db  4, 16, 29, 44, 62,  85, 118, 175
	db  6, 20, 36, 54, 76, 104, 144, 214
upd775xIndexShift:
	db -1, -1, 0, 0, 1, 2, 2, 3

FontTilePalette:
	dw 0xFFF, 0x000

MonoFont:
	db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x18,0x08,0x10,0x00,0x18,0x00
	db 0x6C,0x6C,0x24,0x48,0x00,0x00,0x00,0x00,0x14,0x14,0xFE,0x28,0xFE,0x50,0x50,0x00
	db 0x10,0x7C,0x90,0x7C,0x12,0xFC,0x10,0x00,0x42,0xA4,0xA8,0x54,0x2A,0x4A,0x84,0x00
	db 0x30,0x48,0x38,0x62,0x94,0x88,0x76,0x00,0x18,0x18,0x08,0x10,0x00,0x00,0x00,0x00
	db 0x08,0x10,0x20,0x20,0x20,0x10,0x08,0x00,0x20,0x10,0x08,0x08,0x08,0x10,0x20,0x00
	db 0x10,0x92,0x54,0x38,0x38,0x54,0x92,0x00,0x10,0x10,0x10,0xFE,0x10,0x10,0x10,0x00
	db 0x00,0x00,0x00,0x30,0x30,0x10,0x20,0x00,0x00,0x00,0x00,0xFE,0x00,0x00,0x00,0x00
	db 0x00,0x00,0x00,0x00,0x00,0x60,0x60,0x00,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x00

	db 0x3C,0x42,0x46,0x5A,0x62,0x42,0x3C,0x00,0x08,0x38,0x08,0x08,0x08,0x08,0x08,0x00
	db 0x3C,0x42,0x42,0x0C,0x30,0x40,0x7E,0x00,0x3C,0x42,0x02,0x1C,0x02,0x42,0x3C,0x00
	db 0x0C,0x14,0x24,0x44,0x7E,0x04,0x04,0x00,0x7E,0x40,0x7C,0x02,0x02,0x42,0x3C,0x00
	db 0x3C,0x40,0x7C,0x42,0x42,0x42,0x3C,0x00,0x7E,0x02,0x04,0x08,0x08,0x10,0x10,0x00
	db 0x3C,0x42,0x42,0x3C,0x42,0x42,0x3C,0x00,0x3C,0x42,0x42,0x42,0x3E,0x02,0x3C,0x00
	db 0x00,0x18,0x18,0x00,0x18,0x18,0x00,0x00,0x00,0x18,0x18,0x00,0x18,0x08,0x10,0x00
	db 0x00,0x08,0x10,0x20,0x10,0x08,0x00,0x00,0x00,0x00,0x3C,0x00,0x3C,0x00,0x00,0x00
	db 0x00,0x10,0x08,0x04,0x08,0x10,0x00,0x00,0x3C,0x62,0x62,0x0C,0x18,0x00,0x18,0x00

	db 0x7C,0x82,0xBA,0xA2,0xBA,0x82,0x7C,0x00,0x10,0x28,0x28,0x44,0x7C,0x82,0x82,0x00
	db 0x7C,0x42,0x42,0x7C,0x42,0x42,0x7C,0x00,0x1C,0x22,0x40,0x40,0x40,0x22,0x1C,0x00
	db 0x78,0x44,0x42,0x42,0x42,0x44,0x78,0x00,0x7E,0x40,0x40,0x7E,0x40,0x40,0x7E,0x00
	db 0x7E,0x40,0x40,0x7C,0x40,0x40,0x40,0x00,0x3C,0x42,0x80,0x9E,0x82,0x46,0x3A,0x00
	db 0x42,0x42,0x42,0x7E,0x42,0x42,0x42,0x00,0x10,0x10,0x10,0x10,0x10,0x10,0x10,0x00
	db 0x02,0x02,0x02,0x02,0x42,0x42,0x3C,0x00,0x42,0x44,0x48,0x50,0x68,0x44,0x42,0x00
	db 0x40,0x40,0x40,0x40,0x40,0x40,0x7E,0x00,0x82,0xC6,0xAA,0x92,0x82,0x82,0x82,0x00
	db 0x42,0x62,0x52,0x4A,0x46,0x42,0x42,0x00,0x38,0x44,0x82,0x82,0x82,0x44,0x38,0x00

	db 0x7C,0x42,0x42,0x7C,0x40,0x40,0x40,0x00,0x38,0x44,0x82,0x82,0x8A,0x44,0x3A,0x00
	db 0x7C,0x42,0x42,0x7C,0x48,0x44,0x42,0x00,0x3C,0x42,0x40,0x3C,0x02,0x42,0x3C,0x00
	db 0xFE,0x10,0x10,0x10,0x10,0x10,0x10,0x00,0x42,0x42,0x42,0x42,0x42,0x42,0x3C,0x00
	db 0x82,0x82,0x44,0x44,0x28,0x28,0x10,0x00,0x82,0x92,0x92,0xAA,0xAA,0x44,0x44,0x00
	db 0x82,0x44,0x28,0x10,0x28,0x44,0x82,0x00,0x82,0x44,0x28,0x10,0x10,0x10,0x10,0x00
	db 0x7E,0x04,0x08,0x10,0x20,0x40,0x7E,0x00,0x18,0x10,0x10,0x10,0x10,0x10,0x18,0x00
	db 0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x00,0x18,0x08,0x08,0x08,0x08,0x08,0x18,0x00
	db 0x10,0x28,0x44,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xFE,0x00

	db 0x08,0x10,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x78,0x04,0x7C,0x84,0x84,0x7E,0x00
	db 0x40,0x40,0x7C,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x3C,0x42,0x40,0x42,0x3C,0x00
	db 0x02,0x02,0x3E,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x3C,0x42,0x7E,0x40,0x3E,0x00
	db 0x0C,0x10,0x3E,0x10,0x10,0x10,0x10,0x00,0x00,0x3C,0x42,0x42,0x3E,0x02,0x7C,0x00
	db 0x40,0x40,0x7C,0x42,0x42,0x42,0x42,0x00,0x18,0x18,0x00,0x08,0x08,0x08,0x08,0x00
	db 0x06,0x06,0x00,0x02,0x42,0x42,0x3C,0x00,0x20,0x20,0x26,0x28,0x30,0x28,0x26,0x00
	db 0x30,0x10,0x10,0x10,0x10,0x10,0x10,0x00,0x00,0x80,0xEC,0x92,0x92,0x92,0x92,0x00
	db 0x00,0x40,0x78,0x44,0x44,0x44,0x44,0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x3C,0x00

	db 0x00,0x3C,0x42,0x42,0x7C,0x40,0x40,0x00,0x00,0x78,0x84,0x84,0x7C,0x04,0x06,0x00
	db 0x00,0x00,0x5C,0x62,0x40,0x40,0x40,0x00,0x00,0x00,0x3E,0x40,0x3C,0x02,0x7C,0x00
	db 0x00,0x10,0x7C,0x10,0x10,0x10,0x0E,0x00,0x00,0x00,0x42,0x42,0x42,0x42,0x3F,0x00
	db 0x00,0x00,0x42,0x42,0x24,0x24,0x18,0x00,0x00,0x00,0x92,0x92,0x92,0x92,0x6C,0x00
	db 0x00,0x00,0x42,0x24,0x18,0x24,0x42,0x00,0x00,0x00,0x42,0x42,0x3E,0x02,0x7C,0x00
	db 0x00,0x00,0x7E,0x02,0x3C,0x40,0x7E,0x00,0x08,0x10,0x10,0x20,0x10,0x10,0x08,0x00
	db 0x10,0x10,0x10,0x00,0x10,0x10,0x10,0x00,0x20,0x10,0x10,0x08,0x10,0x10,0x20,0x00
	db 0x00,0x00,0x60,0x92,0x0C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00

pcv2udl:
	db 0x0, 0x8, 0x0, 0x8, 0x4, 0xC, 0x4, 0xC, 0x1, 0x9, 0x1, 0x9, 0x5, 0xD, 0x5, 0xD
pcv2rev:
	db 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1
pcv2pcc:
	db 0x0, 0x8, 0x0, 0x4, 0x8, 0xC, 0x8, 0xC, 0x0, 0x4, 0x0, 0x4, 0x8, 0xC, 0x8, 0xC

adpcmTestValues:
	db 0x04, 0x05, 0x0D, 0x05, 0x0D, 0x05, 0x0D, 0x05
	db 0x0D, 0x05, 0x0D, 0x05, 0x0D, 0x05, 0x0D, 0x08
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x14, 0x15, 0x1D, 0x15, 0x1D, 0x15, 0x1D, 0x18
	db 0x1D, 0x15, 0x1D, 0x15, 0x1D, 0x15, 0x1D, 0x18
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x24, 0x2C, 0x24, 0x2C, 0x24, 0x2C, 0x24, 0x2C
	db 0x24, 0x2C, 0x24, 0x2C, 0x24, 0x2C, 0x24, 0x2C
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x34, 0x3C, 0x34, 0x3C, 0x34, 0x3C, 0x34, 0x3C
	db 0x3C, 0x3C, 0x3C, 0x3C, 0x3C, 0x3C, 0x3C, 0x3C
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x4C, 0x4C
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59
	db 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x5F
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69, 0x69
	db 0x69, 0x69, 0x69, 0xE9, 0x69, 0xE9, 0x69, 0xE9
	db 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99
	db 0x79, 0x97, 0x99, 0x79, 0x97, 0x99, 0x79, 0x97
	db 0x99, 0x79, 0x97, 0x99, 0x79, 0x97, 0x99, 0xF9
	db 0x97, 0x99, 0xF9, 0x97, 0x99, 0xF9, 0x97, 0x99
	db 0x88, 0x88, 0x88, 0x88, 0x88, 0x88, 0x88, 0x88

adpcmSaturationValues:
	db 0x77, 0x77, 0x71, 0x88, 0x08, 0x77, 0x20, 0x88
	db 0x07, 0x54, 0x80, 0x08, 0x00, 0x00, 0x00, 0x00

alphabet: db "ABCDEFGHIJKLMNOPQRSTUVWXYZ!", 10, 0
alphabet2: db "abcdefghijklmnopqrstuvwxyz.,", 10, 0

headLineStr: db "  WS Karnak Tester 20250709",10, 10 , 0

menuDumpIOPortsStr: db "  Dump IO Ports.",10 , 0
menuTestAllStr: db "  Test All.",10 , 0
menuTestKarnakStr3: db "  Test Karnak Table Values.",10 , 0
menuTestKarnakStr4: db "  Test Karnak Saturation.",10 , 0
menuTestKarnakStr5: db "  Test Karnak RND values.",10 , 0
menuTestKarnakStr6: db "  Test Karnak Wr 1, Rd 2.",10 , 0
menuTestKarnakStr7: db "  Test Karnak Wr 2, Rd 1.",10 , 0
menuTestKarnakStr8: db "  Test Karnak Wr Disabled.",10 , 0
menuTestKarnakStr9: db "  Test Karnak Timing.",10 , 0
menuTestKarnakStr10: db "  Karnak Dump All Values.",10 , 0
menuTestKarnakStr11: db "  Karnak Dump All Val/reset",10 , 0

testingTableStr: db "Testing Diff Table", 10, 0
testingSaturationStr: db "Testing Saturation", 10, 0
testingValuesStr: db "Testing Result Values", 10, 0
testingWr1Rd2Str: db "Testing Write 1 Read 2", 10, 0
testingWr2Rd1Str: db "Testing Write 2 Read 1", 10, 0
testingDisabledStr: db "Testing When Disabled", 10, 0
testingTimingStr: db "Testing Timing", 10, 0
testingOnly0Str: db "Write only 0x0", 10, 0
testingOnly1Str: db "Write only 0x1", 10, 0
testingOnly2Str: db "Write only 0x2", 10, 0
testingOnly3Str: db "Write only 0x3", 10, 0
testingOnly4Str: db "Write only 0x4", 10, 0
testingOnly5Str: db "Write only 0x5", 10, 0
testingOnly6Str: db "Write only 0x6", 10, 0
testingOnly7Str: db "Write only 0x7", 10, 0
testingOnly8Str: db "Write only 0x8", 10, 0
testingOnly9Str: db "Write only 0x9", 10, 0
testingOnlyAStr: db "Write only 0xA", 10, 0
testingOnlyBStr: db "Write only 0xB", 10, 0
testingOnlyCStr: db "Write only 0xC", 10, 0
testingOnlyDStr: db "Write only 0xD", 10, 0
testingOnlyEStr: db "Write only 0xE", 10, 0
testingOnlyFStr: db "Write only 0xF", 10, 0


test8InputStr: db "Testing Input: 0x00", 0
test16InputStr: db "Testing Input: 0x0000", 0
test8x8InputStr: db "Testing Input: 0x00, 0x00", 0
test16x8InputStr: db "Testing Input: 0x0000, 0x00", 0
test16x16InputStr: db "Testing Inp: 0x0000, 0x0000", 0
test32x16InputStr: db "Testing: 0x00000000, 0x0000", 0
indexStr: db " Index: 0x", 0
inputStr: db "Input: 0x", 0
inputPrevStr: db "PrevInput: 0x", 0
input32Str: db "I:0x", 0
expectedStr: db "Expected Result: 0x", 0
testedStr: db "Tested Result: 0x", 0
valueStr: db "Value:0x",0
flagsStr: db " Flags:0x",0
okStr: db "Ok!", 10, 0
failedStr: db "Failed!", 10, 0
preFlagStr: db "PreF: ", 0
postFlagStr: db "PostF: ", 0
hexPrefixStr: db " 0x",0
fHexPrefixStr: db " F:0x",0

author: db "Written by Fredrik Ahlström, 2025"

SECTION .bss start=0x0100 ; Keep space for Int Vectors

globalFrameCounter: resw 1
bgPos:
bgXPos: resb 1
bgYPos: resb 1
fgPos:
fgXPos: resb 1
fgYPos: resb 1
cursorPos:
cursorXPos: resb 1
cursorYPos: resb 1
menuXPos: resb 1
menuYPos: resb 1
keysHeld: resb 1
keysDown: resb 1

adpcmAcc: resw 1
adpcmInput: resb 1
adpcmEnable: resb 1
adpcmOdd: resb 1
adpcmIdx: resb 1

adpcmIdxOld: resb 1
adpcmAlign: resb 1

lfsr1: resw 1
lfsr2: resw 1
lfsr3: resw 2

inputVal1: resw 1
inputVal2: resw 1
inputVal3: resw 1
inputFlags: resw 1
inputCarry: resw 1

testedResult1: resw 1
testedResult2: resw 1
testedFlags: resw 1
testedException: resw 1		; If a (division) exception occurred.

expectedResult1: resw 1
expectedResult2: resw 1
expectedFlags: resw 1
expectedException: resw 1

isTesting: resb 1			; If currently running test.
dummy: resb 1

selfModifyingCode: resb 8
