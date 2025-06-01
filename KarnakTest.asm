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

	mov si, menuTestAllStr
	call writeString
	mov si, menuTestKarnakStr
	call writeString
	mov si, menuTestKarnakStr2
	call writeString
	mov si, menuTestLogicStr
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
	cmp cl, 2			; Index of last menu item
	js dontMoveDown
	mov cl, 2			; same
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
	jz testAll
	cmp cl, 1
	jz testKarnakAllValues
	cmp cl, 2
	jz testKarnakAllValuesWithReset
	cmp cl, 3
	jz testLogic
	; No input, restart main loop
	jmp mainLoop
;-----------------------------------------------------------------------------
;
; END main area
;
;-----------------------------------------------------------------------------
testAll:
	call runKarnakTestAllValues
	call runKarnakTestAllValuesWithReset
;	call runLogic

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
testLogic:
	call runLogic

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
runLogic:
	call testEqu
	call testAnd8
	call testNot8
	call testOr8
	call testTest8
	jmp testXor8
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
;-----------------------------------------------------------------------------
resetADPCM:
	mov al, 0		; Reset timer/adpcm
	out 0xD6, al
	mov al, 0x80
	out 0xD6, al
	ret
;-----------------------------------------------------------------------------
; Print result from only writing a specified nibble to the decoder.
;-----------------------------------------------------------------------------
testSingleNibbleOnly:
	mov dl, al

	mov cx, 60
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
; Test equality by CMP, SUB & XOR of all byte/word values.
;-----------------------------------------------------------------------------
testEqu:
	mov si, testingEquStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cl, 0
testEqu8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], cl
	mov al, cl
	cmp al, cl
	jnz equ8Failed
	sub al, cl
	jnz equ8Failed
	mov al, cl
	xor al, cl
	jnz equ8Failed
continueEqu8:
	inc cl
	jnz testEqu8Loop

	mov cl, 0
testNeq8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], cl
	mov al, cl
	inc al
	cmp al, cl
	jz neq8Failed
	sub al, cl
	jz neq8Failed
	mov al, cl
	inc al
	xor al, cl
	jz neq8Failed
continueNeq8:
	inc cl
	jnz testNeq8Loop

	hlt
	mov al, 10
	int 0x10
	mov si, test16x16InputStr
	call writeString
	mov byte [es:isTesting], 3

	mov cx, 0
testEqu16Loop:
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
	mov ax, cx
	cmp ax, cx
	jnz equ16Failed
	sub ax, cx
	jnz equ16Failed
	mov ax, cx
	xor ax, cx
	jnz equ16Failed
continueEqu16:
	inc cx
	jnz testEqu16Loop

	mov cx, 0
testNeq16Loop:
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
	mov ax, cx
	inc ax
	cmp ax, cx
	jz neq16Failed
	sub ax, cx
	jz neq16Failed
	mov ax, cx
	inc ax
	xor ax, cx
	jz neq16Failed
continueNeq16:
	inc cx
	jnz testNeq16Loop
	jmp endTestWriteOk

equ8Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueEqu8
	ret
neq8Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueNeq8
	ret
equ16Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueEqu16
	ret
neq16Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueNeq16
	ret

;-----------------------------------------------------------------------------
; Test logical AND of all byte values.
;-----------------------------------------------------------------------------
testAnd8:
	mov si, testingAnd8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov [es:expectedResult1], cx
testAnd8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov ax, cx
	not ax
	or al, ah
	not al
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testAnd8Single
	xor al, 0
	jnz stopAnd8Test
continueAnd8:
	inc cx
	jnz testAnd8Loop
	jmp endTestWriteOk

stopAnd8Test:
	call checkKeyInput
	xor al, 0
	jnz continueAnd8
	ret

;-----------------------------------------------------------------------------
testAnd8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	and al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz and8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz and8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	and al, cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz and8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz and8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

and8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical NOT of all byte values.
;-----------------------------------------------------------------------------
testNot8:
	mov si, testingNot8Str
	call writeString
	mov si, test8InputStr
	call writeString

	mov byte [es:isTesting], 4

	xor cx, cx
	mov [es:expectedResult1], cx
	dec ch
testNot8Loop:
	mov [es:inputVal1], cl
	mov [es:expectedResult1], ch
	call testNot8Single
	xor al, 0
	jnz stopNot8Test
continueNot8:
	dec ch
	inc cl
	jnz testNot8Loop
	jmp endTestWriteOk

stopNot8Test:
	call checkKeyInput
	xor al, 0
	jnz continueNot8
	ret

;-----------------------------------------------------------------------------
testNot8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax
	mov ax, 0xF202
	mov [es:expectedFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	popf
	not al
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz not8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz not8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax
	mov ax, 0xFAD7
	mov [es:expectedFlags], ax

	xor bh, bh
	mov bl, [es:inputVal1]
	popf
	not bl
	pushf

	mov [es:testedResult1], bx
	pop ax
	mov [es:testedFlags], ax
	mov cx, [es:expectedResult1]
	xor bx, cx
	jnz not8Failed
	mov cx, [es:expectedFlags]
	xor ax, cx
	jnz not8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

not8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical OR of all byte values.
;-----------------------------------------------------------------------------
testOr8:
	mov si, testingOr8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cx, 0
	mov [es:expectedResult1], cx
testOr8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov ax, cx
	not ax
	and al, ah
	not al
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testOr8Single
	xor al, 0
	jnz stopOr8Test
continueOr8:
	inc cx
	jnz testOr8Loop
	jmp endTestWriteOk

stopOr8Test:
	call checkKeyInput
	xor al, 0
	jnz continueOr8
	ret

;-----------------------------------------------------------------------------
testOr8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	or al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz or8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz or8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	or al, cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz or8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz or8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

or8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical TEST of all byte values.
;-----------------------------------------------------------------------------
testTest8:
	mov si, testingTest8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov [es:expectedResult1], cx
testTest8Loop:
	mov [es:inputVal1], cl
	mov [es:expectedResult1], cl
	mov [es:inputVal2], ch
	mov al, cl
	and al, ch
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testTest8Single
	xor al, 0
	jnz stopTest8Test
continueTest8:
	inc cx
	jnz testTest8Loop
	jmp endTestWriteOk

stopTest8Test:
	call checkKeyInput
	xor al, 0
	jnz continueTest8
	ret

;-----------------------------------------------------------------------------
testTest8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	test al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz test8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz test8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	test al, cl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz test8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz test8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

test8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical XOR of all byte values.
;-----------------------------------------------------------------------------
testXor8:
	mov si, testingXor8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cx, 0
	mov [es:expectedResult1], cx
testXor8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov ax, cx
	and al, ah
	mov bl, cl
	or bl, ch
	not al
	and al, bl
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testXor8Single
	cmp al, 0
	jnz stopXor8Test
continueXor8:
	inc cx
	jnz testXor8Loop
	jmp endTestWriteOk

stopXor8Test:
	call checkKeyInput
	cmp al, 0
	jnz continueXor8
	ret

;-----------------------------------------------------------------------------
testXor8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov ah, 0
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	xor al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	cmp ax, cx
	jnz xor8Failed
	mov cx, [es:expectedFlags]
	cmp bx, cx
	jnz xor8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov ah, 0
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	xor al, cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	cmp ax, cx
	jnz xor8Failed
	mov cx, [es:expectedFlags]
	cmp bx, cx
	jnz xor8Failed

	mov ax, 0
	pop cx
	pop bx
	ret

xor8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
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
printFailedResult:
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

PZSTable:
	db PSR_Z|PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0 ,PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S

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

alphabet: db "ABCDEFGHIJKLMNOPQRSTUVWXYZ!", 10, 0
alphabet2: db "abcdefghijklmnopqrstuvwxyz.,", 10, 0

headLineStr: db "  WS Karnak Tester 20250531",10, 10 , 0

menuTestAllStr: db "  Test All.",10 , 0
menuTestKarnakStr: db "  Test Karnak All Values.",10 , 0
menuTestKarnakStr2: db "  Test Karnak All Val/reset.",10 , 0
menuTestLogicStr: db "  Test Logic.",10 , 0
menuTestArithmeticStr: db "  Test Arithmetic.",10 , 0

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
testingEquStr: db "Equal by CMP, SUB & XOR", 10, 0
testingAnd8Str: db "Logical AND bytes", 10, 0
testingAnd16Str: db "Logical AND words", 10, 0
testingOr8Str: db "Logical OR bytes", 10, 0
testingOr16Str: db "Logical OR words", 10, 0
testingTest8Str: db "Logical TEST bytes", 10, 0
testingTest16Str: db "Logical TEST words", 10, 0
testingXor8Str: db "Logical XOR bytes", 10, 0
testingXor16Str: db "Logical XOR words", 10, 0
testingNot8Str: db "Logical NOT bytes", 10, 0
testingNot16Str: db "Logical NOT words", 10, 0


test8InputStr: db "Testing Input: 0x00", 0
test16InputStr: db "Testing Input: 0x0000", 0
test8x8InputStr: db "Testing Input: 0x00, 0x00", 0
test16x8InputStr: db "Testing Input: 0x0000, 0x00", 0
test16x16InputStr: db "Testing Inp: 0x0000, 0x0000", 0
test32x16InputStr: db "Testing: 0x00000000, 0x0000", 0
inputStr: db "Input:0x", 0
input32Str: db "I:0x", 0
expectedStr: db "Expected Result:", 10, 0
testedStr: db "Tested Result:", 10, 0
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
