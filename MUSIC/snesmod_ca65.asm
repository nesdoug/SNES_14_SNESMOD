;*
;* Copyright 2009 Mukunda Johnson (mukunda.com)
;* 
;* This file is part of SNESMOD - gh.mukunda.com/snesmod
;*
;* See LICENSING.txt
;*
;modified doug fraker, 2022.2
;-----CHANGES
;streaming functions removed
;spcEffect now allows values 0-60 for pitch, C2-C7
;-it also allows looped samples to be used as sfx
;spcFxParams is for setting pan and which channel a sfx plays
;spcClone allows a sample in the song to be used as a sfx
;spcGlobalVolume added to set the volume for everything
;some other minor changes
;---see SFX_LIB for more sound effects functions



;.include "snes.inc" see regs.asm

; all jsr rts
; all expect a8 i16

.export spcBoot
.export spcSetBank
.export spcLoad
.export spcPlay
.export spcStop
.export spcReadStatus
.export spcReadPosition
.export spcGetCues

.export spcSetModuleVolume
.export spcFadeModuleVolume
.export spcLoadEffect
.export spcEffect
.export spcFxParams
.export spcClone

.export spcFlush
.export spcProcess

.export spcGlobalVolume



;----------------------------------------------------------------------
; soundbank defs
;----------------------------------------------------------------------

.ifdef HIROM
SB_SAMPCOUNT	=0000h
SB_MODCOUNT	=0002h
SB_MODTABLE	=0004h
SB_SRCTABLE	=0184h
.else
SB_SAMPCOUNT	=8000h
SB_MODCOUNT	=8002h
SB_MODTABLE	=8004h
SB_SRCTABLE	=8184h
.endif

;----------------------------------------------------------------------
; spc commands
;----------------------------------------------------------------------

CMD_LOAD	=00h
CMD_LOADE	=01h
CMD_VOL		=02h
CMD_PLAY	=03h
CMD_STOP	=04h
CMD_MVOL	=05h
CMD_FADE	=06h
CMD_CLONE	=07h
CMD_FX		=08h
CMD_FX2		=09h
CMD_SSIZE	=0Ah




REG_APUI00	=$2140  ; Sound Register				1B/RW
REG_APUI01	=$2141  ; Sound Register				1B/RW
REG_APUI02	=$2142  ; Sound Register				1B/RW
REG_APUI03	=$2143  ; Sound Register				1B/RW
REG_OPVCT	=$213D  ; Y Scanline Location		1B/R D
REG_SLHV	=$2137  ; Sofware Latch For H/V Counter	1B/R

;----------------------------------------------------------------------

; process for 5 scanlines
PROCESS_TIME = 5
INIT_DATACOPY =13

;======================================================================
.segment "ZEROPAGE"
;======================================================================

spc_ptr:	.res 3
spc_v:		.res 1
spc_bank:	.res 1

spc1:		.res 2
spc2:		.res 2

spc_fread:	.res 1
spc_fwrite:	.res 1

; port record [for interruption]
spc_pr:		.res 4

;digi_src:	.res 3
;digi_src2:	.res 3

;SoundTable:	.res 3

;======================================================================
.segment "BSS"
;======================================================================

spc_fifo:	.res 256	; 128-byte command fifo
spc_sfx_next:	.res 1
spc_q:		.res 1

digi_init:	.res 1
digi_pitch:	.res 1
digi_vp:	.res 1
digi_remain:	.res 2
digi_active:	.res 1
digi_copyrate:	.res 1
;spc_fread:	.res 1		;
;spc_fwrite:	.res 1		;

;======================================================================
.segment "RODATA"
;======================================================================


SPC_BOOT = 0400h ; spc entry/load address

;======================================================================
.segment "CODE"
;======================================================================

.i16
.a8

;**********************************************************************
;* upload driver
;*
;* disable time consuming interrupts during this function
;**********************************************************************
spcBoot:			
;**********************************************************************
:	ldx	REG_APUI00	; wait for 'ready signal from SPC
	cpx	#0BBAAh		;
	bne	:-		;--------------------------------------
	stx	REG_APUI01	; start transfer:
	ldx	#SPC_BOOT	; port1 = !0
	stx	REG_APUI02	; port2,3 = transfer address
	lda	#0CCh		; port0 = 0CCh
	sta	REG_APUI00	;--------------------------------------
:	cmp	REG_APUI00	; wait for SPC
	bne	:-		;
;----------------------------------------------------------------------
; ready to transfer
;----------------------------------------------------------------------
	lda	f:SNESMOD_SPC	; read first byte
	xba			;
	lda	#0		;
	ldx	#1		;
	bra	sb_start	;
;----------------------------------------------------------------------
; transfer data
;----------------------------------------------------------------------
sb_send:
;----------------------------------------------------------------------
	xba			; swap DATA into A
	lda	f:SNESMOD_SPC, x; read next byte
	inx			; swap DATA into B
	xba			;--------------------------------------
:	cmp	REG_APUI00	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; increment counter (port0 data)
;----------------------------------------------------------------------
sb_start:
;----------------------------------------------------------------------
	rep	#20h		; write port0+port1 data
	sta	REG_APUI00	;
	sep	#20h		;--------------------------------------
	cpx	#SNESMOD_SPC_END-SNESMOD_SPC	; loop until all bytes transferred
	bcc	sb_send				;
;----------------------------------------------------------------------
; all bytes transferred
;----------------------------------------------------------------------
:	cmp	REG_APUI00	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; add 2 or so...
	ina			;--------------------------------------
				; mask data so invalid 80h message wont get sent
	stz	REG_APUI01	; port1=0
	ldx	#SPC_BOOT	; port2,3 = entry point
	stx	REG_APUI02	;
	sta	REG_APUI00	; write P0 data
				;--------------------------------------
:	cmp	REG_APUI00	; final sync
	bne	:-		;--------------------------------------
	stz	REG_APUI00
	
	stz	spc_v		; reset V
	stz	spc_q		; reset Q
	stz	spc_fwrite	; reset command fifo
	stz	spc_fread	;
	stz	spc_sfx_next	;
	
	stz	spc_pr+0
	stz	spc_pr+1
	stz	spc_pr+2
	stz	spc_pr+3
;----------------------------------------------------------------------
; driver installation successful
;----------------------------------------------------------------------
	rts			; return
;----------------------------------------------------------------------


;**********************************************************************
; set soundbank bank number (important...)
;
; a = bank # of the soundbank
;**********************************************************************
spcSetBank:
;**********************************************************************
	sta	spc_bank
	rts
	
	
	
; increment memory pointer by 2
.macro incptr
.scope
	iny
	iny
	
.ifndef HIROM
	bmi	_catch_overflow
	inc	spc_ptr+2
	ldy	#8000h
.else
	bne	_catch_overflow
	inc	spc_ptr+2
.endif

_catch_overflow:
.endscope
.endmacro



;**********************************************************************
; upload module to spc
; use spcSetBank first
;
; x = module_id
;
; this function takes a while to execute
;**********************************************************************
spcLoad:
;**********************************************************************

	phx				; flush fifo!
	jsr	spcFlush		;
	plx				;
	
	phx
	ldy	#SB_MODTABLE
	sty	spc2
	jsr	get_address
	rep	#20h
	lda	[spc_ptr], y	; X = MODULE SIZE
	tax
	
	incptr
	
	lda	[spc_ptr], y	; read SOURCE LIST SIZE
	
	incptr
	
	sty	spc1		; pointer += listsize*2
	asl			;
	adc	spc1		;
.ifndef HIROM
	bmi	:+		;
	ora	#8000h		;
.else
	bcc	:+
.endif
	inc	spc_ptr+2	;
:	tay			;
	
	sep	#20h		;
	lda	spc_v		; wait for spc
	pha			;
:	cmp	REG_APUI01	;
	bne	:-		;------------------------------
	lda	#CMD_LOAD	; send LOAD message
	sta	REG_APUI00	;
	pla			;
	eor	#80h		;
	ora	#01h		;
	sta	spc_v		;
	sta	REG_APUI01	;------------------------------
:	cmp	REG_APUI01	; wait for spc
	bne	:-		;------------------------------
	jsr	do_transfer
	
	;------------------------------------------------------
	; transfer sources
	;------------------------------------------------------
	
	plx
	ldy	#SB_MODTABLE
	sty	spc2
	jsr	get_address
	incptr
	
	rep	#20h		; x = number of sources
	lda	[spc_ptr], y	;
	tax			;
	
	incptr
	
transfer_sources:
	
	lda	[spc_ptr], y	; read source index
	sta	spc1		;
	
	incptr
	
	phy			; push memory pointer
	sep	#20h		; and counter
	lda	spc_ptr+2	;
	pha			;
	phx			;
	
	jsr	transfer_source
	
	plx			; pull memory pointer
	pla			; and counter
	sta	spc_ptr+2	;
	ply			;
	
	dex
	bne	transfer_sources
@no_more_sources:

	stz	REG_APUI00	; end transfers
	lda	spc_v		;
	eor	#80h		;
	sta	spc_v		;
	sta	REG_APUI01	;-----------------
:	cmp	REG_APUI01	; wait for spc
	bne	:-		;-----------------
	sta	spc_pr+1
	stz	spc_sfx_next	; reset sfx counter
	
	rts
	
	
;**********************************************************************
; spc1 = source index
;**********************************************************************
transfer_source:	
	ldx	spc1
	ldy	#SB_SRCTABLE
	sty	spc2
	jsr	get_address
	
	lda	#01h		; port0=01h
	sta	REG_APUI00	;
	rep	#20h		; x = length (bytes->words)
	lda	[spc_ptr], y	;
	incptr			;
	ina			;
	lsr			;
	tax			;
	lda	[spc_ptr], y	; port2,3 = loop point
	sta	REG_APUI02
	incptr
	sep	#20h
	
	lda	spc_v		; send message
	eor	#80h		;	
	ora	#01h		;
	sta	spc_v		;
	sta	REG_APUI01	;-----------------------
:	cmp	REG_APUI01	; wait for spc
	bne	:-		;-----------------------
	cpx	#0
	beq	end_transfer	; if datalen != 0
	bra	do_transfer	; transfer source data
	
;**********************************************************************
; spc_ptr+y: source address
; x = length of transfer (WORDS)
;**********************************************************************
transfer_again:
	eor	#80h		;
	sta	REG_APUI01	;
	sta	spc_v		;
	incptr			;
:	cmp	REG_APUI01	;
	bne	:-		;
;--------------------------------------------------------------
do_transfer:
;--------------------------------------------------------------

	rep	#20h		; transfer 1 word
	lda	[spc_ptr], y	;
	sta	REG_APUI02	;
	sep	#20h		;
	lda	spc_v		;
	dex			;
	bne	transfer_again	;
	
	incptr

end_transfer:
	lda	#0		; final word was transferred
	sta	REG_APUI01	; write p1=0 to terminate
	sta	spc_v		;
:	cmp	REG_APUI01	;
	bne	:-		;
	sta	spc_pr+1
	rts

;--------------------------------------------------------------
; spc2 = table offset
; x = index
;
; returns: spc_ptr = 0,0,bank, Y = address
get_address:
;--------------------------------------------------------------

	lda	spc_bank	; spc_ptr = bank:SB_MODTABLE+module_id*3
	sta	spc_ptr+2	;
	rep	#20h		;
	stx	spc1		;
	txa			;
	asl			;
	adc	spc1		;
	adc	spc2		;
	sta	spc_ptr		;
	
	lda	[spc_ptr]	; read address
	pha			;
	sep	#20h		;
	ldy	#2		;
	lda	[spc_ptr],y	; read bank#
	
	clc			; spc_ptr = long address to module
	adc	spc_bank	;
	sta	spc_ptr+2	;
	ply			;
	stz	spc_ptr
	stz	spc_ptr+1
	rts			;
	
	
;**********************************************************************
;* x = id (index of the sample within the soundbank)
; use spcSetBank first
; note that spcLoad resets this to zero, so do that before loading effects
;*
;* load effect into memory
;**********************************************************************
spcLoadEffect:
;**********************************************************************
	ldy	#SB_SRCTABLE	; get address of source
	sty	spc2		;
	jsr	get_address	;--------------------------------------
	lda	spc_v		; sync with SPC
:	cmp	REG_APUI01	;
	bne	:-		;--------------------------------------
	lda	#CMD_LOADE	; write message
	sta	REG_APUI00	;--------------------------------------
	lda	spc_v		; dispatch message and wait
	eor	#80h		;
	ora	#01h		;
	sta	spc_v		;
		pha
;	sta	REG_APUI01	; not yet
;:	cmp	REG_APUI01	;
;	bne	:-		;--------------------------------------
	rep	#20h		; x = length (bytes->words)
	lda	[spc_ptr], y	;
	ina			;
	lsr			;
	incptr			;
	tax			;--------------------------------------

;pointer = sample loop value
	lda [spc_ptr], y ; = loop
	sta REG_APUI02 ;2 and 3

	incptr			; 
	sep	#20h		;--------------------------------------

	pla
	sta	REG_APUI01	; now do the signal and wait
:	cmp	REG_APUI01	;
	bne	:-


	jsr	do_transfer	; transfer data
				;--------------------------------------
	lda	spc_sfx_next	; return sfx index
	inc	spc_sfx_next	;
	rts			;
	
	
;**********************************************************************
; a = id
; spc1 = params
;**********************************************************************
QueueMessage:
	sei				; disable IRQ in case user 
					; has spcProcess in irq handler
			
	sep	#10h			; queue data in fifo
	ldx	spc_fwrite		;
	sta	spc_fifo, x		;
	inx				;
	lda	spc1			;
	sta	spc_fifo, x		;
	inx				;
	lda	spc1+1			;
	sta	spc_fifo, x		;
	inx				;
	stx	spc_fwrite		;
	rep	#10h			;
	cli				;
	rts				;


;**********************************************************************
; flush fifo (force sync)
;**********************************************************************
spcFlush:
;----------------------------------------------------------------------
	lda	spc_fread		; call spcProcess until
	cmp	spc_fwrite		; fifo becomes empty
	beq	@exit			;
	jsr	spcProcessMessages	;
	bra	spcFlush		;
@exit:	rts				;
	
	
;**********************************************************************
; process spc messages for x time
;**********************************************************************
spcProcess:
;----------------------------------------------------------------------

;	lda	digi_active
;	beq	:+
;	jsr	spcProcessStream
;:

spcProcessMessages:

	sep	#10h			; 8-bit index during this function
	lda	spc_fwrite		; exit if fifo is empty
	cmp	spc_fread		;
	beq	@exit			;------------------------------
	ldy	#PROCESS_TIME		; y = process time
;----------------------------------------------------------------------
@process_again:
;----------------------------------------------------------------------
	lda	spc_v			; test if spc is ready
	cmp	REG_APUI01		;
	bne	@next			; no: decrement time
					;------------------------------
	ldx	spc_fread		; copy message arguments
	lda	spc_fifo, x		; and update fifo read pos
	sta	REG_APUI00		;
	sta	spc_pr+0
	inx				;
	lda	spc_fifo, x		;
	sta	REG_APUI02		;
	sta	spc_pr+2
	inx				;
	lda	spc_fifo, x		;
	sta	REG_APUI03		;
	sta	spc_pr+3
	inx				;
	stx	spc_fread		;------------------------------
	lda	spc_v			; dispatch message
	eor	#80h			;
	sta	spc_v			;
	sta	REG_APUI01		;------------------------------
	sta	spc_pr+1
	lda	spc_fread		; exit if fifo has become empty
	cmp	spc_fwrite		;
	beq	@exit			;
;----------------------------------------------------------------------
@next:
;----------------------------------------------------------------------
	lda	REG_SLHV		; latch H/V and test for change
	lda	REG_OPVCT		;------------------------------
	cmp	spc1			; we will loop until the VCOUNT
	beq	@process_again		; changes Y times
	sta	spc1			;
	dey				;
	bne	@process_again		;
;----------------------------------------------------------------------
@exit:
;----------------------------------------------------------------------
	rep	#10h			; restore 16-bit index
	rts				;
	
	
;**********************************************************************
; a = starting pattern number (changed, was x)
; play a song in the module (there can be multiple songs in a module)
;**********************************************************************
spcPlay:
;----------------------------------------------------------------------
;	txa				; queue message: 
; just use A as the parameter
	sta	spc1+1			; id -- xx
	lda	#CMD_PLAY		;
	jmp	QueueMessage		;
	
	
;**********************************************************************	
; stop the song	
;**********************************************************************	
spcStop:
;**********************************************************************
	lda	#CMD_STOP
	jmp	QueueMessage


;**********************************************************************
; read status register
;**********************************************************************
spcReadStatus:
	ldx	#5			; read PORT2 with stability checks
	lda	REG_APUI02		; 
@loop:					;
	cmp	REG_APUI02		;
	bne	spcReadStatus		;
	dex				;
	bne	@loop			;
	rts				;
	
;**********************************************************************
; read position register
;**********************************************************************
spcReadPosition:
	ldx	#5			; read PORT3 with stability checks
	lda	REG_APUI02		;
@loop:					;
	cmp	REG_APUI02		;
	bne	spcReadPosition		;
	dex				;
	bne	@loop			;
	rts				;

;**********************************************************************
spcGetCues:
;**********************************************************************
	lda	spc_q
	sta	spc1
	jsr	spcReadStatus
	and	#0Fh
	sta	spc_q
	sec
	sbc	spc1
	bcs	:+
	adc	#16
:	rts


;**********************************************************************
; a = volume 0-127
; note that 80 ($50) is the default
; affects song and sfx
;**********************************************************************
spcGlobalVolume:
;**********************************************************************
	sta spc1
	lda	#CMD_VOL		;
	jmp	QueueMessage		;
	

;**********************************************************************
; a = volume 0-255 (changed, was x)
; note that 255 is default
; affects song, not sfx
;**********************************************************************
spcSetModuleVolume:
;**********************************************************************
;	txa				;queue:
; just use A as the parameter
	sta	spc1+1			; id -- vv
	lda	#CMD_MVOL		;
	jmp	QueueMessage		;


;**********************************************************************
; a = target volume 0-255 (changed, was x)
; y = speed
;**********************************************************************
spcFadeModuleVolume:
;**********************************************************************
;	txa				;queue:
; just use A as the parameter
	sta	spc1+1			; id xx yy
	tya				;
	sta	spc1			;
	lda	#CMD_FADE
	jmp	QueueMessage


;**********************************************************************
; play a sfx sound effect
; a = vol (0-15)
; x = sample id (0-15)
; y = pitch (0-60)
;**********************************************************************
spcEffect:
;**********************************************************************
	asl a ;vol
	asl a
	asl a
	asl a
	sta	spc1			; spc1.l = "vvvvssss"			
	txa		;sample id
	ora	spc1			;
	sta	spc1			
	tya		;pitch
	sta spc1+1 			; spc1.h = "--pppppp"
	;------------------------------
	lda	#CMD_FX			; queue FX message
	jmp	QueueMessage		;
;----------------------------------------------------------------------


;**********************************************************************
; set additional parameters for the sound effect
; do this BEFORE calling spcEffect, especially to set a target channel
; a = pan (0-15, 0=left, 7=center, 15=right)
; x = channel (0-7)
; values for the NEXT spcEffect call
;**********************************************************************
spcFxParams:
;**********************************************************************
	sta spc1+1
	txa
	sta spc1
	lda	#CMD_FX2
	jmp	QueueMessage
	
	
;**********************************************************************
; you must load the song (module) before using this
; ...if you wanted to use the same sample both in a	
; song and as an sfx, you would have to copy it twice
; to the SPC, wasting ARAM space.
; This function copies the pointer to the BRR sample
; from the song table to the sfx table
; (at the current sfx load index)
; so it doesn't have to be loaded twice
;
; a = index of sample in the song module
;**********************************************************************	
spcClone:	
;**********************************************************************	
	sta spc1
	lda	#CMD_CLONE
	jsr	QueueMessage
	jmp spcProcess

	
;======================================================================
;
; STREAMING (removed)
;
;======================================================================




