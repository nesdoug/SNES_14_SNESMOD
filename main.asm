; example 14 SNES code
; snesmod music
; doug fraker, 2022.2

.p816
.smart



.include "regs.asm"
.include "variables.asm"
.include "macros.asm"
.include "init.asm"
;.include "library.asm"


;SNESmod audio code
.include "MUSIC/snesmod_ca65.asm"

.global SNESMOD_SPC, SNESMOD_SPC_END
.segment "CODE"

SNESMOD_SPC:
.incbin "SNESMOD/snesmod_driver.bin"

SNESMOD_SPC_END:
;$15b1 bytes

.segment "RODATA6"
SONGBANK:
.incbin "MUSIC/soundbank.bank"

.segment "RODATA7"
SFXBANK:
.incbin "MUSIC/sfxbank.bank"


;in code..
.include "MUSIC/SFX_LIB.asm"

;can be anywhere..
.include "MUSIC/SFX_DATA.txt"


.segment "CODE"

; enters here in forced blank
Main:
.a16 ; the setting from init code
.i16
	phk
	plb
	
	
; COPY PALETTES to PAL_BUFFER	
	BLOCK_MOVE  256, BG_Palette, PAL_BUFFER
	A8 ;block move will put AXY16. Undo that.
		
	
; DMA from PAL_BUFFER to CGRAM
	jsr DMA_Palette ; in init.asm
	
	
; DMA from Tiles to VRAM	
	lda #V_INC_1 ; the value $80
	sta VMAIN  ; $2115 = set the increment mode +1

	DMA_VRAM  (End_BG_Tiles - BG_Tiles), BG_Tiles, $0000
	
	
	
	
; DMA from Map1 to VRAM	$6000 
	
	DMA_VRAM  $700, Map1, $6000



	
	

	lda #1 ; mode 1, tilesize 8x8 all
	sta BGMODE ; $2105
	
	stz BG12NBA ; $210b BG 1 and 2 TILES at VRAM address $0000
	
	lda #$60 ; bg1 map at VRAM address $6000
	sta BG1SC ; $2107
	
	lda #$68 ; bg2 map at VRAM address $6800
	sta BG2SC ; $2108
	
	lda #$70 ; bg3 map at VRAM address $7000
	sta BG3SC ; $2109	
	
	lda #2 ;sprite tiles at $4000
	sta OBSEL ;= $2101

;main screen	
	lda #BG1_ON
	sta TM ; $212c
	
	
	




; SPC / audio code.... SNESmod

	
;all the audio functions need these sizes
	A8
	XY16
	
	jsr spcBoot ;copy the spc program
	

	;a = bank #
	lda #^SONGBANK
	jsr spcSetBank
	
	;x = module_id
	ldx #0
	jsr spcLoad ; load the module

	;a = bank #
	lda #^SFXBANK
	jsr spcSetBank
	
	ldx #0 ;saw
	jsr spcLoadEffect
	
	ldx #1 ;square
	jsr spcLoadEffect
	
	ldx #2 ;piano
	jsr spcLoadEffect
	
	lda #1 ;strings
	jsr spcClone ;now also = effect #3
;use only AFTER the module is loaded, copies a pointer
;to a sample used by the song module
	
	ldx #3 ;punch
	jsr spcLoadEffect 
; will now = effect #4	
; because it goes in the order of how they are loaded
; not the order that they appear in the module

	jsr spcProcess

	;a = starting position (pattern number)
	lda #0
	jsr spcPlay
	
	lda #$7f ;0-255, 7f is half volume 
	jsr spcSetModuleVolume
	
	jsr spcProcess

	A8
	;turn on NMI interrupts and auto-controller reads
	lda #NMI_ON|AUTO_JOY_ON
	sta NMITIMEN ;$4200
	
	lda #FULL_BRIGHT ; $0f = turn the screen on, full brighness
	sta INIDISP ; $2100
	sta bright_var

	
	
Infinite_Loop:	
;game loop a8 xy16
	A8
	XY16
	jsr Wait_NMI ;wait for the beginning of v-blank

	jsr Pad_Poll ;read controllers
	
;needs to run every frame	
	jsr spcProcess
	jsr Sfx_Process
	
;handle button presses	

;A = play sfx sequence 1
	AXY16
	lda pad1_new
	and #KEY_A
	beq @not_a
	A8
	lda #1 ;non zero
	jsr Sfx_Queue ;or just -- sta sfx_q
	A16
@not_a:	

;B = play sfx sequence 2	
	lda pad1_new
	and #KEY_B
	beq @not_b
	A8
	lda #2 ;non zero
	jsr Sfx_Queue ;or just -- sta sfx_q
	A16
@not_b:	

;Y = play sfx sequence 3
	lda pad1_new
	and #KEY_Y
	beq @not_y
	A8
	lda #3 ;non zero
	jsr Sfx_Queue ;or just -- sta sfx_q
	A16
@not_y:	

;X = play sfx sequence 4
	lda pad1_new
	and #KEY_X
	beq @not_x
	A8
	lda #4 ;non zero
	jsr Sfx_Queue ;or just -- sta sfx_q
	A16
@not_x:	

;SELECT = stop all sfx
	lda pad1_new
	and #KEY_SELECT
	beq @not_select
	A8
	jsr Sfx_Stop_All
	
	lda #8 ;a = pan
	ldx #5 ;x = channel (0-7)
	jsr spcFxParams
	lda #0 ;vol = 0 = off
	tax ;0
	tay ;0
	jsr spcEffect
	jsr spcProcess
	
	A16
@not_select:

;L = not a sfx sequence, just play a sample
	lda pad1_new
	and #KEY_L
	beq @not_L
	A8
	lda #8 ;a = pan
	ldx #5 ;x = channel (0-7)
	jsr spcFxParams
	lda #15 ;vol = 0-15
	ldx #4 ;sample id 0-15
;this plays a punch sfx, it's not looped
	ldy #(NOTE_C4 & $3f) ;pitch	
	jsr spcEffect
	jsr spcProcess
	A16
@not_L:

;R = not a sfx sequence, just play a sample
	lda pad1_new
	and #KEY_R
	beq @not_R
	A8
	lda #8 ;a = pan
	ldx #5 ;x = channel (0-7)
	jsr spcFxParams
	lda #15 ;vol = 0-15
	WDM_BREAK 1
	ldx #0 ;sample id = 0-15
;this plays a saw sfx, it is looped, and plays infinitely
	ldy #(NOTE_C3 & $3f) ;pitch = 0-3f
	jsr spcEffect
	jsr spcProcess
	A16
@not_R:



;	jsr Draw_Sprites
	jmp Infinite_Loop
	
	
	


	
	
	
Wait_NMI:
.a8
.i16
;should work fine regardless of size of A
	lda in_nmi ;load A register with previous in_nmi
@check_again:	
	WAI ;wait for an interrupt
	cmp in_nmi	;compare A to current in_nmi
				;wait for it to change
				;make sure it was an nmi interrupt
	beq @check_again
	rts

	
	
	
Pad_Poll:
.a8
.i16
; reads both controllers to pad1, pad1_new, pad2, pad2_new
; auto controller reads done, call this once per main loop
; copies the current controller reads to these variables
; pad1, pad1_new, pad2, pad2_new (all 16 bit)
	php
	A8
@wait:
; wait till auto-controller reads are done
	lda $4212
	lsr a
	bcs @wait
	
	A16
	lda pad1
	sta temp1 ; save last frame
	lda $4218 ; controller 1
	sta pad1
	eor temp1
	and pad1
	sta pad1_new
	
	lda pad2
	sta temp1 ; save last frame
	lda $421a ; controller 2
	sta pad2
	eor temp1
	and pad2
	sta pad2_new
	plp
	rts
	


	
	

.include "header.asm"	



.segment "RODATA1"



BG_Tiles:
.incbin "Background/Alpha.chr"
End_BG_Tiles:



BG_Palette:
.incbin "Background/bg_pal.pal"


Map1:
.incbin "Background/SNESMOD.map"





