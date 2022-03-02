;sfx library for working with the
;snesmod sfx mod, doug fraker, 2022.2


;values 1-127 = wait # frames
;value 0 = end of data



NOTE_C2 = 128
NOTE_Cs2 = 129
NOTE_D2 = 130
NOTE_Ds2 = 131
NOTE_E2 = 132
NOTE_F2 = 133
NOTE_Fs2 = 134
NOTE_G2 = 135
NOTE_Gs2 = 136
NOTE_A2 = 137
NOTE_As2 = 138
NOTE_B2 = 139

NOTE_C3 = 140
NOTE_Cs3 = 141
NOTE_D3 = 142
NOTE_Ds3 = 143
NOTE_E3 = 144
NOTE_F3 = 145
NOTE_Fs3 = 146
NOTE_G3 = 147
NOTE_Gs3 = 148
NOTE_A3 = 149
NOTE_As3 = 150
NOTE_B3 = 151
;middle C...
NOTE_C4 = 152
NOTE_Cs4 = 153
NOTE_D4 = 154
NOTE_Ds4 = 155
NOTE_E4 = 156
NOTE_F4 = 157
NOTE_Fs4 = 158
NOTE_G4 = 159
NOTE_Gs4 = 160
NOTE_A4 = 161
NOTE_As4 = 162
NOTE_B4 = 163

NOTE_C5 = 164
NOTE_Cs5 = 165
NOTE_D5 = 166
NOTE_Ds5 = 167
NOTE_E5 = 168
NOTE_F5 = 169
NOTE_Fs5 = 170
NOTE_G5 = 171
NOTE_Gs5 = 172
NOTE_A5 = 173
NOTE_As5 = 174
NOTE_B5 = 175

NOTE_C6 = 176
NOTE_Cs6 = 177
NOTE_D6 = 178
NOTE_Ds6 = 179
NOTE_E6 = 180
NOTE_F6 = 181
NOTE_Fs6 = 182
NOTE_G6 = 183
NOTE_Gs6 = 184
NOTE_A6 = 185
NOTE_As6 = 186
NOTE_B6 = 187
NOTE_C7 = 188

;values 189-191 are invalid
;will play a junk note value

SFX_ID = $c0 ; 0-15
SFX_PAN = $d0 ; 0-15, 0 left, 7-8 mid, 15 right
SFX_VOL = $e0 ; 0-15

;$F0 - retrigger off (don't use before first note)
;$FF - retrigger on (default)
;NOTE - volume zero is problematic (except to end the note)

SFX_TRIG_OFF = $F0
SFX_TRIG_ON = $FF


.segment "ZEROPAGE"

sfx_ptr: .res 3

.segment "BSS"

sfx_ptr6: .res 3
sfx_ptr7: .res 3
sfx_on6: .res 1
sfx_on7: .res 1
sfx_offset6: .res 2
sfx_offset7: .res 2
sfx_wait6: .res 1
sfx_wait7: .res 1
sfx_id6:	.res 1
sfx_id7:	.res 1
sfx_vol6: .res 1 
sfx_vol7: .res 1
sfx_pan6: .res 1 
sfx_pan7: .res 1
sfx_flip: .res 1 ;0 = 6, !0 = 7
sfx_q:	.res 1 ;which sfx will play?
sfx_temp: .res 1
sfx_trig6: .res 1 ;retrigger
sfx_trig7: .res 1


.segment "CODE"

.a8
.i16

Sfx_Stop_All:
	stz sfx_q
	jsr Sfx6_Stop
	jmp Sfx7_Stop ;rts
	
	
	
Sfx_Queue:
;call multiple times per frame, but only the last will play
; 0 = nothing, use values starting with 1
;a8 = which sfx
	sta sfx_q
	rts
	

Sfx_Process:
;call once per frame
;alternate between channels 6 and 7
	lda sfx_q
	bne :+
	jmp Sfx_Process6
:
	
;idea... it might be useful to skip triggering a new sfx
;sequence if both channels are already active
	
	lda #0 ;clear upper byte
	xba
	lda sfx_q
	dec a ;
	asl a ;x2
	tax
	lda sfx_flip
	bne Sfx_Play_7
	
Sfx_Play_6:
	
	lda #^SFX_DATA
	sta sfx_ptr6+2
	lda f:SFX_DATA, x
	sta sfx_ptr6
	lda f:SFX_DATA+1, x
	sta sfx_ptr6+1
	lda #1
	sta sfx_on6
	sta sfx_flip ;next time play 7
	stz sfx_offset6
	stz sfx_offset6+1
	stz sfx_wait6
	lda #8 ;default half vol
	sta sfx_vol6
;	lda #8 ;default pan center
	sta sfx_pan6
	stz sfx_q
	lda #$ff
	sta sfx_trig6
	bra Sfx_Process6
		
Sfx_Play_7:

	lda #^SFX_DATA
	sta sfx_ptr7+2
	lda f:SFX_DATA, x
	sta sfx_ptr7
	lda f:SFX_DATA+1, x
	sta sfx_ptr7+1
	lda #1
	sta sfx_on7
	stz sfx_offset7
	stz sfx_offset7+1
	stz sfx_wait7
	lda #8 ;default half vol
	sta sfx_vol7
;	lda #8 ;default pan center
	sta sfx_pan7
	stz sfx_flip ;next time play 6
	stz sfx_q
	lda #$ff
	sta sfx_trig7
	
;fall through	

Sfx_Process6:
;process each sfx channel
	lda sfx_on6
	beq Sfx_Process7
	
	lda sfx_wait6
	beq :+
	dec sfx_wait6 ;frames to wait
	bne Sfx_Process7
:	
	ldx sfx_ptr6
	stx sfx_ptr
	lda sfx_ptr6+2
	sta sfx_ptr+2
	ldy sfx_offset6
@load6:	
	lda [sfx_ptr], y
	bne :+
;end the effect sequence
	jsr Sfx6_Stop
	bra Sfx_Process7
:	
	iny
	sty sfx_offset6
	cmp #$80
	bcs @not_wait
;values 1-127 = wait
	sta sfx_wait6
	bra Sfx_Process7
@not_wait:	
	cmp #$c0
	bcs @not_note
;values $80-$bf = note	
	and #$3f
	jsr Sfx6_Do ;call the sfx function, new note
	bra Sfx_Process7 ;implied wait = 1
@not_note:	
	cmp #$d0
	bcs @not_id	
;value $c0-$cf = set sfx id	
	and #$0f
	sta sfx_id6 
	bra @load6
@not_id:	
	cmp #$e0
	bcs @not_pan
;values $d0-df = pan value
	and #$0f
	sta sfx_pan6
	bra @load6
@not_pan:
	cmp #$f0
	bcs @not_vol
;values $e0-ef = volume
	and #$0f
	sta sfx_vol6
	bra @load6
;values $f0-ff = retrigger
@not_vol:
	and #$0f ; if was f0, now 00 = off
	sta sfx_trig6
	bra @load6
	
Sfx_Process7:	
	lda sfx_on7
	beq Sfx_Exit
	
	lda sfx_wait7
	beq :+
	dec sfx_wait7 ;frames to wait
	bne Sfx_Exit
:	
	ldx sfx_ptr7
	stx sfx_ptr
	lda sfx_ptr7+2
	sta sfx_ptr+2
	ldy sfx_offset7
@load7:	
	lda [sfx_ptr], y
	bne :+
;end the effect sequence
	jsr Sfx7_Stop
	bra Sfx_Exit
:	
	iny
	sty sfx_offset7
	cmp #$80
	bcs @not_wait
;values 1-127 = wait
	sta sfx_wait7
	bra Sfx_Exit
@not_wait:	
	cmp #$c0
	bcs @not_note
;values $80-$bf = note	
	and #$3f
	jsr Sfx7_Do ;call the sfx function, new note
	bra Sfx_Exit ;implied wait = 1
@not_note:	
	cmp #$d0
	bcs @not_id	
;value $c0-$cf = set sfx id	
	and #$0f
	sta sfx_id7 
	bra @load7
@not_id:	
	cmp #$e0
	bcs @not_pan
;values $d0-df = pan value
	and #$0f
	sta sfx_pan7
	bra @load7
@not_pan:
	cmp #$f0
	bcs @not_vol
;values $e0-ef = volume
	and #$0f
	sta sfx_vol7
	bra @load7
;values $f0-ff = retrigger
@not_vol:
	and #$0f ; if was f0, now 00 = off
	sta sfx_trig7
	bra @load7
	
Sfx_Exit:
	rts
	
	
Sfx6_Stop:
	lda sfx_pan6 ;pan
	ldx #0006 ;channel
	jsr spcFxParams

	lda #00 ;vol = 0
	ldx #0000 ;sample id
	txy ;ldy #0000 ;pitch
	jsr spcEffect
	
	jsr spcProcess

	stz sfx_on6 ;0 = off
	rts

Sfx7_Stop:
	lda sfx_pan7 ;pan
	ldx #0007 ;channel
	jsr spcFxParams

	lda #00 ;vol = 0
	ldx #0000 ;sample id
	txy ;ldy #0000 ;pitch
	jsr spcEffect
	
	jsr spcProcess

	stz sfx_on7 ;0 = off
	rts
	
Sfx6_Do:
;a = note 0-$3f
	sta sfx_temp
	lda sfx_trig6
	bne :+
;no retrigger, set bit 7
	lda #$80
	ora sfx_temp
	sta sfx_temp
:	
	
	lda sfx_pan6 ;pan
	ldx #0006 ;channel
	jsr spcFxParams

	lda sfx_vol6 ;vol = 0
	ldx sfx_id6 ;sample id
	ldy sfx_temp ;pitch
;upper bytes are ignored	
	jsr spcEffect
	
	jsr spcProcess

;sfx_on6 should be on already
	rts
	
Sfx7_Do:
;a = note 0-$3f
	sta sfx_temp
	lda sfx_trig7
	bne :+
;no retrigger, set bit 7
	lda #$80
	ora sfx_temp
	sta sfx_temp
:	
	
	lda sfx_pan7 ;pan
	ldx #0007 ;channel
	jsr spcFxParams
	
	lda sfx_vol7 ;vol = 0
	ldx sfx_id7 ;sample id
	ldy sfx_temp ;pitch
;upper bytes are ignored	
	jsr spcEffect
	
	jsr spcProcess

;sfx_on6 should be on already
	rts


