;========================================================
; "sm-spc"
;
; snesmod spc driver
;
; (c) 2009 mukunda johnson
; bugfix by kungfufurby 
; modified to work with ca65 blargg macro pack
;========================================================

;#define debuginc inc debug \ mov spc_port0, debug

;.define lbyte(z) (z & 0ffh)
;.define hbyte(z) (z >> 8)

module = 1a00h

.define sproc tcall 0
.define sproc2 tcall 0

;********************************************************
; protocol
;
; mm = mimic data
; id = message id
; vv = validation data (not previous value)
; v1 = nonzero validation data (not previous value)
;
; spc ports:
; port0 = reserved
; port1 = communication
; port2 = status:
;   msb fep-cccc lsb
;   f = module volume fade[out/in] in progress
;   e = end of module reached (restarted from beginning)
;   p = module is playing (0 means not playing or preparing...)
;   cccc = cue, incremented on sf1 pattern effect
; port3 = module position
; 
; name	id	desc
;--------------------------------------------------------
; load	00	upload module
; 
; >> id vv -- --	send message
; << -- mm -- --	message confirmed
;
; >> -- v1 dd dd	transfer module
; << -- mm -- --	dddd = data, loop until all words xferred
;
; >> -- 00 dd dd	final word
; << -- mm -- --	okay proceed to transfer sources...
;
; for each entry in source_list:
;
; >> 01 vv ll ll	send loop point
; << -- mm -- --	loop point saved
; >> -- v1 dd dd	transfer source data
; << -- mm -- --	dddd = data, loop unti all words xferred
;
; >> -- 00 dd dd	transfer last word
; << -- mm -- --	
;
; [loop until all needed sources are transferred]
;
; >> 00 vv -- --	terminate transfer
; << -- mm -- --
;
; notes:
;   this function resets the memory system
;   all sound effects will become invalid
; 
;   after final sample transferred the system may
;   be halted for some time to setup the echo delay.
;--------------------------------------------------------
; loade	01	upload sound effect
;
; >> id vv ll ll	send message
; << -- mm -- --	source registered, ready for data
;
; >> -- v1 dd dd	transfer source data
; << -- mm -- --	loop until all words xferred
;
; >> -- 00 dd dd	send last word
; << -- mm -- --	okay, ready for playback
;
; sound effects are always one-shot
;  llll is not used (or maybe it is...........)
;--------------------------------------------------------
; vol	02	set master volume
;
; >> id vv vv --
; << -- mm -- --
;
; vv = master volume level (0..127)
;--------------------------------------------------------
; play	03	play module
;
; >> id vv -- pp
; << -- mm -- --
;
; pp = start position
;--------------------------------------------------------
; stop	04	stop playback
;
; >> id vv -- --
; << -- mm -- --
;--------------------------------------------------------
; mvol	05	set module volume
;
; >> id vv -- vv
; << -- mm -- --
;
; vv = 0..255 new module volume scale
;--------------------------------------------------------
; fade	06	fade module volume
;
; >> id vv tt vv
; << -- mm -- --
;
; vv = 0..255 target volume level
; tt = fade speed (added every m tick)
;--------------------------------------------------------
; res	07	reset
;
; >> id vv -- --
; 
; <driver unloaded>
;--------------------------------------------------------
; fx	08	play sound effect
;
; >> id vv vp sh
; << -- mm -- --
; 
; s = sample index
; h = pitch ( 8 = 32000hz, h = pitch height >> 9 )
; v = volume (15 = max)
; p = panning (8 = center)
;--------------------------------------------------------
; test	09	test function
;
; >> id vv -- --
; << -- mm -- --
;--------------------------------------------------------
; ssize	0a	set sound region size
;
; >> id vv -- ss
; << -- mm -- --
;
; ss = size of sound region (ss*256 bytes)
;--------------------------------------------------------
; stream	update digital stream
;
; previously written port data must be buffered.
;
; >> 8m -- -- --	send update flag (8m = previous data or 80h)
; [wait for spc, this is a high-priority signal]
; << 80 -- -- --	receive ready signal
;
; >> nn mm vp hh	nn = number of blocks (9 bytes) to transfer (1..28)
; << nn -- -- --
;
; if mm <> 0 then
;   [new sample, reset sound]
;   v = volume
;   p = panning
;   hh = pitch height h byte (6 bits)
;
; length should be significantly larger than required on
; initial transfer (mm<>0)
;
; [xx is a counter starting with 'nn' written to port0 earlier]
; [add 1 before first message]
;
; transfer 1 chunk:
;  loop 3 times:
;   >> xx d2 d0 d1
;   << xx -- -- --
;   >> xx d5 d3 d4
;   << xx -- -- --
;   >> xx d8 d6 d7
;   << xx -- -- --
; loop nn times
;
;(exit):
; [spc will resume operation after a short period]
; [port status must be restored before the spc resumes (approx. 45us)]
; >> pp pp pp pp	restore port status
;********************************************************

;*****************************************************************************************
; registers
;*****************************************************************************************
spc_test	=0f0h ; undocumented
spc_control	=0f1h ; control register
spc_dsp		=0f2h
spc_dspa	=0f2h
spc_dspd	=0f3h
spc_port0	=0f4h ; i/o port0
spc_port1	=0f5h ; i/o port1
spc_port2	=0f6h ; i/o port2
spc_port3	=0f7h ; i/o port3
spc_flags	=0f8h ; custom flags
spc_timer0	=0fah ; timer0 setting
spc_timer1	=0fbh ; timer1 setting
spc_timer2	=0fch ; timer2 setting
spc_counter0	=0fdh ; timer0 counter
spc_counter1	=0feh ; timer1 counter
spc_counter2	=0ffh ; timer2 counter

debug_p0 = spc_port0
debug_p2 = spc_port2

;*****************************************************************************************
; dsp registers
;*****************************************************************************************
dspv_vol	=00h
dspv_volr	=01h
dspv_pl		=02h
dspv_ph		=03h
dspv_srcn	=04h
dspv_adsr1	=05h
dspv_adsr2	=06h
dspv_gain	=07h
dspv_envx	=08h
dspv_outx	=09h

dsp_mvol	=0ch
dsp_mvolr	=1ch
dsp_evol	=2ch
dsp_evolr	=3ch
dsp_kon		=4ch
dsp_kof		=5ch
dsp_flg		=6ch
dsp_endx	=7ch

dsp_efb		=0dh
dsp_pmon	=2dh
dsp_non		=3dh
dsp_eon		=4dh
dsp_dir		=5dh
dsp_esa		=6dh
dsp_edl		=7dh

dsp_c0		=0fh
dsp_c1		=1fh
dsp_c2		=2fh
dsp_c3		=3fh
dsp_c4		=4fh
dsp_c5		=5fh
dsp_c6		=6fh
dsp_c7		=7fh

flg_reset	=80h
flg_mute	=40h
flg_ecen	=20h

;;#define setdsp(xx,yy) mov spc_dspa, #xx\ mov spc_dspd, #yy
.macro setdsp xx,yy
	mov spc_dspa, #xx
	mov spc_dspd, #yy
.endmacro
	

;*****************************************************************************************
; module defs
;*****************************************************************************************

mod_iv		=00h	; initial volume
mod_it		=01h	; initial tempo
mod_is		=02h	; initial speed
mod_cv		=03h	; initial channel volume
mod_cp		=0bh	; initial channel panning
mod_evol	=13h	; echo volume (left)
mod_evolr	=14h	; echo volume (right)
mod_edl		=15h	; echo delay
mod_efb		=16h	; echo feedback
mod_efir	=17h	; echo fir coefs
mod_eon		=1fh	; echo enable bits
mod_sequ	=20h	; sequence
mod_ptable_l	=0e8h	; pattern table
mod_ptable_h	=128h	; 
mod_itable_l	=168h	; instrument table
mod_itable_h	=1a8h	; 
mod_stable_l	=1e8h	; sample table
mod_stable_h	=228h	;

ins_fadeout	=00h
ins_sample	=01h
ins_gvol	=02h
ins_setpan	=03h
ins_envlen	=04h
ins_envsus	=05h
ins_envloopst	=06h
ins_envloopend	=07h
ins_envdata	=08h

samp_dvol	=00h
samp_gvol	=01h
samp_pitchbase	=02h
samp_dindex	=04h
samp_setpan	=05h

;*****************************************************************************************
; zero-page memory
;*****************************************************************************************

.segment "ZEROPAGE"

xfer_address:	.res 2
m0:		.res 2
m1:		.res 2
m2:		.res 2
m3:		.res 2
m4:		.res 2
m5:		.res 2
m6:		.res 2
next_sample:	.res 1
comms_v:	.res 1 ;communication variable

evol_l:		.res 1
evol_r:		.res 1

module_vol:	.res 1 ;module volume
module_fadet:	.res 1 ;module volume fade target
module_fader:	.res 1 ;module volume fade rate
module_fadec:	.res 1 ;timer counter

mod_tick:	.res 1
mod_row:	.res 1
mod_position:	.res 1
mod_bpm:	.res 1
mod_speed:	.res 1
mod_active:	.res 1
mod_gvol:	.res 1

patt_addr:	.res 2
patt_rows:	.res 1
pattjump_enable: .res 1
pattjump_index:	.res 1
patt_update:	.res 1 ;pattern update flags

ch_start:
ch_pitch_l:	.res 8
ch_pitch_h:	.res 8
ch_volume:	.res 8 ;0..64
ch_cvolume:	.res 8 ;0..128 (it = 0..64)
ch_panning:	.res 8 ;0..64
ch_cmem:	.res 8
ch_note:	.res 8
ch_instr:	.res 8
ch_vcmd:	.res 8
ch_command:	.res 8
ch_param:	.res 8
ch_sample:	.res 8
ch_flags:	.res 8
ch_env_y_l:	.res 8
ch_env_y_h:	.res 8
ch_env_node:	.res 8
ch_env_tick:	.res 8
ch_fadeout:	.res 8
ch_end:

; channel processing variables:
t_hasdata:	.res 1
t_sampoff:	.res 1
t_volume:	.res 1
t_panning:	.res 1
t_pitch:
t_pitch_l:	.res 1
t_pitch_h:	.res 1
t_flags:	.res 1
t_env:		.res 1 ; 0..255

p_instr:	.res 2

status:		.res 1
status_p	=32
status_e	=64
status_f	=128

debug:		.res 1

cf_note		=1
cf_instr	=2
cf_vcmd		=4
cf_cmd		=8
cf_keyon	=16
cf_fade		=32
cf_surround	=64

tf_start	=80h
tf_delay	=2


;---------------------------
; sound effects
;---------------------------

sfx_mask:	.res 1
sfx_next:	.res 1

;-----------------------------------------------------------------------------------------

stream_a:		.res 1
stream_write:		.res 2
stream_rate:		.res 1
stream_voll:		.res 1
stream_volr:		.res 1
stream_gain:		.res 1
stream_initial:		.res 1
stream_size:		.res 1
stream_region:		.res 1

;*****************************************************************************************
; sample directory
;*****************************************************************************************

sampledirectory		=0200h	; 256 bytes	(64-sample directory)
effectdirectory		=0300h	; 16*4 bytes	(16 sound effects)
streamaddress		=0340h  ; 4 bytes       (streaming buffer address)
patternmemory		=0380h	; 16*8 bytes

; [extra ram]

;*****************************************************************************************
; program (load @ 400h)
;*****************************************************************************************

;--------------------------------------------------------
;.org 400h
.segment "CODE"
;--------------------------------------------------------
	
;--------------------------------------------------------
main:
;--------------------------------------------------------

	mov	x, #0
	mov	a, #0
_clrmem:
	mov	(x)+, a
	cmp	x, #0f0h
	bne	_clrmem
	
	mov	spc_port1, #0		; reset some ports
	mov	spc_port2, #0		;
	mov	spc_port3, #0		;
	mov	spc_control, #0		; reset control
	mov	spc_timer1, #255	; reset fade timer
	mov	module_vol, #255	; @reset mvol
	mov	module_fadet, #255	; 
					;----------------
	call !resetsound		;
					;----------------
	mov	spc_dspa, #dsp_mvol	; reset main volume
	mov	spc_dspd, #80		;
	mov	spc_dspa, #dsp_mvolr	;
	mov	spc_dspd, #80		;
					;----------------
	mov	spc_dspa, #dsp_dir	; set source dir
	mov	spc_dspd, #>sampledirectory
	
	call !resetmemory
	
	call !streaming_init
	mov	spc_control, #%110
	
;----------------------------------------------------------------------
	bra	patch1			; patch for it->spc conversion
					;
	call !module_stop		;
	mov	a, #0			;
	call !module_start		;
patch1:					;
;----------------------------------------------------------------------

;--------------------------------------------------------
main_loop:
;--------------------------------------------------------

	sproc2
	call !processcomms
	sproc
	call !processfade
	sproc
	call !module_update
	sproc
	call !updateports
	sproc
	call !sfx_update
	bra	main_loop
	
;--------------------------------------------------------
updateports:
;--------------------------------------------------------
	mov	spc_port2, status
	mov	spc_port3, mod_position
	ret
	
;--------------------------------------------------------
resetmemory:
;--------------------------------------------------------
	mov	xfer_address, #<module	; reset transfer address
	mov	xfer_address+1, #>module	;
	mov	next_sample, #0		; reset sample target
	ret
	
;--------------------------------------------------------
resetsound:
;--------------------------------------------------------
	setdsp dsp_kof, 0ffh ;
	setdsp dsp_flg, flg_ecen ;
	setdsp dsp_pmon, 0 ;
	setdsp dsp_evol, 0 ;
	setdsp dsp_evolr, 0 ;
	setdsp dsp_non, 00h ;
	setdsp dsp_kof, 000h ; this is weird
	
	mov	sfx_mask, #0
	ret
	
;--------------------------------------------------------
processcomms:
;--------------------------------------------------------
	cmp	comms_v, spc_port1	; test for command
	bne	_new_message		;
	ret				; <no message>
_new_message:
	mov	comms_v, spc_port1	; copy v
	mov	a, spc_port0		; jump to message
	nop				; verify data
	cmp	a, spc_port0		;
	bne	_new_message		;
	and	a, #127			; mask 7 bits
	asl	a			;
	mov	x, a			;
	jmp	[!commandtable+x]	;'
;--------------------------------------------------------

commandret:
	mov	spc_port1, comms_v
	ret

;--------------------------------------------------------
commandtable:
;--------------------------------------------------------
	.word	cmd_load		; 00h - load module
	.word	cmd_loade		; 01h - load sound
	.word	cmd_vol			; 02h - set volume
	.word	cmd_play		; 03h - play
	.word	cmd_stop		; 04h - stop
	.word	cmd_mvol		; 05h - set module volume
	.word	cmd_fade		; 06h - fade module volume
	.word	cmd_res			; 07h - reset
	.word	cmd_fx			; 08h - sound effect
	.word	cmd_test		; 09h - test
	.word	cmd_ssize		; 0ah - set stream size
	;	.word	cmd_pds			; 0ah - play streamed sound
;	.word	cmd_dds			; 0bh - disable digital stream
	
;********************************************************
cmd_load:
;********************************************************
	call !module_stop
	call !resetmemory		; reset memory system
	
	call !starttransfer
	
	mov	m1, #0
	
_wait_for_sourcen:			;
	cmp	comms_v, spc_port1	;
	beq	_wait_for_sourcen	;
	mov	comms_v, spc_port1	;
	
	cmp	spc_port0, #0		; if p0 != 0:
	beq	_end_of_sources		; load source
					;
	mov	y, m1			;
	clrc				;
	adc	m1, #4			;
	call !registersource		;
	call !starttransfer		;
					;
	bra	_wait_for_sourcen	; load next source
	
_end_of_sources:			; if p0 == 0:
	jmp	!commandret		;

;-------------------------------------------------------------------
registersource:
;-------------------------------------------------------------------
	mov	a, xfer_address
	mov	!sampledirectory+y, a
	clrc
	adc	a, spc_port2
	mov	!sampledirectory+2+y, a
	
	mov	a, xfer_address+1
	mov	!sampledirectory+1+y, a
	
	adc	a, spc_port3
	mov	!sampledirectory+3+y, a
	
	ret
	
;-------------------------------------------------------------------
starttransfer:
;-------------------------------------------------------------------
	mov	x, comms_v		; start transfer
	mov	y, #0			;
	mov	spc_port1, x		;
	
;-------------------------------------------------------------------
dotransfer:
;-------------------------------------------------------------------
	cmp	x, spc_port1		; wait for data
	beq	dotransfer		;
	mov	x, spc_port1		;
					;---------------------------
	mov	a, spc_port2		; copy data
	mov	[xfer_address]+y, a	;
	mov	a, spc_port3		;
	mov	spc_port1, x		;<- reply to snes
	inc	y			;
	mov	[xfer_address]+y, a	;
	inc	y			;
	beq	_inc_address		; catch index overflow
_cont1:	cmp	x, #0			; loop until x=0
	bne	dotransfer		;
	
	mov	m0, y
	clrc
	adc	xfer_address, m0
	adc	xfer_address+1, #0
	mov	comms_v, x
	ret

_inc_address:
	inc	xfer_address+1
	bra	_cont1
	
;********************************************************
cmd_loade:
;********************************************************
	mov	a, xfer_address
	mov	y, next_sample
	mov	!effectdirectory+y, a
	clrc
	adc	a, spc_port2
	mov	!effectdirectory+2+y, a
	
	mov	a, xfer_address+1
	mov	!effectdirectory+1+y, a
	
	adc	a, spc_port3
	mov	!effectdirectory+3+y, a
	
	clrc					;bugfix by kungfufurby: fixed sample array misalignment bug.
	adc	next_sample, #4
	call !starttransfer
	
	jmp	!commandret
	
;********************************************************
cmd_vol:
;********************************************************
	mov	a, spc_port2
	mov	spc_dspa, #dsp_mvol
	mov	spc_dspd, a
	mov	spc_dspa, #dsp_mvolr
	mov	spc_dspd, a
	call !updateechovolume
	jmp	!commandret
	
;********************************************************
cmd_play:
;********************************************************
	call !module_stop
	mov	a, spc_port3
	and	status, #<~status_p
	mov	spc_port2, status
	mov	spc_port1, comms_v
	jmp	!module_start
	
;********************************************************
cmd_stop:
;********************************************************
	call !module_stop
	jmp	!commandret
	
;********************************************************
cmd_mvol:
;********************************************************
	mov	module_vol, spc_port3
	mov	module_fadet, spc_port3
	jmp !commandret

;********************************************************
cmd_fade:
;********************************************************
	or	status, #status_f
	mov	spc_port2, status
	mov	module_fadet, spc_port3
	mov	module_fader, spc_port2
	jmp !commandret
	
;********************************************************
cmd_res:
;********************************************************
	mov	spc_dspa, #dsp_flg
	mov	spc_dspd, #%11100000
	clrp
	mov	spc_control, #%10000000 ;
	jmp	!0ffc0h
	
;********************************************************
cmd_fx:
;********************************************************
	movw	ya, spc_port2
	movw	m0, ya
	mov	spc_port1, comms_v
	jmp	!sfx_play

;********************************************************
cmd_test:
;********************************************************
	setdsp 00h, 7fh 
	setdsp 01h, 7fh 
	setdsp 02h, 00h 
	setdsp 03h, 10h 
	setdsp 04h, 09h 
	setdsp 05h, 00h 
	setdsp 06h, 00h 
	setdsp 07h, 7fh 
	setdsp 0ch, 70h 
	setdsp 1ch, 70h 
	setdsp 4ch, 01h 
	jmp !commandret
	
;********************************************************
cmd_ssize:
;********************************************************
	call !module_stop
	mov	a, spc_port3
	call !streaming_resize
	jmp !commandret

;********************************************************
cmd_dds:
;********************************************************
;	call !streaming_deactivate
;	jmp !commandret
	

;********************************************************
; setup echo...
;********************************************************
setupecho:
	setdsp dsp_flg, %00100000 ;
	setdsp dsp_evol, 0 ;
	setdsp dsp_evolr, 0 ;
	
	mov	a, !module+mod_evol
	mov	evol_l, a
	mov	a, !module+mod_evolr
	mov	evol_r, a
	
	mov	a, !module+mod_edl	; esa = stream_region - edl*8
	xcn	a			; max = stream_region -1
	lsr	a			;
	mov	m0, a			;
	mov	a, stream_region	;
	setc				;
	sbc	a, m0			;
	cmp	a, stream_region	;
	bne	_edl_not_ss		;
	dec	a			;
_edl_not_ss:				;
	mov	spc_dspa, #dsp_esa	;
	mov	spc_dspd, a		;
	
	mov	m0+1, a			; clear memory region used by echo
	mov	m0, #0			;
	mov	a, #0			;
	mov	y, #0			;
_clearmem:				;
	mov	[m0]+y, a		;
	inc	y			;
	bne	_clearmem		;
	inc	m0+1			;
	cmp	m0+1, stream_region	;
	bne	_clearmem		;
	
	setc				; copy fir coefficients
	mov	spc_dspa, #dsp_c7	;
	mov	y, #7			;
_copy_coef:				;
	mov	a, !module+mod_efir+y	;
	mov	spc_dspd, a		;
	sbc	spc_dspa, #10h		;
	dec	y			;
	bpl	_copy_coef		;
	
	mov	spc_dspa, #dsp_efb	; copy efb
	mov	a, !module+mod_efb	;
	mov	spc_dspd, a		;
	
	mov	spc_dspa, #dsp_eon	; copy eon
	mov	a, !module+mod_eon	;
	mov	spc_dspd, a		;
	
	mov	spc_dspa, #dsp_edl	; read old edl, set new edl
	mov	y, spc_dspd		;
	mov	a, !module+mod_edl		;
	mov	spc_dspd, a		;
	
	;-----------------------------------------
	; delay edl*16ms before enabling echo
	; 16384 clks * edl
	; edl<<14 clks
	;
	; run loop edl<<10 times
	;-----------------------------------------
	mov	a, y			;
	asl	a			;
	asl	a			;
	inc	a
	mov	m0+1, a			;
	mov	m0, #0			;
_delay_16clks:				;
	cmp	a, [0]+y		;
	decw	m0			;
	bne	_delay_16clks		;
	
	
	
	mov	a, !module+mod_edl
	beq	_skip_enable_echo

	call !updateechovolume
	mov	spc_dspa, #dsp_flg	; clear ecen
	mov	spc_dspd, #0
	ret
_skip_enable_echo:

	mov	evol_l, #0
	mov	evol_r, #0
	ret
	
;********************************************************
; set echo volume with master scale applied
;********************************************************
updateechovolume:
	
	mov	spc_dspa, #dsp_mvol	; set evol scaled by main volume
	mov	a, spc_dspd		;
	asl	a			;
	mov	m0, a			;
	mov	spc_dspa, #dsp_evol	;
	mov	y, evol_l		;
	mul	ya			;
	mov	a, y			;
	mov	y, evol_l		;
	bpl	_plus			;
	setc				;
	sbc	a, m0			;
_plus:	mov	spc_dspd, a		;

	mov	a, m0			; set evolr scaled by main volume
	mov	spc_dspa, #dsp_evolr	;
	mov	y, evol_r		;
	mul	ya			;
	mov	a, y			;
	mov	y, evol_r		;
	bpl	_plusr			;
	setc				;
	sbc	a, m0			;
_plusr:	mov	spc_dspd, a		;
	
	ret
	
;********************************************************
; zerofill channel data
;********************************************************
module_resetchannels:
	mov	x, #ch_start
	mov	a, #0
_zerofill_ch:
	mov	(x)+, a
	cmp	x, #ch_end
	bne	_zerofill_ch
	ret
	
module_stop:
	call !resetsound
	mov	spc_control, #%110
	mov	mod_active, #0
	ret
	
;********************************************************
; play module...
;
; a = initial position
;********************************************************
module_start:
	mov	mod_position, a
	call !resetsound
	call !module_resetchannels
	mov	mod_active, #1
	mov	a, !module+mod_is
	mov	mod_speed, a
	mov	a, !module+mod_it
	call !module_changetempo
	mov	a, !module+mod_iv
	mov	mod_gvol, a

	mov	x, #7				;
_copy_cvolume:					; copy volume levels
	mov	a, !module+mod_cv+x		;
	mov	ch_cvolume+x, a			;
	dec	x				;
	bpl	_copy_cvolume			;
	
	mov	x, #7
_copy_cpan:
	mov	a, !module+mod_cp+x
	cmp	a, #65
	bcs	_cpan_surround
	mov	ch_panning+x, a
	bra	_cpan_normal
_cpan_surround:
	mov	a, #32
	mov	ch_panning+x, a
	mov	a, #cf_surround
	mov	ch_flags+x, a
_cpan_normal:
	dec	x
	bpl	_copy_cpan
	
	call !setupecho
	
	mov	a, mod_position
	call !module_changeposition
	
	; start timer
	mov	spc_control, #%111
	
	or	status, #status_p
	mov	spc_port2, status
	
	setdsp dsp_kof, 0 ;	// ??????
	ret

;********************************************************
; set sequence position
;
; a=position
;********************************************************
module_changeposition:
	
	mov	y, a
_skip_pattern:
	mov	a, !module+mod_sequ+y
	cmp	a, #254			; skip +++
	bne	_not_plusplusplus	;
	inc	y			;
	bra	_skip_pattern		;
_not_plusplusplus:
	cmp	a, #255			; restart on ---
	bne	_not_end		;
	mov	y, #0			;
	bra	_skip_pattern		;
_not_end:
	mov	mod_position, y
	mov	y, a
	mov	a, !module+mod_ptable_l+y
	mov	patt_addr, a
	mov	a, !module+mod_ptable_h+y
	mov	patt_addr+1, a
	mov	y, #0
	mov	a, [patt_addr]+y
	mov	patt_rows, a
	
	incw	patt_addr
	
	mov	pattjump_enable, #0
	mov	mod_tick, #0
	mov	mod_row, #0
	ret
	
;********************************************************
; a = new bpm value
;********************************************************
module_changetempo:
	push	x
	mov	mod_bpm, a
	mov	spc_control, #%110
	
	mov	x, a
	mov	y, #50h
	mov	a, #00h
	div	ya, x
	mov	spc_timer0, a
	pop	x
	ret
	
;********************************************************
; process module fading
;********************************************************
processfade:
	mov	a, spc_counter1
	beq	_skipfade
	or	status, #status_f
	mov	a, module_vol
	cmp	a, module_fadet
	beq	_nofade
	bcc	_fadein
;--------------------------------------------
_fadeout:
;--------------------------------------------
	sbc	a, module_fader
	bcs	_fade_satl
	mov	module_vol, module_fadet
	ret
_fade_satl:
	cmp	a, module_fadet
	bcs	_fadeset
	mov	module_vol, module_fadet
	ret
;--------------------------------------------
_fadein:
;--------------------------------------------
	adc	a, module_fader
	bcc	_fade_sath
	mov	module_vol, module_fadet
	ret
_fade_sath:
	cmp	a, module_fadet
	bcc	_fadeset
	mov	module_vol, module_fadet
	ret
_fadeset:
	mov	module_vol, a
	ret
_nofade:
	and	status, #<~status_f
_skipfade:
	ret

;********************************************************
; update module playback
;
;********************************************************
module_update:
	mov	a, mod_active
	beq	_no_tick
	mov	a, spc_counter0		; check for a tick
	beq	_no_tick		;

	call !module_ontick		;
_no_tick:				;
	ret				;

;********************************************************
; module tick!!!
;********************************************************
module_ontick:
	cmp	mod_tick, #0
	bne	_skip_read_pattern
	call !module_readpattern
_skip_read_pattern:

	call !module_updatechannels

	inc	mod_tick		; increment tick until >= speed
	cmp	mod_tick, mod_speed	;
	bcc	_exit_tick		;
	mov	mod_tick, #0		;
	
	cmp	pattjump_enable, #0	; catch pattern jump...
	beq	_no_pattjump		;
	mov	a, pattjump_index	;
	jmp	!module_changeposition	;
_no_pattjump:				;
	
	inc	mod_row			; increment row until > pattern_rows
	beq	_adv_pos
	cmp	mod_row, patt_rows	;
	beq	_exit_tick
	bcc	_exit_tick		;
_adv_pos:
	
	mov	a, mod_position		; advance position
	inc	a			;
	jmp	!module_changeposition	;
_exit_tick:
	ret

;********************************************************
; read pattern data
;********************************************************
module_readpattern:
	
	mov	y, #1			; skip hints
	mov	a, [patt_addr]+y	; copy update flags
	inc	y			;
	mov	patt_update, a		;
	mov	m1, a			;
	mov	x, #0
	
	lsr	m1			; test first bit
	bcc	_no_channel_data	;
_read_pattern_data:
	sproc
	mov	a, [patt_addr]+y	; read maskvar
	inc	y			;
	mov	m0, a			;
	
	bbc	m0.4, _skip_read_note	; test/read new note
	mov	a, [patt_addr]+y	;
	inc	y			;
	mov	ch_note+x, a		;
_skip_read_note:			;

	bbc	m0.5, _skip_read_instr	; test/read new instrument
	mov	a, [patt_addr]+y	;
	inc	y			;
	mov	ch_instr+x, a		;
_skip_read_instr:			;

	bbc	m0.6, _skip_read_vcmd	; test/read new vcmd
	mov	a, [patt_addr]+y	;
	inc	y			;
	mov	ch_vcmd+x, a		;
_skip_read_vcmd:			;

	bbc	m0.7, _skip_read_cmd	; test/read new cmd+param
	mov	a, [patt_addr]+y	;
	inc	y			;
	mov	ch_command+x, a		;
	mov	a, [patt_addr]+y	;
	inc	y			;
	mov	ch_param+x, a		;
_skip_read_cmd:				;

	and	m0, #0fh		; set flags (lower nibble)
	mov	a, ch_flags+x		;
	and	a, #0f0h		;
	or	a, m0			;
	mov	ch_flags+x, a		;
	
_no_channel_data:			;
_rp_nextchannel:
	inc	x			; increment index
	lsr	m1			; shift out next bit
	bcs	_read_pattern_data	; process if set
	bne	_no_channel_data	; loop if bits remain (upto 8 iterations)
	;-------------------------------

	mov	m0, y			; add offset to pattern address
	clrc				;
	adc	patt_addr, m0		;
	adc	patt_addr+1, #0		;
	
	ret
	
bits:
	.byte 1, 2, 4, 8, 16, 32, 64, 128
	
;********************************************************
; update module channels...
;********************************************************
module_updatechannels:
	mov	x, #0
	mov	a, patt_update
	
_muc_loop:
	lsr	a
	push	a
	mov	a, #0
	rol	a
	mov	t_hasdata, a
	
	call !module_updatechannel
	
	pop	a
	
	inc	x
	cmp	x, #8
	bne	_muc_loop
	
	ret
	
;********************************************************
; update module channel
;********************************************************
module_updatechannel:
	sproc
	
	;--------------------------------------
	; get data pointers
	;--------------------------------------
	mov	y, ch_instr+x
	dec	y
	mov	a, !module+mod_itable_l+y
	mov	p_instr, a
	mov	a, !module+mod_itable_h+y
	mov	p_instr+1, a
	

	mov	t_flags, #0
	cmp	t_hasdata, #0
	beq	_muc_nopatterndata
	
	call !channel_processdata
	bra	_muc_pa
_muc_nopatterndata:
	call !channel_copytemps
_muc_pa:
	
	call !channel_processaudio
	ret

;********************************************************	
channel_processdata:
;********************************************************

	cmp	mod_tick, #0		; skip tick0 processing on other ticks
	bne	_cpd_non0		;
	
	mov	a, ch_flags+x
	mov	m6, a
	
	bbc	m6.0, _cpd_no_note	; test for note
	mov	a, ch_note+x		;
	cmp	a, #254			; test notecut/noteoff
	beq	_cpd_notecut		;
	bcs	_cpd_noteoff		;
	
_cpd_note:				; dont start note on glissando
	bbc	m6.3, _cpdn_test_for_glis	;
	mov	a, ch_command+x		;
	cmp	a, #7			;
	beq	_cpd_note_next		;
_cpdn_test_for_glis:			;
					;
	call !channel_startnewnote	;
	bra	_cpd_note_next		;
	
_cpd_notecut:				;notecut:
	mov	a, #0			; cut volume
	mov	ch_volume+x, a		;
	and	m6, #<~cf_note		; clear note flag
	bra	_cpd_note_next		;
	
_cpd_noteoff:				;noteoff:
	and	m6, #<~(cf_note|cf_keyon); clear note and keyon flags
	
_cpd_note_next:
	
	bbc	m6.1, _cpdn_no_instr	; apply instrument setpan
	mov	y, #ins_setpan		;
	mov	a, [p_instr]+y		;
	bmi	_cpdi_nsetpan		;
	mov	ch_panning+x, a		;
_cpdi_nsetpan:				;
	
	mov	y, ch_sample+x		; apply sample setpan
;	beq	_cpdi_nosample		;
	mov	a, !module+mod_stable_l+y	;
	mov	m0, a			;
	mov	a, !module+mod_stable_h+y	;
	mov	m0+1, a			;
	mov	y, #samp_dvol		; copy default volume
	mov	a, [m0]+y		;
	mov	ch_volume+x, a		;
	mov	y, #samp_setpan		;
	mov	a, [m0]+y		;
	bmi	_cpdi_nsetpan_s		;
	mov	ch_panning+x, a		;
_cpdi_nsetpan_s:
_cpdi_nosample:
_cpdn_no_instr:

	and	m6, #<~cf_note
	
_cpd_no_note:				;

	mov	a, m6			; save flag mods
	mov	ch_flags+x, a		;
	
	and	a, #(cf_note|cf_instr)	; test for note or instrument
	beq	_no_note_or_instr	;
	call !channel_resetvolume	; and reset volume things
_no_note_or_instr:			;

_cpd_non0:				; nonzero ticks: just update audio

	sproc
	
	mov	a, ch_flags+x		; test and process volume command
	and	a, #cf_vcmd		;
	beq	_skip_vcmd		;
	call !channel_processvolumecommand
_skip_vcmd:
	sproc
	call !channel_copytemps	; copy t values
	
	mov	a, ch_flags+x		; test and process command
	and	a, #cf_cmd		;
	beq	_skip_cmd		;
	call !channel_processcommand	;
_skip_cmd:
	
	ret

;********************************************************
channel_copytemps:
;********************************************************

	mov	a, ch_pitch_l+x		; prepare for effects processing.....
	mov	y, ch_pitch_h+x		;
	movw	t_pitch, ya		;
	mov	a, ch_volume+x		;
	mov	y, ch_panning+x		;
	movw	t_volume, ya		;
	mov	t_sampoff, #0		;
	
	
	ret

;********************************************************
channel_startnewnote:
;********************************************************
	
	mov	a, ch_note+x		; pitch = note * 64
	mov	y, #64			;
	mul	ya			;
	mov	ch_pitch_l+x, a		;
	mov	ch_pitch_h+x, y		;
	
	mov	a, ch_instr+x		; test for instrument and copy sample!
	beq	_csnn_no_instr		;
	mov	y, #ins_sample		;
	mov	a, [p_instr]+y		;
	mov	ch_sample+x, a		;
_csnn_no_instr:

	or	t_flags, #tf_start	; set start flag
	ret
	
;********************************************************
channel_resetvolume:
;********************************************************
	mov	a, #255			; reset fadeout
	mov	ch_fadeout+x, a		;----------------
	mov	a, #0			; reset envelope
	mov	ch_env_node+x, a	;
	mov	ch_env_tick+x, a	;----------------
	mov	ch_cmem+x, a		; reset cmem
					;----------------
	mov	a, ch_flags+x		; set keyon
	or	a, #cf_keyon		; clear fade
	and	a, #<~cf_fade		;
	mov	ch_flags+x, a		;----------------
	ret
	
;********************************************************
channel_processaudio:
;********************************************************

	sproc
	mov	y, ch_sample+x			; m5 = sample address
;	beq	_cpa_nsample			;
	mov	a, !module+mod_stable_l+y	;
	mov	m5, a				;
	mov	a, !module+mod_stable_h+y	;
	mov	m5+1, a				;
_cpa_nsample:					;
	
	call !channel_processenvelope
	
	mov	a, ch_flags+x			; process fade
	and	a, #cf_fade			;
	beq	_skip_fade			;
	mov	a, ch_fadeout+x			;
	setc					;
	mov	y, #ins_fadeout			;
	sbc	a, [p_instr]+y			;
	bcs	_subfade_noverflow		;	
	mov	a, #0				;
_subfade_noverflow:				;
	mov	ch_fadeout+x, a			;
_skip_fade:					;

	mov	a, !bits+x
	and	a, sfx_mask
	bne	_sfx_override

	mov	a, t_flags			; exit if 'note delay' is set
	and	a, #tf_delay			;
	beq	_cpa_ndelay			;
_sfx_override:
	ret					;
_cpa_ndelay:					;

	;----------------------------------------
	; compute volume:
	; v*cv*sv*gv*vev*fade
	; m0 = result (0..255)
	;----------------------------------------
	
	mov	y, #ins_gvol
	mov	a, [p_instr]+y
	push	a
	mov	y, #samp_gvol
	mov	a, [m5]+y
	push	a
	
	mov	a, t_volume			; y = 8-bit volume
	asl	a				;
	asl	a				;		
	bcc	_cpa_clamp_vol			;	
	mov	a, #255				;
_cpa_clamp_vol:					;
	mov	y, a				;
	
	mov	a, ch_cvolume+x			; *= cv
	asl	a				;
	asl	a
	bcs	_calcvol_skip_cv		;
	mul	ya				;
_calcvol_skip_cv:				;

	pop	a				; *= sv
	asl	a				;
	asl	a
	bcs	_calcvol_skip_sv		;
	mul	ya				;
_calcvol_skip_sv:				;

	pop	a				;
	asl	a				;
	bcs	_calcvol_skip_iv		;
	mul	ya				;
_calcvol_skip_iv:
	
	mov	a, mod_gvol			; *= gv
	asl	a				;
	bcs	_calcvol_skip_gvol		;
	mul	ya				;
_calcvol_skip_gvol:				;

	mov	a, t_env			; *= vev
	mul	ya				;
	
	mov	a, ch_fadeout+x			; *= fade
	mul	ya				;
	
	mov	a, module_vol
	mul	ya
	
	mov	a, y				; store 7bit result
	lsr	a				; 
	mov	m2, a
	
	cmp	t_flags, #80h
	bcs	_dont_hack_gain
	cmp	a, #0
	bne	_gain_not_zero			; map value 0 to fast linear decrease
	mov	a, #%10011100			; (8ms)
_gain_not_zero:					;
	cmp	a, #127				; map value 127 to fast linear increase
	bne	_gain_not_max			; (8ms)
	mov	a, #%11011100			;
_gain_not_max:					;
	mov	m2, a				;
_dont_hack_gain:
	
	;---------------------------------------
	; compute panning
	;---------------------------------------
	mov	a, t_panning			; a = panning 0..127	
	asl	a				;	
	bpl	_clamppan			;
	dec	a				;
_clamppan:					;	
	mov	m1+1, a				; store panning (volume) levels
	eor	a, #127				;
	mov	m1, a				;
	
	mov	a, ch_flags+x			; apply surround (r = -r)
	and	a, #cf_surround			;
	beq	_cpa_nsurround			;
	eor	m1+1, #255			;
	inc	m1+1				;
_cpa_nsurround:					;
	
	;---------------------------------------
	; compute pitch
	;---------------------------------------
	cmp	x, #1

	mov	y, #samp_pitchbase		; m3 = t_pitch pitchbase
	mov	a, [m5]+y			;
	clrc					;
	adc	a, t_pitch_l			;
	mov	m3, a				;
	inc	y				;
	mov	a, [m5]+y			;
	adc	a, t_pitch_h			;
	mov	m3+1, a				;
	
	mov	y, a				; m0 = octave
	mov	a, !lut_div3+y			;
	mov	m0, a				;
	
	asl	a				; m3 -= (oct*3) << 8
	adc	a, m0				;
	mov	m0+1, a				;
	mov	a, m3+1				;
	setc					;
	sbc	a, m0+1				;
	
	
	asl	m3				; m3 = m3*2 + lut_ftab base
	rol	a				;
	adc	m3, #<lut_ftab		;
	adc	a, #>lut_ftab			; 
	mov	m3+1, a				;
	
	mov	y, #0				; read ftab[f]
	mov	a, [m3]+y			;
	mov	m4, a				;
	inc	y				;
	mov	a, [m3]+y			;
	push	a				;
	
	mov	a, #8				; y = 8-oct
	setc					;
	sbc	a, m0				;
	mov	y, a				;
	
	pop	a				; a,m4 = ftab value
	beq	_no_pitch_shift			; skip shift if 0
	
	lsr	a				; shift by (8-oct)
	ror	m4				;
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;
	ror	m4				;
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;
	ror	m4				;
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;
	ror	m4				;
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;	
	ror	m4				;	
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;
	ror	m4				;
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;
	ror	m4				;
	dec	y				;
	beq	_no_pitch_shift			;
	lsr	a				;
	ror	m4				;
	
_no_pitch_shift:
	
	mov	m4+1, a
	
	;----------------------------------------
	; m1 = vol/volr
	; m2 = gain
	; m4 = pitch
	;----------------------------------------
	mov	a, x				; dspa = voices[x]
	xcn	a				;
	mov	spc_dspa, a			;
						;------------------------------
	mov	a, t_flags			; test for keyon
	and	a, #tf_start			;
	beq	_cpa_nstart			;------------------------------
						;keyon:
	mov	y, #samp_dindex			; set srcn
	mov	a, [m5]+y			;
	or	spc_dspa, #dspv_srcn		;
	mov	spc_dspd, a			;------------------------------
	;----------------------------------------
	; **todo: sample offset
	;----------------------------------------
	mov	spc_dspa, #dsp_kon		; set kon bit
	mov	a, !bits+x			;
	mov	spc_dspd, a			;------------------------------
	mov	a, x				; restore dspa = voices[x]
	xcn	a				;
	mov	spc_dspa, a			;
;------------------------------------------------
_cpa_nstart:
;------------------------------------------------
	
	
	mov	spc_dspd, m1			; set volume
	inc	spc_dspa			;
	mov	spc_dspd, m1+1			;
	inc	spc_dspa			;------------------------------
	mov	spc_dspd, m4			; set pitch
	inc	spc_dspa			;
	mov	spc_dspd, m4+1			;
	inc	spc_dspa			;
	inc	spc_dspa			;------------------------------
	mov	spc_dspd, #00h			; disable adsr
	or	spc_dspa, #07h			; set gain
	mov	spc_dspd, m2			;------------------------------

	;----------------------------------------
	; **todo: restore sample offset
	;----------------------------------------
	
	sproc
	ret
	
	
;********************************************************
channel_processenvelope:
;********************************************************
	mov	y, #ins_envlen			; test for envelope
	mov	a, [p_instr]+y			;
	mov	m0, a

	bne	_envelope_valid			;if no envelope:
	mov	t_env, #255			; set to max
	
	mov	a, ch_flags+x			; start fade on keyoff
	and	a, #cf_keyon			;
	beq	_env_quit			;
	jmp	!_env_setfade			;
_env_quit:
	ret					;
_envelope_valid:				;
	
	mov	a, ch_env_node+x		; read envelope node data
	
	clrc					; m1/m2
	adc	a, #ins_envdata			;
	mov	y, a				;
	mov	a, [p_instr]+y			;
	mov	m1, a				;
	inc	y				;
	mov	a, [p_instr]+y			;
	mov	m1+1, a				;
	inc	y				;
	mov	a, [p_instr]+y			;
	mov	m2, a				;
	inc	y				;
	mov	a, [p_instr]+y			;
	mov	m2+1, a				;
	
	sproc
	mov	a, ch_env_tick+x		; test zero/nonzero tick
	bne	_env_nonzero_tick		;
						;zerotick:
	mov	a, m1				; copy y level
	mov	ch_env_y_h+x, a			;
	mov	a, #0				;
	mov	ch_env_y_l+x, a			;
	bra	_env_zerotick			;
	
_env_nonzero_tick:				;nonzero:
	mov	a, ch_env_y_l+x
	clrc
	adc	a, m2
	mov	ch_env_y_l+x, a
	mov	a, ch_env_y_h+x
	adc	a, m2+1
	
	bpl	_catch_negative			; clamp result 0.0->64.0
	mov	a, #0				;
	mov	ch_env_y_h+x, a			;
	mov	ch_env_y_l+x, a			;
	bra	_env_zerotick			;
_catch_negative:				;
	cmp	a, #64				;
	bcc	_catch_plus			;
	mov	a, #64				;
	mov	ch_env_y_h+x, a			;
	mov	a, #0				;
	mov	ch_env_y_l+x, a			;
	bra	_env_zerotick			;
_catch_plus:					;
						;
	mov	ch_env_y_h+x, a			;
	
_env_zerotick:

	mov	a, ch_env_y_l+x			; t_env = env << 2
	mov	m1, a				;
	mov	a, ch_env_y_h+x			;
	asl	m1				;
	rol	a				;
	asl	m1				;
	rol	a				;
	
	bcc	_env_shift_clamp		; clamp to 255
	mov	a, #255				;
_env_shift_clamp:				;
	mov	t_env, a			;
	
	mov	a, ch_flags+x			; dont advance if "keyon" and node=sustain
	and	a, #cf_keyon			;
	beq	_env_nsustain			;
	mov	y, #ins_envsus			;
	mov	a, [p_instr]+y			;
	cmp	a, ch_env_node+x		;
	bne	_env_nsustain			;
	ret					;
_env_nsustain:					;
	
	inc	ch_env_tick+x			; increment tick
	mov	a, ch_env_tick+x		;
	cmp	a, m1+1				; exit if < duration
	bcc	_env_exit			;
	
	mov	a, #0				; reset tick
	mov	ch_env_tick+x, a		;
	
	mov	y, #ins_envloopend		; turn on fade if keyoff and loop
	mov	a, [p_instr]+y			;
	cmp	a, #255				;
	beq	_env_no_loop			;
	mov	a, ch_flags+x			;	
	and	a, #cf_keyon			;	
	bne	_env_no_fade			;	
	mov	a, ch_flags+x			;
	or	a, #cf_fade			;
	mov	ch_flags+x, a			;
_env_no_fade:
	
	mov	a, ch_env_node+x		; test for loop point
;	mov	y, #ins_envloopend		;
	cmp	a, [p_instr]+y			;
	bne	_env_loop_test			;
	mov	y, #ins_envloopst
	mov	a, [p_instr]+y
	mov	ch_env_node+x, a
	ret
_env_loop_test:					;
_env_no_loop:
	
	mov	a, ch_env_node+x
	setc					; suspicious...
	sbc	m0, #4
	cmp	a, m0				; test for envelope end
	beq	_env_setfade			;
	clrc					; increment node
	adc	a, #4				;
	mov	ch_env_node+x, a		;
	
	ret
	
_env_setfade:
	mov	a, ch_flags+x
	or	a, #cf_fade
	mov	ch_flags+x, a
_env_exit:					;
	ret

;********************************************************
channel_processvolumecommand:
;********************************************************
	mov	a, ch_volume+x
	mov	y, ch_vcmd+x
	mov	m0, y
	call !do_vcmd
	mov	ch_volume+x, a
	ret
	
do_vcmd:
	cmp	y, #65
	bcc	vcmd_setvol
	cmp	y, #75
	bcc	vcmd_finevolup
	cmp	y, #85
	bcc	vcmd_finevoldown
	cmp	y, #95
	bcc	vcmd_volup
	cmp	y, #105
	bcc	vcmd_voldown
	cmp	y, #193
	bcs	vcmd_invalid
	cmp	y, #128
	bcs	vcmd_pan
vcmd_invalid:
	ret
	
;--------------------------------------------------------
; 00-64 set volume
;--------------------------------------------------------
vcmd_setvol:
	cmp	mod_tick, #0		; a = volume
	bne	exit_vcmd		;
	mov	a, y			;
exit_vcmd:				;
	ret				;
	
;--------------------------------------------------------
; 65-74 fine vol up
;--------------------------------------------------------
vcmd_finevolup:
	sbc	m0, #65			; m0 = rate (-1)
	
	cmp	mod_tick, #0
	bne	exit_vcmd
	
_vcmd_add_sat64:
	adc	a, m0			; a += rate (+1)
	cmp	a, #65			; saturate to 64
	bcc	exit_vcmd		;
	mov	a, #64			;
	ret				;
	
;--------------------------------------------------------
; 75-84 fine vol down
;--------------------------------------------------------
vcmd_finevoldown:
	sbc	m0, #75-1		; m0 = rate [carry is cleared]

	cmp	mod_tick, #0
	bne	exit_vcmd

_vcmd_sub_sat0:	
	sbc	a, m0			; a -= rate
	bcs	exit_vcmd		; saturate lower bound to 0
	mov	a, #0			;
	ret				;
	
;--------------------------------------------------------
; 85-94 vol up
;--------------------------------------------------------
vcmd_volup:
	sbc	m0, #85			; m0 = rate (-1)
	cmp	mod_tick, #0
	beq	exit_vcmd
	bra	_vcmd_add_sat64
	
;--------------------------------------------------------
; 95-104 vol down
;--------------------------------------------------------
vcmd_voldown:
	sbc	m0, #95-1
	cmp	mod_tick, #0
	beq	exit_vcmd
	bra	_vcmd_sub_sat0
	
;--------------------------------------------------------
; 128-192 set pan
;--------------------------------------------------------
vcmd_pan:
	cmp	mod_tick, #0		; set panning
	bne	exit_vcmd		;
	push	a			;
	mov	a, y			;
	sbc	a, #128			;
	mov	ch_panning+x, a		;
	
	mov   a, ch_flags+x      ; bugfix by kungfufurby 12/20/15
  and   a, #<~cf_surround   ; surround should be disabled
  mov   ch_flags+x, a      ; when panning is set via volume
	
	pop	a			;
	ret				;

command_memory_map:	
	.byte 00h, 00h, 00h, 10h, 20h, 20h, 30h, 70h, 00h
	;       a    b    c    d    e    f    g    h    i
	.byte 40h, 10h, 10h, 00h, 10h, 50h, 10h, 80h, 70h
	;       j    k    l    m    n    o    p    q    r
	.byte 60h, 00h, 70h, 00h, 10h, 00h, 70h, 00h
	;       s    t    u    v    w    x    y    z
	
;********************************************************
channel_processcommandmemory:
;********************************************************
	
	mov	y, ch_command+x
	
	mov	a, !command_memory_map-1+y
	beq	_cpc_quit		; 0 = no memory!
	mov	m0, x
	clrc
	adc	a, m0
	mov	y, a
	
	
	cmp	y, #70h			; <7 : single param
	bcc	_cpcm_single		;
;--------------------------------------------------------
_cpcm_double:				; >=7: double param
;--------------------------------------------------------

	mov	a, !patternmemory-10h+y
	mov	m0, a
	mov	a, ch_param+x
	cmp	a, #10h
	bcc	_cpcmd_h_clr
	push	a
	and	m0, #0fh
	or	a, m0
	mov	m0, a
	pop	a
_cpcmd_h_clr:
	and	a, #0fh
	beq	_cpcmd_l_clr
	and	m0, #0f0h
	or	a, m0
	mov	m0, a
_cpcmd_l_clr:
	mov	a, m0
	mov	ch_param+x, a
	mov	!patternmemory-10h+y, a
	ret
;--------------------------------------------------------
_cpcm_single:
;--------------------------------------------------------

	mov	a, ch_param+x
	beq	_cpcms_clear
	mov	!patternmemory-10h+y, a
	ret
_cpcms_clear:
	mov	a, !patternmemory-10h+y
	mov	ch_param+x, a	
_cpc_quit:
	ret

;********************************************************
channel_processcommand:
;********************************************************
	
	mov	a, ch_command+x		; exit if cmd = 0 
	beq	_cpc_quit		;
	
	cmp	mod_tick, #0		; process memory on t0
	bne	_cpc_nott0		;
	call !channel_processcommandmemory
_cpc_nott0:

	mov	y, ch_command+x		; setup jump address
	mov	a, !cmd_jumptable_l-1+y	;
	mov	!cpc_jump+1, a		;
	mov	a, !cmd_jumptable_h-1+y	;
	mov	!cpc_jump+2, a		;
	
	mov	a, ch_param+x		; preload data
	mov	y, mod_tick		;
	
	;-------------------------------
	; a = param
	; y = tick
	; z = tick=0
	;-------------------------------
	
cpc_jump:
	jmp	!$0011
	
; note: tasm has some kind of bug that removes the 16th character
; in macro args (...?)
;-----------------------------------------------------------------------
cmd_jumptable_l:
;-----------------------------------------------------------------------
	.byte	<command_setspeed			; axx
	.byte	<command_setposit		; bxx
	.byte	<command_patbreak		; cxx
	.byte	<command_vsld		; dxy
	.byte	<command_pitslddo		; exy
	.byte	<command_pitsldup		; fxy
	.byte	<command_glissand		; gxx
	.byte	<command_vibrato			; hxy
	.byte	<command_tremor			; ixy
	.byte	<command_arpeggio			; jxy
	.byte	<command_vsldvibr	; kxy
	.byte	<command_vsldglis	; lxy
	.byte	<command_setchvol	; mxx
	.byte	<command_chavolsl	; nxy
	.byte	<command_sampofs	; oxx
	.byte	<command_pansld		; pxy
	.byte	<command_rtrignot		; qxy
	.byte	<command_tremolo			; rxy
	.byte	<command_extended			; sxy
	.byte	<command_tempo			; txy
	.byte	<command_finevibr		; uxy
	.byte	<command_setglbvo		; vxx
	.byte	<command_glbvolsl	; wxy
	.byte	<command_setpan		; xxx
	.byte	<command_panbrell		; yxy
	.byte	<command_midimac		; zxy
;-----------------------------------------------------------------------
cmd_jumptable_h:
;-----------------------------------------------------------------------
	.byte	>command_setspeed			; axx
	.byte	>command_setposit		; bxx
	.byte	>command_patbreak		; cxx
	.byte	>command_vsld		; dxy
	.byte	>command_pitslddo		; exy
	.byte	>command_pitsldup		; fxy
	.byte	>command_glissand		; gxx
	.byte	>command_vibrato		; hxy
	.byte	>command_tremor			; ixy
	.byte	>command_arpeggio			; jxy
	.byte	>command_vsldvibr	; kxy
	.byte	>command_vsldglis	; lxy
	.byte	>command_setchvol	; mxx
	.byte	>command_chavolsl	; nxy
	.byte	>command_sampofs		; oxx
	.byte	>command_pansld		; pxy
	.byte	>command_rtrignot		; qxy
	.byte	>command_tremolo			; rxy
	.byte	>command_extended			; sxy
	.byte	>command_tempo			; txy
	.byte	>command_finevibr		; uxy
	.byte	>command_setglbvo		; vxx
	.byte	>command_glbvolsl	; wxy
	.byte	>command_setpan		; xxx
	.byte	>command_panbrell		; yxy
	.byte	>command_midimac		; zxy

;=======================================================================
command_setspeed:
;=======================================================================
	bne	cmd_exit1			;on tick0:
	cmp	a, #0				; if param != 0
	beq	cmd_exit1			; mod_speed = param
	mov	mod_speed, a			;
cmd_exit1:					;
	ret					;
;=======================================================================
command_setposit:
;=======================================================================
	bne	cmd_exit1			;on tick0:
	mov	pattjump_index, a		; set jump index
	mov	pattjump_enable, #1		; enable pattern jump
	ret					;
;=======================================================================
command_patbreak:
;=======================================================================
	; nonzero params are not supported
	;
	bne	cmd_exit1			;on tick0:
	mov	pattjump_index, mod_position	; index = position+1
	inc	pattjump_index			; enable pattern jump(break)
	mov	pattjump_enable, #1		;
	ret
;=======================================================================
command_vsld:
;=======================================================================
	mov	m0, t_volume			; slide volume
	mov	m0+1, #64			;
	call !dovolumeslide			;
	mov	t_volume, a			;
	mov	ch_volume+x, a			;
	ret					;
;=======================================================================
command_pitslddo:
;=======================================================================
	call !pitchslide_load			; m0 = slide amount
	movw	ya, t_pitch			; pitch -= m0
	subw	ya, m0				;
	bmi	_exx_zero			; saturate lower to 0
	movw	t_pitch, ya			;
	mov	ch_pitch_l+x, a			;
	mov	ch_pitch_h+x, y			;
	ret					;
;---------------------------------------------------------------------
_exx_zero:
;---------------------------------------------------------------------
	mov	a, #0				; zero pitch
	mov	y, #0				;
	movw	t_pitch, ya			;
	mov	ch_pitch_l+x, a			;
	mov	ch_pitch_h+x, a			;
	ret					;
;=======================================================================
command_pitsldup:
;=======================================================================
	call !pitchslide_load			; m0 = slide amount
	movw	ya, t_pitch			;
	addw	ya, m0				;
	cmp	y, #01ah			;
	bcs	_fxx_max			; clamp upper bound to 1a00h
	movw	t_pitch, ya			;
	mov	ch_pitch_l+x, a			;
	mov	ch_pitch_h+x, y			;
	ret					;
;-----------------------------------------------------------------------
_fxx_max:
;-----------------------------------------------------------------------
	mov	y, #01ah			; max pitch
	mov	a, #0				;
	movw	t_pitch, ya			;
	mov	ch_pitch_l+x, a			;
	mov	ch_pitch_h+x, y			;
	ret					;
;=======================================================================
command_glissand:
;=======================================================================
	beq	cmd_exit1			; on tickn:
	
	mov	m0+1, #0			; m0 = xx*4 (slide amount)
	asl	a				;
	rol	m0+1				;
	asl	a				;
	rol	m0+1				;
	mov	m0, a				;
	
	mov	a, ch_note+x			; m1 = slide target
	mov	m1, #0				;
	lsr	a				;
	ror	m1				;
	lsr	a				;
	ror	m1				;
	mov	m1+1, a				;
	
	movw	ya, t_pitch			; test slide direction
	cmpw	ya, m1				;
	bcc	_gxx_slideup
;-----------------------------------------------
_gxx_slidedown:
;-----------------------------------------------
	subw	ya, m0				; subtract xx*4 from pitch
	bmi	_gxx_set			; saturate lower to target pitch
	cmpw	ya, m1				;
	bcc	_gxx_set			;
_gxx_set2:					;
	movw	t_pitch, ya			;
	mov	ch_pitch_l+x, a			;
	mov	ch_pitch_h+x, y			;
	ret					;
;-----------------------------------------------
_gxx_slideup:
;-----------------------------------------------
	addw	ya, m0				; add xx*4 to pitch
	cmpw	ya, m1				; saturate upper to target pitch
	bcs	_gxx_set			;
	bra	_gxx_set2			;
;-----------------------------------------------
_gxx_set:					; pitch = target
;-----------------------------------------------
	movw	ya, m1				;
	bra	_gxx_set2			;
	
;=======================================================================
command_vibrato:
;=======================================================================
	mov	a, #70h
	mov	m0, x
	clrc
	adc	a, m0
	mov	y, a
	mov	a, !patternmemory-10h+y
	
	mov	m0, a
	and	m0, #0fh
	
	lsr	a				; cmem += x*4
	lsr	a				;
	and	a, #%00111100			;
	clrc					;
	adc	a, ch_cmem+x			;
	mov	ch_cmem+x, a			;
	
	mov	y, a				; a = sine[cmem]
	mov	a, !it_finesinedata+y		;
	bpl	_hxx_plus
	
_hxx_neg:
	eor	a, #255
	inc	a
	mov	y, m0
	mul	ya
	mov	m0+1, y
	lsr	m0+1
	ror	a
	lsr	m0+1
	ror	a
	lsr	m0+1
	ror	a
	lsr	m0+1
	ror	a
	mov	m0, a
	movw	ya, t_pitch
	subw	ya, m0
	bmi	_hxx_zero
	movw	t_pitch, ya
	ret
_hxx_plus:
	mov	y, m0
	mul	ya
	mov	m0+1, y
	lsr	m0+1
	ror	a
	lsr	m0+1
	ror	a
	lsr	m0+1
	ror	a
	lsr	m0+1
	ror	a
	mov	y, m0+1
	addw	ya, t_pitch			; warning: might break something on highest note
	movw	t_pitch, ya
	ret
	
_hxx_zero:
	mov	t_pitch, #0
	mov	t_pitch+1, #0
	ret
	
;=======================================================================
command_tremor:					; unimplemented
;=======================================================================
	ret

;=======================================================================
command_arpeggio:
;=======================================================================
	bne	_jxx_other
	mov	a, #0
	mov	ch_cmem+x, a
	ret
_jxx_other:
	mov	a, ch_cmem+x
	inc	a
	cmp	a, #3
	bcc	_jxx_less3
	mov	a, #0
_jxx_less3:
	mov	ch_cmem+x, a
	
	cmp	a, #1
	beq	_jxx_x
	bcs	_jxx_y
	ret
	
_jxx_x:
	mov	a, ch_param+x
	
_jxx_add:
	
	and	a, #0f0h
	asl	a
	mov	m0+1, #0
	rol	m0+1
	asl	a
	rol	m0+1
	mov	m0, a
	movw	ya, t_pitch
	addw	ya, m0
	movw	t_pitch, ya
	ret
_jxx_y:
	mov	a, ch_param+x
	xcn	a
	bra	_jxx_add

;=======================================================================
command_vsldvibr:
;=======================================================================
	call !command_vibrato
	
	mov	a, ch_param+x
	mov	y, mod_tick
	mov	m0, t_volume			; slide volume
	mov	m0+1, #64			;
	call !dovolumeslide			;
	mov	t_volume, a			;
	mov	ch_volume+x, a			;
cmd_exit2:
	ret					;

;=======================================================================
command_vsldglis:			; unimplemented
;=======================================================================
	ret

;=======================================================================
command_setchvol:
;=======================================================================
	bne	cmd_exit2			; on tick0:
	cmp	a, #65				;  cvolume = param > 64 ? 64 : param
	bcc	cscv_under65			;
	mov	a, #64				;
cscv_under65:					;
	mov	ch_cvolume+x, a			;
	ret					;

;=======================================================================
command_chavolsl:
;=======================================================================
	mov	a, ch_cvolume+x			; slide channel volume
	mov	m0, a				; 
	mov	m0+1, #64			;
	mov	a, ch_param+x			;
	call !dovolumeslide			;
	mov	ch_cvolume+x, a			;
	ret					;
	
;=======================================================================
command_sampofs:
;=======================================================================
	bne	cmd_exit2			; on tick0:
	mov	t_sampoff, a			;   set sampoff data
	ret					;
	
;=======================================================================
command_pansld:
;=======================================================================
	xcn	a
	mov	m0, t_panning			; slide panning
	mov	m0+1, #64			;
	call !dovolumeslide			;
	mov	t_panning, a			;
	mov	ch_panning+x, a			;
	ret					;
	
;=======================================================================
command_rtrignot:
;=======================================================================
	
	and	a, #0fh				; m0 = y == 0 ? 1 : x
	bne	_crn_x1				;
	inc	a				;
_crn_x1:					;	
	mov	m0, a				;
	
	mov	a, ch_cmem+x			;if cmem is 0:
	bne	_crn_cmem_n0			;  cmem = m0
	mov	a, m0				;
_crn_count_ret:
	mov	ch_cmem+x, a			;
	ret					;	
_crn_cmem_n0:					;else:
	dec	a				; dec cmem until 0
	bne	_crn_count_ret			;
						;retrigger note:
	mov	a, m0				; cmem = m0
	mov	ch_cmem+x, a			;
	
	;----------------------------------------
	; affect volume
	;----------------------------------------
	mov	a, ch_param+x
	xcn	a
	and	a, #0fh
	mov	m1, a
	asl	a
	push	x
	mov	x, a
	mov	a, t_volume
	clrc
	jmp	[!rnvtable+x]
rnvtable:
	.word	rnv_0
	.word	rnv_1
	.word	rnv_2
	.word	rnv_3
	.word	rnv_4
	.word	rnv_5
	.word	rnv_6
	.word	rnv_7
	.word	rnv_8
	.word	rnv_9
	.word	rnv_a
	.word	rnv_b
	.word	rnv_c
	.word	rnv_d
	.word	rnv_e
	.word	rnv_f
	
rnv_1:	dec	a
	bra	_rnv_sat0
rnv_2:	sbc	a, #2-1
	bra	_rnv_sat0
rnv_3:	sbc	a, #4-1
	bra	_rnv_sat0
rnv_4:	sbc	a, #8-1
	bra	_rnv_sat0
rnv_5:	sbc	a, #16-1
	bra	_rnv_sat0
rnv_6:	mov	y, #170
	mul	ya
	mov	a, y
	bra	_rnv_set
rnv_7:	lsr	a
	bra	_rnv_set
rnv_8:
rnv_0:	bra	_rnv_set
rnv_9:	inc	a
	bra	_rnv_sat64
rnv_a:	adc	a, #2
	bra	_rnv_sat64
rnv_b:	adc	a, #4
	bra	_rnv_sat64
rnv_c:	adc	a, #8
	bra	_rnv_sat64
rnv_d:	adc	a, #16
	bra	_rnv_sat64
rnv_e:	mov	y, #3
	mul	ya
	lsr	a
	bra	_rnv_sat64
rnv_f:	asl	a
	bra	_rnv_sat64
	
_rnv_sat0:
	bpl	_rnv_set
	mov	a, #0
	bra	_rnv_set
_rnv_sat64:
	cmp	a, #65
	bcc	_rnv_set
	mov	a, #64
_rnv_set:
	pop	x
	mov	t_volume, a
	mov	ch_volume+x, a
	or	t_flags, #tf_start
	
	
	ret
	
;=======================================================================
command_tremolo:				; unimplemented
;=======================================================================
	ret

;=======================================================================
command_extended:
;=======================================================================
	xcn	a				; setup jump to:
	and	a, #0fh				; cmdextab[x]
	mov	y, a				;
	mov	a, !cmdextab_l+y		;
	mov	!cmdex_jmp+1, a			;
	mov	a, !cmdextab_h+y		;
	mov	!cmdex_jmp+2, a			;
	
	mov	a, ch_param+x			; a = y
	and	a, #0fh				; y = tick
	mov	y, mod_tick			; z = tick0
	
cmdex_jmp:
	jmp	!0a0bh
	
scommand_null:
	ret
	
cmdextab_l:
	.byte	<scommand_echo
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_panning
	.byte	<scommand_soundco
	.byte	<scommand_null
	.byte	<scommand_null
	.byte	<scommand_notecut
	.byte	<scommand_notedel
	.byte	<scommand_null
	.byte	<scommand_cue
cmdextab_h:
	.byte	>scommand_echo
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_panning
	.byte	>scommand_soundco
	.byte	>scommand_null
	.byte	>scommand_null
	.byte	>scommand_notecut
	.byte	>scommand_notedel
	.byte	>scommand_null
	.byte	>scommand_cue

; s01 = turn on echo
; s02 = turn off echo
; s03 = turn on echo for all
; s04 = turn off echo for all
;=======================================================================
scommand_echo:
;=======================================================================
	mov	spc_dspa, #dsp_eon
	cmp	a, #1
	beq	_sce_enable_one
	bcc	cmd_exit3
	cmp	a, #3
	bcc	_sce_disable_one
	beq	_sce_disable_all
	cmp	a, #4
	beq	_sce_enable_all
cmd_exit3:
	ret
_sce_enable_one:
	mov	a, !bits+x
	or	a, spc_dspd
	mov	spc_dspd, a
	ret
_sce_disable_one:
	mov	a, !bits+x
	eor	a, #255
	and	a, spc_dspd
	mov	spc_dspd, a
	ret
_sce_enable_all:
	mov	spc_dspd, #0ffh
	ret
_sce_disable_all:
	mov	spc_dspd, #0
	ret
;=======================================================================
scommand_panning:
;=======================================================================
	bne	cmd_exit3			; on tick0:
	mov	m0, a				; panning = (y << 2) + (y >> 2)
	asl	a				;
	asl	a				;
	lsr	m0				;
	lsr	m0				;
	adc	a, m0				;
	mov	t_panning, a			;
	mov	ch_panning+x, a			;
	ret					;
;=======================================================================
scommand_soundco:
;=======================================================================
	bne	cmd_exit3
	cmp	a, #1
	bne	cmd_exit3
	mov	a, ch_flags+x
	or	a, #cf_surround
	mov	ch_flags+x, a
	mov	a, #32
	mov	ch_panning+x, a
	mov	t_panning, a
	ret
;=======================================================================
scommand_notecut:
;=======================================================================
	cmp	a, mod_tick			; on tick y:
	bne	cmd_exit3			;
	mov	a, #0				; zero volume
	mov	t_volume, a			;
	mov	ch_volume+x, a			;
	ret					;
;=======================================================================
scommand_notedel:
;=======================================================================
	cmp	a, mod_tick
	beq	scdelay_equ
	bcs	scdelay_lower
	
	ret
scdelay_lower:
	or	t_flags, #tf_delay
	ret
scdelay_equ:
	or	t_flags, #tf_start
	ret
;=======================================================================
scommand_cue:
;=======================================================================
	bne	cmd_exit3			;on tick0:
	inc	status				; increment cue value
	and	status, #%11101111		; in status and send to
	mov	spc_port2, status		; snes
	ret					;
;=======================================================================
command_tempo:
;=======================================================================
	cmp	a, #20h
	bcc	_temposlide
	cmp	a, #80
	bcs	_txxu1
	mov	a, #80
_txxu1:	cmp	a, #200
	bcc	_txxu2
	mov	a, #200
_txxu2:	call !module_changetempo
	mov	spc_control, #%111
	ret
_temposlide:
	cmp	a, #10h
	bcc	_txx_down
	and	a, #0fh
	clrc
	adc	a, mod_bpm
	cmp	a, #200
	bcc	_txx_sath
	mov	a, #200
_txx_sath:
	call !module_changetempo
	mov	spc_control, #%111
	ret
_txx_down:
	mov	m0, a
	mov	a, mod_bpm
	setc
	sbc	a, m0
	cmp	a, #80
	bcs	_txx_sath
	mov	a, #80
	call !module_changetempo
	mov	spc_control, #%111
	ret
;=======================================================================
command_finevibr:				; unimplemented
;=======================================================================
	ret

;=======================================================================
command_setglbvo:
;=======================================================================
	bne	cmd_exit4			; set global volume on tick0
	cmp	a, #80h				;
	bcc	_vxx_nsat			; saturate to 80h
	mov	a, #80h				;
_vxx_nsat:					;
	mov	mod_gvol, a			;
cmd_exit4:					;
	ret					;
;=======================================================================
command_glbvolsl:
;=======================================================================
	mov	m0, mod_gvol			; slide global volume
	mov	m0+1, #128			; max 128
	call !dovolumeslide			;
	mov	mod_gvol, a			;
	ret					;
;=======================================================================
command_setpan:
;=======================================================================
	bne	cmd_exit4			; set panning on tick0	
	lsr	a				;
	lsr	a				;
	mov	t_panning, a			;
	mov	ch_panning+x, a			;
	mov	a, ch_flags+x
	and	a, #<~cf_surround
	mov	ch_flags+x, a
	ret					;
;=======================================================================
command_panbrell:				; unimplemented
;=======================================================================
	ret
;=======================================================================
command_midimac:				; ?
;=======================================================================
	ret

;-----------------------------------------------------------------------
; a = param
; y = tick
; m0 = value
; m0+1 = upper bound
;
; return: a = result
;-----------------------------------------------------------------------
dovolumeslide:
;-----------------------------------------------------------------------
	mov	m1, a			; test param for slide behavior
					;-------------------------------
	and	a, #0fh			; dx0 : slide up
	beq	_dvs_up			;-------------------------------
	mov	a, m1			; d0y : slide down
	and	a, #0f0h		;
	beq	_dvs_down		;-------------------------------
	mov	a, m1			; dxf : slide up fine
	and	a, #0fh			;
	cmp	a, #0fh			;
	beq	_dvs_fineup		;-------------------------------
	mov	a, m1			; dfy : slide down fine
	cmp	a, #0f0h		;
	bcs	_dvs_finedown		;
_dvs_quit:				;-------------------------------
	mov	a, m0			; (invalid)
_dvs_exit:				;
	ret				;
;-----------------------------------------------------------------------
_dvs_finedown:				; dfy
;-----------------------------------------------------------------------
	cmp	y, #0			;on tick0:
	bne	_dvs_quit		;
	mov	a, m0			; a = volume - y
	and	m1, #0fh		;
	sbc	a, m1			;
	bcs	_dvs_exit		; saturate lower bound to 0
	mov	a, #0			;
	ret				;
;-----------------------------------------------------------------------
_dvs_fineup:				; dxf
;-----------------------------------------------------------------------
	cmp	y, #0			;on tick0:
	bne	_dvs_quit		;
	mov	a, m1			; a = x + volume
	xcn	a			;
	and	a, #0fh			;
	clrc				;
	adc	a, m0			;
	cmp	a, m0+1			; saturate upper to [m0.h]
	bcc	_dvs_exit		;
	mov	a, m0+1			;
	ret				;
;-----------------------------------------------------------------------
_dvs_down:				; d0y
;-----------------------------------------------------------------------
	cmp	m1,#0fh			;on tick0 or y == 15
	beq	_dvsd_15		;
	cmp	y, #0			;
	beq	_dvs_quit		;
_dvsd_15:				;
	mov	a, m0			; a = volume - param
	setc				;
	sbc	a, m1			;
	bcs	_dvs_exit		; saturate lower to 0
	mov	a, #0			;
	ret				;
;-----------------------------------------------------------------------
_dvs_up:				;
;-----------------------------------------------------------------------
	cmp	m1, #0f0h		;on tick0 or x == 15
	beq	_dvsu_15		;
	cmp	y, #0			;
	beq	_dvs_quit		;
_dvsu_15:				;
	mov	a, m1			; a = x + volume
	xcn	a			;
	and	a, #0fh			;
	clrc				;
	adc	a, m0			;
	cmp	a, m0+1			; saturte upper to [m0.h]
	bcc	_dvs_exit		;
	mov	a, m0+1			;
	ret				;
;-----------------------------------------------------------------------

;=======================================================================
; a = param
; y = tick
; return m0:word = slide amount
;=======================================================================
pitchslide_load:
;=======================================================================
	cmp	a, #0f0h			; fx: fine slide
	bcs	_psl_fine			;
	cmp	a, #0e0h			; ex: extra fine slide
	bcs	_psl_exfine			;
;-----------------------------------------------------------------------
_psl_normal:
;-----------------------------------------------------------------------
	cmp	y, #0				; no slide on tick0
	beq	_psl_zero			;
	mov	m0+1, #0			; m0 = a*4
	asl	a				;	
	rol	m0+1				;
	asl	a				;
	rol	m0+1				;
	mov	m0, a				;
	ret					;
;-----------------------------------------------------------------------
_psl_fine:
;-----------------------------------------------------------------------
	cmp	y, #0				; no slide on not tick0
	bne	_psl_zero			;
	mov	m0+1, #0			; m0 = y*4
	and	a, #0fh				;	
	asl	a				;
	asl	a				;
	mov	m0, a				;
	ret					;
;-----------------------------------------------------------------------
_psl_exfine:
;-----------------------------------------------------------------------
	cmp	y, #0				; no slide on not tick0
	bne	_psl_zero			;
	mov	m0+1, #0			; m0 = y
	and	a, #0fh				;	
	mov	m0, a				;
	ret					;
;-----------------------------------------------------------------------
_psl_zero:
;-----------------------------------------------------------------------
	mov	m0, #0
	mov	m0+1, #0
	ret

;************************************************************************************************************************************************

lut_div3:
	.byte 0, 0, 0, 1, 1, 1, 2, 2, 2
	.byte 3, 3, 3, 4, 4, 4, 5, 5, 5
	.byte 6, 6, 6, 7, 7, 7, 8, 8, 8
	.byte 9, 9, 9,10,10
	
lut_ftab:
        .word 02174h, 0217bh, 02183h, 0218bh, 02193h, 0219ah, 021a2h, 021aah, 021b2h, 021bah, 021c1h, 021c9h, 021d1h, 021d9h, 021e1h, 021e8h
        .word 021f0h, 021f8h, 02200h, 02208h, 02210h, 02218h, 0221fh, 02227h, 0222fh, 02237h, 0223fh, 02247h, 0224fh, 02257h, 0225fh, 02267h
        .word 0226fh, 02277h, 0227fh, 02287h, 0228fh, 02297h, 0229fh, 022a7h, 022afh, 022b7h, 022bfh, 022c7h, 022cfh, 022d7h, 022dfh, 022e7h
        .word 022efh, 022f7h, 022ffh, 02307h, 0230fh, 02317h, 0231fh, 02328h, 02330h, 02338h, 02340h, 02348h, 02350h, 02358h, 02361h, 02369h
        .word 02371h, 02379h, 02381h, 0238ah, 02392h, 0239ah, 023a2h, 023aah, 023b3h, 023bbh, 023c3h, 023cbh, 023d4h, 023dch, 023e4h, 023edh
        .word 023f5h, 023fdh, 02406h, 0240eh, 02416h, 0241fh, 02427h, 0242fh, 02438h, 02440h, 02448h, 02451h, 02459h, 02462h, 0246ah, 02472h
        .word 0247bh, 02483h, 0248ch, 02494h, 0249dh, 024a5h, 024aeh, 024b6h, 024beh, 024c7h, 024cfh, 024d8h, 024e0h, 024e9h, 024f2h, 024fah
        .word 02503h, 0250bh, 02514h, 0251ch, 02525h, 0252dh, 02536h, 0253fh, 02547h, 02550h, 02559h, 02561h, 0256ah, 02572h, 0257bh, 02584h
        .word 0258ch, 02595h, 0259eh, 025a7h, 025afh, 025b8h, 025c1h, 025c9h, 025d2h, 025dbh, 025e4h, 025ech, 025f5h, 025feh, 02607h, 0260fh
        .word 02618h, 02621h, 0262ah, 02633h, 0263ch, 02644h, 0264dh, 02656h, 0265fh, 02668h, 02671h, 0267ah, 02682h, 0268bh, 02694h, 0269dh
        .word 026a6h, 026afh, 026b8h, 026c1h, 026cah, 026d3h, 026dch, 026e5h, 026eeh, 026f7h, 02700h, 02709h, 02712h, 0271bh, 02724h, 0272dh
        .word 02736h, 0273fh, 02748h, 02751h, 0275ah, 02763h, 0276dh, 02776h, 0277fh, 02788h, 02791h, 0279ah, 027a3h, 027ach, 027b6h, 027bfh
        .word 027c8h, 027d1h, 027dah, 027e4h, 027edh, 027f6h, 027ffh, 02809h, 02812h, 0281bh, 02824h, 0282eh, 02837h, 02840h, 0284ah, 02853h
        .word 0285ch, 02865h, 0286fh, 02878h, 02882h, 0288bh, 02894h, 0289eh, 028a7h, 028b0h, 028bah, 028c3h, 028cdh, 028d6h, 028e0h, 028e9h
        .word 028f2h, 028fch, 02905h, 0290fh, 02918h, 02922h, 0292bh, 02935h, 0293eh, 02948h, 02951h, 0295bh, 02965h, 0296eh, 02978h, 02981h
        .word 0298bh, 02995h, 0299eh, 029a8h, 029b1h, 029bbh, 029c5h, 029ceh, 029d8h, 029e2h, 029ebh, 029f5h, 029ffh, 02a08h, 02a12h, 02a1ch
        .word 02a26h, 02a2fh, 02a39h, 02a43h, 02a4dh, 02a56h, 02a60h, 02a6ah, 02a74h, 02a7eh, 02a87h, 02a91h, 02a9bh, 02aa5h, 02aafh, 02ab9h
        .word 02ac3h, 02acch, 02ad6h, 02ae0h, 02aeah, 02af4h, 02afeh, 02b08h, 02b12h, 02b1ch, 02b26h, 02b30h, 02b3ah, 02b44h, 02b4eh, 02b58h
        .word 02b62h, 02b6ch, 02b76h, 02b80h, 02b8ah, 02b94h, 02b9eh, 02ba8h, 02bb2h, 02bbch, 02bc6h, 02bd1h, 02bdbh, 02be5h, 02befh, 02bf9h
        .word 02c03h, 02c0dh, 02c18h, 02c22h, 02c2ch, 02c36h, 02c40h, 02c4bh, 02c55h, 02c5fh, 02c69h, 02c74h, 02c7eh, 02c88h, 02c93h, 02c9dh
        .word 02ca7h, 02cb2h, 02cbch, 02cc6h, 02cd1h, 02cdbh, 02ce5h, 02cf0h, 02cfah, 02d04h, 02d0fh, 02d19h, 02d24h, 02d2eh, 02d39h, 02d43h
        .word 02d4dh, 02d58h, 02d62h, 02d6dh, 02d77h, 02d82h, 02d8ch, 02d97h, 02da1h, 02dach, 02db7h, 02dc1h, 02dcch, 02dd6h, 02de1h, 02dech
        .word 02df6h, 02e01h, 02e0bh, 02e16h, 02e21h, 02e2bh, 02e36h, 02e41h, 02e4bh, 02e56h, 02e61h, 02e6ch, 02e76h, 02e81h, 02e8ch, 02e97h
        .word 02ea1h, 02each, 02eb7h, 02ec2h, 02ecch, 02ed7h, 02ee2h, 02eedh, 02ef8h, 02f03h, 02f0eh, 02f18h, 02f23h, 02f2eh, 02f39h, 02f44h
        .word 02f4fh, 02f5ah, 02f65h, 02f70h, 02f7bh, 02f86h, 02f91h, 02f9ch, 02fa7h, 02fb2h, 02fbdh, 02fc8h, 02fd3h, 02fdeh, 02fe9h, 02ff4h
        .word 02fffh, 0300ah, 03015h, 03020h, 0302ch, 03037h, 03042h, 0304dh, 03058h, 03063h, 0306eh, 0307ah, 03085h, 03090h, 0309bh, 030a7h
        .word 030b2h, 030bdh, 030c8h, 030d4h, 030dfh, 030eah, 030f5h, 03101h, 0310ch, 03117h, 03123h, 0312eh, 0313ah, 03145h, 03150h, 0315ch
        .word 03167h, 03173h, 0317eh, 03189h, 03195h, 031a0h, 031ach, 031b7h, 031c3h, 031ceh, 031dah, 031e5h, 031f1h, 031fch, 03208h, 03213h
        .word 0321fh, 0322bh, 03236h, 03242h, 0324dh, 03259h, 03265h, 03270h, 0327ch, 03288h, 03293h, 0329fh, 032abh, 032b7h, 032c2h, 032ceh
        .word 032dah, 032e5h, 032f1h, 032fdh, 03309h, 03315h, 03320h, 0332ch, 03338h, 03344h, 03350h, 0335ch, 03367h, 03373h, 0337fh, 0338bh
        .word 03397h, 033a3h, 033afh, 033bbh, 033c7h, 033d3h, 033dfh, 033ebh, 033f7h, 03403h, 0340fh, 0341bh, 03427h, 03433h, 0343fh, 0344bh
        .word 03457h, 03463h, 0346fh, 0347bh, 03488h, 03494h, 034a0h, 034ach, 034b8h, 034c4h, 034d1h, 034ddh, 034e9h, 034f5h, 03502h, 0350eh
        .word 0351ah, 03526h, 03533h, 0353fh, 0354bh, 03558h, 03564h, 03570h, 0357dh, 03589h, 03595h, 035a2h, 035aeh, 035bah, 035c7h, 035d3h
        .word 035e0h, 035ech, 035f9h, 03605h, 03612h, 0361eh, 0362bh, 03637h, 03644h, 03650h, 0365dh, 03669h, 03676h, 03683h, 0368fh, 0369ch
        .word 036a8h, 036b5h, 036c2h, 036ceh, 036dbh, 036e8h, 036f4h, 03701h, 0370eh, 0371bh, 03727h, 03734h, 03741h, 0374eh, 0375ah, 03767h
        .word 03774h, 03781h, 0378eh, 0379ah, 037a7h, 037b4h, 037c1h, 037ceh, 037dbh, 037e8h, 037f5h, 03802h, 0380eh, 0381bh, 03828h, 03835h
        .word 03842h, 0384fh, 0385ch, 03869h, 03876h, 03884h, 03891h, 0389eh, 038abh, 038b8h, 038c5h, 038d2h, 038dfh, 038ech, 038fah, 03907h
        .word 03914h, 03921h, 0392eh, 0393bh, 03949h, 03956h, 03963h, 03970h, 0397eh, 0398bh, 03998h, 039a6h, 039b3h, 039c0h, 039ceh, 039dbh
        .word 039e8h, 039f6h, 03a03h, 03a11h, 03a1eh, 03a2bh, 03a39h, 03a46h, 03a54h, 03a61h, 03a6fh, 03a7ch, 03a8ah, 03a97h, 03aa5h, 03ab2h
        .word 03ac0h, 03aceh, 03adbh, 03ae9h, 03af6h, 03b04h, 03b12h, 03b1fh, 03b2dh, 03b3bh, 03b48h, 03b56h, 03b64h, 03b72h, 03b7fh, 03b8dh
        .word 03b9bh, 03ba9h, 03bb6h, 03bc4h, 03bd2h, 03be0h, 03beeh, 03bfch, 03c09h, 03c17h, 03c25h, 03c33h, 03c41h, 03c4fh, 03c5dh, 03c6bh
        .word 03c79h, 03c87h, 03c95h, 03ca3h, 03cb1h, 03cbfh, 03ccdh, 03cdbh, 03ce9h, 03cf7h, 03d05h, 03d13h, 03d21h, 03d2fh, 03d3eh, 03d4ch
        .word 03d5ah, 03d68h, 03d76h, 03d85h, 03d93h, 03da1h, 03dafh, 03dbdh, 03dcch, 03ddah, 03de8h, 03df7h, 03e05h, 03e13h, 03e22h, 03e30h
        .word 03e3eh, 03e4dh, 03e5bh, 03e6ah, 03e78h, 03e86h, 03e95h, 03ea3h, 03eb2h, 03ec0h, 03ecfh, 03eddh, 03eech, 03efah, 03f09h, 03f18h
        .word 03f26h, 03f35h, 03f43h, 03f52h, 03f61h, 03f6fh, 03f7eh, 03f8dh, 03f9bh, 03faah, 03fb9h, 03fc7h, 03fd6h, 03fe5h, 03ff4h, 04002h
        .word 04011h, 04020h, 0402fh, 0403eh, 0404dh, 0405bh, 0406ah, 04079h, 04088h, 04097h, 040a6h, 040b5h, 040c4h, 040d3h, 040e2h, 040f1h
        .word 04100h, 0410fh, 0411eh, 0412dh, 0413ch, 0414bh, 0415ah, 04169h, 04178h, 04188h, 04197h, 041a6h, 041b5h, 041c4h, 041d3h, 041e3h
        .word 041f2h, 04201h, 04210h, 04220h, 0422fh, 0423eh, 0424eh, 0425dh, 0426ch, 0427ch, 0428bh, 0429ah, 042aah, 042b9h, 042c9h, 042d8h

it_finesinedata:
	.byte   0,  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23
	.byte  24, 26, 27, 29, 30, 32, 33, 34, 36, 37, 38, 39, 41, 42, 43, 44
	.byte  45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 56, 57, 58, 59
	.byte  59, 60, 60, 61, 61, 62, 62, 62, 63, 63, 63, 64, 64, 64, 64, 64
	.byte  64, 64, 64, 64, 64, 64, 63, 63, 63, 62, 62, 62, 61, 61, 60, 60
	.byte  59, 59, 58, 57, 56, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46
	.byte  45, 44, 43, 42, 41, 39, 38, 37, 36, 34, 33, 32, 30, 29, 27, 26
	.byte  24, 23, 22, 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2
	.byte   0, 256-2, 256-3, 256-5, 256-6, 256-8, 256-9, 256-11 ,256-12
	.byte 256-14, 256-16,256-17,256-19,256-20,256-22,256-23
	.byte 256-24, 256-26, 256-27, 256-29, 256-30, 256-32, 256-33, 256-34
	.byte 256-36, 256-37, 256-38, 256-39, 256-41, 256-42, 256-43, 256-44
	.byte 256-45, 256-46, 256-47, 256-48, 256-49, 256-50, 256-51, 256-52
	.byte 256-53, 256-54, 256-55, 256-56, 256-56, 256-57, 256-58, 256-59
	.byte 256-59, 256-60, 256-60, 256-61, 256-61, 256-62, 256-62, 256-62
	.byte 256-63, 256-63, 256-63, 256-64, 256-64, 256-64, 256-64, 256-64
	.byte 256-64, 256-64, 256-64, 256-64, 256-64, 256-64, 256-63, 256-63
	.byte 256-63, 256-62, 256-62, 256-62, 256-61, 256-61, 256-60, 256-60
	.byte 256-59, 256-59, 256-58, 256-57, 256-56, 256-56, 256-55, 256-54
	.byte 256-53, 256-52, 256-51, 256-50, 256-49, 256-48, 256-47, 256-46
	.byte 256-45, 256-44, 256-43, 256-42, 256-41, 256-39, 256-38, 256-37
	.byte 256-36, 256-34, 256-33, 256-32, 256-30, 256-29, 256-27, 256-26
	.byte 256-24, 256-23, 256-22, 256-20, 256-19, 256-17, 256-16, 256-14
	.byte 256-12, 256-11,  256-9,  256-8,  256-6,  256-5,  256-3,  256-2
	
;****************************************************************************************
;* sound effects
;****************************************************************************************

map_15_127:
	 .byte 0,  8, 17, 25
	 .byte 34, 42, 51, 59
	 .byte 68, 76, 85, 93
	 .byte 102, 110, 119, 127

;*************************************************************************
;* play sound effect
;*
;* m0 = params
;* vp sh
;* 
;* s = sample index
;* h = pitch ( 8 = 32000hz, h = pitch height >> 9 )
;* v = volume (15 = max)
;* p = panning (8 = center)
;*************************************************************************
sfx_play:
;-------------------------------------------------------------------------
	mov	a, m0			; m1 = gain (0-15 = 0-127)
	xcn	a			;
	and	a, #0fh			;
	mov	y, a			;
	mov	a, !map_15_127+y	;
	mov	m1, a			;---------------------------------
	mov	a, m0			; m2 = volumes
	and	a, #0fh			;
	mov	y, a			;
	mov	a, !map_15_127+y	;
	mov	m2+1, a			;
	eor	a, #127			;
	mov	m2, a			;---------------------------------
	mov	a, m0+1			; m1.h = src
	and	a, #0f0h		;
	xcn	a			;
	clrc				;
	adc	a, #64			;
	mov	m1+1, a			;---------------------------------
	mov	a, m0+1			; m3 = pitch.h
	and	a, #0fh			; (pitch.l = 0)
	asl	a			;
	mov	m3, a			;---------------------------------
	mov	a, sfx_mask		; test for unused channels
	asl	a			;
	bcc	_sfx_use1		;
	beq	_sfx_use0		;---------------------------------
	eor	sfx_next, #1		; otherwise alternate channels
	bne	_sfx_use1		;
;-------------------------------------------------------------------------
_sfx_use0:
;-------------------------------------------------------------------------
	mov	sfx_next, #0		;
	mov	spc_dspa, #064h		; set srcn value for channel
	mov	spc_dspd, m1+1		;---------------------------------
	mov	spc_dspa, #dsp_kon	; set kon bit
	mov	spc_dspd, #%01000000	;
	or	sfx_mask, #%01000000	; set sfx flag
	mov	spc_dspa, #060h		; setup dsp pointer
	bra	_sfx_start		;
;-------------------------------------------------------------------------
_sfx_use1:
;-------------------------------------------------------------------------
;	cmp	stream_active, #0	; [streaming reserves channel7]
;	bne	_sfx_use0		;
	mov	sfx_next, #1
	mov	spc_dspa, #074h
	mov	spc_dspd, m1+1
	mov	spc_dspa, #dsp_kon
	mov	spc_dspd, #%10000000
	or	sfx_mask, #%10000000
	mov	spc_dspa, #070h
;-------------------------------------------------------------------------
_sfx_start:
;-------------------------------------------------------------------------
	mov	spc_dspd, m2		; volume l
	inc	spc_dspa		;
	mov	spc_dspd, m2+1		; volume r
	inc	spc_dspa		;
	mov	spc_dspd, #0		; pitch l
	inc	spc_dspa		;
	mov	spc_dspd, m3		; pitch h
	inc	spc_dspa		;
	inc	spc_dspa		;
	mov	spc_dspd, #0		; adsr1
	or	spc_dspa, #7		;
	mov	spc_dspd, m1		; gain
	ret				;
;-------------------------------------------------------------------------

;*************************************************************************
;* update sound effects
;*************************************************************************
sfx_update:
;-------------------------------------------------------------------------
	mov	spc_dspa, #dsp_endx	; reset sfx mask flags with endx
	mov	a, spc_dspd		;
	mov	spc_dspd, a		; <- clear endx
;	cmp	stream_active, #0
;	beq	_sfxu_nstreaming
;	and	a, #127
;_sfxu_nstreaming:
	eor	a, sfx_mask		;
	and	a, sfx_mask		;
	mov	sfx_mask, a		;
	ret				;
;-------------------------------------------------------------------------

;*************************************************************************
;*
;* streaming
;*
;*************************************************************************

;**************************************************************************************
;* setup streaming system
;**************************************************************************************
streaming_init:
;--------------------------------------------------------------------------------------
	mov	a, #0				; reset region size
	call !streaming_resize		;
;--------------------------------------------------------------------------------------
	mov	a, #<__brk_routine__	; set brk/tcall0 vector
	mov	!0ffdeh, a			;
	mov	a, #>__brk_routine__	;
	mov	!0ffdfh, a			;
;--------------------------------------------------------------------------------------
	ret
	
;**************************************************************************************
;* resize stream
;* a = newsize
;**************************************************************************************
streaming_resize:
;--------------------------------------------------------------------------------------
;	call !streaming_cancelactive
;--------------------------------------------------------------------------------------
	mov	stream_size, a			;
	mov	a, #0ffh			; calc streaming region address h
	setc					;
	sbc	a, stream_size			;
	mov	stream_region, a		;
;--------------------------------------------------------------------------------------
	mov	a, #0			; copy stream buffer address
	mov	!streamaddress, a	;
	mov	!streamaddress+2, a	;
	mov	a, stream_region	;
	mov	!streamaddress+1, a	;
	mov	!streamaddress+3, a	;
;--------------------------------------------------------------------------------------
	ret
	
;streaming_cancelactive:
;	mov	a, sfx_mask
;	and	a, #80h
;	beq	streaming_is_inactive
;	mov	y, #70h|dspv_gain
;	mov	a, #0
;	movw	spc_dsp, ya
;	
;streaming_is_inactive:
;	ret
	
;**************************************************************************************
;* start stream
;**************************************************************************************
streaming_activate:
;--------------------------------------------------------------------------------------
	mov	a, spc_port2			; compute volume from panning
	and	a, #15				;
	asl	a				;
	asl	a				;
	asl	a				;
	mov	stream_volr, a			;
	eor	a, #127				;
	mov	stream_voll, a			;
;--------------------------------------------------------------------------------------
	mov	a, spc_port2			; compute gain (v<<3)
	and	a, #0f0h			;
	lsr	a				;
	mov	stream_gain, a			;
;--------------------------------------------------------------------------------------
	mov	stream_rate, spc_port3		; copy rate/pitch
;--------------------------------------------------------------------------------------
	mov	stream_initial, #1		; set initial flag for data routine
;--------------------------------------------------------------------------------------
	call !streamresetaddress		;
;--------------------------------------------------------------------------------------
	ret
	
;======================================================================================
streamstartchannel:
;======================================================================================
	mov	stream_initial, #0	; reset flag
	or	sfx_mask, #80h		; patch sfx system
	mov	sfx_next, #1		; 
;--------------------------------------------------------------------------------------
	mov	spc_dspa, #074h		; srcn = stream
	mov	spc_dspd, #80		;
;--------------------------------------------------------------------------------------
	mov	spc_dspa, #dsp_kon	; keyon channel
	mov	spc_dspd, #80h		;
;--------------------------------------------------------------------------------------
	mov	spc_dspa, #070h		; copy volume (panning)
	mov	spc_dspd, stream_voll	; 
	inc	spc_dspa		;
	mov	spc_dspd, stream_volr	;
	inc	spc_dspa		;
;--------------------------------------------------------------------------------------
	mov	spc_dspd, #00h		; copy pitch
	inc	spc_dspa		;
	mov	spc_dspd, stream_rate	;
	inc	spc_dspa		;
	inc	spc_dspa		;
;--------------------------------------------------------------------------------------
	mov	spc_dspd, #0		; clear adsr
	inc	spc_dspa		;
	inc	spc_dspa		;
;--------------------------------------------------------------------------------------
	mov	spc_dspd, stream_gain	; copy gain
;--------------------------------------------------------------------------------------

	ret
	
;**************************************************************************************
;* update stream
;**************************************************************************************
streaming_run:
;--------------------------------------------------------------------------------------
	mov	spc_port0, #80h		; respond to snes
;--------------------------------------------------------------------------------------
	push	a			; preserve regs
	push	x			;
	push	y			;
;--------------------------------------------------------------------------------------
_srw1:	cmp	spc_port0, #80h		; wait for snes
	bcs	_srw1			;
;--------------------------------------------------------------------------------------
	mov	a, spc_port0		; copy nchunks
	mov	stream_a, a		;
	mov	a, spc_port1		; check for new note
	beq	_sr_nstart		;	
	call !streaming_activate	;
_sr_nstart:				;
	mov	x, spc_port0		;
	mov	spc_port0, x		; respond to snes
;--------------------------------------------------------------------------------------
_sr_start:
	mov	y, #0			; prepare copying...
	inc	x
_sr_wait_for_snes:			;
	cmp	x, spc_port0		;
	bne	_sr_wait_for_snes	;
;--------------------------------------------------------------------------------------
	bra	_sr_copy

_sr_nextcopy:
	inc	x
_sr_wait3:
	cmp	x, spc_port0
	bne	_sr_wait3
	
;--------------------------------------------------------------------------------------
_sr_copy:				; copy 9 bytes (16 samples)
;--------------------------------------------------------------------------------------
	mov	a, spc_port2		; copy first 3 bytes
strc0:	mov	!0fe00h+0+y, a	;
	mov	a, spc_port3		;
strc1:	mov	!0fe00h+1+y, a	;
	mov	spc_port0, x		;-signal
	mov	a, spc_port1		;
strc2:	mov	!0fe00h+2+y, a	;
	inc	x			;
_wait1:					; wait for data
	cmp	x, spc_port0		;
	bne	_wait1			;
;--------------------------------------------------------------------------------------
	mov	a, spc_port2		; copy next 3 bytes
strc3:	mov	!0fe00h+3+y, a	;
	mov	a, spc_port3		;
strc4:	mov	!0fe00h+4+y, a	;
	mov	spc_port0, x		;-signal
	mov	a, spc_port1		;
strc5:	mov	!0fe00h+5+y, a	;
	inc	x			;
_wait2:					; wait for data
	cmp	x, spc_port0		;
	bne	_wait2			;
;--------------------------------------------------------------------------------------
	mov	a, spc_port2		; copy last 3 bytes
strc6:	mov	!0fe00h+6+y, a	;
	mov	a, spc_port3		;
strc7:	mov	!0fe00h+7+y, a	;
	mov	spc_port0, x		;-signal
	mov	a, spc_port1		;
strc8:	mov	!0fe00h+8+y, a	; wait for data
;--------------------------------------------------------------------------------------
	mov	a, y			; wr += 9
	clrc
	adc	a, #9			;
	mov	y, a			;
;--------------------------------------------------------------------------------------
	dec	stream_a		; decrement chunk counter
	bne	_sr_nextcopy		; loop until all blocks transferred
;--------------------------------------------------------------------------------------
_sr_exit:				; update write address
	mov	a, y			;
	mov	y, #0			;
	addw	ya, stream_write	;
	movw	stream_write, ya	;
	call !streamsetupaddress	;
	cmp	stream_initial, #0
	beq	_sr_nstart2
	call !streamstartchannel
_sr_nstart2:
;--------------------------------------------------------------------------------------
	pop	y			;4
	pop	x			;4
	pop	a			;4
	ret				;6
	
__brk_routine__:
	asl	spc_port0
	bcs	_brk_pass
	ret
_brk_pass:
	jmp	!streaming_run
	
; (faster version without overflow checks)
;======================================================================================
streamresetaddress:
;======================================================================================
	mov	y, stream_region
	mov	a, #0 
	movw	stream_write, ya
do_fast_ssa:
	mov	!strc0+1, a
	inc	a
	mov	!strc1+1, a
	inc	a
	mov	!strc2+1, a
	inc	a
	mov	!strc3+1, a
	inc	a
	mov	!strc4+1, a
	inc	a
	mov	!strc5+1, a
	inc	a
	mov	!strc6+1, a
	inc	a
	mov	!strc7+1, a
	inc	a
	mov	!strc8+1, a
	mov	!strc0+2, y
	mov	!strc1+2, y
	mov	!strc2+2, y
	mov	!strc3+2, y
	mov	!strc4+2, y
	mov	!strc5+2, y
	mov	!strc6+2, y
	mov	!strc7+2, y
	mov	!strc8+2, y
	ret
	
;======================================================================================
streamsetupaddress:
;======================================================================================
	movw	ya, stream_write
;--------------------------------------------------------------------------------------
	cmp	a, #240				; do fast setup if akku won't overflow
	bcc	do_fast_ssa
	mov	!strc0+1, a			; 1st address
	mov	!strc0+2, y			;
	inc	a				;
	beq	_ssa_over_1			;
_ssa1:	mov	!strc1+1, a			; 2nd
	mov	!strc1+2, y			;
	inc	a				;
	beq	_ssa_over_2			;
_ssa2:	mov	!strc2+1, a			; 3rd
	mov	!strc2+2, y			;
	inc	a				;
	beq	_ssa_over_3			;
_ssa3:	mov	!strc3+1, a			; 4th
	mov	!strc3+2, y			;
	inc	a				;
	beq	_ssa_over_4			;
_ssa4:	mov	!strc4+1, a			; 5th
	mov	!strc4+2, y			;
	inc	a				;
	beq	_ssa_over_5			; 
_ssa5:	mov	!strc5+1, a			; 6th
	mov	!strc5+2, y			;
	inc	a				;
	beq	_ssa_over_6			;
_ssa6:	mov	!strc6+1, a			; 7th
	mov	!strc6+2, y			;
	inc	a				;
	beq	_ssa_over_7			;
_ssa7:	mov	!strc7+1, a			; 8th
	mov	!strc7+2, y			;
	inc	a				;
	beq	_ssa_over_8			;
_ssa8:	mov	!strc8+1, a			; 9th
	mov	!strc8+2, y			;
;--------------------------------------------------------------------------------------
	ret
	
_ssa_over_1:
	inc	y
	jmp	!_ssa1
_ssa_over_2:
	inc	y
	jmp	!_ssa2
_ssa_over_3:
	inc	y
	jmp	!_ssa3
_ssa_over_4:
	inc	y
	jmp	!_ssa4
_ssa_over_5:
	inc	y
	jmp	!_ssa5
_ssa_over_6:
	inc	y
	jmp	!_ssa6
_ssa_over_7:
	inc	y
	jmp	!_ssa7
_ssa_over_8:
	inc	y
	jmp	!_ssa8

;--------------------------------------------------------
;module = 1a00h ;moved to top
;--------------------------------------------------------
	
;--------------------------------------------------------
;.end
;--------------------------------------------------------
