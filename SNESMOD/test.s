
;test for spc700 assembly


start:

	nop              ;$00
	brk              ;$0f
	ret  ;rts        ;$6f
	reti ;rti        ;$7f
	xcn a ;xcn       ;$9f
	sleep ;wai       ;$ef
	stop ;stp        ;$ff

	clrp ;clp        ;$20
	clrc ;clc        ;$60
	di   ;cli        ;$c0
	clrv ;clv              ;$e0
	setp ;sep              ;$40
	setc ;sec              ;$80
	ei   ;sei              ;$a0
	notc ;cmc              ;$ed		

	push psw ;php              ;$0d
	push a ;pha              ;$2d
	push x ;phx              ;$4d
	push y ;phy              ;$6d
	pop psw ;plp              ;$8e
	pop a ;pla              ;$ae
	pop x ;plx              ;$ce
	pop y ;ply              ;$ee		

	inc x ;inx              ;$3d
	inc y ;iny              ;$fc
	dec x ;dex              ;$1d
	dec y ;dey              ;$dc		

	mov x,a ;tax              ;$5d
	mov y,a ;tay              ;$fd
	mov x,sp ;tsx              ;$9d
	mov a,x ;txa              ;$7d
	mov sp,x ;txs              ;$bd
	mov a,y ;tya              ;$dd		
	
	mul ya ;mul              ;$cf
	div ya,x ;div              ;$9e

	das a             ;$be
	daa a             ;$df				
	
	tcall 0 ;$01   jst *04  
	tcall 1 ;$11
	tcall 2 ;$21 
	tcall 3  ;$31
	tcall 4  ;$41
	tcall 5  ;$51
	tcall 6  ;$61
	tcall 7  ;$71
	tcall 8  ;$81
	tcall 9  ;$91
	tcall 10 ;$a1
	tcall 11 ;$b1
	tcall 12 ;$c1
	tcall 13 ;$d1
	tcall 14 ;$e1
	tcall 15 ;$f1	
	
	set1 $12.0 ;$02
	set1 $12.1 ;$22
	set1 $12.2 ;$42
	set1 $12.3 ;$62
	set1 $12.4 ;$82
	set1 $12.5 ;$a2
	set1 $12.6 ;$c2
	set1 $12.7 ;$e2		

	clr1 $12.0 ;$12
	clr1 $12.1 ;$32
	clr1 $12.2 ;$52
	clr1 $12.3 ;$72
	clr1 $12.4 ;$92
	clr1 $12.5 ;$b2
	clr1 $12.6 ;$d2
	clr1 $12.7 ;$f2		
	
	bbs $12.0,label ;$03
	bbs $12.1,label ;$23
	bbs $12.2,label ;$43
	bbs $12.3,label ;$63
	bbs $12.4,label ;$83
	bbs $12.5,label ;$a3
	bbs $12.6,label ;$c3
	bbs $12.7,label ;$e3

	bbc $12.0,label ;$13
	bbc $12.1,label ;$33
	bbc $12.2,label ;$53
	bbc $12.3,label ;$73
	bbc $12.4,label ;$93
	bbc $12.5,label ;$b3
	bbc $12.6,label ;$d3
	bbc $12.7,label ;$f3		
	
label:

	jmp_ [!$1234+x] ;jmp (*16,x)      ;$1f =a
	jmp_ !label ;jmp *16          ;$5f =a
	call !label ;jsr *16          ;$3f =a
	pcall $12 ;jsp *08          ;$4f =a

	dbnz y,label    ;bne --y=*08      ;$fe +2a
	dbnz $12,label   ;bne --*08=*08    ;$6e =a +3b
	cbne $12+x,label ;bne *08,x=*08    ;$de =a +3b 
	cbne $12,label ;bne *08=*08      ;$2e =a +3b		

	bra label ; bra *08          ;$2f +2a
	bpl label ; bpl *08          ;$10 +2a
	bmi label ; bmi *08          ;$30 +2a
	bvc label ; bvc *08          ;$50 +2a
	bvs label ; bvs *08          ;$70 +2a
	bcc label ; bcc *08          ;$90 +2a
	bcs label ; bcs *08          ;$b0 +2a
	bne label ; bne *08          ;$d0 +2a
	beq label ; beq *08          ;$f0 +2a	

	tset1 !$1234 ;tsb *16          ;$0e =a
	tclr1 !$1234 ;trb *16          ;$4e =a

	incw $12 ;inw *08          ;$3a =a
	decw $12 ;dew *08          ;$1a =a
	addw ya,$12 ;adw *08          ;$7a =a
	subw ya,$12 ;sbw *08          ;$9a =a
	cmpw ya,$12 ;cpw *08          ;$5a =a
	movw ya,$12 ;ldw *08          ;$ba =a
	movw $12,ya ;stw *08          ;$da =a	

;?? what is / ... it's a ~ operator
; example mov1 (mem+(bit<<13)),c
	or1 c,/$1234.0 ;orc !*13:*03     ;$2a ~b ~a
	or1 c,$1234.0 ;orc *13:*03      ;$0a ~b ~a
	and1 c,/$1234.0 ;and !*13:*03     ;$6a ~b ~a
	and1 c,$1234.0 ;and *13:*03      ;$4a ~b ~a
	eor1 c,$1234.0 ;eor *13:*03      ;$8a ~b ~a
	mov1 c,$1234.0 ;ldc *13:*03      ;$aa ~b ~a
	mov1 $1234.0,c ;stc *13:*03      ;$ca ~b ~a
	not1 $1234.0 ;not *13:*03      ;$ea ~b ~a		

	or a,(x) ;ora (x)          ;$06
	or (x),(y) ;orr (x)=(y)      ;$19
	or a,[$12+x] ;ora (*08,x)      ;$07 =a
	or a,[$12]+y ;ora (*08),y      ;$17 =a
	or a,#$12 ;ora #*08         ;$08 =a
	or $12,#$34 ;orr *08=#*08     ;$18 =b =a
	or a,!$1234+x ;ora *16,x        ;$15 =a
	or a,!$1234+y ;ora *16,y        ;$16 =a 
	or a,$12+x ;ora *08,x        ;$14 =a
	or $12,$34 ;orr *08=*08      ;$09 =b =a
	or a,!$1234 ;ora *16      ;$05 =a
	or a,$12 ;ora *08          ;$04 =a		

	and a,(x) ;and (x)          ;$26
	and (x),(y) ;and (x)=(y)      ;$39
	and a,[$12+x] ;and (*08,x)      ;$27 =a
	and a,[$12]+y ;and (*08),y      ;$37 =a
	and a,#$12 ;and #*08         ;$28 =a
	and $12,#$34 ;and *08=#*08     ;$38 =b =a
	and a,!$1234+x ;and *16,x        ;$35 =a
	and a,!$1234+y ;and *16,y        ;$36 =a
	and a,$12+x ;and *08,x        ;$34 =a
	and $12,$34 ;and *08=*08      ;$29 =b =a
	and a,!$1234 ;and *16          ;$25 =a
	and a,$12 ;and *08          ;$24 =a		

	eor a,(x) ;eor (x)          ;$46
	eor (x),(y) ;eor (x)=(y)      ;$59
	eor a,[$12+x] ;eor (*08,x)      ;$47 =a
	eor a,[$12]+y ;eor (*08),y      ;$57 =a
	eor a,#$12 ;eor #*08         ;$48 =a
	eor $12,#$34 ; eor *08=#*08     ;$58 =b =a
	eor a,!$1234+x ;eor *16,x        ;$55 =a
	eor a,!$1234+y ;eor *16,y        ;$56 =a
	eor a,$12+x ;eor *08,x        ;$54 =a
	eor $12,$34 ;eor *08=*08      ;$49 =b =a
	eor a,!$1234 ;eor *16          ;$45 =a
	eor a,$12 ;eor *08          ;$44 =a		

	cmp a,(x) ;cmp (x)          ;$66
	cmp (x),(y) ;cmp (x)=(y)      ;$79
	cmp a,[$12+x] ;cmp (*08,x)      ;$67 =a
	cmp a,[$12]+y;cmp (*08),y      ;$77 =a
	cmp a,#$12 ;cmp #*08         ;$68 =a
	cmp $12,#$34 ;cmp *08=#*08     ;$78 =b =a
	cmp a,!$1234+x ;cmp *16,x        ;$75 =a
	cmp a,!$1234+y ;cmp *16,y        ;$76 =a
	cmp a,$12+x ;cmp *08,x        ;$74 =a
	cmp $12,$34 ;cmp *08=*08      ;$69 =b =a
	cmp a,!$1234 ;cmp *16          ;$65 =a
	cmp a,$12 ;cmp *08          ;$64 =a		

	adc a,(x) ;adc (x)          ;$86
	adc (x),(y) ;adc (x)=(y)      ;$99 
	adc a,[$12+x] ;adc (*08,x)      ;$87 =a
	adc a,[$12]+y ;adc (*08),y      ;$97 =a
	adc a,#$12 ;adc #*08         ;$88 =a
	adc $12,#$34 ;adc *08=#*08     ;$98 =b =a
	adc a,!$1234+x ;adc *16,x        ;$95 =a
	adc a,!$1234+y ;adc *16,y        ;$96 =a
	adc a,$12+x ;adc *08,x        ;$94 =a
	adc $12,$34 ;adc *08=*08      ;$89 =b =a
	adc a,!$1234 ;adc *16          ;$85 =a
	adc a,$12 ;adc *08          ;$84 =a		

	sbc a,(x) ;sbc (x)          ;$a6
	sbc (x),(y) ;sbc (x)=(y)      ;$b9
	sbc a,[$12+x] ;sbc (*08,x)      ;$a7 =a
	sbc a,[$12]+y ;sbc (*08),y      ;$b7 =a
	sbc a,#$12 ;sbc #*08         ;$a8 =a
	sbc $12,#$34 ;sbc *08=#*08     ;$b8 =b =a
	sbc a,!$1234+x ;sbc *16,x        ;$b5 =a
	sbc a,!$1234+y ;sbc *16,y        ;$b6 =a
	sbc a,$12+x ;sbc *08,x        ;$b4 =a
	sbc $12,$34 ;sbc *08=*08      ;$a9 =b =a
	sbc a,!$1234 ;sbc *16          ;$a5 =a
	sbc a,$12 ;sbc *08          ;$a4 =a		

	mov (x),a  ;sta (x)          ;$c6
	mov (x)+,a ;sta (x++)        ;$af
	mov [$12+x],a ;sta (*08,x)      ;$c7 =a
	mov [$12]+y,a ;sta (*08),y      ;$d7 =a
	mov $12,#$34 ;str *08=#*08     ;$8f =b =a
	mov !$1234+x,a ;sta *16,x        ;$d5 =a
	mov !$1234+y,a ;sta *16,y        ;$d6 =a
	mov $12+x,a ;sta *08,x        ;$d4 =a
	mov $12,$34 ;str *08=*08      ;$fa =b =a
	mov !$1234,a ;sta *16          ;$c5 =a
	mov $12,a ;sta *08          ;$c4 =a		

	mov a,(x) ;lda (x)          ;$e6
	mov a,(x)+ ;lda (x++)        ;$bf
	mov a,[$12+x] ;lda (*08,x)      ;$e7 =a
	mov a,[$12]+y ;lda (*08),y      ;$f7 =a
	mov a,#$12 ;lda #*08         ;$e8 =a
	mov a,!$1234+x ;lda *16,x        ;$f5 =a
	mov a,!$1234+y ;lda *16,y        ;$f6 =a
	mov a,$12+x ;lda *08,x        ;$f4 =a
	mov a,!$1234 ;lda *16          ;$e5 =a
	mov a,$12 ;lda *08          ;$e4 =a		

	mov $12+y,x ;stx *08,y        ;$d9 =a
	mov !$1234,x ;stx *16          ;$c9 =a
	mov $12,x ;stx *08          ;$d8 =a		

	mov $12+x,y ;sty *08,x        ;$db =a
	mov !$1234,y ;sty *16          ;$cc =a
	mov $12,y ;sty *08          ;$cb =a

	mov x,#$12 ;ldx #*08         ;$cd =a
	mov x,$12+y ;ldx *08,y        ;$f9 =a
	mov x,!$1234 ;ldx *16          ;$e9 =a
	mov x,$12 ;ldx *08          ;$f8 =a		

	mov y,#$12 ;ldy #*08         ;$8d =a
	mov y,$12+x ;ldy *08,x        ;$fb =a
	mov y,!$1234 ;ldy *16          ;$ec =a
	mov y,$12 ;ldy *08          ;$eb =a

	cmp x,#$12 ;cpx #*08         ;$c8 =a
	cmp x,!$1234 ;cpx *16          ;$1e =a
	cmp x,$12 ;cpx *08          ;$3e =a		

	cmp y,#$12 ;cpy #*08         ;$ad =a
	cmp y,!$1234 ;cpy *16          ;$5e =a
	cmp y,$12 ;cpy *08          ;$7e =a

	asl a ;asl              ;$1c
	asl $12+x ;asl *08,x        ;$1b =a
	asl !$1234 ;asl *16          ;$0c =a
	asl $12 ;asl *08          ;$0b =a		

	lsr a ;lsr              ;$5c	
	lsr $12+x ;lsr *08,x        ;$5b =a
	lsr !$1234 ;lsr *16          ;$4c =a
	lsr $12 ;lsr *08          ;$4b =a	

	rol a ;rol              ;$3c
	rol $12+x ;rol *08,x        ;$3b =a
	rol !$1234 ;rol *16          ;$2c =a
	rol $12 ;rol *08          ;$2b =a		

	ror a ;ror              ;$7c
	ror $12+x ;ror *08,x        ;$7b =a
	ror !$1234 ;ror *16          ;$6c =a
	ror $12 ;ror *08          ;$6b =a

	inc a ;inc              ;$bc
	inc $12+x ;inc *08,x        ;$bb =a
	inc !$1234 ;inc *16          ;$ac =a		
	inc $12 ;inc *08          ;$ab =a		

	dec a ;dec              ;$9c
	dec $12+x ;dec *08,x        ;$9b =a
	dec !$1234 ;dec *16          ;$8c =a
	dec $12 ;dec *08          ;$8b =a 		





;.end


