;************************************************
; snesmod soundbank data                        *
; total size:      14560 bytes                  *
;************************************************

	.global __SOUNDBANK__
	.segment "SOUNDBANK" ; need dedicated bank(s)

__SOUNDBANK__:
	.incbin "soundbank.bank"
