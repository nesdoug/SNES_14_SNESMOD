;************************************************
; snesmod soundbank data                        *
; total size:       7328 bytes                  *
;************************************************

	.global __SOUNDBANK__
	.segment "SOUNDBANK" ; need dedicated bank(s)

__SOUNDBANK__:
	.incbin "sfxbank.bank"
