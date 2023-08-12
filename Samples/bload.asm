// TEST

data1		equ	1234h
data2		equ	5678h

; === BSAVE header (7 Bytes) ===

PROGSTART	equ	0C000h

		ORG	PROGSTART - 7

		db	0FEh
		dw	PROGSTART
		dw	PROGEND - 1
		dw	PROGRUN

test_data:
		db	12,13,14,15,16,17,18,19,20
		dw	12,13,14,15,16,17,18,19,20
		dd	12,13,14,15,16,17,18,19,20
		dq	12,13,14,15,16,17,18,19,20
		dw	data1 * 4 / 2
		dw	3 + (data2 - 5) 
		ds	16
		ds	64,0ABh

testjr:
		nop
		ld	bc,data1
		ld	(bc),a
		inc	bc
		inc	b
		dec	b
		ld	b,data2
		rlca

		ex	af,af'
		add	hl,bc
		ld	a,(bc)
		dec	bc
		inc	c
		dec	c
		ld	c,data1
		rrca

		djnz	testjr
		ld	de,data1
		ld	(de),a
		inc	de
		inc	d
		dec	d
		ld	d,data2
		rla

		jr	testjr
		add	hl,de
		ld	a,(de)
		dec	de
		inc	e
		dec	e
		ld	e,data2
		rra

		jr	nz,testjr
		ld	hl,data1
		ld	(test_data),hl
		inc	hl
		inc	h
		dec	h
		ld	h,data2
		daa

		jr	z,testjr
		add	hl,hl
		ld	hl,(test_data)
		dec	hl
		inc	l
		dec	l
		ld	l,data2
		cpl

		jr	nc,testjr
		ld	sp,data1
		ld	(test_data),a
		inc	sp
		inc	(hl)
		dec	(hl)
		ld	(hl),data2
		scf

		jr	c,testjr
		add	hl,sp
		ld	a,(test_data)
		dec	sp
		inc	a
		dec	a
		ld	a,data2
		ccf

		ld	b,b
		ld	b,c
		ld	b,d
		ld	b,e
		ld	b,h
		ld	b,l
		ld	b,(hl)
		ld	b,a

		ld	c,b
		ld	c,c
		ld	c,d
		ld	c,e
		ld	c,h
		ld	c,l
		ld	c,(hl)
		ld	c,a

		ld	d,b
		ld	d,c
		ld	d,d
		ld	d,e
		ld	d,h
		ld	d,l
		ld	d,(hl)
		ld	d,a

		ld	e,b
		ld	e,c
		ld	e,d
		ld	e,e
		ld	e,h
		ld	e,l
		ld	e,(hl)
		ld	e,a

		ld	h,b
		ld	h,c
		ld	h,d
		ld	h,e
		ld	h,h
		ld	h,l
		ld	h,(hl)
		ld	h,a

		ld	l,b
		ld	l,c
		ld	l,d
		ld	l,e
		ld	l,h
		ld	l,l
		ld	l,(hl)
		ld	l,a

		ld	(hl),b
		ld	(hl),c
		ld	(hl),d
		ld	(hl),e
		ld	(hl),h
		ld	(hl),l
		halt
		ld	(hl),a

		ld	a,b
		ld	a,c
		ld	a,d
		ld	a,e
		ld	a,h
		ld	a,l
		ld	a,(hl)
		ld	a,a

		add	a,b
		add	a,c
		add	a,d
		add	a,e
		add	a,h
		add	a,l
		add	a,(hl)
		add	a,a

		adc	a,b
		adc	a,c
		adc	a,d
		adc	a,e
		adc	a,h
		adc	a,l
		adc	a,(hl)
		adc	a,a

		sub	b
		sub	c
		sub	d
		sub	e
		sub	h
		sub	l
		sub	(hl)
		sub	a

		sbc	a,b
		sbc	a,c
		sbc	a,d
		sbc	a,e
		sbc	a,h
		sbc	a,l
		sbc	a,(hl)
		sbc	a,a

		and	b
		and	c
		and	d
		and	e
		and	h
		and	l
		and	(hl)
		and	a

		xor	b
		xor	c
		xor	d
		xor	e
		xor	h
		xor	l
		xor	(hl)
		xor	a

		or	b
		or	c
		or	d
		or	e
		or	h
		or	l
		or	(hl)
		or	a

		cp	b
		cp	c
		cp	d
		cp	e
		cp	h
		cp	l
		cp	(hl)
		cp	a

		ret	nz
		pop	bc
		jp	nz,testjr
		jp	testjr
		call	nz,testjr
		push	bc
		add	a,data1
		rst	0

		ret	z
		ret
		jp	z,testjr
		nop
		call	z,testjr
		call	testjr
		adc	a,data2
		rst	8

		ret	nc
		pop	de
		jp	nc,testjr
		out	(98H),a
		call	nc,testjr
		push	de
		sub	data1
		rst	16

		ret	c
		exx
		jp	c,testjr
		in	a,(98H)
		call	c,testjr
		nop
		sbc	a,data2
		rst	18H

		ret	po
		pop	hl
		jp	po,testjr
		ex	(sp),hl
		call	po,testjr
		push	hl
		and	data1
		rst	20H

		ret	pe
		jp	(hl)
		jp	pe,testjr
		ex	de,hl
		call	pe,testjr
		nop
		xor	data2
		rst	28H

		ret	p
		pop	af
		jp	p,testjr
		di
		call	p,testjr
		push	af
		or	data1
		rst	30H

		ret	m
		ld	sp,hl
		jp	m,testjr
		ei
		call	m,testjr
		nop
		cp	data2
		rst	38H

		in	b,(c)
		out	(c),b
		sbc	hl,bc
		ld	(test_data),bc
		neg
		retn
		; im	0
                db      $ed, $46
		ld	i,a

		in	c,(c)
		out	(c),c
		adc	hl,bc
		ld	bc,(test_data)
		nop
		reti
		; im	0/1
                db      $ed, $4e
		ld	r,a

		in	d,(c)
		out	(c),d
		sbc	hl,de
		ld	(test_data),de
		nop
		retn
		; im	1
                db      $ed, $56
		ld	a,i

		in	e,(c)
		out	(c),e
		adc	hl,de
		ld	de,(test_data)
		nop
		retn
		; im	2
                db      $ed, $5e
		ld	a,r

		in	h,(c)
		out	(c),h
		sbc	hl,hl
		ld	(test_data),hl
		nop
		retn
		; im	0
                db      $ed, $66
		rrd

		in	l,(c)
		out	(c),l
		adc	hl,hl
		ld	hl,(test_data)
		nop
		retn
		; im	0/1
                db      $ed, $6e
		rld

		nop
		nop
		sbc	hl,sp
		ld	(test_data),sp
		nop
		retn
		; im	1
                db      $ed, $76
		nop

		in	a,(c)
		out	(c),a
		adc	hl,sp
		ld	sp,(test_data)
		nop
		retn
		; im	2
                db      $ed, $7e
		nop

		ldi
		cpi
		ini
		outi

		ldd
		cpd
		ind
		outd

		ldir
		cpir
		inir
		otir

		lddr
		cpdr
		indr
		otdr

		rlc	b
		rlc	c
		rlc	d
		rlc	e
		rlc	h
		rlc	l
		rlc	(hl)
		rlc	a
		rlc	(ix + 20)
		rlc	(ix + 40)

		rrc	b
		rrc	c
		rrc	d
		rrc	e
		rrc	h
		rrc	l
		rrc	(hl)
		rrc	a
		rrc	(ix + 20)
		rrc	(ix + 40)

		rl	b
		rl	c
		rl	d
		rl	e
		rl	h
		rl	l
		rl	(hl)
		rl	a
		rl	(ix + 20)
		rl	(ix + 40)

		rr	b
		rr	c
		rr	d
		rr	e
		rr	h
		rr	l
		rr	(hl)
		rr	a
		rr	(ix + 20)
		rr	(ix + 40)

		sla	b
		sla	c
		sla	d
		sla	e
		sla	h
		sla	l
		sla	(hl)
		sla	a
		sla	(ix + 20)
		sla	(ix + 40)

		sra	b
		sra	c
		sra	d
		sra	e
		sra	h
		sra	l
		sra	(hl)
		sra	a
		sra	(ix + 20)
		sra	(ix + 40)

		sll	b
		sll	c
		sll	d
		sll	e
		sll	h
		sll	l
		sll	(hl)
		sll	a
		sll	(ix + 20)
		sll	(ix + 40)

		srl	b
		srl	c
		srl	d
		srl	e
		srl	h
		srl	l
		srl	(hl)
		srl	a
		srl	(ix + 20)
		srl	(ix + 40)

		bit	0,b
		bit	0,c
		bit	0,d
		bit	0,e
		bit	0,h
		bit	0,l
		bit	0,(hl)
		bit	0,a
		bit	0,(ix + 20)
		bit	0,(iy + 40)

		bit	1,b
		bit	1,c
		bit	1,d
		bit	1,e
		bit	1,h
		bit	1,l
		bit	1,(hl)
		bit	1,a
		bit	1,(ix + 20)
		bit	1,(iy + 40)

		bit	2,b
		bit	2,c
		bit	2,d
		bit	2,e
		bit	2,h
		bit	2,l
		bit	2,(hl)
		bit	2,a
		bit	2,(ix + 20)
		bit	2,(iy + 40)

		bit	3,b
		bit	3,c
		bit	3,d
		bit	3,e
		bit	3,h
		bit	3,l
		bit	3,(hl)
		bit	3,a
		bit	3,(ix + 20)
		bit	3,(iy + 40)

		bit	4,b
		bit	4,c
		bit	4,d
		bit	4,e
		bit	4,h
		bit	4,l
		bit	4,(hl)
		bit	4,a
		bit	4,(ix + 20)
		bit	4,(iy + 40)

		bit	5,b
		bit	5,c
		bit	5,d
		bit	5,e
		bit	5,h
		bit	5,l
		bit	5,(hl)
		bit	5,a
		bit	5,(ix + 20)
		bit	5,(iy + 40)

		bit	6,b
		bit	6,c
		bit	6,d
		bit	6,e
		bit	6,h
		bit	6,l
		bit	6,(hl)
		bit	6,a
		bit	6,(ix + 20)
		bit	6,(iy + 40)

		bit	7,b
		bit	7,c
		bit	7,d
		bit	7,e
		bit	7,h
		bit	7,l
		bit	7,(hl)
		bit	7,a
		bit	7,(ix + 127)
		bit	7,(iy - 128)

		res	0,b
		res	0,c
		res	0,d
		res	0,e
		res	0,h
		res	0,l
		res	0,(hl)
		res	0,a
		res	0,(ix + 20)
		res	0,(iy - 40)

		res	1,b
		res	1,c
		res	1,d
		res	1,e
		res	1,h
		res	1,l
		res	1,(hl)
		res	1,a
		res	1,(ix + 20)
		res	1,(iy + 40)

		res	2,b
		res	2,c
		res	2,d
		res	2,e
		res	2,h
		res	2,l
		res	2,(hl)
		res	2,a
		res	2,(ix + 20)
		res	2,(iy + 40)

		res	3,b
		res	3,c
		res	3,d
		res	3,e
		res	3,h
		res	3,l
		res	3,(hl)
		res	3,a
		res	3,(ix + 20)
		res	3,(iy + 40)

		res	4,b
		res	4,c
		res	4,d
		res	4,e
		res	4,h
		res	4,l
		res	4,(hl)
		res	4,a
		res	4,(ix + 20)
		res	4,(iy + 40)

		res	5,b
		res	5,c
		res	5,d
		res	5,e
		res	5,h
		res	5,l
		res	5,(hl)
		res	5,a
		res	5,(ix + 20)
		res	5,(iy + 40)

		res	6,b
		res	6,c
		res	6,d
		res	6,e
		res	6,h
		res	6,l
		res	6,(hl)
		res	6,a
		res	6,(ix + 20)
		res	6,(iy + 40)

		res	7,b
		res	7,c
		res	7,d
		res	7,e
		res	7,h
		res	7,l
		res	7,(hl)
		res	7,a
		res	7,(ix + 20)
		res	7,(iy + 40)

		set	0,b
		set	0,c
		set	0,d
		set	0,e
		set	0,h
		set	0,l
		set	0,(hl)
		set	0,a
		set	0,(ix + 20)
		set	0,(iy + 40)

		set	1,b
		set	1,c
		set	1,d
		set	1,e
		set	1,h
		set	1,l
		set	1,(hl)
		set	1,a
		set	1,(ix + 20)
		set	1,(iy + 40)

		set	2,b
		set	2,c
		set	2,d
		set	2,e
		set	2,h
		set	2,l
		set	2,(hl)
		set	2,a
		set	2,(ix + 20)
		set	2,(iy + 40)

		set	3,b
		set	3,c
		set	3,d
		set	3,e
		set	3,h
		set	3,l
		set	3,(hl)
		set	3,a
		set	3,(ix + 20)
		set	3,(iy + 40)

		set	4,b
		set	4,c
		set	4,d
		set	4,e
		set	4,h
		set	4,l
		set	4,(hl)
		set	4,a
		set	4,(ix + 20)
		set	4,(iy + 40)

		set	5,b
		set	5,c
		set	5,d
		set	5,e
		set	5,h
		set	5,l
		set	5,(hl)
		set	5,a
		set	5,(ix + 20)
		set	5,(iy + 40)

		set	6,b
		set	6,c
		set	6,d
		set	6,e
		set	6,h
		set	6,l
		set	6,(hl)
		set	6,a
		set	6,(ix + 20)
		set	6,(iy + 40)

		set	7,b
		set	7,c
		set	7,d
		set	7,e
		set	7,h
		set	7,l
		set	7,(hl)
		set	7,a
		set	7,(ix + 20)
		set	7,(iy + 40)

		add	ix,bc
		add	ix,de

		ld	ix,data1
		ld	(test_data),ix
		inc	ix
		inc	ixh
		dec	ixh
		ld	ixh,data2

		add	ix,ix
		ld	ix,(test_data)
		dec	ix
		inc	ixl
		dec	ixl
		ld	ixl,data2

		inc	(ix + 11H)
		dec	(ix + 22H)
		ld	(ix + 33H),data1
		add	ix,sp

		ld	b,ixh
		ld	b,ixl
		ld	b,(ix+11H)
		ld	c,ixh
		ld	c,ixl
		ld	c,(ix+22H)
		ld	d,ixh
		ld	d,ixl
		ld	d,(ix+33H)
		ld	e,ixh
		ld	e,ixl
		ld	e,(ix+44H)

		ld	ixh,b
		ld	ixh,c
		ld	ixh,d
		ld	ixh,e
		ld	ixh,ixh
		ld	ixh,ixl
		ld	h,(ix + 20)
		ld	ixh,a

		ld	ixl,b
		ld	ixl,c
		ld	ixl,d
		ld	ixl,e
		ld	ixl,ixh
		ld	ixl,ixl
		ld	l,(ix + 40)
		ld	ixl,a

		ld	(ix + 11H),b
		ld	(ix + 22H),c
		ld	(ix + 33H),d
		ld	(ix + 44H),e
		ld	(ix + 55H),h
		ld	(ix + 66H),l
		ld	(ix + 77H),a

		ld	a,ixh
		ld	a,ixl
		ld	a,(ix + 20)

		add	a,ixh
		add	a,ixl
		add	a,(ix + 40)

		adc	a,ixh
		adc	a,ixl
		adc	a,(ix + 40)

		sub	ixh
		sub	ixl
		sub	(ix + 20)

		sbc	a,ixh
		sbc	a,ixl
		sbc	a,(ix + 40)

		and	ixh
		and	ixl
		and	(ix + 20)

		xor	ixh
		xor	ixl
		xor	(ix + 40)

		or	ixh
		or	ixl
		or	(ix + 20)

		cp	ixh
		cp	ixl
		cp	(ix + 40)

		pop	ix
		ex	(sp),ix
		push	ix
		jp	(ix)
		ld	sp,ix

		add	iy,bc
		add	iy,de

		ld	iy,data1
		ld	(test_data),iy
		inc	iy
		inc	iyh
		dec	iyh
		ld	iyh,data2

		add	iy,iy
		ld	iy,(test_data)
		dec	iy
		inc	iyl
		dec	iyl
		ld	iyl,data2

		inc	(iy + 11H)
		dec	(iy + 22H)
		ld	(iy + 33H),data1
		add	iy,sp

		ld	b,iyh
		ld	b,iyl
		ld	b,(iy+11H)
		ld	c,iyh
		ld	c,iyl
		ld	c,(iy+22H)
		ld	d,iyh
		ld	d,iyl
		ld	d,(iy+33H)
		ld	e,iyh
		ld	e,iyl
		ld	e,(iy+44H)

		ld	iyh,b
		ld	iyh,c
		ld	iyh,d
		ld	iyh,e
		ld	iyh,iyh
		ld	iyh,iyl
		ld	h,(iy + 20)
		ld	iyh,a

		ld	iyl,b
		ld	iyl,c
		ld	iyl,d
		ld	iyl,e
		ld	iyl,iyh
		ld	iyl,iyl
		ld	l,(iy + 40)
		ld	iyl,a

		ld	(iy + 11H),b
		ld	(iy + 22H),c
		ld	(iy + 33H),d
		ld	(iy + 44H),e
		ld	(iy + 55H),h
		ld	(iy + 66H),l
		ld	(iy + 77H),a

		ld	a,iyh
		ld	a,iyl
		ld	a,(iy + 20)

		add	a,iyh
		add	a,iyl
		add	a,(iy + 40)

		adc	a,iyh
		adc	a,iyl
		adc	a,(iy + 40)

		sub	iyh
		sub	iyl
		sub	(iy + 20)

		sbc	a,iyh
		sbc	a,iyl
		sbc	a,(iy + 40)

		and	iyh
		and	iyl
		and	(iy + 20)

		xor	iyh
		xor	iyl
		xor	(iy + 40)

		or	iyh
		or	iyl
		or	(iy + 20)

		cp	iyh
		cp	iyl
		cp	(iy + 40)

		pop	iy
		ex	(sp),iy
		push	iy
		jp	(iy)
		ld	sp,iy

PROGRUN:
		ret

PROGEND:
