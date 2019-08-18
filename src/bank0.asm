;-------------
; Includes
;-------------
	
INCLUDE "hardware.asm"
INCLUDE "header.asm"
INCLUDE "sprite.inc"
INCLUDE "dinotiles.z80"
INCLUDE "dinomap.z80"
INCLUDE "gameover.z80"

MOVESPEED EQU 2
FALLSPEED EQU 3

;-------------
; Start
;-------------

SECTION "Program Start", ROM0[$150]
START:
	ei				 ;enable interrupts
	ld  sp,$FFFE
	ld  a,IEF_VBLANK ;enable vblank interrupt
	ld  [rIE],a

	ld  a,$0
	ldh [rLCDC],a 	 ;LCD off
	ldh [rSTAT],a

	ld  a,%11100100  ;shade palette (11 10 01 00)
	ldh [rBGP],a 	 ;setup palettes
	ldh [rOCPD],a
	ldh [rOBP0],a

	call DMA_COPY    ;move DMA routine to HRAM
	call CLEAR_MAP
	call CLEAR_OAM
	call LOAD_TILES
	call LOAD_MAP
	call LOAD_WINDOW
	call LOAD_DINO
	call LOAD_CACTI

	ld  a,%11010011  ;turn on LCD, BG0, OBJ0, etc
	ldh [rLCDC],a    ;load LCD flags

LOOP_INIT:
	call WAIT_VBLANK
	call READ_JOYPAD ; return DULRTSBA on a
	or %10111111     ; DULRTSBA - only all 1 if up is pressed
	cpl              ; 00000000 if up is pressed
	add a, $0        ; set flags
	jp z, START_GAME
	call _HRAM		 ;call DMA routine from HRAM
	jp LOOP_INIT

START_GAME:
	call LOAD_SCORE
	call START_RUN
	ld a, 32
	ld [airtime], a

LOOP_GAME:
	call WAIT_VBLANK
	call CHECK_COLLISION
	call READ_JOYPAD
	call CHECK_INPUT
	call SCROLL_BGD
	call SCROLL_CACTUS
	call UPDATE_SCORE
	call _HRAM
	jp LOOP_GAME

END_GAME:
	call WAIT_VBLANK
	ld a, %11110011 ; enable window
	ld [rLCDC], a
	call CLEANUP
	call _HRAM
.loop
	call WAIT_VBLANK
	call READ_JOYPAD ; return DULRTSBA on a
	or %10111111     ; DULRTSBA - only all 1 if up is pressed
	cpl              ; 00000000 if up is pressed
	add a, $0        ; set flags
	jp z, START
	jp .loop

;-------------
; Subroutines
;-------------

LOAD_SCORE:
	SpriteAttr score0
	PutSpriteXAddr score0, 148
	PutSpriteYAddr score0, 5
	sprite_PutTile score0, $4
	sprite_PutFlags score0, $00
	SpriteAttr score1
	PutSpriteXAddr score1, 138
	PutSpriteYAddr score1, 5
	sprite_PutTile score1, $4
	sprite_PutFlags score1, $00
	SpriteAttr score2
	PutSpriteXAddr score2, 128
	PutSpriteYAddr score2, 5
	sprite_PutTile score2, $4
	sprite_PutFlags score2, $00
	SpriteAttr score3
	PutSpriteXAddr score3, 118
	PutSpriteYAddr score3, 5
	sprite_PutTile score3, $4
	sprite_PutFlags score3, $00
	SpriteAttr score4
	PutSpriteXAddr score4, 108
	PutSpriteYAddr score4, 5
	sprite_PutTile score4, $4
	sprite_PutFlags score4, $00
	ret

UPDATE_SCORE:
	ld a, [vblank_count]
	cpl           ; multiples of 8 - every 8/60 fps
	or %11111000      ; sets front bits to 1
	cpl
	add a, $0    ; set flags - 0 if mult of 8 vblanks
	jr z, .incscore
	ret
.incscore
	call INC_S0
	ret

INC_S0:
	sprite_GetTile score0
	ld b, a
	sub a, $D ; if at 9
	ld a, b
	jr z, .wrap
	inc a
	sprite_PutTile score0, a
	ret
.wrap
	sprite_PutTile score0, $4
	call INC_S1
	ret

INC_S1:
	sprite_GetTile score1
	ld b, a
	sub a, $D ; if at 9
	ld a, b
	jr z, .wrap
	inc a
	sprite_PutTile score1, a
	ret
.wrap
	sprite_PutTile score1, $4
	call INC_S2
	ret

INC_S2:
	sprite_GetTile score2
	ld b, a
	sub a, $D ; if at 9
	ld a, b
	jr z, .wrap
	inc a
	sprite_PutTile score2, a
	ret
.wrap
	sprite_PutTile score2, $4
	call INC_S3
	ret

INC_S3:
	sprite_GetTile score3
	ld b, a
	sub a, $D ; if at 9
	ld a, b
	jr z, .wrap
	inc a
	sprite_PutTile score3, a
	ret
.wrap
	sprite_PutTile score3, $4
	call INC_S4
	ret

INC_S4:
	sprite_GetTile score4
	ld b, a
	sub a, $D ; if at 9
	ld a, b
	jr z, .wrap
	inc a
	sprite_PutTile score4, a
	ret
.wrap
	sprite_PutTile score4, $4
	ret

LOAD_CACTI:
	SpriteAttr cactusA0
	PutSpriteXAddr cactusA0, 168
	PutSpriteYAddr cactusA0, 90
	sprite_PutTile cactusA0, $3D
	sprite_PutFlags cactusA0, $00
	SpriteAttr cactusA1
	PutSpriteXAddr cactusA1, 168
	PutSpriteYAddr cactusA1, 82
	sprite_PutTile cactusA1, $3C
	sprite_PutFlags cactusA1, $00
	SpriteAttr cactusA2
	PutSpriteXAddr cactusA2, 176
	PutSpriteYAddr cactusA2, 82
	sprite_PutTile cactusA2, $3E
	sprite_PutFlags cactusA2, $00
	SpriteAttr cactusA3
	PutSpriteXAddr cactusA3, 168
	PutSpriteYAddr cactusA3, 74
	sprite_PutTile cactusA3, $39
	sprite_PutFlags cactusA3, $00
	SpriteAttr cactusA4
	PutSpriteXAddr cactusA4, 176
	PutSpriteYAddr cactusA4, 74
	sprite_PutTile cactusA4, $3B
	sprite_PutFlags cactusA4, $00

	SpriteAttr cactusB0
	PutSpriteXAddr cactusB0, 244
	PutSpriteYAddr cactusB0, 90
	sprite_PutTile cactusB0, $31
	sprite_PutFlags cactusB0, $00
	SpriteAttr cactusB1
	PutSpriteXAddr cactusB1, 244
	PutSpriteYAddr cactusB1, 82
	sprite_PutTile cactusB1, $30
	sprite_PutFlags cactusB1, $00
	ret

SCROLL_CACTUS:
	GetSpriteXAddr cactusA0
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftA0
	PutSpriteXAddr cactusA0, 160
	jr .updateA1
.moveleftA0
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusA0, a

.updateA1
	GetSpriteXAddr cactusA1
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftA1
	PutSpriteXAddr cactusA1, 160
	jr .updateA2
.moveleftA1
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusA1, a

.updateA2
	GetSpriteXAddr cactusA2
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftA2
	PutSpriteXAddr cactusA2, 160
	jr .updateA3
.moveleftA2
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusA2, a

.updateA3
	GetSpriteXAddr cactusA3
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftA3
	PutSpriteXAddr cactusA3, 160
	jr .updateA4
.moveleftA3
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusA3, a

.updateA4
	GetSpriteXAddr cactusA4
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftA4
	PutSpriteXAddr cactusA4, 160
	jr .updateB0
.moveleftA4
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusA4, a

.updateB0
	GetSpriteXAddr cactusB0
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftB0
	PutSpriteXAddr cactusB0, 160
	jr .updateB1
.moveleftB0
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusB0, a

.updateB1
	GetSpriteXAddr cactusB1
	add a, 8 ; zero if original location was -8 aka hidden
	jr nz, .moveleftB1
	PutSpriteXAddr cactusB1, 160
	ret
.moveleftB1
	sub a, 8
	sub a, MOVESPEED
	PutSpriteXAddr cactusB1, a
	ret

CHECK_INPUT:
	ld b, a
	ld a, [airtime]
	bit 5, a ; if bit 5 (val 32) is 0, in the air, continue jump
	jr nz, .checkuppress
	call DINO_JUMP
	ret
.checkuppress
	ld a, b
	or %10111111     ; DULRTSBA - only all 1 if up is pressed
	cpl              ; 00000000 if up is pressed
	add a, $0        ; set flags
	jp nz, .gorun    ; if up not pressed
	ld a, [airtime] ; just starting jump
	dec a
	ld [airtime], a
	call DINO_JUMP
.gorun
	call DINO_RUN
	ret

LOAD_DINO:
	SpriteAttr dino0
	PutSpriteXAddr dino0, 20
	PutSpriteYAddr dino0, 90
	sprite_PutTile dino0, $45
	sprite_PutFlags dino0, $00

	SpriteAttr dino1
	PutSpriteXAddr dino1, 28
	PutSpriteYAddr dino1, 90
	sprite_PutTile dino1, $47
	sprite_PutFlags dino1, $00

	SpriteAttr dino2
	PutSpriteXAddr dino2, 20
	PutSpriteYAddr dino2, 82
	sprite_PutTile dino2, $44
	sprite_PutFlags dino2, $00

	SpriteAttr dino3
	PutSpriteXAddr dino3, 28
	PutSpriteYAddr dino3, 82
	sprite_PutTile dino3, $46
	sprite_PutFlags dino3, $00

	SpriteAttr dino4
	PutSpriteXAddr dino4, 36
	PutSpriteYAddr dino4, 82
	sprite_PutTile dino4, $4C
	sprite_PutFlags dino4, $00

	SpriteAttr dino5
	PutSpriteXAddr dino5, 28
	PutSpriteYAddr dino5, 74
	sprite_PutTile dino5, $43
	sprite_PutFlags dino5, $00

	SpriteAttr dino6
	PutSpriteXAddr dino6, 36
	PutSpriteYAddr dino6, 74
	sprite_PutTile dino6, $59
	sprite_PutFlags dino6, $00
	ret

CHECK_COLLISION:
	ld a, $0
	ld [touching], a

	GetSpriteXAddr cactusB0
	add a, 8 ; width of cactus
	ld b,a
	GetSpriteXAddr dino0
	sub a,b ; set flags - d_x < a_x + 8
	jr c, .next1
	ld a, [touching]
	inc a
	ld [touching], a
.next1
	GetSpriteXAddr dino0
	add a, 16 ; width of dino
	ld b,a
	GetSpriteXAddr cactusB0
	sub a,b ; set flags - a_x < d_x + 16
	jr c, .next2
	ld a, [touching]
	inc a
	ld [touching], a
.next2
	GetSpriteYAddr cactusB0
	add a, 16 ; height of cactus
	ld b,a
	GetSpriteYAddr dino0
	sub a,b ; set flags - d_y < a_x + 16
	jr c, .next3
	ld a, [touching]
	inc a
	ld [touching], a
.next3
	GetSpriteYAddr dino0
	add a, 24 ; height of dino
	ld b,a
	GetSpriteYAddr cactusB0
	sub a,b ; set flags - a_y < d_y + 24
	jr c, .firstcheck
	ld a, [touching]
	inc a
	ld [touching], a

.firstcheck
	ld a, [touching] ; should be 0 if nothing added - all true
	add a, 0
	jp z, END_GAME
	ld a, 0
	ld [touching],a ; reset

	GetSpriteXAddr cactusA0
	add a, 8 ; width of cactus - NARROWER THAN REAL LIFE or its too hard
	ld b,a
	GetSpriteXAddr dino0
	sub a,b ; set flags - d_x < a_x + 8
	jr c, .next5
	ld a, [touching]
	inc a
	ld [touching], a
.next5
	GetSpriteXAddr dino0
	add a, 16 ; width of dino
	ld b,a
	GetSpriteXAddr cactusA0
	sub a,b ; set flags - a_x < d_x + 16
	jr c, .next6
	ld a, [touching]
	inc a
	ld [touching], a
.next6
	GetSpriteYAddr cactusA0
	add a, 24 ; height of cactus
	ld b,a
	GetSpriteYAddr dino0
	sub a,b ; set flags - d_y < a_x + 16
	jr c, .next7
	ld a, [touching]
	inc a
	ld [touching], a
.next7
	GetSpriteYAddr dino0
	add a, 24 ; height of dino
	ld b,a
	GetSpriteYAddr cactusA0
	sub a,b ; set flags - a_y < d_y + 24
	jr c, .finalcheck
	ld a, [touching]
	inc a
	ld [touching], a

.finalcheck
	ld a, [touching] ; should be 0 if nothing added - all true
	add a, 0
	jp z, END_GAME
	ret

DINO_JUMP:
	sprite_PutTile dino0, $45
	sprite_PutTile dino1, $47
	ld a, [airtime]
	ld b, a ; backup
	and a, %11110000 ; will be 00XX0000 if 32-16, 00000000 if 15-0
	add a, 0 ; set flags, a is now airtime
	ld a, b ; does not set flags
	jr z, .falling ; second half
; rising
	dec a
	ld [airtime], a
	call MOVE_DINO_UP
	ret
.falling
	add a, 0
	jr z, .reset ; if at 0
	dec a
	ld [airtime], a
	call MOVE_DINO_DOWN
	ret
.reset
	ld a, 32
	ld [airtime], a
	call MOVE_DINO_DOWN
	ret

MOVE_DINO_DOWN:
	GetSpriteYAddr dino0
	add a, FALLSPEED
	PutSpriteYAddr dino0, a 
	GetSpriteYAddr dino1
	add a, FALLSPEED
	PutSpriteYAddr dino1, a 
	GetSpriteYAddr dino2
	add a, FALLSPEED
	PutSpriteYAddr dino2, a 
	GetSpriteYAddr dino3
	add a, FALLSPEED
	PutSpriteYAddr dino3, a 
	GetSpriteYAddr dino4
	add a, FALLSPEED
	PutSpriteYAddr dino4, a 
	GetSpriteYAddr dino5
	add a, FALLSPEED
	PutSpriteYAddr dino5, a 
	GetSpriteYAddr dino6
	add a, FALLSPEED
	PutSpriteYAddr dino6, a 
	ret

MOVE_DINO_UP:
	GetSpriteYAddr dino0
	sub a, FALLSPEED
	PutSpriteYAddr dino0, a 
	GetSpriteYAddr dino1
	sub a, FALLSPEED
	PutSpriteYAddr dino1, a 
	GetSpriteYAddr dino2
	sub a, FALLSPEED
	PutSpriteYAddr dino2, a 
	GetSpriteYAddr dino3
	sub a, FALLSPEED
	PutSpriteYAddr dino3, a 
	GetSpriteYAddr dino4
	sub a, FALLSPEED
	PutSpriteYAddr dino4, a 
	GetSpriteYAddr dino5
	sub a, FALLSPEED
	PutSpriteYAddr dino5, a 
	GetSpriteYAddr dino6
	sub a, FALLSPEED
	PutSpriteYAddr dino6, a 
	ret

START_RUN:
	ld a, 0
	ld [run_phase], a
	sprite_PutTile dino0, $65
	sprite_PutTile dino1, $67
	ret

DINO_RUN:
	ld a, [vblank_count]
	cpl           ; multiples of 8 - every 8/60 fps
	or %11111000      ; sets front bits to 1
	cpl
	add a, $0    ; set flags
	jr z, .changerunphase
	ret
.changerunphase ; starts 0 -> 1
	ld a, [run_phase]
	add a, $0
	jr nz, .changerunphase2
	inc a ; phase is now 1
	ld [run_phase], a
	sprite_PutTile dino0, $55
	sprite_PutTile dino1, $57
	ret
.changerunphase2 ; 1 -> 0 instead
	dec a ; phase is now 0
	ld [run_phase], a
	sprite_PutTile dino0, $65
	sprite_PutTile dino1, $67
	ret

CLEANUP:
	sprite_PutTile dino0, $00
	sprite_PutTile dino1, $00
	sprite_PutTile dino2, $00
	sprite_PutTile dino3, $00
	sprite_PutTile dino4, $00
	sprite_PutTile dino5, $00
	sprite_PutTile dino6, $00
	sprite_PutTile cactusB0, $00
	sprite_PutTile cactusB1, $00
	sprite_PutTile cactusA0, $00
	sprite_PutTile cactusA1, $00
	sprite_PutTile cactusA2, $00
	sprite_PutTile cactusA3, $00
	sprite_PutTile cactusA4, $00

DOWNOFFSET EQU 95
LEFTOFFSET EQU 55
	MoveDown score0, DOWNOFFSET
	MoveDown score1, DOWNOFFSET
	MoveDown score2, DOWNOFFSET
	MoveDown score3, DOWNOFFSET
	MoveDown score4, DOWNOFFSET
	MoveLeft score0, LEFTOFFSET
	MoveLeft score1, LEFTOFFSET
	MoveLeft score2, LEFTOFFSET
	MoveLeft score3, LEFTOFFSET
	MoveLeft score4, LEFTOFFSET
	ret

SCROLL_BGD:
	ld a, [rSCX]
	add a, MOVESPEED
	ld [rSCX], a
	ret

BSCROLL_BGD:
	ld a, [rSCX]
	dec a
	ld [rSCX], a
	ret

WAIT_VBLANK:
	ld  hl,vblank_flag
.wait_vblank_loop
	halt
	nop  			 ;Hardware bug
	ld  a,$0
	cp  [hl]
	jr  z,.wait_vblank_loop
	ld  [hl],a
	ld  a,[vblank_count]
	inc a
	ld  [vblank_count],a
	ret

DMA_COPY:
	ld  de,$FF80  	 ;DMA routine, gets placed in HRAM
	rst $28
	DB  $00,$0D
	DB  $F5, $3E, $C1, $EA, $46, $FF, $3E, $28, $3D, $20, $FD, $F1, $D9
	ret

CLEAR_OAM:
; clears $FE00-FE9F
	ld hl, $C100
	ld bc, $A0
.clear_oam_loop
	ld a, $0
	ld [hli],a ; fill 0, inc hl (location)
	dec bc
	ld a, b
	or c
	jr nz, .clear_oam_loop
	ret

CLEAR_MAP:
	ld  hl,_SCRN0    ;load map0 ram
	ld  bc, $400 ; size
.clear_map_loop
	ld  a,$0
	ld  [hli],a      ;clear tile, increment hl
	dec bc
	ld  a,b
	or  c
	jr  nz,.clear_map_loop
	ret

LOAD_TILES:
	ld  hl, TileLabel
	ld  de,_VRAM ; $8000 tile pattern table 0
	ld  bc,TILE_COUNT
.load_tiles_loop
	ld  a,[hli]      ;grab a byte
	ld  [de],a       ;store the byte in VRAM
	inc de
	dec bc
	ld  a,b
	or  c
	jr  nz,.load_tiles_loop
	ret

LOAD_WINDOW:
	ld  hl, TileMapLabel_GO
	ld  de,_SCRN1
	ld  bc, 1024 ; 32x32 tiles
.load_map_loop
	ld  a,[hli]
	ld  [de],a
	inc de
	dec bc
	ld  a,b
	or  c
	jr  nz,.load_map_loop

	ld a, 4
	ld [rWX], a
	ret

LOAD_MAP:
	ld  hl, TileMapLabel  ;same as LOAD_TILES
	ld  de,_SCRN0
	ld  bc, 1024 ; 32x32 tiles
.load_map_loop
	ld  a,[hli]
	ld  [de],a
	inc de
	dec bc
	ld  a,b
	or  c
	jr  nz,.load_map_loop
	ret

READ_JOYPAD:
	ld  a,%00100000  ;select dpad (directions)
	ld  [rP1],a
	ld  a,[rP1]		 ;takes a few cycles to get accurate reading
	ld  a,[rP1]
	ld  a,[rP1]
	ld  a,[rP1]		 ; a is 0010D'U'L'R' (0 is select)
	cpl 			 ;complement a - a is now 1101DULR
	and %00001111    ;select dpad buttons - a is now 0000DULR
	swap a           ; a is now DULR0000
	ld  b,a

	ld  a,%00010000  ;select other buttons
	ld  [rP1],a  
	ld  a,[rP1]
	ld  a,[rP1]
	ld  a,[rP1]
	ld  a,[rP1]      ; a is 0001T'/S'/B'/A' - s(t)art
	cpl
	and %00001111    ; 0000/TSBA
	or  b            ; a is DULRTSBA
					 ;lower nybble is other
	ld  b,a          ; b is also DULRTSBA
	ld  a,[joypad_down] ; already pressed
	cpl
	and b
	ld  [joypad_pressed],a ; pressed this turn
					 ;upper nybble is dpad
	ld  a,b
	ld  [joypad_down],a
	ret

JOY_RIGHT:
	and %00010000
	cp  %00010000
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_LEFT:
	and %00100000
	cp  %00100000
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_UP:
	and %01000000
	cp  %01000000
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_DOWN:
	and %10000000
	cp  %10000000
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_A:
	and %00000001
	cp  %00000001
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_B:
	and %00000010
	cp  %00000010
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_SELECT:
	and %00000100
	cp  %00000100
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_START:
	and %00001000
	cp  %00001000
	jp  nz,JOY_FALSE
	ld  a,$1
	ret
JOY_FALSE:
	ld  a,$0
	ret

SECTION "RAM Vars", WRAM0[$C000]
vblank_flag:
db
vblank_count:
db
joypad_down:
db                   ;dow/up/lef/rig/sta/sel/a/b
joypad_pressed:
db
run_phase:
db
airtime:
db
touching:
db
