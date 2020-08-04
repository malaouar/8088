;-------------------
; helico Game -- shoot on planes!!
; inspired from game "fisherman" from MIKEOS
;---------------------

define reclen  6	   ;object  length 6 bytes: (column) x, (row) y,  behavior,  graphic, term,  direction
						; graphic + term = zero terminated string (term=0)
define obcnt   5       ;count of total objects : 3 helicos +  canon + fire
define helicocnt 3	   ;number of helicos


;----------
	use16
	ORG 32768
	
;=========================
  ; disable cursor
  mov al, 04
  mov ah, 1
  int 10h

  ; clear screen
  call cls
    
main_loop:
	mov ah, 1
	int 16h
  ;check for key press
	cmp ah,  0x76		 ;was it Esc ?
	je gameover			;if yes, exit game

	call move_obs		;else, move objects 
	call collision		; test for collision and calculate score
	call drawscreen 	;refresh screen and draw score
	
	; pause
	mov cx,03FFFh     ;
    call delay
    
	; clear screen
	call cls
	jmp main_loop      ; loop forever
	
gameover:
	; Enable cursor
	mov al, 03
	mov ah, 1
	int 10h

	call cls
	ret
  
;------------------------------------------------
; Move objects
; Must be called with scancode in Ah

move_obs:
	;user input
	cmp ah,04	     ; F3=fire?
	je shot
	cmp ah,05	     ; F1=left?
	je left
	cmp ah, 06	      ;F2=right?
	je right
	jmp move_all

shot:
	mov bx,fire		;fire array
	mov al, byte [bx+3]  ; already fire?
	cmp al, ':'
	je move_all	; yes, exit
  mov byte [bx+3], ':'	; No then new fire
  ; copy canon position to fire position
  mov si, canon
  lodsb  ; al = [si +0]
  mov byte [bx +0], al		; column
  mov byte [bx +1], 23		; row
  jmp movehelico
  
	
 ; move canon:
left:
	mov bx,canon			;canon array
	mov al, byte [bx+0]	      ;x position
    dec al
    cmp al, 1
    ja suite1
    mov byte [bx+0], 39 	;reset to start
    jmp move_all
suite1:
  mov byte [bx+0], al
  jmp move_all
right:
	mov bx,canon				;canon array
	mov al, byte [bx+0]	      ;x position
    inc al
    cmp al, 39
    jb suite2 ; below
    mov byte [bx+0], 0		;reset to start
    jmp move_all
suite2:
  mov byte [bx+0], al

;--------------------- 
move_all:
; first move_fire:
  mov bx,fire				;fire array
  mov al, byte [bx +1]		; row 
  dec al      				; fire reachs top?
  jz stop_fire	   		 	; yes, stop
  mov byte [bx +1], al 		; no, up
  jmp movehelico
stop_fire:
	mov byte [bx+3], ' '	; delete fire

;-----------------------------------------
; move helicos:
movehelico: 
	inc byte [behav]		;inc behavior byte
	xor cl,cl				;loop counter
	mov bx,helico			;helico array
helicoloop:
	cmp cl,helicocnt		;last helico?
	je exit1
	mov al, byte [behav]
	and al, byte [bx+2]		;helico behavior mask
	cmp al,0
	je endl 	       		;doesn't move this loop
	mov al, byte [bx+5]	      ;direction flag
	cmp al,0				;0=right, 1=left
	je right_helico
;left_helico:
	mov al, byte [bx+0]	      ;x position
	cmp al,1				;check if at start of screen
	ja move_left
	mov byte [bx+0], 39		;reset to start
	jmp endl
move_left:
	dec al
	mov byte [bx+0],al
	jmp endl
right_helico:
	mov al,byte [bx+0]	     ;x position
	cmp al,39				;check if at end of screen
	jb move_right
	mov byte [bx+0], 1		;reset to start
	jmp endl
move_right:
	inc al
	mov byte [bx+0],al
	;jmp endl
endl:
	add bx,reclen			;next helico 
	inc cl					;loop counter
	jmp helicoloop
exit1:
	ret
	
	
;============================
collision:
  mov bx, fire				; get position of fire
  mov ax, word [bx+0] 		; row, column
  mov bx, helico  			; get position of helicos
  xor cl,cl		  			;loop counter
next_helico:
	cmp ax, word [bx+0]	      ;collision with this  helico?
	je coll     			; Yes, 
	add bx,reclen			;No, next helico
	inc cl					;loop counter
	cmp cl,helicocnt	 	 ;last helico?
	je quit ; yes, quit
	jmp next_helico

coll: 
      mov al, ' '   		; test if the helico is already shoted
      cmp al, byte [bx+3]
      je quit				; yes quit
      mov byte [bx+3], ' '	; else, delete helico
      mov bx,fire			;fire array
      mov byte [bx+3], ' '	; delete fire
      inc byte [score]		; 
      ;or byte [score], 0x30  ; convert bin to decimal (0 à 9 seulement) 

quit:
      ret
;====================================
;Draw the screen
;This will get run each time through the game loop

drawscreen:
    ; print score
    mov si, score_str
    call print_string
    mov al, byte [score]
    mov ah, 9
    int 10h
    
	;draw objects in array 

	xor cl,cl				;loop counter
	mov bx,helico	     	;object array
screenloop:
	cmp cl,obcnt			;last object?
	je exit2
	mov dx, word [bx+0]	      ;x and y values (as word)
	call move_cursor
	mov si,bx				;print graphic
	add si,3				;graphic offset
	call print_string
	add bx,reclen			;next object
	inc cl					;loop counter
	jmp screenloop
exit2:
	ret

;--------------------------------------------
; INPUT:  DH = row, DL= column
; to move cursor we use functions of ATMEGA8 VGA adapter
move_cursor:
  or dh, 0xE0	; 
  mov ah, 9
  mov al, dh
  int 10h
  
  or dl, 0x80
  mov al, dl
  int 10h
  ret
;===========
print_string:
	lodsb				; Get character from string into AL
	or al, al			; AL= 0 ?
	jz BON	      		 ; yes,  char is zero (end of string) then return
	mov ah, 9			; else, print caracter 
	int 10h
	
	jmp print_string	; next caracter
BON:
	ret  
;=================
delay:
    loop delay	
    ret

;=============
cls:
  mov al, 05
  mov ah, 01
  int 10h
  ret
;------------------------
behav	db 0					;will inc 1 each time thru game loop
score db 0x30	 				; score = 0 (ascii)
score_str db ' SCORE = ', 0


helico: 
  ;   x, y,  behavior,  graphic,term, direction
  db 00, 5, 00010100b, 0x26, 0,    0  
  db 40, 7, 01000100b, 0x2A, 0,	  1 
  db 40, 8, 00000010b, 0x2B, 0,	 1
canon:
  db 20, 24, 00000000b, 'A', 0,  0
fire:
  db 20, 23, 00000000b, ' ', 0,  0  
   
