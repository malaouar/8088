;-------------------
; Displays ASCII Characters
;---------------------

	use16
	ORG 32768
;=========================
;start:

  ; clear screen
 call CLS
  
  ; print some thing
	mov si, msg1
	call print_string

  call new_line  ; 
	
	;-------------------
	
  mov bl, ' '  ; space
next_char:
  inc bl
  mov al, bl
  call print_char
  cmp bl, 0x7F
  jne next_char

  call new_line  ; 
  call new_line
  mov si, msg2
  call print_string

  ;wait_for_key
  hlt
  
  ret

;==================
new_line:
	mov al, 13
	mov ah , 01
	int 10h
	ret
;=========
CLS:
	mov al, 5
	mov ah , 01
	int 10h
	ret
;=========
print_char:
	mov ah , 09
	int 10h
	ret

;============   
print_string:
	lodsb			; Get character from string into AL
	or al, al		; AL= 0 ?
	jz BON	      	 ; yes,  char is zero (end of string) then return
	mov ah, 9		; else, print caracter 
	int 10h
	
	jmp print_string      ; next caracter
BON:
	ret

;-----------------------------------------------
	msg1 db 'ASCII character set: ', 13,  0
	msg2 db 'hit any key to exit ...',   0

