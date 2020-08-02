
; 2x16 LCD module connected to the 8088 SBC via 8255 PIO:
; - D4-D7 are connected to portC C4-C7
; - E is connected to PORTC C0
; - RS is connected to portC C1
; 8255 : PORTA= 00    PORTB=01    PORTC=02      PORT COMMAND=03
;=========================================
    rom_size  = 2000h 			;  8K byte EEPROM 
    rom_start = 100000h - rom_size
;=========================================
    use16
;-----------------------------------------
start:
	; init 8255
	mov al, 80h  ; MODE 0 and  all ports as outputs
	out 03h, al ; send on command port

	  ;  wait  >15mS
    mov cx,07FFFh  ; ?? s
delay1:
    loop delay1
	;----------------------------------------------
	
	mov al, 020h ; mode 4bits
    mov sp,@F
	jmp lcd_command
@@:
	dw @F
@@:	
	
	;	At this point, the normal 4 wire command routine can be used 
	;---------------------------------------------------------------------------------
	; Now we send the command "28h"  Function set, 4 wire, 2 lines, 5x7 font
	mov al, 028h ; 
	mov sp,@F
	jmp lcd_command
@@:
	dw @F
@@:
	;---------------------------------------------------------------------------------
	; Now we send the command "0Ch"   = Display on, cursor off,  blink off
	mov al, 0x0C ; 
	mov sp,@F
	jmp lcd_command
@@:
	dw @F
@@:
	;---------------------------------------------------------------------------------
	; Now we send the command "06h"   = Address increment, no scrolling
	mov al, 0x06 ; 
	mov sp,@F
	jmp lcd_command
@@:
	dw @F
@@:
	
	;---------------------------------------------------------------------------------
	; Now we send the command "01h"  Clear LCD command

	mov al, 0x01 ; 
	mov sp,@F
	jmp lcd_command
@@:
	dw @F
@@:

	
;============================================= 
; first message
msg db "Hello WORLD!", 0
	mov si, msg	; Put string position into SI
next:
	lodsb			; Get character from string into AL
	cmp al, 0
	je done			; If char is zero, end of string
	mov sp,@F
	jmp lcd_data
@@:
	dw @F
@@:
	
	jmp next

done:
	hlt
;========================================
lcd_command:
    xor bl,bl ;  Equivalent to clear BL  (mov bl,0)  --> set RS=0
    jmp lcd_write
lcd_data:
    mov bl,2 ; set RS=1
lcd_write:
    mov ah,al 		;  Pseudo PUSH  (sotre  AL in AH) : al - byte to send
    and al,0xF0		; Clear Low nible
    or   al,bl		; set RS to 1 or 0 (this depend on BL)
    out 0x02,al		;send to portC
    mov al,0x01		; E is connected to C0 to set E to 1 we write "0000 000 1" in portC !  (8255 datasheet)
    out 0x03,al   	; turn E on
    dec al
    out 0x03,al   	; turn E off
    mov al,ah		; POP AL
    mov cl,4		; 4 = shift AL four times
    shl al,cl		; Shift left 4 times
    and al,0xF0		; Clear Low nible
    or   al,bl		; set RS to 1 or 0
    out 0x02,al		;send to portC
    mov al,0x01; 
    out 0x03,al   	; turn E on
    dec al
    out 0x03,al   	; turn E off
	
	;   wait 5mS
	mov cx,0FFFh
delay2:
    loop delay2
    ret
	
;-----------------------------------------
    times rom_size - 16 - $ db 0xFF ; on remplit l'espace vide par FFh sauf les derniers 16 octets
;-----------------------------------------
reset:             ; reset vector: FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF ; on remplit le rest vide du 16 octets
	
	
	