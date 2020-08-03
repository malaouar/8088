
; Hardware connection of 3410 LCD to PORTC: 
;        DC ----> C1, CE ---> C3, CLK ---> C4 
;		 RST --> C5,  Data ---> C7,
;=========================================
    rom_size  = 2000h ;  8K byte ROM 
    rom_start = 100000h - rom_size
;=========================================
use16
;========================================

start:
	; init segment pointers
	xor ax,ax		; AX=0000
	mov ss,ax		; SS=0000  
	mov ds,ax		; DS=0000
	mov ax,7FFFh	; Bottom of stak at 7FFFh (32K)
	mov sp,ax		; 
	
	; init 8255
	mov al,0x92		; MODE0, PORTA input, PORTB input, 
					; PORTC output
	out 0x03,al
	
;---------------------------------------
	; Reset 3410 :
	mov al,0x26   	; 00101000  ==> CE=1, RST=1
	out 0x02,al
	; wait a moment:
	mov cx,0x0FFF
delay:
    loop delay
	mov al, 0x0A	; RST on portC C5 "0000 101 0" = A                      
	out 0x03,al		; RST = 0
	; wait a moment:
	mov cx,0x0FFF
delay1:
    loop delay1	
	inc al			; al= "0000 101 1" = B
	out 0x03,al		; RST = 1

	; Intialize LCD:	
	mov ah, 0x21		; Function set - extended commands set
	call	Send_Command
	mov ah, 0xC8		; Set Vop  (contrast) = 3.06 + 0.06*66 = 7.02 V
	call	Send_Command
	mov ah, 0x06		; Set Temp coefficent
	call	Send_Command
	mov ah, 0x13		; Set bias system, MUX = 1:48
	call	Send_Command
	mov ah, 0x20		; Function set -Standard Commands,Horizontal addressing mode
	call	Send_Command
	mov ah, 0x0C		; Display control - normal mode
	call	Send_Command

;******************
; Clear LCD Device
	mov ah, 0x80		;  X adress = 0
	call	Send_Command
	mov ah, 0x40		; Y adress = 0
	call	Send_Command
	
; write zeros to LCD's RAM:
	mov cx,918
	mov ah, 0
clr_loop:
	call	Send_Data
	dec cx
	jnz clr_loop
	
	
;**************************************
; Write some stuff to LCD
	mov ah, 0x80		;  X adress = 0
	call	Send_Command
	mov ah, 0x40		; Y adress = 0
	call	Send_Command

; write lettre 'H' on the screen
	mov ah, 255
	call Send_Data
	mov ah, 255
	call Send_Data
	mov ah, 0x18
	call Send_Data
	mov ah, 0x18
	call Send_Data
	mov ah, 0x18
	call Send_Data
	mov ah, 0x18
	call Send_Data
	mov ah, 255
	call Send_Data
	mov ah, 255
	call Send_Data
	
; Goto position 32, 5  and write 'L'
	mov ah, 5  ; row
	mov bh, 32  ; column
	call gotoXY

	mov ah, 255
	call Send_Data
	mov ah, 255
	call Send_Data
	mov ah, 0xC0
	call Send_Data
	mov ah, 0xC0
	call Send_Data
	mov ah, 0xC0
	call Send_Data
	mov ah, 0xC0
	call Send_Data
	mov ah, 0xC0
	call Send_Data
	mov ah, 0xC0
	call Send_Data
	
; wait a moment
	call delayn

; invert video mode:
	mov ah, 0x0D
	call Send_Command

; wait a moment
	call delayn
 
; Normal mode:
	mov ah, 0x0C
	call Send_Command
	
	
; STOP:
	hlt
;=========================================================
; subroutine to send one byte (in AH) via bit-banging SPI
;=========================================================
Send_Data:
	mov al,0x03		; D/C is connected to C1: byte to send on command port
					; to set C1 high is "0000 001 1" = 03
    out 0x03,al 	 ; DC=1
	jmp send

Send_Command:		; byte to send is in AH
	mov al,0x02		; D/C is connected to C1: byte to send on command port
					; to clear C1 is "0000 001 0" = 02 
    out 0x03,al 	 ; DC = 0
send:
	mov al,0x06		; CE is connected to C3 byte to send on command port
					; to clear C3  is "0000 011 0" = 06
    out 0x03,al 	 ; CE=0	
	mov bl,08h	; 8bits to send
next:
	test ah,0x80	; MSBit  1 or 0??
	jz A			; if MSBit =0  jmp to A
	mov al, 0x0F		;  Else,  MSBit =1 then  Set C7(MOSI) : 0000 111 1
	jmp B
A:
	mov al, 0x0E	; Clear C7 (MOSI): 0000 111 0
B:
	out 0x03,al		;  write Mosi (must be on portC C7) : change MOSI without changing CLK and CS	 
	shl ah,1		; left shift 1 bit = Next bit to send
	mov al,0x09		; CLK is connected to C4: byte to send on command port
					; to set C4 high is "0000 100 1" = 09 
    out 0x03,al 	 ;  CLK = 1
	nop
	nop
    dec al			; we decrement AL then AL = "0000 100 0" --> C4=0
    out 0x03,al 	 ; CLK =0

	dec bl		; next bit
	jnz next	; if not last bit we continue sending bits	
	mov al,0x07		; CE is connected to C3: byte to send on command port
					; to setC1  is "0000 11 1" = 07
    out 0x03,al 	 ; CE=1
	ret

;===============================================
; Move cursor to position (X, Y): bh= X, ah= Y 
; X= 0 to 95,  Y= 0 to 7        
gotoXY:
	or ah, 0x40   ; row
	call Send_Command
	or bh, 0x80		; column
	mov ah, bh
	call Send_Command
	ret
;==========================
delayn:
	mov dl, 10
attend:
	mov cx, 0xFFFF
delay2:
    loop delay2
	dec dl
	jnz attend
	ret
	
;=========================
	
   times rom_size - 16 - $ db 0xFF 
;-----------------------------------------
reset:		  ;  reset vector : FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF 
	