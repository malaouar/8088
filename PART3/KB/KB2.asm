
; TEST KEYBOARD  using NMI interrupt.
; CLK ---> A6
; Data ---> A7
; LED1 on B7, LED2 on B6
; 7 segment common anode display on PORTC
;=========================================
    rom_size  = 2000h ;  8K octet EPROM 
    rom_start = 100000h - rom_size
    rom_empty = 0FFh
;=========================================
    use16
;-----------------------------------------
VAR = 00040h	      ; variable in RAM to store the break code


start:
	; init stack pointer
	xor ax,ax		; AX=0000
	mov ss,ax		; SS=0000  
	mov ds,ax		; DS=0000
	mov es,ax		; ES= 0000
	mov ax,7C00h
	mov sp,ax		; 

	; init 8255
	mov al,0x90		; MODE0, PORTA input, PORTB output, 
					; PORTC  output
	out 0x03,al
	
	cli		 		;disable interrupts

;--------------------------------       
	; init interrupt vector type 2 (NMI)
	mov word[02h*4], NMI		; formula for this is "Interrupt*4 = offset"
	mov word[02h*4+2], cs 		; we must put our segment after the offset


;------------------
	; We start with some LED animation!
	mov bl,3
led:
	mov al,07Fh		;  0111 1111 = LED1 ON
    out 01h,al		;  set LED1
	
    mov cx,0FFFFh
delay1:
    loop delay1  
	
	mov al,0BFh		;  1011 1111 = LED2 ON
    out 01h,al		;  set LED2
	
    mov cx,0FFFFh
delay2:
    loop delay2 
	dec bl
	;jnz led
	
	mov al,0FFh    	; Turn the two LEDs OFF
	out 0x01, al
	out 0x02, al	; 7seg OFF
	mov di,  0040h
	
	mov byte [VAR], 0	  ;  reset break variable
	
;================================
KB:   
	hlt				; wait for NMI
	or bl, bl		; BL=0?
	jz KB			; Yes, ignor
	mov al, bl		; No, display scan code
	out 0x02, al

	jmp KB			; loop 
	
;==================================
; receive scan code of pressed Key
NMI:				
	; we discard first bit
	mov ah, 8			; 8 bits to read
	xor bl, bl			; BL stors the received byte

WH:	  ; wait CLK to set
	in al, 0x00	
	test al, 0x40		;  CLK? CLK ---> A6=0100 0000
	jz WH				; if CLK =0 wait
	
WL:  ; wait CLK to fall
	in al, 0x00	;
	test al, 0x40		;  CLK 1 or 0? CLK ---> A6=0100 0000
	jnz WL				; if CLK =1 wait

	; Read bit
	test al, 0x80		; Data 1 or 0? data ---> A7 = 1000 000
	jz K1				; Data = 0 
	stc					; Data = 1 ---> set FC: C=1
	jmp K2
K1:
	clc					; Clear FC: C=0
K2:
	rcr bl, 1			;  rotate right throw carry ---> BL7 = C ---->  C=BL0=0
	dec ah				; 8 bits received? 
	jnz  WH 			; No next bit
;------------
	; traiting received code
	cmp bl, 0xF0	  	    ; break code?
	jnz K3					; No, jmp  to K3
	mov byte [VAR], 0xA5  	; Yes, set break variable
	jmp Kx					; return
K3:
	cmp byte [VAR], 0xA5 	; the previous code is it break?
	jnz K4					; No, 
	cmp bl, 0xE0	  	    ; Yes, this code is Extended ?
	jz Kx					; Yes, return
	mov byte [VAR], 0		; No, reset break variable
	jz Kx					; Return        
	
K4:
	cmp bl, 0xE0	      	;  Extended  key?
	jz Kx		   			; Yes , return BL=0
	jmp Ky	       			; No, return BL=CODE	
Kx:
	xor bl, bl
Ky:
	iret					; Yes, return BL= DATA 
	
;===============================================
	
   times rom_size - 16 - $ db 0xFF 
;-----------------------------------------
boot:		  ; reset : FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF	
