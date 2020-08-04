; TEST KEYBOARD without using interrupt.
; CLK ---> A6
; Data ---> A7
; LED1 on B7, LED2 on B6
; 7 segment common anode display on PORTC
;=========================================
    rom_size  = 2000h ;  8K  EPROM 
    rom_start = 100000h - rom_size
;=========================================
    use16
;-----------------------------------------

start:
	; init stack pointer
	xor ax,ax		; AX=0000
	mov ss,ax		; SS=0000  
	mov ds,ax		; DS=0000
	mov es,ax		; ES= 0000
	mov ax,0800h
	mov sp,ax		; 

	; init 8255
	mov al,0x90		; MODE0, PORTA input, PORTB output, 
					; PORTC  output
	out 0x03,al

;------------------
	; We start with some LED animation!
	mov bl,3
led:
	call LED1
	
    mov cx,0FFFFh
delay1:
    loop delay1  ; 
	
	call LED2
	
    mov cx,0FFFFh
delay2:
    loop delay2 
	dec bl
	jnz led
	
	mov al,0FFh    
	out 0x01, al	; Turn the two LEDs OFF
	out 0x02, al	; 7seg OFF
	
;================================
KB:   
	call Rec_Key   ; get scan code
	
	mov al, bl
	out 0x02, al   ; display it 

	; wait a moment
    mov cx,0FFFFh
delay3:
    loop delay3 

	jmp KB			; loop forever
	
		
;---------------------------------------------  
; receive scan code of pressed Key
Rec_Key:			
	;wait until CLK falls to 0
	call WL
	; we discard first bit
	mov ah, 8		; 8 bits to read
	xor bl, bl		; BL stors the received byte

B:
	call WH
	call WL
	; Read bit
	in al, 0x00	;
	test al, 0x80		; Data 1 or 0? data ---> A7 = 1000 000
	jz A				; Data = 0 
	inc bl				; Data = 1
A:
	shl bl, 1			;  shift right throw carry ---> BL7 = C
	dec ah				; 8 bits received? 
	jnz  B				; No next bit
	ret					; Yes, return BL= DATA 
	
WL:  ; wait CLK to fall
	in al, 0x00	;
	test al, 0x40		;  CLK 1 or 0? CLK ---> A6=0100 0000
	jnz WL				; if CLK =1 wait
	ret					; CLK =0  then return   
	
WH:	  ; wait CLK to set
	in al, 0x00	;
	test al, 0x40		;  CLK? CLK ---> A6=0100 0000
	jz WH				; if CLK =0 wait
	ret					; CLK =1  then return   
	
;--------------------------------------------------     
LED1:
	mov al,07Fh		;  0111 1111 = LED1 ON
    out 01h,al		;  set LED1
	ret
	
LED2:
	mov al,0BFh		;  1011 1111 = LED2 ON
    out 01h,al		;  set LED2
	ret

;===============================================
	
   times rom_size - 16 - $ db 0xFF 
;-----------------------------------------
boot:		  ; reset vector  : FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF 
	