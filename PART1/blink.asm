
; This program blinks 2 LEDs one on port B the other on port C
; connect LEDs to any pin of the port
; 8255 : PORTA= 00    PORTB=01    PORTC=02      PORT COMMAND=03


;=========================================
    rom_size  = 2000h 				; we have 8K byte EEPROM 
    rom_start = 100000h - rom_size
;=========================================
    use16			; Tell FASM to generate 16bits code
;-----------------------------------------
start:
	mov al, 80h  	; initialize the 8255 to MODE0 and all ports as outputs
	out 03h, al		; write value to command port
	
	mov al, 0		; AL = 0
next:
    out 01h, al  	; portB = 0
	not al			; AL = 0xFF
	out 02h, al 	; portC = 0xFF
	
	; wait a moment
    mov cx, 0x7FFF	; arbitrary value for delay
delay:
    loop delay  	; LOOP instruction decrements CX
	
    jmp next   		; loop forever 
	

;-----------------------------------------
    times rom_size - 16 - $ db 0xFF	; fill empty space with 0xFF (NOT the last 16 bytes)
;-----------------------------------------
reset:             ;  reset vector: FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF	; on remplit le rest vide du 16 octets
	
	
	
	