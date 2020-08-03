
; In this program we copy one sector to another
; We use the PC and an SD card reader/writer to write data to a sector on the SD
; MOSI ---> C7, CLK ---> C4, CS ---> C5
; MISO ---> C0
; LED1 on B7, LED2 on B6
;=========================================
    rom_size  = 2000h ;  8K byte ROM 
    rom_start = 100000h - rom_size
;=========================================
    use16
;-----------------------------------------
; Common SD commands
CMD0 = 040h
CMD1 = 041h
CMD16 = 050h
CMD17 = 051h
CMD24 = 058h
CMD9 = 049h
CMD10 = 04Ah

;---------------
start:
	; init segment pointers
	xor ax,ax		; AX=0000
	mov ss,ax		; SS=0000  
	mov ds,ax		; DS=0000
	mov ax,0800h
	mov sp,ax		; Start of stak at 800h 

	; init 8255
	mov al,0x91		; MODE0, PORTA input, PORTB output, 
					; PORTC:C4-C7 output, PORTC:C0-C3 input
	out 0x03,al
	
	; LEDs OFF
	mov al,0xFF
	out 0x01, al
	
;-------------------------
main:
;------------------------
	; First give a sign of life (Blink LEDs 3 times)
	mov bl,3
led:
	call LED1	
    mov cx,0FFFFh
delay1:
    loop delay1   
	
	call LED2	
    mov cx,0FFFFh
delay2:
    loop delay2 
	dec bl
	jnz led
	; at this satge LED1 is OFF and LED2 ON
	
;------------------------------------------
; we read sector 3 and write it to sector 4
SD:   
	; initialize SD CARD
	call INIT_SD 
	
	; read sector 3
	; sector adress : CL=0 (always), DH-DL-CH = Nbr of sector x2

    xor cl, cl	 	; CL=0 always
    mov ch, 0x06	; sector adress = [ Nbr of sector X2(one  left shift) + 00 at right]    
					; here sector No 3 ==> adress= 00 00 06 00
					; or make 9  left shift of  sector Nbr to get the 4 byts of adress
    xor dx, dx		; DL=DH=0 
 
	
	call SD_RdSector

	;  write to sector 4
	mov ch, 0x08
	call SD_WrSector
	
	; success?
	mov al,03Fh    ; Turn the two LEDs ON
	out 0x01, al
	
	hlt				; STOP processor

;--------------------------------------------------     
LED1:
	mov al,07Fh		;  0111 1111 = LED1 ON, LED2 OFF
    out 01h,al		;  set LED1
	ret
	
LED2:
	mov al,0BFh		;  1011 1111 = LED2 ON, LED1 OFF
    out 01h,al		;  set LED2
	ret
	
;=====================================================
;read one sector
;===============
SD_RdSector:
    mov bh, 0x64		; We try  writing 100 times !!

RD_sect:
	mov	ah,CMD17		;CMD17: read one sector
	call	Send_Cmd	;  execute command
	cmp ah,0		    ; response=0?
	je BON				; good we continue
	dec bh				; finised 100 times?
	jnz RD_sect			; No, then CMD24 again

err:
    mov al, 0x7F		; Yes, alas,  error reading sector !! 
	out 0x01, al 		; LED1 ON 	  
	hlt		

BON:
	; SD chip select
	mov al,0x0A			; CS is connected to C5: byte to send on command port
						; to reset C5 low is "0000 101 0" = 0A 
	out 0x03,al	 		; turn CS LOW

	mov bh, 0x64		; We try  receiving 0xFE tag 100 times !!
encore:
	call RD_SPI
	cmp	ah, 0xFE	; The response is 0xFE
	jz C			; Yes, well continue
	dec bh			; No, try again
	jz err			; fini 100 trys? Yes,  then error
	jmp encore		; No try again
		
C:		; Tag 0xFE received

	; we copy the sector in 0000:7C00 
	mov si, 512
	mov di, 7C00h
		
read_loop:
	call	RD_SPI			; Read byte from MMC
	mov [di], ah		  	;  AH --> buffer
	inc di
	dec	si					; 512 bytes received?
	jnz	read_loop			; No, then again
	; Yes
	call RD_SPI			; write CRC =0xFF (in SPI mode by default CRC is not cheked)
	call RD_SPI			; write CRC

	; set CS high
	mov al,0x0B			; CS is connected to C5: byte to send on command port
						; to set C5 high is "0000 101 1" = 0B
	out 0x03,al	 		; turn CS high
		     
	call RD_SPI	   		; one empty SPI cycle
	ret		

;===========================================================
; write one sector
;=============
SD_WrSector:
    mov bh, 0x64			; We try  writing 100 times !!

again:	
	mov	ah,CMD24		  	; CMD24: write one sector
	call	Send_Cmd		; execute command
	cmp ah,0		    	; response=0?
	je OK					; good we continue
	dec bh					; finised 100 times?
	jnz again		  		; No, then CMD24 again	
    mov al, 0x7F			; alas,  error writing sector !! 
	out 0x01, al 			; LED1 ON 	  
	hlt				
	
OK:		     
	; SD chip select
	mov al,0x0A			; CS is connected to C5: byte to send on command port
						; to reset C5 low is "0000 101 0" = 0A 
	out 0x03,al	 		; turn CS LOW
	mov	ah,0xFE 		; send tag
	call	WR_SPI

	; copy the sector saved at 7C00h to the given sector	  
	mov di, 512  		; Nr of bytes
	mov si, 7C00h		; start of buffer in RAM
		
write_loop:
						; buffer --> AH
	mov ah,[si]
	call	WR_SPI		; write byte to MMC
	inc si
	dec	di				; 512 bytes sent?
	jnz	write_loop		; No, then again
	; Yes
	call RD_SPI			; write CRC =0xFF (in SPI mode by default CRC is not cheked)
	call RD_SPI			; write CRC

	; set CS high
	mov al,0x0B			; CS is connected to C5: byte to send on command port
						; to set C5 high is "0000 101 1" = 0B
	out 0x03,al	 		; turn CS high
		     
	call RD_SPI	  		; one empty SPI cycle
	ret		

;===================================
; subroutine to initialize SD card
;=================================
INIT_SD:
	; set CS high
	mov al,0x0B		
	out 0x03,al	 		; turn CS high	
	mov bh,10		  	; send + 74 empty spi clocks (here 10 x 8 = 80)
loop1:			
	call RD_SPI
	dec	bh
	jnz	loop1
		
; -------------------
	mov	ah,CMD0 	  	   ; CMD0 set card in idle state
	xor	dh,dh			   ; all parameters = 0
	xor	dl,dl
	xor	ch,ch
	xor	cl,cl
	call Send_Cmd		
	cmp	ah,0x01 	  		; compare with 1  (card in idle state)
	je	WELL		  		; response =1, then OK
	jmp INIT_error			; else error   


WELL:
	mov	ah,CMD1 		    ; CMD1 initialize operating conditions
							; all parameters = 0 (we have already cl=ch=dl=dh=0 when CMD0 is sent)
	call Send_Cmd				 
	cmp	ah, 0		  	    ; response =0? (0  No errors) 
	je SEND_CMD16			; Yes, continue
	jmp WELL				; else, we send CMD1 again and again!
SEND_CMD16:
	mov	ah,CMD16		    ; CMD16  Set MMC block length 
	mov	ch,2			    ; arg [8..15]
							; 2 = 512 bytes per sector (00 00 02 00 = 512) we have cl=dl=dh=0
	call	Send_Cmd	
	cmp ah,0				; response=0?
	je	INIT_OK 			; Yes, then OK
				
INIT_error:			
	; error? then turn LED2 ON
	mov al, 0xBF		
	out 0x01, al		; LED2 ON : initialization error
	hlt   				; stop          						
		
INIT_OK:		
    ret				    ;  INIT OK
	
;==========================
; send a command
;======================

Send_Cmd:					; CL, CH,DL,DH - params, BH - cmd		
	; SD chip select
	mov al,0x0A		
	out 0x03,al	 		; turn CS LOW

	; start sending		
	call	WR_SPI		; send the command		
	mov	ah,dh			; send arg [24..31]
	call	WR_SPI		; 
	mov	ah,dl			; send arg [16..23]
	call	WR_SPI		; 
	mov	ah,ch			; send  arg [8..15]
	call	WR_SPI		; 
	mov	ah,cl			;send  arg [0..7]
	call	WR_SPI		; 
	mov	ah,0x95 		; CRC
	call	WR_SPI		; 	
	mov	bh, 10			; wait for card response (wich comes between 0 and 10 bytes)
get_resp:	 
	call	RD_SPI		; 
	test	ah,080h 	;  bit 7 of response =1?
	jz	cmdok	       	;No (bit7=0) then we have a response
	dec	bh		 		; else wait for response
	jnz	get_resp
		
cmdok:	
    push ax				; save AH (don't loose card response)	   
	; CS=1 (after each  command turn CS to 1 and send 8 impulsions)
	mov al,0x0B		
	out 0x03,al	 		; turn CS high
	; one (empty) SPI write cycle
	call	RD_SPI		; 
	pop ax	 			; get AH (the response)		
	ret

;==========================================
; subroutine to read/write one byte via sPi
;==========================================
RD_SPI:
	mov ah,0xFF
WR_SPI: 			; byte to send is in AH
	mov bl,08h		; 8bits to send
next:
	test ah,0x80	; MSBit  1 or 0??
	jz A			; if MSBit =0  jmp to A
	mov al, 0x0F		;  Else,  MSBit =1 then  Set C7(MOSI) : 0000 111 1
	jmp B
A:
	mov al, 0x0E	; Clear C7 (MOSI): 0000 111 0
B:
	out 0x03,al		;  write Mosi (must be on portC C7)	 
	shl ah,1		; left shift 1 bit = Next bit to send

	; we go read MISO BEFOR sending pulse on CLCK !!!!
	in al,0x02		 ; MISO is connected on C0
	TEST AL, 01H	; test C0: 0000 0001
	jz MISO_L		 ; if MISO = 0 jump
	inc ah			; else  MISO high then set D0 of AH : AH0=1
	
MISO_L:
	mov al,0x09		; CLK is connected to C4: byte to send on command port
					; to set C4 high is "0000 100 1" = 09 
    out 0x03,al 	 ; set CLK HIGH
    dec al			; we decrement AL then AL = "0000 100 0" --> C4=0
    out 0x03,al 	 ; turn CLK LOW

	dec bl		; next bit
	jnz next	; if not last bit we continue sending bits
	ret			; return, read byte is in AH
	
	
;=========================
	
   times rom_size - 16 - $ db 0xFF 
;-----------------------------------------
reset:		  ; reset vector : FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF 
	