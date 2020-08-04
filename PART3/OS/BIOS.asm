; SD CARD: MOSI ---> C7, CLK ---> C4, CS ---> C5, MISO ---> C1

; VGA:  D0-D7 ---> A0-A7, CLK----> C6, BUSY ---> C0

; LED1 on B7, LED2 on B6

;  KEYBOARD:  CLK ---> C2,  Data ---> C3

;=========================================
    rom_size  = 2000h       ;8K bytes EPROM : 2000h = 1FFF +1
    rom_start = 100000h - rom_size  ;100000h = FFFFF + 1
;=========================================
    use16
;-----------------------------------------
; Common SD Card commands:
CMD0 = 040h
CMD1 = 041h
CMD9 = 049h
CMD10 = 04Ah
CMD16 = 050h
CMD17 = 051h
CMD24 = 058h
;-----------------------------------------
VAR = 00400h	    ; variable in RAM to store the break code
VAR2= 00401h	    ; variable in RAM to store the scan code
;==============================================
start:
  ; Initialisation:
	xor ax,ax		; AX=0000
	mov ds,ax		;DS=0000
	mov es,ax		; ES= 0000
	
	; init stack pointer
	mov ax,7000h	; SS=7000  (last 64K byts of 512k)
	mov ss,ax		; ss=7000h
	mov ax,0FFFFh	; top of RAM 512K
	mov sp,ax		; Start  of stak pointer at top of RAM: 7000x16+FFFF= 7FFFFh 

	; init 8255
	mov al,0x83		; MODE0, PORTA output, PORTB input, 
					; PORTC:C4-C7 output, PORTC:C0-C3 input
	out 0x03,al
	
	
;--------------------------------       
	; init interrupt vector type 2 (NMI)
	mov word[02h*4], NMI 	; formula for this is "Interrupt*4 = offset"
	mov word[02h*4+2], cs 	; we must put our segment after the offset
;--------------------------------       
	; init interrupt vector type 10 (VIDEO)
	mov word[010h*4], INT10 
	mov word[010h*4+2], cs 
;--------------------------------       
	; init interrupt vector type 13 (SD Card)
	mov word[013h*4], INT13
	mov word[013h*4+2], cs 
;--------------------------------       
	; init interrupt vector type 16 (KEYBOARD)
	mov word[016h*4], INT16
	mov word[016h*4+2], cs 
;==============================
main:
 
  ; Goto line 22 :
  call Ready		; Is VGA ready?
  mov al, 0xF6		; 111 10110
  out 0x00, al	 	; send to portA
  call CLCK
  
; Print  "  Please hit any key to continue...":
  mov si, msg5  	 ; Put string position into SI
  call AFFICH


; wait until a key is pressed:
  hlt
  hlt
  
; initialize SD CARD
SD_INIT:
  call INIT_SD 

;----------------------
  ; Clear screen  
  mov al,0x05	; 
  out 00h,al 	; send to portA
  call CLCK 
	
		;-----------------
; Goto line 1 :
  call Ready		; Is VGA ready?
  mov al, 0xE1		;  111 00001
  out 0x00, al	  	; send to portA
  call CLCK
  
  ; display message6: "SD Card found."
	mov si, msg6	; Put string position into SI
	call AFFICH

; wait a moment
    mov cx,07FFFh
    call delay
    call delay
    call delay
    call delay

  
	;display message7: "Loading first sector ....."
	mov si, msg7		; Put string position into SI
	call AFFICH
; wait a moment
    mov cx,07FFFh
    call delay
    call delay
    call delay
    call delay

	;------------------------       
	; read first sector   from SD (BOOTLOADER)
	mov ah, 2		; function= Read
	mov al,1		; Nbr of sectors to read or write
	mov cx, 0  		; first sector  to read
	mov bx, 7C00h	; start of buffer in RAM to copy in or read from (here bootloader to 0000:7C00)
	int 13h 		; Read sector

  
	;  display message8: "First sector loaded."
	mov si, msg8	; Put string position into SI
	call AFFICH
; wait a moment
    mov cx,07FFFh
    call delay
    call delay
    call delay
    call delay

	mov si, 7C00h
	mov ax, word [si + 0x1FE]  ; Get the last two bytes
	cmp ax, 0xAA55	 	; BOOTABLE SD?
	je continue			; Yes
	 
	; No then display message2: "NOT A BOOTABLE SD !!"
	mov si, msg4		; Put string position into SI
	call AFFICH
; wait a moment
    mov cx,07FFFh
    call delay
    call delay
    call delay
    call delay
		
  
	; No then display message9: "Change card and hit any key when ready."
	mov si, msg9		; Put string position into SI
	call AFFICH
; wait a moment
    mov cx,07FFFh
    call delay
    call delay
    call delay
    call delay

	hlt 			; wait until a key is pressed after inserting a bootable card
	nop
	jmp  SD_INIT 	; try again
	
;---------------------
; Bootable SD
  
	; No then display message10: "Starting bootloader ..."
	mov si, msg10		; Put string position into SI
	call AFFICH
; wait a moment
    mov cx,07FFFh
    call delay
    call delay
    call delay
    call delay

; Start bootloader      
continue:	
	jmp 0000:7C00h		;  go to our program

;---------
; we should not arrive here !! 
NO_BOOT: 
	hlt
	nop
	jmp NO_BOOT   	; loop forever 



;================================================
;read one sector 
; Inputs:  DI : Offset where to load sector in RAM
;          CX: sectors's No
;=================================================
SD_RdSector:
    mov bh, 0x64			; We try  writing 100 times !!

RD_sect:
	mov	ah,CMD17			; CMD17: read one sector
	call	Send_Cmd		; execute command
	cmp ah,0		    	; response=0?
	je BON					; good we continue
	dec bh					; finised 100 times?
	jnz RD_sect				; No, then CMD17 again

err1:
  ; Clear screen  
  mov al,0x05	 
  out 00h,al 		; send to portA
  call CLCK 

; Goto line 3:
  call Ready		; Is VGA ready?
  mov al, 0xE3		; 111 00011
  out 0x00, al	  	; send to portA
  call CLCK
  
	; display  message2: "Read ERROR !!"	
	mov si, msg2  	 ; Put string position into SI
	call AFFICH	  
	hlt		

BON:
	; SD chip select
	mov al,0x0A		; CS is connected to C5: byte to send on command port
					; to reset C5 low is "0000 101 0" = 0A 
	out 0x03,al	 	; turn CS LOW

	mov bh, 0x64	; We try  receiving 0xFE tag 100 times !!
encore:
	call RD_SPI
	cmp	ah, 0xFE	; The response is 0xFE
	jz C			; Yes, well continue
	dec bh			; No, try again
	jz err1 		; fini 100 trys? Yes,  then error
	jmp encore		; No try again
		
C:		; Tag 0xFE received

	; we copy the sector in 0000:7C00 
	mov si, 512
		
read_loop:
	call	RD_SPI		; Read byte from MMC
	mov [di], ah		;  AH --> buffer
	inc di
	dec	si				; 512 bytes received?
	jnz	read_loop		; No, then again

	; Yes
	call RD_SPI			; write CRC =0xFF (in SPI mode by default CRC is not cheked)
	call RD_SPI			; write CRC

	; set CS high
	mov al,0x0B			; CS is connected to C5: byte to send on command port
						; to set C5 high is "0000 101 1" = 0B
	out 0x03,al	 		; turn CS high
		     
	call RD_SPI	  		; one empty SPI cycle
	ret		
;====================================================
; write sector
; Inputs:  SI : Offset where to read sector from RAM
;          CX: sectors's No
;====================================================
SD_WrSector:

    mov bh, 0x64		; We try  writing 100 times !!
again:	
	mov	ah,CMD24		;CMD24
	call	Send_Cmd	;  execute command
	cmp ah,0		    ; response=0?
	je OK				; good we continue
	dec bh				; finised 100 times?
	jnz again		    ; No, then CMD24 again

    ; alas,  error writing sector !! 
	; Clear screen  
  mov al,0x05			; 
  out 00h,al 			; send to portA
  call CLCK 

; Goto line 3:
  call Ready			; Is VGA ready?
  mov al, 0xE3		   	; 111 00011
  out 0x00, al	  		; send to portA
  call CLCK
  
	; display : "Write ERROR !!"	
	mov si, msg3   		; Put string position into SI
	call AFFICH	  
	hlt		 
						
OK:		     
	; SD chip select
	mov al,0x0A			; CS is connected to C5: byte to send on command port
						; to reset C5 low is "0000 101 0" = 0A 
	out 0x03,al	 		; turn CS LOW
	mov	ah,0xFE 		; send tag
	call	WR_SPI
	; write one sector        
	mov di, 512
		
write_loop:				      
	mov ah,[si]	  		; buffer --> AH
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
		     
	call RD_SPI	   		; one empty SPI cycle
	ret		
;=================================
; subroutine to initialize SD card
;=================================
INIT_SD:
	; set CS high
	mov al,0x0B		
	out 0x03,al	 		; turn CS high	
	mov bh,10		 	; + 74 empty spi clocks ( 10x8)
loop1:			
	call RD_SPI
	dec	bh
	jnz	loop1
		
; -------------------

	mov	ah,CMD0 	    ; CMD0 set card in idle state
	xor	cx,cx	  		; all parameters = 0
	call Send_Cmd		
	cmp	ah,0x01 	   	; compare with 1  (card in idle state)
	je	WELL		  	; response =1, then OK
	jmp INIT_error		; else error   

WELL:
	mov	ah,CMD1 	    ; CMD1 initialize operating conditions
						; all parameters = 0 (we have already cl=ch=0 when CMD0 is sent)
	call Send_Cmd				 
	cmp	ah, 0		    ; response =0? (0  No errors) 
	je SEND_CMD16		; Yes, continue
	jmp WELL			; else, we send CMD1 again and again!

SEND_CMD16:
	mov	ah,CMD16		    ; CMD16  Set MMC block length 
	mov	cl,2			    ; arg [8..15]
							; 2 = 512 bytes per sector (00 00 02 00 = 512) we have ch=0 already
	call	Send_Cmd	
	cmp ah,0				; response=0?
	je	INIT_OK 			; Yes, then OK
				
INIT_error:	
	; Clear screen  
  mov al,0x05		; 
  out 00h,al 		; send to portA
  call CLCK 

; Goto line 3:
  call Ready		; Is VGA ready?
  mov al, 0xE3		;  111 00011
  out 0x00, al	  	; send to portA
  call CLCK
  
	; display message2: "SD CARD ERROR !!"	
	mov si, msg1 	; Put string position into SI
	call AFFICH
	hlt   			; stop          
								
INIT_OK:		
    ret			    ; INIT OK
	
;===================================
; send a command  to SD
; Inputs: ; CL, CH -> params, AH -> cmd
;====================================

Send_Cmd:					
		
	; SD chip select
	mov al,0x0A		
	out 0x03,al	 ; turn CS LOW

	; start sending		
	call	WR_SPI		; send the command in AH		
	mov	ah,0			; send arg [24..31]
	call	WR_SPI		; 
	mov	ah,ch			; send arg [16..23]
	call	WR_SPI		; 
	mov	ah,cl			; send  arg [8..15]
	call	WR_SPI		; 
	mov	ah,0			; send  arg [0..7]
	call	WR_SPI		; 
	mov	ah,0x95 		; CRC
	call	WR_SPI		; 	
	mov	bh, 10			; wait for card response (wich comes between 0 and 10 bytes)
get_resp:	 
	call	RD_SPI	; 
	test	ah,080h 	; bit 7 of response =1?
	jz	cmdok	       	; No (bit7=0) then we have a response
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
; subroutine to Write/Read one byte via SPI
;==========================================
RD_SPI:
	mov ah,0xFF
WR_SPI: 			; byte to send is in AH
	mov bl,08h		; 8bits to send
next:
	test ah,0x80	; MSBit  1 or 0??
	jz A			; if MSBit =0  jmp to A
	mov al, 0x0F	;  Else,  MSBit =1 then  Set C7(MOSI) : 0000 111 1
	jmp B
A:
	mov al, 0x0E	; Clear C7 (MOSI): 0000 111 0
B:
	out 0x03,al		;  write Mosi (must be on portC C7)
	 
	shl ah,1		; left shift 1 bit = Next bit to send

	; we read MISO BEFOR sending pulse on CLCK
	in al,0x02		; MISO is connected on C1
	TEST AL, 02H	; test C1: 0000 0010
	jz MISO_L		; if MISO = 0 jump
	inc ah			; else  MISO high then set D0 of AH : AH0=1
	
MISO_L:
	mov al,0x09		; CLK is connected to C4: byte to send on command port
					; to set C4 high is "0000 100 1" = 09 
    out 0x03,al 	 ; set CLK HIGH
    nop
    dec al			; we decrement AL then AL = "0000 100 0" --> C4=0
    out 0x03,al 	; turn CLK LOW

	dec bl			; next bit
	jnz next		; if not last bit we continue sending bits
	ret				; return, read byte is in AH


;==================================
; VGA subroutines
;==================================  
CLCK:  
    mov al,0x0D 		; CLK is connected to C6: byte to send on command port
						; to set C6 high is "0000 110 1" = 0D
    out 0x03,al 		; set CLK HIGH
    dec al				; we decrement AL then AL = "0000 110 0" --> C6=0
    out 0x03,al 		; turn CLK LOW
    ret
;---------------------------------
Ready:
	;  read Busy signal coming from VGA
	in al,0x02			; Busy is connected to C0
	TEST AL, 01H		; test C1: 0000 0001
	jz OK1		 		; if Busy = 0 return
	jmp Ready  			; else wait
OK1:
  ret
;-----------------------------------
AFFICH:
	call Ready			; Is VGA ready?
	cs lodsb			; Get character from string into AL (here we use CS:SI instead of DS:SI)
	; we use CS to load from CS:SI instead of DS:SI
	or al, al			; AL= 0 ?
	je FIN2 			; If char is 0, end of string
	out    0x00, al  	; send to portA
	call CLCK   		; send to VGA
	jmp AFFICH
FIN2:
  ret	


;********************************
;   INTERRUPTS SERVICE ROUTINES
;********************************
;=========================================== 
; Keyboard:  get scan code of pressed Key
;===========================================
NMI:			
	PUSH	DS
	PUSH	DX
	PUSH	AX
	
	; we discard first bit
	mov ah, 8			; 8 bits to read
	xor dl, dl			; DL stors the received byte

WH:	  ; wait CLK to set (CLK on C2)
	in al, 0x02			; read portC
	test al, 0x04		;  CLK? CLK ---> C2=0000 0100
	jz WH				; if CLK =0 wait
	
WL:  ; wait CLK to fall
	in al, 0x02	;
	test al, 0x04		;  CLK 1 or 0? CLK ---> C2=0000 0100
	jnz WL				; if CLK =1 wait

	; Read bit: data on C3   
	test al, 0x08		; Data 1 or 0? data ---> C3 = 0000 1000
	jz K1				; Data = 0 
	stc					; Data = 1 ---> set FC: C=1
	jmp K2
K1:
	clc					; Clear FC: C=0
K2:
	rcr dl, 1			;  rotate right throw carry ---> DL7 = C ---->  C=BL0=0
	dec ah				; 8 bits received? 
	jnz  WH 			; No next bit
;----------------
	xor ax, ax
	mov DS, ax  		 ; DS=0

	; traiting received code
	cmp dl, 0xF0	    ; break code?
	jne K3				; No, jmp  to K3
	mov byte [VAR], 0xA5  ; Yes, set break variable
	jmp Kx				; return with [VAR2] = 0
K3:
	mov al, byte [VAR]
	cmp al, 0xA5  		; the previous code is it break?
	jne K4				; No, jump
	mov byte [VAR], 0	  ; Yes, reset break variable
	jz Kx				; Return  with [VAR2] = 0     
	
K4:
	mov byte [VAR2], dl	; No,  [VAR2]=SCAN CODE
	jmp Ky
		
Kx:
	mov byte [VAR2], 0
	
Ky:
	POP	AX
	POP	DX
	POP	DS
	iret					; return, [VAR2] = scan code or 0 
;=====================
; VIDEO
;======================
INT10:	
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI
	PUSH	DS
	PUSH	ES
	PUSH	BP

	or ah, ah		;  AH= 0? 
	jz CLR_LCD		; yes, then clear LCD
	cmp ah, 1		; Function: send command to LCD
	jz V1
	cmp ah, 0x09	; print caracter AL= Code ascii , BL= attribute
	jz  V1
	cmp ah, 0x0E	; print caracter AL= Code ascii , BL= attribute
	jz  V1
	jmp ret10

; Clear screen
CLR_LCD:
  call Ready	; Is VGA ready?
  mov al,0x05	; clear screen command
  out 00h,al 	; send to portA
  call CLCK   	; send to VGA

  jmp ret10
		
V1:   ; Data or command in  AL
  push ax    		; save data/command cause we use AL in "Reday" subroutine
  call Ready		; Is VGA ready?
  pop ax
  out	0x00, al   	; send to portA
  call CLCK   		; send to VGA
	
ret10:
	POP	BP
	POP	ES
	POP	DS
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP BX
	iret
;===============================
; DISK (SD) operations:
    ;AL = number of sectors to read/write (must be non zero)
    ;Cx = Sector to start read/write from  (in LBA form not CHS) 
    ;BX = RAM buffer pointer.
;==========================
INT13:		
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI
	PUSH	DS
	PUSH	ES
	PUSH	BP

	; Wich function ?
	or ah,ah		; AH= 0 (INIT)?
	jz INIT 		; Yes
	dec ah 			; skip ah =1
	dec ah			; AH =2 Read
	jz Rd_Sect		; Yes
	dec ah			; AH = 3  (Write)
	jz Wr_Sect		; Yes
	jmp ret13		; Else return
	
;---------------------------------
INIT:
	call INIT_SD 
	cmp ax, 0xAA55
	je SD_ERR
	xor ah, ah		; IBM
	clc				; idem
	jmp ret13
SD_ERR:
	mov ax, 0006h 	; NO SD
	stc		 
	jmp ret13
;---------------------------------
Rd_Sect:
  mov di, bx	 		 ; BX is the offset wher we save data but BL is used in SPI fonction R/W one byte so we must use an other index
	call Cal_NrSect 	; calculate the equivalent sector on SD

RS:
	call SD_RdSector
	shr cx,1
	inc cx
	shl cx,1
	dec dl		     	; next sctor
	jnz RS		      	; done? No, again
	jmp ret13			; Yes,  return
;-------------------------------
Wr_Sect:
  mov si, bx
	call Cal_NrSect 	; calculate the equivalent sector on SD
	
WS:
	call SD_WrSector  ; decomment this later
	shr cx,1
	inc cx
	shl cx,1
	dec dl		       ; next sctor
	jnz WS		       ; done? No, again
	
ret13:		; RESTORE STATE
	POP	BP
	POP	ES
	POP	DS
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP BX			    
	iret

;-----------------------
Cal_NrSect:
	; save registres   
	mov dl, al	    ; save the of sectors to read or write in DL 

	; sector address 4 bytes:  MSByte=0  (always in our case)  , CH-CL = No of sector x2,  LSByte=0 (always),
	; sector address = [ No of sector X2(one  left shift) + 00 at right]  
	; or make 9  left shift of  sector Nbr to get the 4 byts of address
	; exemple sector No 3 ==> address= 00 00 06 00
	shl cx,1		;  shift to lift --> CX= CX x 2   
		;  No of scector after shift :     CH = MSByte,  CL=  LSByte 
	ret


;========================
; Keyboard  functions
;=========================
INT16:
	PUSH	BX
	PUSH	DS
	; Wich function ?
	or ah,ah			; AH= 0 (wait for key)?
	jz get_key 			; Yes
	dec ah      		; ah= 1? get code (don't wait)
	jz get_code
	jmp ret16	 		; No, return

get_key:
	hlt					; wait for NMI (keyboard interrupt)
get_code:
	xor ax, ax
	mov DS, ax   		; DS=0 (we need VAR2)
	mov bl, byte [VAR2]	; bl= scan code
	
	; ------
	; convert scan code to ascii 
	mov ax, CS  		; get current segment for translation
	mov DS, ax  		; 
	mov ah, bl			; ah= scan code

	mov al, bl			; 
	mov bx, tabl		; conversion table offset (address= DS:BX)
	xlatb				; AL= ASCII code

ret16:
	POP	DS
	POP	BX			    
	iret				; return (ah= scan code, AL= ASCII code) OR AX=0

;=================
delay:
    loop delay	
    ret
	 
;==============

; message1: 
msg1 db 'SD CARD ERROR !!' , 13, 0

; message2: 
msg2 db 'Read ERROR !!' , 13, 0

; message3: 
msg3 db 'Write ERROR !!' , 13, 0	

; message4: 
msg4 db 'NOT A BOOTABLE SD !!' , 0 

; message5: 
msg5 db '  Please hit any key to continue...', 0

; message6: 
msg6 db 'SD Card found.', 13, 0

; message7: 
msg7 db 'Loading first sector .....', 13, 0

; message8: 
msg8 db 'First sector loaded.', 13, 0

; message9: 
msg9 db 'Change card and hit any key when ready.', 13, 0

; message10: 
msg10 db 'Starting bootloader ...', 13, 0 


;=============================================================================

; Table to convert scan codes to ASCII using  xlatb  instruction:
      ; to get ascii from scan code
      
      ;  mov al, "scan code" ; vaue of AL is used as offset in the table
      ;  mov bx, tabl        ; BX= address of table
      ;  xlatb               ; contents of address (BX + AL) --> AL


tabl:
    ;    0      1     2     3     4     5     6      7     8    9     A      B    C     D     E    F
	db 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00  ; 0
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x41, 0x31, 0x00, 0x00, 0x00, 0x57, 0x53, 0x51, 0x5A, 0x32, 0x00  ; 1
	db 0x00, 0x43, 0x58, 0x44, 0x45, 0x34, 0x33, 0x00, 0x00, 0x20, 0x56, 0x46, 0x54, 0x52, 0x35, 0x00  ; 2
	db 0x00, 0x4E, 0x42, 0x48, 0x47, 0x59, 0x36, 0x00, 0x00, 0x00, 0x4D, 0x4A, 0x55, 0x37, 0x38, 0x00  ; 3
	db 0x00, 0x00, 0x4B, 0x49, 0x4F, 0x30, 0x39, 0x00, 0x00, 0x2E, 0x00, 0x4C, 0x00, 0x50, 0x00, 0x00  ; 4
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x00, 0x00  ; 5
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; 6
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; 7
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; 8
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; 9
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; A
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; B
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; C
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; D
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; E
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  ; F


;===============================================================================	
   times rom_size - 16 - $ db 0xFF 
;-----------------------------------------
boot:		  ; reset vector: FFFF0
    jmp (rom_start shr 4):start
;-----------------------------------------
    times rom_size - $ db 0xFF 


