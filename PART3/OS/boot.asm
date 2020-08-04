
;************************************************************************************
;       A Simple FAT16 Bootloader
; 2 FATs of 9 sectors and a 224 entries root directory
; cluster size is 1 sector
; This program browse the root directory for "KERNEL.BIN" and load it to 0000:0500h
;*************************************************************************************

use16						; we are in 16 bit real mode

org	7C00h					; This code is loaded at 0000:7C00h     

start:	jmp	main			; jump to start of bootloader
		nop					; BPB Begins 3 bytes from start.
							; If you use a short jump, add a "nop" after it to offset the 3rd byte.
							;  or do a far jump, which is 3 bytes in size.

;*********************************************
;       BIOS Parameter Block  "BPB"
;*********************************************
bpbOEM				db "My OS   "			; OEM identifier (Cannot exceed 8 bytes!)
bpbBytesPerSector:	DW 512	  				; sector size
bpbSectorsPerCluster:	DB 1				; sectors per cluster
bpbReservedSectors:	DW 1					; Only the boot sector
bpbNumberOfFATs:	DB 2
bpbRootEntries: 	DW 224
bpbTotalSectors:	DW 0
bpbMedia:			DB 0xf8  
bpbSectorsPerFAT:	DW 9
bpbSectorsPerTrack:	DW 18
bpbHeadsPerCylinder:	DW 2
bpbHiddenSectors:	DD 0
bpbTotalSectorsBig:	DD 131070
bsDriveNumber:		DB 0
bsUnused:			DB 0
bsExtBootSignature:	DB 0x29
bsSerialNumber: 	DD 0xa0a1a2a3
bsVolumeLabel:		DB "MOS FLOPPY "
bsFileSystem:		DB "FAT16   "

;**********************************************

main:
  ; Clear screen  
  mov al,0x05	; 
  out 00h,al ; send to portA
  call CLCK 

; Goto line 1 :
  call Ready		; Is VGA ready?
  mov al, 0xE1		; 111 00001
  out 0x00, al	  	; send to portA
  call CLCK

; Loading message ............
	mov si, ld_msg ; Put string position into SI
	call AFFICH

;load FAT into memory

	mov cx,1		; First copy of FAT  satrts at sector Nbr 1
	mov ah,2    	; function read
	mov al,9		; FAT length is 9 sectors
	mov bx,3FFFh	; we load FAT at 0000:3FFFh
	int 13h

;load root 
	mov si, msg_dot ; Put string position into SI
	call AFFICH
 
	mov cx,19		; ROOT dir starts at sector 19
	mov ah,2
	mov al,14		; and it is 14 sectors length
	mov bx,500h		; we load ROOT Dir  at 0000:0500h
	int 13h
	
; display one dot
	mov si, msg_dot ; Put string position into SI
	call AFFICH

; browse root directory for "KERNEL.BIN"

	mov	  cx, 224		;  loop counter(Nbr of entries)
	mov	  di, 500h		; locate first root entry
Next_Entry:
	push	  cx
	mov	  cx, 0x000B	; eleven characters name
	mov	  si, FileName	; File name to find
	push	  di	 	 ; save DI we use it next instruction ( comparaison of strings)
  rep  cmpsb			; Repeat test for entry match: compare SI string to DI string
	pop	  di
	je	  found 		; file found
	pop	  cx
	add	  di, 0x0020	; ( di +32) = queue next directory entry
	loop	  Next_Entry  ; LOOP instrcution decrements CX, so if CX =/= 0  we continue searching
	jmp	  not_found		; Else, file not found
	  
found:
; display one dot
	mov si, msg_dot ; Put string position into SI
	call AFFICH

	; di = entry of our file, so  we add 1Ah to point first byte (byte 26) of file's first cluster Nbr
	mov ax, word [di + 1Ah] ; get Nbr of first cluster (word)
	mov bx, 500h			; we load our file at 0000:0500h

load_file:
	push ax    		; save current cluster number
	call CLUST_LBA	; convert cluster Nbr to LBA
	mov ax,0201h	; read 1 sector: ah=02 --> function read, al=01 --> 1 sector
	int 13h 		;load cluster (sector) into memory
	add bx,512		; offset of next sector into memory
	
	; compute next cluster Nbr
	;In FAT16 2 bytes hold the clusters Nr
	
	pop ax			; recover current cluster number
	shl ax, 1   	; multipy by 2      
	mov si,ax		;
	mov dx, word [si+3FFFh]  ; get word at that address: 3FFF adress of FAT in RAM

	mov ax,dx		;  ax = Nbr of next cluster 
	cmp ax,0FF7h	;if > 0FF7h, end reading
	ja DONE
	jmp load_file
  
DONE:
	mov si, msg_dot ; Put string position into SI
	call AFFICH

	mov ax, 050h	; DS=CS   Tiny Model
	mov ds, ax
	jmp 0050h:0000	 ;jump to 500h (where the file was loaded)


not_found:
; Goto line 3 :
  call Ready		; Is VGA ready?
  mov al, 0xE3		; 111 00011
  out 0x00, al	 	; send to portA
  call CLCK

	mov si, err_msg ; Put string position into SI
	call AFFICH

	hlt
	
;---------------------------    
CLUST_LBA:  
	add ax, 31		; In our case cluster = 1 sector, so:
		 ; Nbr of (logical) sector =  cluster + 31  (first cluster (Nr 2) his first sector = 1+ 2x9 + 14 = 33) 
		 ; where: 1=bootsector,  2x9= 2Fats 9 sectors each , 14= root directory length = (224 entries X 32) / 512
	mov cx, ax
	ret

;==================================
; VGA subroutines
;==================================  
CLCK:  
    mov al,0x0D 	; CLK is connected to C6: byte to send on command port
					; to set C6 high is "0000 110 1" = 0D
    out 0x03,al 	; set CLK HIGH
    dec al			; we decrement AL then AL = "0000 110 0" --> C6=0
    out 0x03,al 	; turn CLK LOW
    ret
Ready:
	;  read Busy signal coming from VGA
	in al,0x02		; Busy is connected to C0
	TEST AL, 01H	; test C1: 0000 0001
	jz OK1		 	; if Busy = 0 return
	jmp Ready  		; else wait
OK1:
  ret
;======================
AFFICH:
	call Ready			; Is VGA ready?
	lodsb				; Get character from string into AL
						; we use CS to load from CS:SI instead of DS:SI
	or al, al			; AL= 0 ?
	je FIN2 			; If char is 0, end of string
	out    0x00, al   	; send to portA
	call CLCK   		; send to TV
	jmp AFFICH
FIN2:
  ret
;----------------------------
FileName db 'KERNEL  BIN'
ld_msg db 'loading OS .', 0
msg_dot db '.', 0
err_msg db 'no kernel found !!', 0

    
	TIMES 510-($-$$) DB 0
	DW 0xAA55
