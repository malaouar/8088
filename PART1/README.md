Minimal 8088 based Board.

 Only 6 ICs: 8088 + 8284, PPI 8255, 7404, EEROM and one address latch (373),  NO RAM.

The simplified schematic is given in photo "schem1.jpg". 
more details in "schem2.jpg" and "schem3.jpg".

- Address lines A0-A8 are demultiplexed using 74373 latch.
- ADDRESS and DATA buses are not buffered.
- We use IO/M line to select the ROM or the PPI by pulling the CE pin low:
  if IO/M is LOW the ROM is selected.
  if it is HIGH the PPI is selected (through the inverter).

- The circuit must be powered by a regulated 5V supply @ 0.5A or more.

SOFTWARE:

Assemble these programs using FASM free assembler.


PROGRAM1- blink.asm: a program blink LEds connected to PORTA and PORTB.
  The program, in an infinite loop, sets PORTA (all pins) LOW and PORTB HIGH then waits a moment, after that it inverts the operation (PORTA high and PORTB low).
  
  we can connect the LEDs to any pin of the ports.


PROGRAM2- LCD.asm: prints "hello world" on an 2x16 LCD module.


TIP1:  How you can call subroutine without RAM  on the board??

what call does?

it stores address of next instruction in location pointed by SP and jumps to subroutine.
RET jumps to address pointed by SP.
so, what we have to do to simulate CALL with no RAM?
store somewhere address of next instruction, before "call" load into SP pointer to that address and JUMP to subroutine.

Code:
    ...
here_we_call_sub:
    mov sp,continue_after_return_addr
    jmp subroutine    
continue_after_return_addr:
    dw continue_after_return
continue_after_return:
    ...

just note: unlike real call you can not use nested calls with this technique.

Example:
-------------
   ...
   mov sp,@F
   JMP subr
@@:
   dw @F
@@:
   ...   


subr:
    xor ax,ax
    ret
------------------


TIP2:  8255 has a special control word to set a separate bit on port C:

	0000NNNV (binary format)

NNN: bit Number (0 to 7)
V : value (0 or 1)

example: to set or clear C2 (all other bits of port C stay unchanged)
To set C2 write 0x05 in control register of 8255. 
	0x05 = 0000-010-1 ==> bit number 2, value = 1  

To clear C2 write 0x04 in control register of 8255.
	0x04 = 0000-010-0 ==> bit number 2, value = 0

Note: 0x04 = 0x05 - 1 (so we can simply decrement 0x05).


=========================================================
Resources:

http://helmpcb.com/electronics/8088-computer


* The 8088 Project Book by Robert Grossblatt
 
