Intel 8088 based Single Board Computer (SBC).

The goal of this repo is to share my emperiments with the 8088 microprocessor.

Why the 8088?
It's the uP at the heart of the first PC in history produced by IBM in 1981 (PC5150) and it's successor the PC-XT.
It was a powerfull (by the creteria of early 80's) 8bits uP with internal 16bits functionality (8086 compatible).
So it's well documented and we can find easily a lot of resources on the web (docs, assemblers, compilers ....).
The drawback is the relative complexity of the circuit (compared to other uPs like 6809) due to address/data multiplexing and the need for an external Clock generator circuit.

I've built a simple board using the 8088 in MIN MODE @ 4.77MHz with only a couple of ICs:
   - 8284 Clock generator.
   - Two 74LS374 for address demultiplexing.
   - a 7404 for address decoding.
   - a 8255 PPI.
   - 8K ROM  (AT28C64B CMOS).
   - 512K SRAM  (BS62LV4006 CMOS). 

I've used FASM as assembler and a TOP2005 programmer to flash code in ROM.

DONE:   
- I've successfuly interfaced to this board: 2x16 LCD, Nokia 3410 LCD, SD card, PS2 keyboard .....
- I've also built an ATMEGA8 based 640x480 VGA adaptor to draw text on any VGA compatible screen.
- I've writen a simple BIOS like firmware implemeting some functions like writing on screen (int10), SD card access (int13), keyboard (int??) ...
 At startup it initializes the 8255 then searchs for a bootable SD card, loads the first sector at 0x7C00 like the PC does.

TO DO:
- Use an AVR as a clock generator + interrupt controller + Timer ...
- Build a better VGA adapter (using a more powerfull microcontroller like Blue Pill).

Voilà !
for any suggestions: malaouar67@gmail.com



=========================================================
Resources:

http://helmpcb.com/electronics/8088-computer

http://www.cs.binghamton.edu/~reckert/sbc.htm

http://technologyinterface.nmsu.edu/fall98/electronics/zargari/zargari.html

http://sasteven.multics.org/8088page.html

http://neazoi.com/microwave/giannopk/computer.htm

http://www.malinov.com/Home/sergeys-projects/sergey-s-xt

http://kaput.retroarchive.org/8088.html

http://www.8051bits.com/misc-projects/rit-8088/rit-8088.html

http://www.retroarchive.org/dos/docs/


* Build your own computer : how to construct an 8088 based single board microcomputer
	Walter Fuller

* The 8088 Project Book 
Robert Grossblatt

* THE 80X86 IBM PC and Compatible Computers: Assembly Language, Design and Interfacing 
Muhammad Ali Mazidi
