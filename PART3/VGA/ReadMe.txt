
 An ATMEGA8 is used to generate VGA signals in 640x480 mode and display text with auto scroll up. 
 This work is inspired by many projects in the web:
 
http://sbc.rictor.org/vid2c.html
http://tinyvga.com/avr-vga

Flash the given "vga.hex" (lfuse=0x3f , hfuse=0xc0).
 Use a 16 MHZ crystal
 
For test purpose no need of the 7474 IC or the connection to the 8088 SBC, just connect it to a VGA monitor, u'll get some thing like this on the screen:

	 SEB-SEB 
 (c) Mahmoud LAOUAR

If OK add the 7474 to the circuit and connect it to the SBC.

------------------------------------------------------------------- 
 
  PORTC:    
          C1 = input for DATA Ready signal from host (ouput of D flip-flop)
          C5= output for clearing D flip-flop (Busy signal from generator to host) 
  PORTB:    B3(MOSI)  = Video, feeds 470 Ohm  resistor to R, G, or B (or both)
            B0: VSync 
            B1(OC1A): HSync
            
  PORTD:  Received DATA/command from host
  
7474:
  - Pin1 (CLR) --> C5 (ATMEGA8)
  - Pin3 (CLK) --> HOST 
  - Pin5 (Q)   --> Both C1 (ATMEGA8) + HOST 
  - Pin2 (D) and Pin4 (PRE) --> +5V
  
 When the SBC wants to send Data/Command to VGA adaptor it puts the byte on PORTA and sets CLK High --> Q = H, then ATMEGA8 can read the sent byte.
 When ATMEGA8 finishes processing Data/Command it pulls CLR down  --> Q= L then SBC can send new Data/Command.
 
input data/commands:
- 000  xxxxx  (input < 0x20) :
   0x08  Back space 
   0x0D  return
   0x05  CLS 
   0x0A  LineFeed  = return
   0x01  SCROLLUP
   0x02  SCROLLDOWN
   0X03  Enable cursor
   0x04  Diasable cursor
- 0 01xxxxx to 0 1111111  ASCII chars
- 10  xxxxxx  --> Goto clumn X
- 111 yyyyy   --> Goto line Y