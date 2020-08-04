NOTE: the connections of SD Card and Keyboard may be different from previous experiments.
      look at bios.asm
	  
How to:	  
- Format an SD Card in FAT16 format.
- Assemble "boot.asm" and raw copy the generated "boot.bin" binary to the first sector of the SD.
- Copy "kernel.bin" to the root of the SD.
- assemble the apps and copy their binaries to the root of the SD.
- Boot the SBC. The first sector (bootloader) will be loaded in RAM by the BIOS.
- Then the bootloader searchs for the file "kernel.bin" and load it into RAM.
- kernel starts a minimal CLI which prints ">" and waits for commands:
  * CLS: clear screen
  * LS or DIR: prints the contents of the SD (subdirs are not supported).
  * Enter the name of an app to start it.
- The app "helico" is a simple game where we shoot on planes.
  F1 and F2 keys to move the canon.
  F3 to shoot
  ESC to quit.
- ASCII app: prints the ascii chars.