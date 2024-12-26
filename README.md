# GB2AS

Rudimentary assembler for the Gameboy. Without the bells & whistles

## About

> [!IMPORTANT]
> This assembler is currently only suitable for writing small test programs and booting 
> without boot rom

I wrote this in about 4 days because I needed an assembler for my gameboy emulator, and
needed an assembler that supported starting at offset 0 (no bootrom), because I haven't 
implemented that in my emulator yet. I also decided to write it because it was a smaller
project that I thought I actually could finish in a few days, without letting 
perfetionism and procrastination take over.

## Syntax
```asm
    ; This is a comment
Label:
    add a,b             ; opcode [arg][,arg]
    ld a,0x50           ; literals
    jp Label            ; use of labels
    ld (0x16),a         ; specifying memory locations
    ld a,(0xff00+16)    ; last page addressing
```

## Caveats

 - Currently only writes machine code starting from offset 0, (i.e) no bootrom
 - Doesn't support any sort of literal arithmetic, labels in local jumps
 - No support fort preprocessor or constants
 - No way of writing data/bytes into the binary

 ## Features
 - [x] Full support for all instructions
 - [x] Labels
 - [ ] Ability to change starting offset
 - [ ] Line numbers in errors
 - [ ] Literal arithmetic, some sort of optimization, labels in relative jumps, etc.
 - [ ] Preproccessor language
 - [ ] Simple linking of some sort
