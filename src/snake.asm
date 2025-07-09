; NES ROM Header - tells emulator/hardware about the ROM
.segment "HEADER"
.byte 'N', 'E', 'S', $1a      ; "NES" followed by MS-DOS EOF marker
.byte $02                     ; 2 x 16KB PRG-ROM banks
.byte $01                     ; 1 x 8KB CHR-ROM bank
.byte $00, $00                ; Mapper 0, no special features



; Zero Page Variables
; These variables reside in the first 256 bytes of RAM ($0000-$00FF)
; and can be accessed more quickly by the CPU.
.segment "ZEROPAGE"

; Controller input variables
current_button_state:   .res 1 ; Stores the ID of the currently pressed button (1-8)
previous_button_state:  .res 1 ; Stores the ID of the previously pressed button
button_just_pressed:    .res 1 ; Stores the ID of the button that was just pressed
button_just_released:   .res 1 ; Stores the ID of the button that was just released

; Temporary variables
temp_var1:            .res 1 ; Temporary variable 1
temp_var2:            .res 1 ; Temporary variable 2

.segment "RODATA"
bit_masks:
  .byte %00000001 ; A
  .byte %00000010 ; B
  .byte %00000100 ; Select
  .byte %00001000 ; Start
  .byte %00010000 ; Up
  .byte %00100000 ; Down
  .byte %01000000 ; Left
  .byte %10000000 ; Right

; Main program code section
.segment "CODE"

; Interrupt Request Handler - called when IRQ interrupt occurs
.proc irq_handler
  RTI                     ; Return from interrupt (we don't use IRQ)
.endproc

; Non-Maskable Interrupt Handler - called during VBlank
.proc nmi_handler
  RTI                     ; Return from interrupt (not using NMI yet)
.endproc

; Reset Handler - called when system starts up or resets
.proc reset_handler

  ; === CPU Initialization ===
  SEI                     ; Set interrupt disable flag (ignore IRQ)
  CLD                     ; Clear decimal mode flag (NES doesn't support BCD)

  ; === APU Initialization ===
  LDX #$40                ; Load X with $40
  STX $4017               ; Write to APU Frame Counter register
                          ; Disables APU frame IRQ

  ; === Stack Initialization ===
  LDX #$FF                ; Load X with $FF (top of stack page)
  TXS                     ; Transfer X to Stack pointer ($01FF)

  ; === PPU Initialization ===
  INX                     ; Increment X (now $00)
  STX $2000               ; PPUCTRL = 0 (disable NMI, sprites, background)
  STX $2001               ; PPUMASK = 0 (disable rendering)
  STX $4010               ; DMC frequency register = 0 (disable DMC)

  ; === Wait for PPU to be ready ===
  BIT $2002               ; Read PPUSTATUS to clear VBlank flag

  ; First VBlank wait - PPU needs time to stabilize
  vblankwait:
    BIT $2002               ; Read PPUSTATUS register
    BPL vblankwait          ; Branch if Plus (bit 7 = 0, no VBlank)
                            ; Loop until VBlank flag is set

    ; Second VBlank wait - ensures PPU is fully ready
  vblankwait2:
    BIT $2002               ; Read PPUSTATUS register again
    BPL vblankwait2         ; Branch if Plus (bit 7 = 0, no VBlank)
                            ; Loop until second VBlank occurs

    JSR load_palette        ; Load palette colors
    JSR load_background     ; Load background tiles
    JMP main                ; Jump to main program

.endproc

; Load palette data
.proc load_palette

    ; Set PPU address to palette RAM
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    ; Load background palette (4 colors)
    LDA #$01                ; Color 1
    STA $2007

    LDA #$29                ; Color 2
    STA $2007

    LDA #$1A                ; Color 3
    STA $2007

    LDA #$28                ; Color 4
    STA $2007

    RTS

.endproc

; load_background: Clears the nametable and then draws a specific tile.
.proc load_background

  ; Set PPU address to start of nametable 0 ($2000) for clearing
  LDA #$20
  STA $2006               ; High byte of $2000
  LDA #$00
  STA $2006               ; Low byte of $2000

  ; Clear the screen (fill nametable 0 with tile ID 0)
  LDX #$04
  LDY #$00
  LDA #$00

  clear_loop:
    STA $2007
    INY
    BNE clear_loop
    DEX
    BNE clear_loop

    RTS                     ; Return from subroutine

.endproc

; Main program logic
.proc main

  ; == Enable Rendering ===
  LDA #%00001110          ; Enable background rendering properly
                          ; bit 3 = 1: Show background
                          ; bit 2 = 1: Show background in leftmost 8 pixels
                          ; bit 1 = 1: Show background everywhere
  STA $2001               ; Write to PPUMASK register

  JSR init              ; Initialize variables and state

  ; === Main Loop ===
  forever:

    JSR update

    JMP forever             ; Stay here

.endproc

.proc init

  LDA #$00

  STA current_button_state
  STA previous_button_state
  STA button_just_pressed
  STA button_just_released

.endproc

.proc update

  JSR handle_input

.endproc

.proc handle_input

  JSR read_controller

  ; Check up button just pressed
  LDA button_just_pressed
  AND #%00010000
  BEQ skip_up_button_just_pressed

  ; Up button just pressed
  ; INC temp_var1

  skip_up_button_just_pressed:

    ; Check down button just pressed
    LDA button_just_pressed
    AND #%00100000
    BEQ skip_down_button_just_pressed

    ; Down button just pressed
    ; DEC temp_var1

  skip_down_button_just_pressed:

    ; Check left button just pressed
    LDA button_just_pressed
    AND #%01000000
    BEQ skip_left_button_just_pressed

    ; Left button just pressed
    ; DEC temp_var2

  skip_left_button_just_pressed:

    ; Check right button just pressed
    LDA button_just_pressed
    AND #%10000000
    BEQ skip_right_button_just_pressed

    ; Right button just pressed
    ; INC temp_var2

  skip_right_button_just_pressed:

    ; Do next checks

  RTS

.endproc

.proc read_controller

  LDA current_button_state
  STA previous_button_state

  ; Read controller state
  ; Controller 1 is at $4016 ( controller 2 at $4017)
  ; Strobe the controller to reset the state
  ; Writing $01 to $4016 seems to reset it to first button (a, b, select, start, up, down, left, right)
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016

  LDX #$00        ; Bit index (0-7)
  STA current_button_state ; Clear previous button state

  read_controller_loop:

    LDA $4016
    AND #$01        ; Mask to get button press
    BEQ no_press

    ; Set the corresponding bit in current_button_state
    LDA current_button_state
    ORA bit_masks, X
    STA current_button_state

  no_press:
    INX
    CPX #$08
    BNE read_controller_loop

  ; Compute button_just_pressed = current & ~previous
  ; (button that is pressed this frame but not last)
  LDA previous_button_state
  EOR #$FF
  AND current_button_state
  STA button_just_pressed

  ; Compute button_just_released = previous & ~current
  ; (button that was pressed last frame but not this)
  LDA current_button_state
  EOR #$FF
  AND previous_button_state
  STA button_just_released

  RTS

.endproc

; Interrupt vectors - tells CPU where to jump for each interrupt
.segment "VECTORS"
.addr nmi_handler         ; NMI vector ($FFFA-$FFFB)
.addr reset_handler       ; Reset vector ($FFFC-$FFFD)
.addr irq_handler         ; IRQ vector ($FFFE-$FFFF)

; Character ROM data (graphics patterns)
.segment "CHARS"
.incbin "assets/snake.chr"

; Startup segment
.segment "STARTUP"
