  NAME    main
        PUBLIC  main
        SECTION .text : CODE (2)
        THUMB

;A port - buttons
PIOA_WPMR EQU  0x400E0EE4       ; Write Protect Mode Register

PIOA_PER  EQU  0x400E0E00      ; PIO Enable Register
PIOA_ODR  EQU  0x400E0E14      ; Output Disable Register
PIOA_IDR  EQU  0x400E0E44      ; Interrupt Disable Register  
PIOA_PUER EQU  0x400E0E64      ; Pull Up Enable Register
PIOA_PDSR EQU  0x400E0E3C      ; Pin Data Status Register


;Port A - Interrupts

PIOA_IER EQU 0x400E0E40         ; Interrupt Enable register
PIOA_FELLSR EQU 0x400E0ED0      ; Falling Edge select register
PIOA_ESR EQU 0x400E0EC0         ; Edge select register
PIOA_AIMER EQU 0x400E0EB0       ; Additional Interrupt Modes Enable Register

PIOA_IMR EQU 0x400E0E48         ; Controller interrupt mask register
PIOA_ISR EQU 0x400E0E4C         ; Controller interrupt status register

; C port - (leds)
PIOC_WPMR EQU  0x400E12E4       ; Write Protect Mode Register
  
PIOC_PER  EQU 0x400E1200       ; PIO Enable Register - Arduino Due
PIOC_OER  EQU 0x400E1210       ; Output Enable Register
PIOC_SODR EQU 0x400E1230       ; Set Output Data Register
PIOC_PUDR EQU 0x400E1260       ; Pull Up Disable Register
PIOC_CODR EQU 0x400E1234       ; Clear Output Data Register
PIOC_PDSR EQU 0x400E123C       ; Pin Data Status Register


; PMC
PMC_WPMR  EQU   0x400E06E4      ; PMC Write Protect Mode Register

PMC_PCER  EQU   0x400E0610      ; Peripheral Clock Enable Register 0
PMC_PCDR  EQU   0x400E0614      ; Peripheral Clock Disable Register 0
PMC_PCSR  EQU   0x400E0618      ; Peripheral Clock Status Register 0

; SYSTICK
SYSTICK_CTRL EQU 0xE000E010      ; Systick control register
SYSTICK_LOAD EQU 0xE000E014      ; Systick reload value register
SYSTICK_VAL  EQU 0xE000E018      ; Systick current value register
SYSTICK_CALIB EQU 0xE000E01C     ; Systick valibtration value register

;ICPR register adresser

SETENA EQU 0xE000E100            ; Interrupt set enable register
CLRENA EQU 0xE000E180            ; Interrupt clear enable register
CLRPEN EQU 0xE000E280            ; Interrupt clear pending register
ACTIVE EQU 0xE000E300            ; Interrupt active status register

main
          LDR   R0,=PMC_PCER
          LDR   R1,=(5 << 11)  ; Peripheral Identifier: bit11=PIOA, bit13=PIOC 
          STR   R1,[R0]

; Enable systick with interrupt every 1024 processor clockcycles
;======================= INIT av avbrott =============================
          LDR R0, =SYSTICK_CTRL   ; Control and Status Register
          MOV R1, #0              ; all bits clear
          STR R1, [R0]            ; => stop counter, prevent interrupts
          
          LDR R0, =SYSTICK_VAL    ; Current Value Register
          STR R1, [R0]            ; => clear current value
          
          LDR R0, =SYSTICK_LOAD   ; Reload Value Register
          LDR R1,=0xB71AFF       ; trigger every 1024 cycles, counter decrements from 1023 to 0, 1024 cycles total
          STR R1, [R0]            ; => set reload value
          
          LDR R0, =SYSTICK_CTRL   ; Control and Status Register
          MOV R1, #0x07           ; ENABLE(bit0), TICKINT(bit1) and CLKSOURCE(bit2) set
          STR R1, [R0]            ; => start counter datasheet p.195 


;======================= INIT av outport =============================
          LDR R1, =0xE                ; bit masking the LEDs   (1110)
          
          LDR R0, =PIOC_PER           ; PIO Enable Register - Arduino Due
          STR R1, [R0]                ; Use 0xE to initialize arduino output (LEDs)  
          
          LDR R0, = PIOC_OER          ; Output Enable Register
          STR R1, [R0]                ; same as above
         
          LDR R0, =PIOC_PUDR          ; Pull Up Disable Register    
          STR R1, [R0]                ; we only want an reaction on pull down  
;=========================================================================
; Initialization InPort  a
          LDR R0,=PIOA_PER            ; PIO (A) Enable register
          LDR R1,=0x00004000          ; Enable interrupt for the 14th bit (left button)
          STR R1,[R0]
          
          LDR R0,=PIOA_ODR            ; PIOA is INPUT, so we disable output register
          STR R1,[R0] 
          ;----------------------------------------------------------------
          LDR R0,=PIOA_IER            ; Interrupt enable register
          STR R1,[R0]                 ; Enable it by put 1 in the 14th bit
          
          LDR R0,=PIOA_PUER           ; enable pull-up
          STR R1,[R0]                 ;see above
          
          LDR R0,=PIOA_AIMER          ;additional interrupt modes enable register
          STR R1,[R0]                   ; which enables rising command and falling command
          
          LDR R0,=PIOA_ESR            ; (Edge select register) enable edge detection
          STR R1,[R0]
 
          LDR R0,=PIOA_FELLSR         ; Falling edge select register 
          STR R1,[R0]                 
          
          LDR R0,=PIOA_ISR            ; interrupt status register
          LDR R0,[R0]
          
          LDR R1,=0x0000800           ; bit 11
          LDR R0,=CLRENA              ; Interrupt clear-enable bits
          STR R1,[R0]                 ; store the 1 at bit 11 to enable interrupt
          
          LDR R0,=SETENA              ;Interrupt set-enable bits
          STR R1,[R0]                 ; enable at bit 11
          
;=====================================================================
         
          MOV R3, #0                  ; count variable, counts active LEDs
          B LOOP                      ;branch to LOPP        

          PUBLIC SysTick_Handler
SysTick_Handler
           
          ;if the right button is pressed down
          LDR   R0, = PIOC_PDSR      ;read LED 3 (Pin data status register)
          LDR   R1,[R0]              ; fetch the value in the adress above        
          ANDS  R1, R1, #0x8        ; check the 3d bit (LED 3) 
          BEQ   TURNON
          BNE   TURNOF
          
TURNON
            LDR R2, =0x8
            LDR R1,=PIOC_SODR      ; PIO PortC Clear Output Data Register
            STR R2,[R1]           ; set all 1s to 0
            
            
           B THEEND
TURNOF
            LDR R7,=0x8         ;0x00000010
            LDR R1,=PIOC_CODR   ; PIO PortC Set Output Data Register
            STR R7,[R1]         ; set all 0s to 1
            
            B THEEND 
            
THEEND      
        BX    LR                    ; Return interrupt
        B SysTick_Handler

        
;============================= MAIN LOOP =================================
      
LOOP    
       
        LDR R0,=PIOA_PDSR           ; (input) pin data status register
        LDR R1, [R0]                ; fetch the value in the adress above
        LDR R2, [R0]                ; same as above
        LDR R4,[R0]                 ; -----"-----
        
        MOV R7,#200                 ;the delay (ms)
        BL Delay_ms
        ANDS R4,R4,#0xC000          ; check if both button is pressed down
        BNE GO                      ; if NOT, branch to GO     
        LDR R4,=PIOC_CODR           ; reset led 1 and 2
        MOV R5,#0x6                 
        STR R5,[R4]
        MOV R3,#0                   ; reset the counter
        
        
GO       
        ANDS R1, R1, #0x4000        ; check 14th bit in pin data status register                                       
        BEQ LOOP                    ; Interrupt takes care of the action to perform at button down, branch to LOOP 
        
        ANDS R2, R2, #0x8000        ; bit mask 15th bit (right button)
        BNE LOOP                    ; if not pushed down, branch to LOOP
        ADD R3, R3, #1              ; if pushed down, count++
        CMP R3, #4                  ; check count variable
        BNE ON                      ; if not 4, branch to ON
        
        MOV R3, #0
      
ON
        BL NewComb                  ; branch (with Link) to NewComb
       
        B LOOP
        
;=========================================================================

        PUBLIC PIOA_Handler
PIOA_Handler
     
        CPSID i                          ; Disable interrupts

        
        LDR R2,=PIOC_PDSR
        LDR R2, [R2]
        ANDS R2, R2, #0x6              ; bit masking the LEDs 0110, enable flags

START
        BEQ ZERO
        B NEXT
        
        
ZERO    
        LDR R0, =SYSTICK_LOAD
        LDR R3,=11999999                ;set 0.5 Hz
        STR R3, [R0]            
        B SLUT2

NEXT    
        CMP R2, #2                      ; check Left LED, (is it active?)
        BEQ ONE                         ; if so, set 1 Hz (branch to ONE)
        B NEXT2
        
ONE     
        LDR R0, =SYSTICK_LOAD
        LDR R3,=0x5B8D7F                ; set 1 Hz
        STR R3, [R0]            
        B SLUT2
        
NEXT2
        CMP R2, #4                      ; check if Right LED is active
        BEQ TWO                         ; if so branch to TWO
        B NEXT3

TWO     
        LDR R0, =SYSTICK_LOAD
        LDR R3,=0x2DC6BF                 ; 2 Hz
        STR R3, [R0]            
        B SLUT2

NEXT3   
        CMP R2, #6                      ; Are both LEDs active (?)
        BEQ THREE
        B SLUT2

THREE   
        LDR R0, =SYSTICK_LOAD
        LDR R3,=0x16E35F                ;set 4 Hz
        STR R3, [R0]            
        B SLUT2
        
        
        LDR   R0,=PIOA_ISR            ; interrupt sregister
        LDR R0,[R0]
          
SLUT2   
        LDR   R0,=PIOA_ISR           ; interrupt register
        LDR R0,[R0]                  ; clear everything so we dont get "stuck"
     
        CPSIE i                      ; (re)enable interrupts
        BX LR                        ; branch back (via the link register)

NewComb  
         Push{R0-R2,LR}
         
         
         LDR R0,=PIOC_CODR           ;reset the two LEDs
         MOV R1, #0x6
         STR R1, [R0]
         
         LDR R0,=PIOC_SODR           ; set output data register (write)
         LSL R2, R3, #1              ; shift one step to the right
        
         STR R2,[R0]                 ; LEDs turns on
         
PAUSE    LDR R0,=PIOA_PDSR
         LDR R0,[R0]
         ANDS R0,R0,#0x8000          ; read right button
         BEQ PAUSE
         
         MOV R7,#100                 ; use delay to avoid bounce
         BL Delay_ms
         
         
         
         
         Pop{R0-R2,LR}
         BX LR

; Delay function
; Input (argument): R7 - delay in ms
; ---------------------------------------------------------
DELAY_CALIB EQU 1200            ; calculated value (based on master clock) 12Mhz)
Delay_ms
      
      STMFD SP!,{R0,R1, LR}     ; Store R0, R1 and Link register at stack pointer
      MOV R0,R7
do_delay_ms
      LDR R1,=DELAY_CALIB
loop_ms
      SUBS R1,R1,#1  
      BNE loop_ms
      SUBS R0,R0,#1
      BNE do_delay_ms
      LDMFD SP!,{R0,R1, LR}
      BX LR                     ;branch back, via the link register
; ---------------------------------------------------------------------------------------------------------------------
             
        
        
    
STOP	B STOP


        END
