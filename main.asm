LIST P=PIC18F4321, F=INHX32
#include <p18f4321.inc>

;--------------------------------
; CONFIGURATION
;--------------------------------
CONFIG  OSC=HSPLL
CONFIG  PBADEN=DIG
CONFIG  WDT=OFF

;--------------------------------
; VARIABLES (RAM)
;--------------------------------
    CBLOCK 0x22         ; Start variables at 0x22
        contador_10ms
        contador_1s
        decadas
        baby            ; Flag
        teenager        ; Flag
        adult           ; Flag
        dead            ; Flag
        contador_1m
        t0
        t1
        tact
	bucle_green
	bucle_yellow
	bucle_red
	current_colour
	counter_alive ;contador que me mira si cambia de color por alimentaciÃ³n
	which_table
	rows_printed
	sano ;para pintarlo verde
	advertencia ;para pintarlo amarillo
	critico ;para pintarlo rojo
    contador_90s
    comida
    ENDC

Posicio_RAM EQU 0x81
baby_table EQU 0x0100
teen_table EQU 0x0110
adult_table EQU 0x0120

;--------------------------------
; RESET VECTOR
;--------------------------------
ORG     0x0000
    GOTO    MAIN

;--------------------------------
; HIGH PRIORITY INTERRUPT VECTOR
;--------------------------------
ORG     0x0008
    GOTO    HIGH_IRS

;--------------------------------
; LOW PRIORITY INTERRUPT VECTOR
;--------------------------------
ORG     0x0018
    RETFIE  FAST

;--------------------------------
; DATA TABLES (FLASH MEMORY)
;--------------------------------
    ORG baby_table
    DB b'00001000', b'00001000'
    DB b'00001000', b'00001000'
    DB b'00001000', b'00001000'
    DB b'00001000', b'00001000'    

    ORG teen_table
    DB b'00000000', b'00111100'
    DB b'01000010', b'01000010'
    DB b'01011010', b'01000010'
    DB b'00111100', b'00000000'

    ORG adult_table
    DB b'01111110', b'10000001'
    DB b'10111101', b'10000001'
    DB b'10100101', b'10100101'
    DB b'10000001', b'01111110' 

;--------------------------------
; CODE
;--------------------------------

;--------------------------------
; INITIALIZE PORTS AND VARIABLES
;--------------------------------
INIT_PORTS
    ;PORTA Output
    MOVLW B'00000000'
    MOVWF TRISA, ACCESS
    BCF LATA,1,ACCESS
    BCF LATA,0,ACCESS

    ;PORTB Input
    MOVLW B'11111111'
    MOVWF TRISB, ACCESS

    ;PORTC Output
    MOVLW B'00000000'
    MOVWF TRISC, ACCESS
    

    ;PORTD Output
    MOVLW B'00000000'
    MOVWF TRISD, ACCESS

    CLRF contador_10ms,ACCESS
    CLRF contador_1s,ACCESS
    CLRF contador_1m,ACCESS
    CLRF contador_90s,ACCESS
    CLRF t0,ACCESS
    CLRF t1,ACCESS
    CLRF tact,ACCESS
    BSF baby,ACCESS
    CLRF decadas,ACCESS
    CLRF current_colour,ACCESS
    BSF LATA,0,ACCESS
    BSF LATA,1
    MOVLW .90
    MOVWF counter_alive,ACCESS
    CLRF which_table, ACCESS
    MOVLW .8
    MOVWF rows_printed,ACCESS

    
    RETURN

INIT_CONFIG
    CLRF TRISC
    MOVLW B'11100000'
    MOVWF INTCON, ACCESS
    MOVLW B'10001000'
    MOVWF T0CON, ACCESS
    BSF RCON,IPEN
    MOVLW B'11010000'
    MOVWF INTCON3
    RETURN

RESET_INTERRUPTS
    ; 1ms / 100ns = 1000 ticks (Assuming logic, your math said 0.1ms but calc implies 1000)
    ; 65536 - 1000 = 64536
    MOVLW HIGH(.64535)
    MOVWF TMR0H,ACCESS
    MOVLW LOW(.64535)
    MOVWF TMR0L,ACCESS
    RETURN

HIGH_IRS
    BTFSC INTCON, TMR0IF,ACCESS
    CALL TMR0_INTERRUPT
    RETFIE FAST

TMR0_INTERRUPT
    BSF PORTC,0
    BCF PORTC,0
    CALL RESET_INTERRUPTS
    BCF INTCON, TMR0IF,ACCESS
    CALL BUCLE_10MS
    CALL PMW
    RETURN

BUCLE_10MS           ; cuenta 10ms
    INCF    contador_10ms, F
    MOVLW   .99
    CPFSEQ  contador_10ms
    RETURN
    ; Si llegamos aqui, contador_10ms == 100
    CLRF    contador_10ms
    CALL    BUCLE_SEG
    RETURN

BUCLE_SEG            ; cuenta segundos

    INCF    contador_1s, F
    MOVLW   .99
    CPFSEQ  contador_1s
    RETURN
    ; Si llegamos aqui, contador_seg == 100
    CLRF    contador_1s
    CALL    BUCLE_MIN
    CALL    BUCLE_90s
    RETURN

BUCLE_MIN            ; cuenta minutos
    INCF    contador_1m, F
    MOVLW   .59
    CPFSEQ  contador_1m
    RETURN
    CALL    RESET_BUCLES
    RETURN

BUCLE_90s
    INCF    contador_90s, F
    MOVLW   .89
    CPFSEQ  contador_90s
    RETURN
    BTFSS advertencia,ACCESS
    GOTO    WARNING_STATE 
    BTFSC advertencia,ACCESS
    GOTO    CRITICAL_STATE


RESET_BUCLES
    INCF decadas,F
    CLRF contador_1m
    CALL ENVEJECER
    RETURN

ENVEJECER
    MOVLW .3
    CPFSLT decadas,ACCESS
    BCF baby,ACCESS
    CPFSLT decadas,ACCESS
    BSF teenager,ACCESS

    MOVLW .6
    CPFSLT decadas,ACCESS
    BCF teenager,ACCESS
    CPFSLT decadas,ACCESS
    BSF adult,ACCESS

    MOVLW .10
    CPFSLT decadas,ACCESS
    BCF adult,ACCESS
    CPFSLT decadas,ACCESS
    BSF dead,ACCESS
    CPFSLT decadas,ACCESS
    CALL IS_DEAD
    RETURN

WARNING_STATE
    BCF sano,ACCESS
    BSF advertencia,ACCESS
    BCF critico,ACCESS
    RETURN

CRITICAL_STATE
    BCF sano,ACCESS
    BCF advertencia,ACCESS
    BSF critico,ACCESS
    CALL IS_DEAD
    RETURN

IS_DEAD
    GOTO IS_DEAD
    RETURN

PMW
    INCF tact, F
    ; t1 = 2*decadas + 4
    MOVF    decadas, W
    MULLW   .2
    MOVF PRODL, W
    ADDLW .4
    MOVWF t1

    MOVLW   .200
    MOVWF   t0
    MOVF    t1, W
    SUBWF   t0, F     ; t0 = 200 - t1

    MOVF t1, W
    BTFSS PORTA,1,ACCESS
    MOVF t0, W

    CPFSLT tact
    BTG LATA,1
    CPFSLT tact
    CLRF tact
    RETURN

;----------------------------------------------
; MATRIX
;----------------------------------------------

CODE_ONE
    BSF LATA, 0, ACCESS
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP            
    BCF LATA, 0, ACCESS
    NOP             
    RETURN          
    
CODE_ZERO
    BSF LATA, 0, ACCESS
    NOP
    NOP
    NOP            
    BCF LATA, 0, ACCESS
    NOP
    NOP
    NOP
    NOP
    NOP           
    RETURN
    
    
COLOR_ON
    CALL CODE_ZERO   ;intensidad maxima
    CALL CODE_ZERO  
    CALL CODE_ONE  
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ZERO   ;intensidad minima
    RETURN

    COLOR_ON_1
    CALL CODE_ZERO   ;intensidad maxima
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ONE  
    CALL CODE_ZERO   ;intensidad minima
    RETURN
    
COLOR_OFF
    CALL CODE_ZERO
    CALL CODE_ZERO
    CALL CODE_ZERO
    CALL CODE_ZERO
    CALL CODE_ZERO
    CALL CODE_ZERO
    CALL CODE_ZERO
    CALL CODE_ZERO
    RETURN    
PIXEL_OFF
    CALL COLOR_OFF 
    CALL COLOR_OFF 
    CALL COLOR_OFF 
    RETURN

PAINT_GREEN
    CALL COLOR_ON
    CALL COLOR_OFF
    CALL COLOR_OFF
    RETURN

PAINT_RED
    CALL COLOR_OFF
    CALL COLOR_ON
    CALL COLOR_OFF
    RETURN

PAINT_YELLOW
    CALL COLOR_ON
    CALL COLOR_ON
    CALL COLOR_OFF
    RETURN
    
PAINT_WHITE
    CALL COLOR_ON_1
    CALL COLOR_ON_1
    CALL COLOR_ON_1
    RETURN

;------------------------------
; ACTUALIZAR COLOR
;------------------------------

SELECT_COLOR
    BTFSC sano,ACCESS
    CALL PAINT_GREEN
    BTFSC advertencia,ACCESS
    CALL PAINT_YELLOW
    BTFSC critico,ACCESS
    CALL PAINT_RED
    RETURN

PRINT_HEALTHY
    TBLRD*+
    BTFSC TABLAT, 0
    CALL PAINT_GREEN
    BTFSS TABLAT, 0
    CALL PAINT_WHITE

    BTFSC TABLAT, 1
    CALL PAINT_GREEN
    BTFSS TABLAT, 1
    CALL PAINT_WHITE

    BTFSC TABLAT, 2
    CALL PAINT_GREEN
    BTFSS TABLAT, 2
    CALL PAINT_WHITE

    BTFSC TABLAT, 3
    CALL PAINT_GREEN
    BTFSS TABLAT, 3
    CALL PAINT_WHITE

    BTFSC TABLAT, 4
    CALL PAINT_GREEN
    BTFSS TABLAT, 4
    CALL PAINT_WHITE

    BTFSC TABLAT, 5
    CALL PAINT_GREEN
    BTFSS TABLAT, 5
    CALL PAINT_WHITE

    BTFSC TABLAT, 6
    CALL PAINT_GREEN
    BTFSS TABLAT, 6
    CALL PAINT_WHITE

    BTFSC TABLAT, 7
    CALL PAINT_GREEN
    BTFSS TABLAT, 7
    CALL PAINT_WHITE

    ;TBLRD*+
    DECFSZ rows_printed, ACCESS
    CALL PRINT_HEALTHY
    RETURN

PRINT_WARNING
    TBLRD*+
    BTFSC TABLAT, 0
    CALL PAINT_YELLOW
    BTFSS TABLAT, 0
    CALL PAINT_WHITE

    BTFSC TABLAT, 1
    CALL PAINT_YELLOW       
    BTFSS TABLAT, 1
    CALL PAINT_WHITE

    BTFSC TABLAT, 2
    CALL PAINT_YELLOW
    BTFSS TABLAT, 2
    CALL PAINT_WHITE

    BTFSC TABLAT, 3
    CALL PAINT_YELLOW
    BTFSS TABLAT, 3
    CALL PAINT_WHITE

    BTFSC TABLAT, 4
    CALL PAINT_YELLOW
    BTFSS TABLAT, 4
    CALL PAINT_WHITE

    BTFSC TABLAT, 5
    CALL PAINT_YELLOW
    BTFSS TABLAT, 5
    CALL PAINT_WHITE

    BTFSC TABLAT, 6
    CALL PAINT_YELLOW
    BTFSS TABLAT, 6
    CALL PAINT_WHITE

    BTFSC TABLAT, 7
    CALL PAINT_YELLOW
    BTFSS TABLAT, 7
    CALL PAINT_WHITE

    ;TBLRD*+
    DECFSZ rows_printed, ACCESS
    CALL PRINT_WARNING
    RETURN

PRINT_CRITICAL
    TBLRD*+
    BTFSC TABLAT, 0
    CALL PAINT_RED
    BTFSS TABLAT, 0
    CALL PAINT_WHITE

    BTFSC TABLAT, 1
    CALL PAINT_RED
    BTFSS TABLAT, 1
    CALL PAINT_WHITE

    BTFSC TABLAT, 2
    CALL PAINT_RED
    BTFSS TABLAT, 2
    CALL PAINT_WHITE

    BTFSC TABLAT, 3
    CALL PAINT_RED
    BTFSS TABLAT, 3
    CALL PAINT_WHITE

    BTFSC TABLAT, 4
    CALL PAINT_RED
    BTFSS TABLAT, 4
    CALL PAINT_WHITE

    BTFSC TABLAT, 5
    CALL PAINT_RED
    BTFSS TABLAT, 5
    CALL PAINT_WHITE

    BTFSC TABLAT, 6
    CALL PAINT_RED
    BTFSS TABLAT, 6
    CALL PAINT_WHITE

    BTFSC TABLAT, 7
    CALL PAINT_RED
    BTFSS TABLAT, 7
    CALL PAINT_WHITE

    ;TBLRD*+
    DECFSZ rows_printed, ACCESS
    CALL PRINT_CRITICAL
    RETURN



INIT_TABLE
    MOVWF TBLPTRL      ; Move value from W (0, 16, or 32) into Table Pointer Low
    MOVLW .1           ; Set High Byte to 01 (Base address 0x0100)
    MOVWF TBLPTRH
    MOVLW .0           ; Set Upper Byte to 00
    MOVWF TBLPTRU
    RETURN

GROW
    MOVLW .16
    MOVWF which_table,ACCESS
    RETURN
    
GROW2
    MOVLW .32
    MOVWF which_table,ACCESS
    RETURN
    
START_MATRIX
    BCF INTCON, 5
    BTFSC baby,ACCESS
    CLRF which_table   
    
    BTFSC teenager, ACCESS
    CALL GROW
    
    BTFSC adult,ACCESS
    CALL GROW2    
    
    MOVF which_table,W,ACCESS
    CALL INIT_TABLE
    
    MOVLW .8
    MOVWF rows_printed,ACCESS
    BTFSC sano,ACCESS
    CALL PRINT_HEALTHY
    BTFSC advertencia,ACCESS
    CALL PRINT_WARNING
    BTFSC critico,ACCESS 
    CALL PRINT_CRITICAL
    BSF INTCON, 5
    RETURN

    
    
    
;------------------------------------------
; ALIMENTAR TAMAGOTCHI
;------------------------------------------
FEED_TAMAGOTCHI ;función ALIMENTAR del menu
    ;comprobar si tengo tokens disponibles para alimentar
    ;si tengo, alimentar y resto un token
    ;si no tengo tokens, hago la rutina de no alimentar
    MOVLW   .0
    CPFSEQ comida,ACCESS
    CALL HEALTHY_STATE
    RETURN


HEALTHY_STATE
    DECF comida,ACCESS
    BCF sano,ACCESS
    BSF advertencia,ACCESS
    BCF critico,ACCESS
    RETURN
    
    
FIRE ; FUNCION DEBUG
    MOVLW   b'00000000'
    MOVWF   TRISD
    MOVLW   b'10101010'
    MOVWF   PORTD
    GOTO    FIRE

MAIN
    ;CALL FIRE
    CALL INIT_PORTS
    CALL INIT_CONFIG
    CALL RESET_INTERRUPTS
    
LOOP
    BSF LATA,2,0
    CALL START_MATRIX
    GOTO LOOP

    END
