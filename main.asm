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
	counter_alive ;contador que me mira si cambia de color por alimentación
	which_table
	rows_printed
	alimentado ;flag que comprueba si se ha alimentado 
	sano ;para pintarlo verde
	advertencia ;para pintarlo amarillo
	critico ;para pintarlo rojo
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
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    
    

    ORG teen_table
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'

    ORG adult_table
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'
    DB b'10000001', b'10000001'

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

    ;Rutina para incrementar el contador de hambre
    ;--------------------------------
    MOVLW .255
    CPFSGT counter_alive,ACCESS
    INCF counter_alive, F
    ;--------------------------------

    INCF    contador_1s, F
    MOVLW   .99
    CPFSEQ  contador_1s
    RETURN
    ; Si llegamos aqui, contador_seg == 100
    CLRF    contador_1s
    CALL    BUCLE_MIN
    RETURN

BUCLE_MIN            ; cuenta minutos
    INCF    contador_1m, F
    MOVLW   .59
    CPFSEQ  contador_1m
    RETURN
    CALL    RESET_BUCLES
    RETURN

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
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ONE   
    CALL CODE_ONE   
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ONE   
    CALL CODE_ZERO  
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

PRINT_SHAPE
    BTFSC TABLAT, 0
    CALL PAINT_GREEN
    BTFSS TABLAT, 0
    CALL PIXEL_OFF

    BTFSC TABLAT, 1
    CALL PAINT_GREEN
    BTFSS TABLAT, 1
    CALL PIXEL_OFF

    BTFSC TABLAT, 2
    CALL PAINT_GREEN
    BTFSS TABLAT, 2
    CALL PIXEL_OFF

    BTFSC TABLAT, 3
    CALL PAINT_GREEN
    BTFSS TABLAT, 3
    CALL PIXEL_OFF

    BTFSC TABLAT, 4
    CALL PAINT_GREEN
    BTFSS TABLAT, 4
    CALL PIXEL_OFF

    BTFSC TABLAT, 5
    CALL PAINT_GREEN
    BTFSS TABLAT, 5
    CALL PIXEL_OFF

    BTFSC TABLAT, 6
    CALL PAINT_GREEN
    BTFSS TABLAT, 6
    CALL PIXEL_OFF

    BTFSC TABLAT, 7
    CALL PAINT_GREEN
    BTFSS TABLAT, 7
    CALL PIXEL_OFF

    TBLRD*+
    DECFSZ rows_printed, ACCESS
    CALL PRINT_SHAPE
    RETURN

INIT_TABLE
    MOVLW .0
    MOVWF TBLPTRU
    MOVLW .1
    MOVWF TBLPTRH
    MOVLW which_table
    MOVWF TBLPTRL 
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
    
    MOVWF which_table,ACCESS
    
    CALL INIT_TABLE
    
    MOVLW .8
    MOVWF rows_printed,ACCESS
    CALL PRINT_SHAPE
    BSF INTCON, 5
    RETURN

    
    
    
;------------------------------------------
; SEND COLOUR ACCORDING TO HEALTH STATUS
;------------------------------------------
CHECK_HEALTH
    ;primero miro si me han alimentado 
    ;si NO me han alimentado, mirar cuanto tiempo ha pasado desde la última comida
    ;si han pasado 0-89 seg, sigo verde
    ;si han pasado 90 seg, cambio a amarillo
    ;si estoy en amarillo y me alimentan vuelvo a verde
    ;si no me alimentan, reseteo la cuenta de 90 seg o bien sigo contando hasta 180 seg
    ;si llego a 180 seg cambio a rojo y muero
    ;--------------------------------------------------
    ;comprobar si me han alimentado
    BTFSC alimentado,ACCESS
    ;si me han alimentado, vuelvo a verde
    BSF sano,ACCESS
    BCF advertencia,ACCESS
    BCF critico,ACCESS
    ;cuento 90 seg para volver a amarillo
    MOVLW .90
    MOVWF counter_alive,ACCESS
    BCF alimentado,ACCESS
    BCF sano,ACCESS
    BSF advertencia,ACCESS
    BCF critico,ACCESS
    CALL PAINT_YELLOW
    ;ahora estoy en amarillo, si me alimentan vuelvo a verde
    BTFSC alimentado,ACCESS
    BSF sano,ACCESS
    BCF advertencia,ACCESS
    BCF critico,ACCESS
    ;si no me alimentan, cuento 90 seg más para llegar a rojo
    MOVLW .90
    MOVWF counter_alive,ACCESS
    BCF alimentado,ACCESS
    BCF sano,ACCESS
    BCF advertencia,ACCESS
    BSF critico,ACCESS
    CALL PAINT_RED
    CALL IS_DEAD
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
