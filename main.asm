LIST P=PIC18F4321 F=INHX32
#include <p18f4321.inc>
CONFIG  OSC=HSPLL ; L?oscil.lador.
CONFIG  PBADEN=DIG ; Volem que el PORTB sigui DIGital.
CONFIG  WDT=OFF    ; Desactivem el WatchDog Timer.
CONFIG LVP=OFF


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
    flag_16ms
    contador_16ms
    mode
    random_number         
    flag_game		
    contador_game	
    flag_rpulse		
    contador_rpulse	
    random_seed	
    espera_random	
    espera_random_counter 
    espera_random_base
    need_refresh
    reset_valor
    decadas_save
    ENDC   
    Posicio_RAM EQU 0x81
    baby_table EQU 0x0110
    teen_table EQU 0x0120
    adult_table EQU 0x0130
    dead_table EQU 0x0140
    TAULA7S EQU 0x0150

ORG	    0x0000
GOTO	    MAIN
ORG	    0x0008  ;Interrupts de alta prioridad
GOTO	    HIGH_IRS ;Zona de interrupciones
ORG	    0x0018
RETFIE FAST
     ;--------------------------------
; DATA TABLES (FLASH MEMORY)
;--------------------------------
    ORG baby_table 
    DB b'00000000', b'00000000'
    DB b'00011000', b'00100100'
    DB b'00100100', b'00011000'
    DB b'00000000', b'00000000'

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
    
     ORG dead_table
    DB b'00011000', b'00011000'
    DB b'00011000', b'00011000'
    DB b'01111110', b'01111110'
    DB b'00011000', b'00011000'    
    
    ORG TAULA7S
   ;Segments del 0, segments del 1
    DB 0x7D, 0x30
    ;Segments del 2, segments del 3
    DB 0x6E, 0x7A
    ;Segments del 4, segments del 5
    DB 0X33, 0x5B
    ;Segments del 6, segments del 7
    DB 0x5F, 0x70
    ;Segments del 8, segments del 9
    DB 0x7F, 0x73   
    
INIT_PORTS
    CLRF decadas_save,ACCESS
    CLRF comida,ACCESS
    CLRF  reset_valor,ACCESS
    CLRF need_refresh,ACCESS
    ;PORTA Todo de salida; grid, servo, RandomGenerated, Menu[2..0]
    MOVLW B'00000000' ;A7 nada, A6 nada, A5 menu 2, A4 menu 1, A3 menu 0, A2 RandomGenerated, A1 servo, A0 grid 
    MOVWF TRISA, ACCESS
    BCF LATA,1,ACCESS
        BSF LATA,1,ACCESS
    BCF LATA,0,ACCESS
    ;PORTB Todo de entrada; botones Left option, Right option, Select, PCI, ResultPulse, NewNumber
    MOVLW B'11111111' ;B7 nada, B6 nada, B5 NewNumber, B4 ResultPulse, B3 PCI, B2 BtnSelect, B1 BtnLeftOption, B0 BtnRightOption
    MOVWF TRISB, ACCESS
    ;PORTC Todo de salida; C7 nada, C6 nada, C5 nada, C4 nada, C3 RandomNumber0, C2 RandomNumber1, C1 RandomNumber2, C0 RandomNumber3
    MOVLW B'00000000'
    MOVWF TRISC, ACCESS
    ;PORTD Todo de salida; 7Seg
    MOVLW B'00000000' ;D7 nada, D6 7Seg6, D5 7Seg5, D4 7Seg4, D3 7Seg3, D2 7Seg2, D1 7Seg1, D0 7Seg0
    MOVWF TRISD, ACCESS
    CLRF    flag_16ms, ACCESS
    CLRF    flag_game, ACCESS
    CLRF    flag_rpulse, ACCESS
    CLRF espera_random_base, ACCESS
    CLRF contador_game,ACCESS
    ;CLRF contador_rpulse,ACCESS
    ;CLRF espera_random_counter
    CLRF contador_10ms,ACCESS
    CLRF contador_1s,ACCESS
    CLRF contador_90s,ACCESS
    CLRF contador_1m,ACCESS
    CLRF t0,ACCESS
    CLRF t1,ACCESS
    CLRF tact,ACCESS
    BSF baby,ACCESS
    CLRF decadas,ACCESS
    BCF flag_16ms,0
    MOVLW .1
    MOVWF mode,ACCESS
    CALL UPDATE_LEDS
    CLRF LATD
    BSF   LATA,2,0
    BCF adult,ACCESS
    BCF teenager,ACCESS
    BCF dead,ACCESS
    CLRF current_colour,ACCESS
    BSF LATA,0,ACCESS
    MOVLW .90
    MOVWF counter_alive,ACCESS
    CLRF which_table, ACCESS
    MOVLW .8
    MOVLW .1
    MOVWF rows_printed,ACCESS
    BSF sano,ACCESS
    BCF advertencia,ACCESS
    BCF critico,ACCESS
    BCF LATC,4,0

    RETURN
    
INIT_CONFIG
    CLRF TRISC
    MOVLW B'11100000'
    MOVWF INTCON, ACCESS
    MOVLW B'10001000'
    MOVWF T0CON, ACCESS
    BSF RCON,IPEN ;Se activan las high-priority
    MOVLW B'11000000'
    MOVWF INTCON3
    BCF INTCON2,RBPU
    RETURN

RESET_INTERRUPTS
    ;Tins = 4/40MHz = 100ns
    ;0,1ms/100ns = 1k tics
    ;Usamos el timer0 de 16 bits (2^16 = 65536)
    ;65535 - 1000 = 64536 paradas cada 0,1ms
 
    MOVLW LOW(.64536)
    MOVWF TMR0L,ACCESS
    MOVLW HIGH(.64536) 
    MOVWF TMR0H,ACCESS
    
    RETURN 

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Funciones para controlar los botones y menus

BOTON
    BSF flag_16ms,ACCESS

    MOVLW   .159                 ; ¿hemos llegado a 16ms?
    CPFSGT  contador_16ms
    GOTO BOTON                        ; si NO es 16ms vuelvo
    ; Si llegamos aquí, contador_16ms == 99
    BCF flag_16ms,ACCESS
    CLRF    contador_16ms         ; reseteo segundos
MS16
    BTFSS   PORTB,0,ACCESS      ; si RB0=1 (no pulsado) salta
    CALL    NEXT_MODE

    ; ---- Botón izquierda (RB1) ----
    BTFSS   PORTB,1,ACCESS
    CALL    PREV_MODE

    ; ---- Botón select ---
    BTFSS   PORTB,2,ACCESS
    CALL    SELECT_MODE

MS16Vuelta_sinpulsar
    BTFSS   PORTB,0,ACCESS
    GOTO    MS16Vuelta_sinpulsar

    BTFSS   PORTB,1,ACCESS
    GOTO    MS16Vuelta_sinpulsar

    BTFSS   PORTB,2,ACCESS
    GOTO    MS16Vuelta_sinpulsar
MS16Vuelta
    BSF flag_16ms,ACCESS

    MOVLW   .159                 ; ¿hemos llegado a 16ms?
    CPFSGT  contador_16ms
    GOTO MS16Vuelta                        ; si NO es 16ms vuelvo

    ; Si llegamos aquí, contador_16ms == 99
    BCF flag_16ms,ACCESS
    CLRF contador_16ms       ; reseteo segundos
    RETURN
NEXT_MODE
    INCF    mode, F            ; MODE++
    MOVLW   .4                 ; si MODE == 4 -> volver a 1
    CPFSEQ  mode, ACCESS
    GOTO    NM_OK
    MOVLW   .1
    MOVWF   mode, ACCESS
NM_OK
    CALL    UPDATE_LEDS
    RETURN
    
PREV_MODE
    DECF    mode, F            ; MODE--
    MOVF    mode, W            ; si MODE == 0 -> poner 3
    BNZ     PM_OK              ; (BNZ = branch if not zero)
    MOVLW   .3
    MOVWF   mode, ACCESS
PM_OK
    CALL    UPDATE_LEDS
    RETURN
    
SELECT_MODE  
    MOVLW   .1
    CPFSEQ  mode, ACCESS
    GOTO SELECT_MODE_2
    GOTO GAME
SELECT_MODE_2
    MOVLW   .2
    CPFSEQ  mode, ACCESS
    BSF reset_valor,ACCESS
    MOVLW   .3
    CPFSEQ  mode, ACCESS
    CALL FEED_TAMAGOTCHI
    RETURN

;fin menu
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////modo de juego con la fase 1

GAME ;poner el numero en latc y latd
	;aleatoriedad

    MOVLW .3
    ADDWF espera_random_base,F,ACCESS
    MOVF espera_random_base,W,ACCESS
    MOVWF espera_random
    MOVF  random_seed,W,ACCESS
    ADDWF  espera_random,F,ACCESS
    MOVF	random_number,W,ACCESS
    ADDWF  espera_random,F,ACCESS
    MOVF	contador_10ms,W,ACCESS
    ADDWF  espera_random,F,ACCESS
;fin aleatoriedad printar el numero
GAME_TU_MADRE
CLRF espera_random_counter,ACCESS  
GAME_ESPERA_3 ;espera aleatoria para el siguiente numero

  MOVF    espera_random,W,ACCESS
  CPFSGT  espera_random_counter,ACCESS
  GOTO GAME_ESPERA_3

  MOVF random_seed,W,ACCESS
  CPFSEQ  random_number,ACCESS
  GOTO CONSEGUIDO
  GOTO GAME_TU_MADRE
  
CONSEGUIDO
  
  
  MOVFF  random_seed,random_number
  MOVFF  random_number,LATC
  BTFSC need_refresh,ACCESS
  CALL START_MATRIX
  CLRF LATD
  MOVLW TAULA7S
  MOVWF TBLPTRL
  MOVF    random_number, W, ACCESS
  ADDWF  TBLPTRL,F,ACCESS
      MOVLW .1           ; Set High Byte to 01 (Base address 0x0100)
    MOVWF TBLPTRH
    MOVLW .0           ; Set Upper Byte to 00
    MOVWF TBLPTRU
  TBLRD*
  MOVFF TABLAT,LATD 

  BSF flag_game,ACCESS
  CLRF contador_game,ACCESS
  
GAME_ESPERA_1 ;(espera visual)
  MOVLW   .200 ;para terminated
    CPFSGT  contador_game,ACCESS
    GOTO GAME_ESPERA_1
    
    BCF	LATA,2,0 ; cambiamos al estado 2
    CLRF LATD

GAME_ESPERA_2 ;espera parpadeo, aviso de que el numero se va a enviar
    MOVLW   .210
    CPFSGT  contador_game,ACCESS
    GOTO GAME_ESPERA_2
    
    CLRF espera_random_counter,ACCESS
    
    BSF	LATA,2,0 ; cambiamos al estado 3
GAME_NEW_NUMBER
    BTFSC need_refresh,ACCESS
    CALL START_MATRIX
    BTFSS PORTB,4,0;rpulse
    GOTO FIN_GAME
    BTFSC PORTB,5,0; newnumber
    GOTO GAME_WAIT
    GOTO GAME_NEW_NUMBER
GAME_WAIT; mantener los datos 1 tiempo

    BSF flag_game,ACCESS
    CLRF contador_game,ACCESS
GAME_WAIT_1

    MOVLW   .3
    CPFSGT  contador_game,ACCESS
    GOTO GAME_WAIT_1
    GOTO GAME
    
FIN_GAME
    BCF LATC,4,0

    BSF	LATA,2,0
    BSF flag_rpulse,ACCESS
    BTFSS PORTB,4,0;rpulse
    GOTO FIN_GAME
        
    MOVLW   .14 ; si rpulse
    CPFSGT  contador_rpulse,ACCESS
    GOTO REINICIO_GAME ;x!>1 todo mal

MAS_COMIDA

    MOVLW   .5
    CPFSEQ  comida, ACCESS   ; ¿contador == 5?
    INCF    comida,F,ACCESS        ; si NO es 5 ? contador++

REINICIO_GAME
    MOVF contador_rpulse,W,ACCESS
    ;MOVWF LATC,0
    BCF flag_game,ACCESS
    BCF flag_rpulse,ACCESS 
    CLRF contador_rpulse,ACCESS 
    CLRF LATD
        CLRF espera_random_base,ACCESS
    GOTO LOOP
    
UPDATE_LEDS
    ; Limpia solo RA2..RA0 (deja el resto de LATA igual)
    MOVLW   b'11000111'
    ANDWF   LATA, F, ACCESS
    ; MODE == 1 ?
    MOVF    mode, W, ACCESS
    MOVLW   .1
    CPFSEQ  mode, ACCESS
    GOTO    CHECK_M2
    ; Leds = 011  cian, opcion jugar
    BCF     LATA,3,ACCESS
    BSF     LATA,4,ACCESS
    BSF     LATA,5,ACCESS
    RETURN
CHECK_M2
    MOVLW   .2
    CPFSEQ  mode, ACCESS
    GOTO    MODE3
    ; Leds = 101 ; magenta opc 2
    BSF     LATA,3,ACCESS
    BCF     LATA,4,ACCESS
    BSF     LATA,5,ACCESS
    RETURN 
MODE3
    ; Leds = 111 blanco opc 3
    BSF     LATA,3,ACCESS
    BSF     LATA,4,ACCESS
    BSF     LATA,5,ACCESS
    RETURN
    
    
    
; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////INTERRUPCIÓN DEL TIMER
HIGH_IRS
    ;La interrupcion saltara cada 1ms
    BTFSC INTCON, TMR0IF,ACCESS
    CALL TMR0_INTERRUPT
    RETFIE FAST
TMR0_INTERRUPT
    CALL RESET_INTERRUPTS
    BCF INTCON, TMR0IF,ACCESS ;es el bit 2 del INTCON
    CALL BUCLE_10MS
    CALL PMW
    RETURN
BUCLE_10MS          ; cuenta 10ms, 0,1ms
    INCF    contador_10ms, F,ACCESS      ; contador_10ms++    
    INCF    espera_random_counter,F,ACCESS  ;contador numero random
    
    BTFSC flag_16ms,0,ACCESS	; contar 16ms
    INCF    contador_16ms,F,ACCESS         ; contador_seg++
   
    BTFSC flag_rpulse,0,ACCESS	; contar ms de game
    INCF    contador_rpulse,F,ACCESS  
    
    INCF random_seed,F,ACCESS
    MOVLW   .10                  ; ¿hemos llegado a 10 ticks?
    CPFSLT  random_seed,ACCESS           ; ¿contador_10ms == 10?
    CLRF      random_seed,ACCESS
    
    MOVLW   .99                  ; ¿hemos llegado a 10 ticks?
    CPFSGT  contador_10ms,ACCESS	; contar 16ms

    RETURN                        ; si NO es igual
           
    ; Si llegamos aquí, contador_10ms == 10
    CLRF    contador_10ms,ACCESS           ; reseteo a 0
    CALL    BUCLE_SEG             ; acumulo 10ms
    
    RETURN
;CPFSGT contador_10ms,ACSC flag_16ms,1,ACCESS
BUCLE_SEG           ; cuenta segundos, 10ms
    INCF    contador_1s, F,ACCESS         ; contador_seg++
    
    BTFSC flag_game,ACCESS	; contar ms de game
    INCF    contador_game,F,ACCESS  
    
    MOVLW   .98                ; ¿hemos llegado a 1s?(1000ms)
    CPFSGT  contador_1s,ACCESS  
    RETURN                        ; si NO es 100, salgo
    
    ; Si llegamos aquí, contador_seg == 100
    CLRF    contador_1s,ACCESS           ; reseteo segundos
    CALL    BUCLE_MIN             ; acumulo 1s
    CALL    BUCLE_90s
    RETURN

BUCLE_MIN           ; cuenta minutos
    INCF    contador_1m, F,ACCESS         ; contador_min++
   
    MOVLW   .59             ; hemos llegado a 1min?
    CPFSEQ  contador_1m,ACCESS  
    RETURN                        ; si aún no he llegado, salgo
    CALL    RESET_BUCLES          ;llegada a la decada
    RETURN

BUCLE_90s ;llamada a cambiar estado
    BTFSC dead,ACCESS
    CALL    CRITICAL_STATE
    BTFSC dead,ACCESS
    RETURN
    
    INCF    contador_90s, F
    MOVLW   .89
    CPFSEQ  contador_90s
    RETURN
    BTFSC advertencia,ACCESS
    CALL    CRITICAL_STATE
    BTFSC sano,ACCESS
    CALL    WARNING_STATE 
    RETURN

RESET_BUCLES
    INCF decadas,F,ACCESS  
    CLRF contador_1m,ACCESS  
    CALL ENVEJECER
    RETURN
WARNING_STATE
    CLRF contador_90s, ACCESS
    BCF sano,ACCESS
    BSF advertencia,ACCESS
    BCF critico,ACCESS
    BSF need_refresh,ACCESS
    RETURN

CRITICAL_STATE
    CLRF contador_90s, ACCESS

    BCF sano,ACCESS
    BCF advertencia,ACCESS
    BSF critico,ACCESS
    CALL IS_DEAD
    RETURN

ENVEJECER    
    MOVLW .3
    CPFSLT decadas,ACCESS ;Aquí cambio a teenager
    BCF baby,ACCESS
    CPFSLT decadas,ACCESS
    CALL TEEN
    
    MOVLW .6
    CPFSLT decadas,ACCESS ;Aquí cambio a adult
    BCF teenager,ACCESS
    CPFSLT decadas,ACCESS
    CALL ADULT
    
    MOVLW .10
    ;CPFSLT decadas,ACCESS
    ;BCF adult,ACCESS
    CPFSLT decadas,ACCESS
    BSF dead,ACCESS
    CPFSLT decadas,ACCESS
    CALL IS_DEAD
    RETURN
    

    
PMW

    INCF tact, F 

    ; t1 = 2*decadas + 4
    MOVF    decadas, W
    MULLW   .2
    MOVF PRODL, W
    
    ADDLW .4
    MOVWF t1

    MOVLW .4
    cpfsgt t1,ACCESS
    INCF t1,F,ACCESS
    cpfsgt t1,ACCESS
    INCF t1,F,ACCESS
    MOVLW   .200      ; periodo total = 200 ticks * 0,1ms = 20ms
    MOVWF   t0
    MOVF    t1, W
    SUBWF   t0, F     ; t0 = 200 - t1

    MOVF t1, W
    BTFSS LATA,1,ACCESS ; si RA1=1 ? usa t1, si no ? t0
    MOVF t0, W

    CPFSLT tact       ; si tact < W ? salta BTG/CLRF
    BTG LATA,1        ; si tact >= W ? toggle
    CPFSLT tact
    CLRF tact
    RETURN

    ;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////MATRIZ Y SUS MOVIDAS
    
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
    CALL CODE_ZERO  
    CALL CODE_ZERO  
    CALL CODE_ONE  
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
    MOVLW .32
    MOVWF which_table,ACCESS
    RETURN
    
GROW2
    MOVLW .48
    MOVWF which_table,ACCESS
    RETURN

    
TEEN
    BSF teenager,ACCESS
    BSF need_refresh,ACCESS

    RETURN
ADULT
    BSF adult,ACCESS ;llamada 2 envejecer
    BSF need_refresh,ACCESS

    RETURN
IS_DEAD  ; hacer algo mas?
    BSF dead,ACCESS
    ;BSF need_refresh,ACCESS
    RETURN

DEAD
CALL START_MATRIX
MOVFF decadas,decadas_save
    DEAD_1
    MOVFF decadas_save,decadas
    GOTO DEAD_1
   
START_MATRIX
    BCF need_refresh,ACCESS
    BCF INTCON, GIEH, ACCESS 
    MOVLW .16
    MOVWF which_table,ACCESS
    
    BTFSC teenager, ACCESS
    CALL GROW
    
    BTFSC adult,ACCESS
    CALL GROW2    
    
    MOVF which_table, W, ACCESS

    CALL INIT_TABLE
    
    MOVLW .8
    MOVWF rows_printed,ACCESS
    BTFSC sano,ACCESS
    CALL PRINT_HEALTHY
    BTFSC advertencia,ACCESS
    CALL PRINT_WARNING
    BTFSC critico,ACCESS 
    CALL PRINT_CRITICAL
BSF INTCON, GIEH, ACCESS
    RETURN
;------------------------------------------
; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////ALIMENTAR TAMAGOTCHI
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
    CLRF contador_90s, ACCESS
    DECF comida,F,ACCESS
    BSF sano,ACCESS
    BCF advertencia,ACCESS
    BCF critico,ACCESS
    BSF need_refresh,ACCESS
    RETURN
    

    
MAIN	  

CALL INIT_PORTS
CALL INIT_CONFIG
CALL RESET_INTERRUPTS
CALL START_MATRIX

LOOP
    BTFSS PORTB,0,ACCESS
	CALL BOTON
    BTFSS PORTB,1,ACCESS
	CALL BOTON
    BTFSS PORTB,2,ACCESS
	CALL BOTON
	BTFSC need_refresh,ACCESS
    CALL START_MATRIX
    BTFSC dead,ACCESS 
    GOTO DEAD
    BTFSC reset_valor,ACCESS
    GOTO MAIN


    
GOTO LOOP
	
END
