LIST P=PIC18F4321 F=INHX32
#include <p18f4321.inc>
CONFIG  OSC=HSPLL ; L?oscil.lador.
CONFIG  PBADEN=DIG ; Volem que el PORTB sigui DIGital.
CONFIG  WDT=OFF    ; Desactivem el WatchDog Timer.
CONFIG LVP=OFF


TAULA7S		EQU 0x03A; del 0x3A a 0x41

ORG	    0x0000
GOTO	    MAIN
ORG	    0x0008  ;Interrupts de alta prioridad
GOTO	    HIGH_IRS ;Zona de interrupciones
ORG	    0x0018
RETFIE FAST
;--------------------------------
;   AsignaciÃ³n de variables
;--------------------------------
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
    
contador_10ms	EQU 0x022  ; contador de 99 vueltas
contador_1s    EQU 0x023  ; contador de 99 vueltas
decadas		EQU 0x024  ; almacena las decadas 
baby		EQU 0x025  ; Flag que indica estado baby
teenager	EQU 0x026  ; Flag que indica estado teenager
adult		EQU 0x027  ; Flag que indica estado adult
dead		EQU 0x028  ; Flag que indica la muerte del tamagotchi
contador_1m	EQU 0x029  ; contador de 59 vueltas
flag_16ms		EQU 0X02A ; flag contar 16ms
contador_16ms	EQU 0X02B ; counter 16ms
mode		EQU 0x02C ; menu del juego
random_number         EQU 0x02D ; numero aleatorio 
flag_game		EQU 0x02E 
contador_game	EQU 0x02F
t0		EQU 0X030 ; tiempo que el pmw ha de estar a 0
t1		EQU 0X031 ; Tiempo que ha de estar a 1
tact		EQU 0x032; tiempo actual del estado actual del pmw
comida		EQU 0x033 ;contador de comida
flag_rpulse		EQU 0x034
contador_rpulse	EQU 0x035
random_seed	EQU 0x036
espera_random	EQU 0x037
espera_random_counter EQU 0x038
 espera_random_base EQU 0x039

Posicio_RAM EQU 0x081 ;en la posicion 0x81 de memoria 
 
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
    ;65535 - 1000 = 64536 paradas cada 1ms
 
    MOVLW LOW(.64536)
    MOVWF TMR0L,ACCESS
    MOVLW HIGH(.64536) 
    MOVWF TMR0H,ACCESS
    
    RETURN 
INIT_PORTS
    ;PORTA Todo de salida; grid, servo, RandomGenerated, Menu[2..0]
    MOVLW B'00000000' ;A7 nada, A6 nada, A5 menu 2, A4 menu 1, A3 menu 0, A2 RandomGenerated, A1 servo, A0 grid 
    MOVWF TRISA, ACCESS
    BCF LATA,1,ACCESS

    ;PORTB Todo de entrada; botones Left option, Right option, Select, PCI, ResultPulse, NewNumber
    MOVLW B'11111111' ;B7 nada, B6 nada, B5 NewNumber, B4 ResultPulse, B3 PCI, B2 BtnSelect, B1 BtnLeftOption, B0 BtnRightOption
    MOVWF TRISB, ACCESS
    
    ;PORTC Todo de salida; C7 nada, C6 nada, C5 nada, C4 nada, C3 RandomNumber0, C2 RandomNumber1, C1 RandomNumber2, C0 RandomNumber3
    MOVLW B'00000000'
    MOVWF TRISC, ACCESS
    ;PORTD Todo de salida; 7Seg
    MOVLW B'00000000' ;D7 nada, D6 7Seg6, D5 7Seg5, D4 7Seg4, D3 7Seg3, D2 7Seg2, D1 7Seg1, D0 7Seg0
    MOVWF TRISD, ACCESS
    CLRF espera_random_base
    CLRF contador_game
    CLRF flag_game
    CLRF espera_random_counter
    CLRF contador_10ms,ACCESS
    CLRF contador_1s,ACCESS
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
    
    MOVLW TAULA7S
    MOVWF TBLPTRL
    CLRF TBLPTRH
    RETURN

BOTON
    BSF flag_16ms,1

    MOVLW   .160                 ; ¿hemos llegado a 16ms?
    CPFSEQ  contador_16ms
    GOTO BOTON                        ; si NO es 16ms vuelvo

    ; Si llegamos aquí, contador_16ms == 99
    BCF flag_16ms,1
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
        BSF flag_16ms,1

    MOVLW   .160                 ; ¿hemos llegado a 16ms?
    CPFSEQ  contador_16ms
    GOTO MS16Vuelta                        ; si NO es 16ms vuelvo

    ; Si llegamos aquí, contador_16ms == 99
    BCF flag_16ms,1
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
    ;GOTO ALIMENTAR
    ;GOTO REINICIAR
    RETURN; eliminar
    RETURN


GAME ;poner el numero en latc y latd
	;aleatoriedad
  MOVLW .3
  ADDWF espera_random_base,F,ACCESS
   MOVF espera_random_base,W,ACCESS
  MOVWF espera_random
  MOVF	random_seed,W,ACCESS
  ADDWF  espera_random,F,0
 MOVF	random_number,W,ACCESS
  ADDWF  espera_random,F,0
 MOVF	contador_10ms,W,ACCESS
  ADDWF  espera_random,F,0
;fin aleatoriedad printar el numero
GAME_TU_MADRE
CLRF espera_random_counter,ACCESS  
GAME_ESPERA_3 ;espera aleatoria para el siguiente numero
  MOVF    espera_random,W,ACCESS
  CPFSEQ  espera_random_counter,ACCESS
  GOTO GAME_ESPERA_3
    
  MOVF random_seed,W,ACCESS
  CPFSEQ  random_number,ACCESS
  GOTO CONSEGUIDO
  GOTO GAME_TU_MADRE
CONSEGUIDO
  MOVFF  random_seed,random_number
  MOVFF  random_number,LATC
  CLRF LATD
  CLRF TBLPTRH
  CLRF TBLPTRU
  MOVLW TAULA7S
  MOVWF TBLPTRL
  MOVF    random_number, W, ACCESS
  ADDWF  TBLPTRL,F,0
  TBLRD*
  MOVFF TABLAT,LATD 
  
  BSF flag_game,1
  CLRF contador_game,ACCESS
  
GAME_ESPERA_1 ;(espera visual)
    MOVLW   .100 ;para terminated
    CPFSEQ  contador_game,ACCESS
    GOTO GAME_ESPERA_1
    
    BCF	LATA,2,0 ; cambiamos al estado 2
    CLRF LATD
GAME_ESPERA_2 ;espera parpadeo, aviso de que el numero se va a enviar
    MOVLW   .110
    CPFSEQ  contador_game,ACCESS
    GOTO GAME_ESPERA_2
    
    CLRF espera_random_counter,ACCESS
    
    BSF	LATA,2,0 ; cambiamos al estado 3
GAME_NEW_NUMBER
    BTFSS PORTB,4,0;rpulse
    GOTO FIN_GAME
    BTFSC PORTB,5,0; newnumber
    GOTO GAME_WAIT
    GOTO GAME_NEW_NUMBER
GAME_WAIT; mantener los datos 1 tiempo
      BSF flag_game,1
  CLRF contador_game,ACCESS
GAME_WAIT_1
    MOVLW   .3
    CPFSEQ  contador_game,ACCESS
    GOTO GAME_WAIT_1
    GOTO GAME
    
FIN_GAME
    BSF	LATA,2,0
    BSF flag_rpulse,1
    BTFSC PORTB,4,0;rpulse
    GOTO FIN_GAME
    
    
    MOVLW   .14 ; si rpulse
    CPFSGT  contador_rpulse,ACCESS
    GOTO REINICIO_GAME ;x!>1 todo mal

MAS_COMIDA
    MOVLW   .5
    CPFSEQ  comida, ACCESS   ; ¿contador == 5?
    INCF    comida, F        ; si NO es 5 ? contador++

REINICIO_GAME
          BCF flag_game,1
    BCF flag_rpulse,1 
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

    
    BTFSC flag_16ms,1,ACCESS	; contar 16ms
    INCF    contador_16ms,F,ACCESS         ; contador_seg++
    
        BTFSC flag_rpulse,1,ACCESS	; contar ms de game
    INCF    contador_rpulse,F,ACCESS  
    
    INCF random_seed,F,ACCESS
    MOVLW   .10                  ; ¿hemos llegado a 10 ticks?
    CPFSLT  random_seed,ACCESS           ; ¿contador_10ms == 10?
    CLRF      random_seed,ACCESS
    
    MOVLW   .100                  ; ¿hemos llegado a 10 ticks?
    CPFSEQ  contador_10ms,ACCESS           ; ¿contador_10ms == 10?
    RETURN                        ; si NO es igual
       
    ; Si llegamos aquí, contador_10ms == 10
    CLRF    contador_10ms,ACCESS           ; reseteo a 0
    CALL    BUCLE_SEG             ; acumulo 10ms
    
    RETURN

BUCLE_SEG           ; cuenta segundos, 10ms
    INCF    contador_1s, F,ACCESS         ; contador_seg++
    
    BTFSC flag_game,1,ACCESS	; contar ms de game
    INCF    contador_game,F,ACCESS  
    
    MOVLW   .100                  ; ¿hemos llegado a 1s?(1000ms)
    CPFSEQ  contador_1s,ACCESS  
    RETURN                        ; si NO es 100, salgo
    
    ; Si llegamos aquí, contador_seg == 100
    CLRF    contador_1s,ACCESS           ; reseteo segundos
    CALL    BUCLE_MIN             ; acumulo 1s
    RETURN

BUCLE_MIN           ; cuenta minutos
    INCF    contador_1m, F,ACCESS         ; contador_min++
   
    MOVLW   .2                ; hemos llegado a 1min?
    CPFSEQ  contador_1m,ACCESS  
    RETURN                        ; si aún no he llegado, salgo

    CALL    RESET_BUCLES          ;llegada a la decada
    RETURN
    
RESET_BUCLES
    INCF decadas,F,ACCESS  
    CLRF contador_1m,ACCESS  
    CALL ENVEJECER
    RETURN
    

ENVEJECER    

    MOVLW .3
    CPFSLT decadas,ACCESS ;AquÃ­ cambio a teenager
    BCF baby,ACCESS
    CPFSLT decadas,ACCESS
    BSF teenager,ACCESS
    
    MOVLW .6
    CPFSLT decadas,ACCESS ;AquÃ­ cambio a adult
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
    MOVLW .10
    MOVWF decadas, ACCESS
    RETURN
PMW

    INCF tact, F

    ; t1 = 2*decadas + 5
    MOVF    decadas, W
    MULLW   .2
    MOVF PRODL, W
    ADDLW .5
    MOVWF t1

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

MAIN	  

;CALL FIRE
CALL INIT_PORTS
CALL INIT_CONFIG
CALL RESET_INTERRUPTS

LOOP
    BTFSS   PORTB,0,ACCESS
	CALL BOTON
    BTFSS   PORTB,1,ACCESS
	CALL BOTON
    BTFSS   PORTB,2,ACCESS
	CALL BOTON

    
GOTO LOOP
	
END