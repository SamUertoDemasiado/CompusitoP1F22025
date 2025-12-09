LIST P=PIC18F4321 F=INHX32
#include <p18f4321.inc>
CONFIG  OSC=HSPLL ; L?oscil.lador.
CONFIG  PBADEN=DIG ; Volem que el PORTB sigui DIGital.
CONFIG  WDT=OFF    ; Desactivem el WatchDog Timer.

ORG	    0x0000
GOTO	    MAIN
ORG	    0x0008  ;Interrupts de alta prioridad
GOTO	    HIGH_IRS ;Zona de interrupciones
ORG	    0x0018
RETFIE  FAST  

;--------------------------------
;   Asignación de variables
;--------------------------------
   

flag_bucle200   EQU 0x21  ; flag de que he contado 200 vueltas
contador_200	EQU 0x22  ; contador de 200 vueltas
contador_30     EQU 0x23  ; contador de 30 vueltas
decadas		EQU 0x24  ; almacena las decadas 
baby		EQU 0x25  ; Flag que indica estado baby
teenager	EQU 0x26  ; Flag que indica estado teenager
adult		EQU 0x27  ; Flag que indica estado adult
dead		EQU 0x28  ; Flag que indica la muerte del tamagotchi

Posicio_RAM EQU 0x81 ;en la posicion 0x81 de memoria 
 
BCF flag_bucle200,ACCESS
MOVLW .55
MOVWF contador_200,ACCESS
CLRF contador_30,ACCESS
BSF baby,ACCESS
CLRF decadas,ACCESS


INIT_PORTS
    ;PORTA Todo de salida; grid, servo, RandomGenerated, Menu[2..0]
    MOVLW B'00000000' ;A7 nada, A6 nada, A5 menu 2, A4 menu 1, A3 menu 0, A2 RandomGenerated, A1 servo, A0 grid 
    MOVWF TRISA, ACCESS
    ;PORTB Todo de entrada; botones Left option, Right option, Select, PCI, ResultPulse, NewNumber
    MOVLW B'11111111' ;B7 nada, B6 nada, B5 NewNumber, B4 ResultPulse, B3 PCI, B2 BtnSelect, B1 BtnLeftOption, B0 BtnRightOption
    MOVWF TRISB, ACCESS
    ;PORTC Todo de salida; C7 nada, C6 nada, C5 nada, C4 nada, C3 RandomNumber0, C2 RandomNumber1, C1 RandomNumber2, C0 RandomNumber3
    MOVLW B'00000000'
    MOVWF TRISC, ACCESS
    ;PORTD Todo de salida; 7Seg
    MOVLW B'00000000' ;D7 nada, D6 7Seg6, D5 7Seg5, D4 7Seg4, D3 7Seg3, D2 7Seg2, D1 7Seg1, D0 7Seg0
    MOVWF TRISD, ACCESS
    RETURN
    

INIT_CONFIG
    CLRF TRISC
    MOVLW B'11100000'
    MOVWF INTCON, ACCESS
    MOVLW B'10000000'
    MOVWF T0CON, ACCESS
    BSF RCON,IPEN ;Se activan las high-priority
    MOVLW B'11010000'
    MOVWF INTCON3
    
    RETURN

RESET_INTERRUPTS
    ;Tins = 4/40MHz = 100ns
    ;10ms/100ns = 100k tics
    ;Usamos el timer0 de 16 bits (2^16 = 65536)
    ;65535 - 3125 = 62411. Se ha usado el 1:32 preescaler 100k/32 = 3125
 
    MOVLW LOW(.62411) 
    MOVWF TMR0L,ACCESS
    MOVLW HIGH(.62411) 
    MOVWF TMR0H,ACCESS
    
    RETURN 

HIGH_IRS
    ;La interrupcion saltara cada 10ms
    BTFSC INTCON, TMR0IF,ACCESS
    CALL TMR0_INTERRUPT
    RETFIE FAST

TMR0_INTERRUPT
    BSF PORTC,0
    BCF PORTC,0
    CALL RESET_INTERRUPTS
    BCF INTCON, TMR0IF,ACCESS ;es el bit 2 del INTCON
    RETURN

BUCLE_200
    BSF flag_bucle200,ACCESS
    INCFSZ contador_200,ACCESS
    BCF flag_bucle200,ACCESS
    BTFSS flag_bucle200,ACCESS
    CALL BUCLE_30
    RETURN

BUCLE_30
    MOVLW .55
    MOVWF contador_200,ACCESS
    INCF contador_30,ACCESS
    MOVLW .30   
    CPFSLT contador_30,ACCESS ;Comparo 30 a ver si he llegado a 30, si he llegado a 30 reinicio el contador_30
    ;Comparo 30 a ver si he llegado a 30, si he llegado a 30, cuando vaya a contar 31, me suma 1 decada
    CALL RESET_BUCLES
    
    RETURN

RESET_BUCLES
    INCF decadas,ACCESS
    CLRF contador_30,ACCESS
    CALL ENVEJECER
    RETURN
    

ENVEJECER    
    MOVLW .3
    CPFSLT decadas,ACCESS ;Aquí cambio a teenager
    BCF baby,ACCESS
    CPFSLT decadas,ACCESS
    BSF teenager,ACCESS
    
    MOVLW .6
    CPFSLT decadas,ACCESS ;Aquí cambio a adult
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
    
    

    

FIRE ;FUNCION DEBUG

MOVLW   b'00000000'
MOVWF   TRISD
MOVLW   b'10101010'
MOVWF   PORTD

GOTO FIRE


MAIN	  

;CALL FIRE
CALL INIT_CONFIG
CALL RESET_INTERRUPTS
CALL TMR0_INTERRUPT

GOTO LOOP

LOOP

GOTO LOOP
END


;Las prioridades high son de timer
;Las proridades low son para perifericos
    


    
