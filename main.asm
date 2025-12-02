LIST P=PIC18F4321 F=INHX32
#include <p18f4321.inc>
CONFIG  OSC=HSPLL ; L?oscil.lador.
CONFIG  PBADEN=DIG ; Volem que el PORTB sigui DIGital.
CONFIG  WDT=OFF    ; Desactivem el WatchDog Timer.

ORG	    0x0000
GOTO	    MAIN
ORG	    0x0008  ;Interrupts de alta prioridad
GOTO	    HIGH_IRS
;GOTO    INTERRUPTS
ORG	    0x0018
RETFIE  FAST  

;--------------------------------
;   Asignación de variables
;--------------------------------
;Tengo que pensar nombres mejores (let me think)
flag        EQU 0x21  ; BUCLE_200 
valor       EQU 0x22  ; BUCLE_200, BUCLE_30
valor_2     EQU 0x23  ; BUCLE_30
decadas     EQU 0x24  ; BUCLE_30

Posicio_RAM EQU 0x81 ;en la posicion 0x81 de memoria 



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
    RETURN

RESET_INTERRUPTS
    ;Tins = 4/40MHz = 100ns
    ;10ms/100ns = 100k tics
    ;Usamos el timer0 de 16 bits (2^16 = 65536)
    ;65535 - 50000 = 15536 (3CB0 en hexa) Se ha usado el 1:2 preescaler 100k/2 = 50k
    
    ;------MIRAR DE AUMENTAR EL PRESCALER A 8---------------
    MOVLW 0xB0 ;B0 parte baja
    MOVWF TMR0L,0
    MOVLW 0x3C ;3C parte alta, es el valor TMR0H
    MOVWF TMR0H,0
   
    ; Activar Timer0
    BSF T0CON, TMR0ON, 0
    ; Activar interrupción Timer0
    ;BSF INTCON, TMR0IE, 0
    RETURN 

HIGH_IRS
    ;La interrupcion saltara cada 10ms
    BTFSC INTCON, TMR0IF,0
    GOTO TMR0_INTERRUPT
    RETFIE FAST

TMR0_INTERRUPT
    BSF PORTC,0
    BCF PORTC,0
    CALL RESET_INTERRUPTS
    BCF INTCON, TMR0IF ;es el bit 2 del INTCON

BUCLE_200
    BSF flag,0
    INCFSZ valor,0
    BCF flag,0
    BTFSC flag,0
    CALL BUCLE_30
    RETURN

BUCLE_30
    INCF valor_2,0
    MOVLW valor_2
    MOVWF valor, ACCESS
    CPFSEQ valor
    INCF decadas,0
    RETURN

FIRE ;FUNCION DEBUG

MOVLW   b'00000000'
MOVWF   TRISD
MOVLW   b'10101010'
MOVWF   PORTD

GOTO FIRE


MAIN	  
;BSF RCON,IPEN ;Se activan las high-priority


;MOVLW B'11010000'
;MOVWF INTCON3
;CALL FIRE
CALL INIT_CONFIG
CALL RESET_INTERRUPTS
GOTO LOOP

LOOP

GOTO LOOP
END


;Las prioridades high son de timer
;Las proridades low son para perifericos
    


    
