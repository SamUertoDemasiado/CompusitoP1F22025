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
;   AsignaciÃ³n de variables
;--------------------------------
   

contador_10ms	EQU 0x22  ; contador de 99 vueltas
contador_1s    EQU 0x23  ; contador de 99 vueltas
decadas		EQU 0x24  ; almacena las decadas 
baby		EQU 0x25  ; Flag que indica estado baby
teenager	EQU 0x26  ; Flag que indica estado teenager
adult		EQU 0x27  ; Flag que indica estado adult
dead		EQU 0x28  ; Flag que indica la muerte del tamagotchi
contador_1m	EQU 0x29  ; contador de 59 vueltas
t0		EQU 0X30 ; tiempo que el pmw ha de estar a 0
t1		EQU 0X31 ; Tiempo que ha de estar a 1
tact		EQU 0x32; tiempo actual del estado actual del pmw
		
Posicio_RAM EQU 0x81 ;en la posicion 0x81 de memoria 


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
    
    CLRF contador_10ms,ACCESS
 CLRF contador_1s,ACCESS
 CLRF contador_1m,ACCESS
 CLRF t0,ACCESS
 CLRF t1,ACCESS
 CLRF tact,ACCESS
BSF baby,ACCESS
CLRF decadas,ACCESS
BSF LATA,0,ACCESS
 BSF LATA,1 
    RETURN
    

INIT_CONFIG
    CLRF TRISC
    MOVLW B'11100000'
    MOVWF INTCON, ACCESS
    MOVLW B'10001000'
    MOVWF T0CON, ACCESS
    BSF RCON,IPEN ;Se activan las high-priority
    MOVLW B'11010000'
    MOVWF INTCON3
    
    RETURN

RESET_INTERRUPTS
    ;Tins = 4/40MHz = 100ns
    ;0,1ms/100ns = 1k tics
    ;Usamos el timer0 de 16 bits (2^16 = 65536)
    ;65535 - 1000 = 64535
 
    MOVLW LOW(.64535) 
    MOVWF TMR0L,ACCESS
    MOVLW HIGH(.64535) 
    MOVWF TMR0H,ACCESS
    
    RETURN 

HIGH_IRS
    ;La interrupcion saltara cada 10ms
    BTFSC INTCON, TMR0IF,ACCESS
    CALL TMR0_INTERRUPT
    RETFIE FAST

TMR0_INTERRUPT
    CALL RESET_INTERRUPTS
    BCF INTCON, TMR0IF,ACCESS ;es el bit 2 del INTCON
    CALL BUCLE_10MS
    CALL PMW
    RETURN

BUCLE_10MS          ; cuenta 10ms
    INCF    contador_10ms, F      ; contador_10ms++
    
    MOVLW   .99                  ; ¿hemos llegado a 100 ticks?
    CPFSEQ  contador_10ms         ; ¿contador_10ms == 100?
    RETURN                        ; si NO es igual

    ; Si llegamos aquí, contador_10ms == 100
    CLRF    contador_10ms         ; reseteo a 0
    CALL    BUCLE_SEG             ; acumulo 10ms
    RETURN

BUCLE_SEG           ; cuenta segundos
    INCF    contador_1s, F       ; contador_seg++

    MOVLW   .99                  ; ¿hemos llegado a 1000ms?
    CPFSEQ  contador_1s
    RETURN                        ; si NO es 100, salgo

    ; Si llegamos aquí, contador_seg == 99
    CLRF    contador_1s          ; reseteo segundos
    CALL    BUCLE_MIN             ; acumulo 1 minuto
    RETURN

BUCLE_MIN           ; cuenta minutos
    INCF    contador_1m, F       ; contador_min++

    MOVLW   .59                    ; hemos llegado a 1min?
    CPFSEQ  contador_1m
    RETURN                        ; si aún no he llegado, salgo

    CALL    RESET_BUCLES          ;llegada a la decada
    RETURN
    
RESET_BUCLES
    INCF decadas,F
    CLRF contador_1m
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

    MOVLW   .200      ; periodo total = 200 ticks = 20ms
    MOVWF   t0
    MOVF    t1, W
    SUBWF   t0, F     ; t0 = 200 - t1

    MOVF t1, W
    BTFSS PORTA,1,ACCESS ; si RA1=1 ? usa t1, si no ? t0
    MOVF t0, W

    CPFSLT tact       ; si tact < W ? salta BTG/CLRF
    BTG LATA,1        ; si tact >= W ? toggle
    CPFSLT tact
    CLRF tact
    RETURN

FIRE ;FUNCION DEBUG

MOVLW   b'00000000'
MOVWF   TRISD
MOVLW   b'10101010'
MOVWF   PORTD

GOTO FIRE


MAIN	  

;CALL FIRE
CALL INIT_PORTS
CALL INIT_CONFIG
CALL RESET_INTERRUPTS

LOOP
BSF LATA,2,0

GOTO LOOP
END


;Las prioridades high son de timer
;Las proridades low son para perifericos
    


    
