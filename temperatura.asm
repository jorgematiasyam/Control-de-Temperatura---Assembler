;*******************************************************************
;P0= entrada del valor de T del sensor
;P2= multiplexor de los 7segmentos
;P3= interrupciones						      
;*******************************************************************						      
						      org 0
                        ajmp inicio
                        org 03h
                        ajmp subir
                        org 13h
                        ajmp bajar
                        org 0bh
                        ajmp timer0
                        org 1bh
						      ajmp timer1
						      
                        
                        org 100h
inicio:						setb EA                                                       
								setb EX0                ;habilita int externas
								setb EX1
								setb IT0                ;habilita interrupcion
								setb IT1                ;por flanco descendente
                        setb P2.3
						      setb ET0  		         ;habilita interrupcion por timer0
						      setb ET1  		         ;habilita interrupcion por timer1
						      mov  TMOD,#11h
						      setb P2.0
								setb P2.1
								setb P2.2
								mov DPTR,#tabla
								clr C
								mov 20h,#00h            ;cifra decimal
								mov 21h,#00h            ;cifra unidad
								mov 22h,#00h            ;cifra decena
								mov 70h,#30d            ;contador
								mov 1Ch,#0ffh           ;subir antirebote
								mov 1Dh,#20d
								mov 1Eh,#0ffh
								mov 1Fh,#20d
								
       rutinas:		  	ACALL barrido
								nop
								ACALL lector
								nop
								ACALL barrido
								nop
								ACALL definir_timers
								nop
								ACALL barrido
								nop
								ACALL pwm
								nop
								ACALL barrido
								nop
								sjmp rutinas
								
empieza:
barrido:
   decimal:					mov DPTR,#tabla
                        mov A,20h		
								movc A,@A+DPTR
								mov P1,A
								clr P2.2
ciclo3:						nop
								djnz 70h,ciclo3
								setb P2.2
								mov 70h,#30d
   unidad:              mov A,21h
                        movc A,@A+DPTR
                        mov P1,A
                        clr P2.0                 ;enciende 
ciclo:                  nop                      ;espera un tiempo para
               			djnz 70h,ciclo           ;escribir en el display unidad 
                        setb p2.0                ;apaga un 7segmentos
                        mov 70h,#30d             ;recarga el contador
    decena:             mov A,22h                         
                        movc A,@A+DPTR
                        mov P1,A                 ;escribe el numero que esta en la direc 40h
                        clr P2.1                 ;apaga el display
ciclo2:                 nop                      ;espera un tiempo para
                        djnz 70h,ciclo2          ;escribir en el display decena
                        setb P2.1                ;enciende el 7segmentos
                        mov 70h,#30d             ;recarga el contador
								nop
																 ;ojoooo			
								ret
								
subir:                  
  ;subir_antirebote:     mov A,1Ch
  ;                      dec A
  ;                      mov 1Ch,A
  ;                      cjne A,#00h,subir_antirebote
  ;                      mov A,1Dh
  ;                      dec A
  ;                      mov 1Dh,A
  ;                      cjne A,#00h,subir_antirebote


                        mov A,22h
                        cjne A,#4h,falta
                        sjmp terminasube
falta:                  
                        mov A,20h
                        cjne A,#9,subir_decimal
                        sjmp subir_unidad
subir_decimal:          mov A,20h
                        inc A
                        mov 20h,A                        
                        nop                      
                        sjmp terminasube
subir_unidad:           mov A,21h
                        cjne A,#9h,seguir_unidad
                        sjmp subir_decena
   seguir_unidad:       mov A,#00h
   			            mov 20h,A
                        mov A,21h
                        inc A
                        mov 21h,A
                        sjmp terminasube
   subir_decena:        mov A,#00h
                        mov 20h,A
                        mov A,#00h
                        mov 21h,A
                        mov A,22h
                        inc A
                        mov 22h,A                     								                             
terminasube:            reti     
     
bajar:                  
;bajar_antirebote:       mov A,1Eh
;                        dec A
;                        mov 1Eh,A
;                        cjne A,#00h,bajar_antirebote
;                        mov A,1Fh
;                        dec A
;                        mov 1Fh,A
;                        cjne A,#00h,bajar_antirebote



                        mov A,22h
                        cjne A,#00h,bajar_decimal       ;pregunta si estan todos
                        sjmp preg_unidad                ;los digitos en cero
   preg_unidad:         mov A,21h
                        cjne A,#00h,bajar_decimal
                        sjmp preg_decimal
   preg_decimal:        mov A,20h
                        cjne A,#00h,bajar_decimal
                        sjmp terminabajar
                         
   bajar_decimal:       mov A,20h
                        cjne A,#00h,seguir_decimal
                        sjmp bajar_unidad                     
      seguir_decimal:   mov A,20h
                        dec A
                        mov 20h,A                     
                        sjmp terminabajar
   bajar_unidad:        mov A,21h
                        cjne A,#00,seguir_unidad_bajar
                        sjmp bajar_decena
seguir_unidad_bajar:    mov A,#9h
                        mov 20h,A
                        mov A,21h
                        dec A
                        mov 21h,A
                        sjmp terminabajar
    bajar_decena:       mov A,#9h
    							mov 20h,A
    							mov A,#9h
    							mov 21h,A
    							mov A,22h
    							dec A
    							mov 22h,A                     
                        sjmp terminabajar
terminabajar:				reti                                                               
    
tabla: 						DB 3fh,6h,5bh,4fh,66h,6dh,7ch,7h,7fh,67h 
;*******************************************************************
;leo la temperatura existente en el ambiente
;******************************************************************* 				
				   org 250h
lector:			;mov  A,P2.4                     ;si quiero poner una llave
               ;jz   no_comienza                ;de encendido
               ;jnz  comienzo_lectura
;no_comienza:   ljmp fin               

comienzo_lectura:
               mov   DPH,#00h
               mov   DPL,#00h        
               sjmp inicio_lectura	    
inicio_lectura:				
					;mov     65h,#00h
					;mov     66h,#00h
					;mov     6Dh,#00h
					;mov     6Eh,#00h
					setb    P2.3
					nop
					nop
					nop
					nop
					nop
					nop     
					clr     P2.3
					nop
					nop
					nop
					nop
					nop
					setb    P2.3
					mov     A,P0             ;valor de entrada X 40d
					mov     R0,A
					mov     B,#28h
					mul     AB
					mov     33h,B            ;resultado de la 
					mov     32h,A				 ;multiplicacion
				
				
division:      mov     r3,#00h          ; parte alta del divisor
               mov     r2,#0ffh         ; parte baja del divisor
               mov     r1,33h           ; parte alta del dividendo (resultado anterior)
               mov     r0,32h           ; parte baja del dividendo
               sjmp    UDIV16
               
UDIV16:        mov     r7, #0          ; clear partial remainder
               mov     r6, #0
               mov     B, #16          ; set loop count

div_loop:      clr     C               ; clear carry flag
               mov     A, r0           ; shift the highest bit of
               rlc     A               ; the dividend into...
               mov     r0, A
               mov     A, r1
               rlc     A
               mov     r1, A
               mov     A, r6           ; ... the lowest bit of the
               rlc     A               ; partial remainder
               mov     r6, A
               mov     A, r7
               rlc     A
               mov     r7, A
               mov     A, r6           ; trial subtract divisor
               clr     C               ; from partial remainder
               subb    A, r2
               mov     dpl, A
               mov     A, r7
               subb    A, r3
               
               mov     dph, A
               cpl     C               ; complement external borrow
               jnc     div_1           ; update partial remainder if
                                       ; borrow
               mov     r7, dph         ; update partial remainder
               mov     r6, dpl
div_1:         mov     A, r4           ; shift result bit into partial
               rlc     A               ; quotient
               mov     r4, A
               mov     A, r5
               rlc     A
               mov     r5, A
               djnz    B, div_loop
               mov     A, r5           ; put quotient in r0, and r1
               mov     r1, A
               mov     A, r4
               mov     r0, A
               mov     A, r7           ; get remainder, saved before the
               mov     r3, A           ; last subtraction
               mov     A, r6
               mov     r2, A
               
               mov     31h,R1          ;resultado final en posiciones 30h y 31h
               mov     30h,R0          ;es la parte entera del resultado 
               mov     35h,R3          ;mueve el resto a la 
               mov     34h,R2          ;posicion de memoria 33h y 34h
               mov     A,34h
               mov     B,#0Ah          ;10h o A ????
               mul     AB
               mov     35h,B
               mov     34h,A
                                 
               sjmp    division_decimal
;****************************************************************************               
;a partir de aca cálculo de decimal              
;****************************************************************************  
             
division_decimal:
               mov     r3,#00h          ; parte alta del divisor
               mov     r2,#0ffh         ; parte baja del divisor
               mov     R1,35h           ; parte alta del dividendo
               mov     R0,34h           ; parte baja del dividendo
               sjmp    UDIV16_decimal
               
UDIV16_decimal:
               mov     r7, #0          ; clear partial remainder
               mov     r6, #0
               mov     B, #16          ; set loop count
div_loop_decimal:
               clr     C               ; clear carry flag
               mov     A, r0           ; shift the highest bit of
               rlc     A               ; the dividend into...
               mov     r0, A
               mov     a, r1
               rlc     A
               mov     r1, A
               mov     A, r6           ; ... the lowest bit of the
               rlc     A               ; partial remainder
               mov     r6, A
               mov     A, r7
               rlc     A
               mov     r7, A
               mov     A, r6           ; trial subtract divisor
               clr     C               ; from partial remainder
               subb    A, r2
               mov     dpl, A
               mov     A, r7
               subb    A, r3
               
               mov     dph, A
               cpl     C               ; complement external borrow
               jnc     div_1_decimal           ; update partial remainder if
                                       ; borrow
               mov     r7, dph         ; update partial remainder
               mov     r6, dpl
div_1_decimal: mov     A, r4           ; shift result bit into partial
               rlc     A               ; quotient
               mov     r4, A
               mov     A, r5
               rlc     A
               mov     r5, A
               djnz    B, div_loop_decimal
               mov     A, r5           ; put quotient in r0, and r1
               mov     r1, A
               mov     A, r4
               mov     r0, A
               mov     A, r7           ; get remainder, saved before the
               mov     r3, A           ; last subtraction
               mov     A, r6
               mov     r2, A
               mov     41h,R1          ;dejo el resto en 40h y 41h representando
               mov     40h,R0          ;el primer digito decimal
               sjmp    ajuste
               
;***********************************************************************
;calculo la diferencia entre                
;la temperatura elegida y la medida
;***********************************************************************                                          
ajuste:        mov  A,40h
               mov  B,#10d
               div  AB
               swap A
               add  A,B
               mov  50h,A
               sjmp aj_unidad
aj_unidad:     mov  B,#00h
               mov  A,30h
               mov  B,#10d
               div  AB
               swap A
               add  A,B
               mov  51h,A
               sjmp aj_decena
aj_decena:     mov  B,#00h
               mov  A,31h
               mov  B,#10h
               div  AB
               swap A
               add  A,B
               sjmp aj_eleccion          
aj_eleccion:   mov  B,21h
               mov  A,22h
               swap A
               add  A,B
               mov  49h,A
               mov  A,20h
               mov  48h,A         ;ojo
               mov  B,#10d
               mov  A,22h
               mul  AB
               add  A,21h
               mov  38h,A         ;ojo          
                           
                             
resta:         clr  C
               mov  A,50h
               subb A,48h
               jc   pido_uno
               jnc  no_pido_uno
pido_uno:      clr  C
               mov  A,50h
               add  A,#0Ah
               subb A,48h
               mov  52h,A            ;RESULTADO DE LA RESTA PARTE DECIMAL
               clr  C
               mov  A,30h
               subb A,#1h
               subb A,38h
               mov  55h,A            ;55h valor en hexa de la resta
               sjmp aj_resta
no_pido_uno:   clr  C
               mov  A,50h
               subb A,48h
               mov  52h,A
               mov  A,30h
               subb A,38h
               mov  55h,A            ;55h valor en hexa de la resta           
               sjmp aj_resta
aj_resta:      mov  B,#10d
               div  AB
               swap A
               add  A,B
               mov  53h,A            ;RESULTADO DE LA RESTA PARTE ENTERA  
               sjmp fin
;--------------------------------------------------
                                           
;traba:         sjmp traba    
    fin:       nop
               ret
definir_timers:
;               mov  A,P2.4            ;llave encendido
;               jz   no_definir
;               jnz  comienzo_definir
;no_definir:    ljmp fin_timers

comienzo_definir:
               mov  R0,#0              ;multiplicando parte baja
               mov  R1,#0              ;multiplicando parte alta
               mov  R2,#0              ;multiplicador parte baja
               mov  R3,#0              ;multiplicador parte alta
               mov  R4,#0       
               mov  R5,#0
               mov  R6,#0
               mov  R7,#0
               sjmp UMUL16
UMUL16:        mov    R0,#00h
               mov    R1,55h
               mov    R2,#84h          ;0384h valor del timer para 900 useg
               mov    R3,#03h          ;es el max valor para la maxima diferencia

               push    B
               push    dpl
               mov     a, r0
               mov     b, r2
               mul     ab              ; multiply XL x YL
               push    acc             ; stack result low byte
               push    b               ; stack result high byte
               mov     a, r0
               mov     b, r3
               mul     ab              ; multiply XL x YH
               pop     00H
               add     a, r0
               mov     r0, a
               clr     a
               addc    a, b
               mov     dpl, a
               mov     a, r2
               mov     b, r1
               mul     ab              ; multiply XH x YL
               add     a, r0
               mov     r0, a
               mov     a, dpl
               addc    a, b
               mov     dpl, a
               clr     a
               addc    a, #0
               push    acc             ; save intermediate carry
               mov     a, r3
               mov     b, r1
               mul     ab              ; multiply XH x YH
               add     a, dpl
               mov     r2, a
               pop     acc             ; retrieve carry
               addc    a, b
               mov     r3, a
               mov     r1, 00H
               pop     00H             ; retrieve result low byte
               pop     dpl
               pop     B
               mov     A,R2
               mov     58h,A
               mov     A,R1
               mov     59h,A
               ;mov     A,R2
               ;mov     5ah,A
               ;mov     A,R3
               ;mov     5bh,A
               sjmp    div_timers
;empezar division--------------------------
div_timers:                      
               mov  R0,#0              
               mov  R1,#0              
               mov  R2,#0              
               mov  R3,#0              
               mov  R4,#0       
               mov  R5,#0
               mov  R6,#0
               mov  R7,#0
               mov  R1,58h
               mov  R0,59h
               mov  R3,#00h
               mov  R2,#28h

UDIV16_timers: 
               mov     B, #16          ; set loop count
               
div_loop_timers:
               clr     C               ; clear carry flag
               mov     a, r0           ; shift the highest bit of
               rlc     a               ; the dividend into...
               mov     r0, a
               mov     a, r1
               rlc     a
               mov     r1, a
               mov     a, r6           ; ... the lowest bit of the
               rlc     a               ; partial remainder
               mov     r6, a
               mov     a, r7
               rlc     a
               mov     r7, a
               mov     a, r6           ; trial subtract divisor
               clr     C               ; from partial remainder
               subb    a, r2
               mov     dpl, a
               mov     a, r7
               subb    a, r3
               mov     dph, a
               cpl     C               ; complement external borrow
               jnc     div_1_timers    ; update partial remainder if
                                       ; borrow
               mov     r7, dph         ; update partial remainder
               mov     r6, dpl
div_1_timers:  mov     a, r4           ; shift result bit into partial
               rlc     a               ; quotient
               mov     r4, a
               mov     a, r5
               rlc     a
               mov     r5, a
               djnz    B, div_loop_timers
               mov     a, r5           ; put quotient in r0, and r1
               mov     r1, a
               mov     a, r4
               mov     r0, a
               mov     a, r7           ; get remainder, saved before the
               mov     r3, a           ; last subtraction
               mov     a, r6
               mov     r2, a
               mov     60h,R1          ;valores para setear             
               mov     61h,R0          ;los timers del PWM
              
               
               sjmp    fin_timers
traba:         sjmp traba      
fin_timers:    nop
               ret 
         
;***************************************************************    
;--seteo timers PWM
;***************************************************************
pwm:    
;               mov  A,P2.4
;               jz   no_pwm               ;llave encendido
;               jnz  comienzo_pwm
;no_pwm:        ljmp terminar
               
               
comienzo_pwm:  mov  A,10h
               jnz  terminar
               mov  R0,#0      ;parte baja del timer0
               mov  R1,#0      ;parte alta del timer0
               mov  R2,#0      ;parte baja del timer1
               mov  R3,#0      ;parte alta del timer1
               mov  R4,#0       
               mov  R5,#0
               mov  R6,#0
               mov  R7,#0
               clr  C
               
               mov  TMOD,#11h
               mov  A,#0ffh
               subb A,60h
               mov  TH0,A
               mov  A,#0ffh
               subb A,61h
               mov  TL0,A
															
					setb  TR0          ;arranque del timer0
					mov   10h,#1d
				
               sjmp  terminar
timer0:							        ;diferencia de ffff- 61h 60h
						mov   2Dh,A        ;para setear el timer
						;mov  TH0,#00h
						;mov  TL0,#00h
						mov  A,#0ffh
                  subb A,60h
                  mov 65h,A        ;65h parte Alta
                  ;mov  TH0,A
                  mov  A,#0ffh
                  subb A,61h
                  mov 66h,A        ;66h parte baja
                  ;mov  TL0,A       ;quedaron guardados en memoria 65h y 66h
						mov   A,#03h
						subb  A,60h
						mov   62h,A
						mov   A,#0FFh     ;3FF, para que sea una señal de 1KHZ
						subb  A,61h
						mov   63h,A
						mov   A,#0ffh
						subb  A,62h
						mov   6Dh,A
						;mov   TH1,A
						mov   A,#0ffh
						subb  A,63h
						mov   6Eh,A
						;mov   TL1,A
						mov   TH0,65h
						mov   TL0,66h
						mov   TH1,6Dh
						mov   TL1,6Eh
						mov   A,2Dh			
						
						clr  P2.5        ;pone la pata P2.5 en 1 (salida pwm)
						clr  TR0
						setb TR1
						reti
						
timer1:									
						PUSH  ACC
						mov   TH1,6Dh
						mov   TL1,6Eh
																		
						setb  P2.5       ;apaga la salida P2.5 (salida pwm)
						setb TR0
						clr  TR1
						POP  ACC
						reti
					               
                              
terminar:      nop
               ret             
          
          END
