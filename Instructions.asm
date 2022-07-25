.MODEL SMALL
.STACK 64
.DATA
REG DB 32 DUP(00H)
CARRY DB ?
STACK1 DB 100 DUP(?)
NUM2 DB ?
NUM4 Db 2 DUP(?)
power_stuck DB 1
stuck_val DB 0
dataline DB 3

ORG 100

CHECKEVEN MACRO 
    LOCAL label67
    LOCAL Regindirect2

    POP AX
    CMP AL,'B'
    JE Regindirect2

    CMP AL,'D'
    JE Regindirect2

    CMP AL,'R'
    JE Regindirect2

    JMP Error2

    Regindirect2:
    sub al,42h   ;0 2 10
    mov ah,00
    MOV BX,AX  

    mov Dh,REG[BX] 
    mov DL,REG[BX+1]
    cmp DX,0fh
    jbe label67
    jmp Error2
    ;DX  0001 ADDRESS
    label67:
    add DX,10H ;==> MOV TO INDEX 17 DEC==> HEX 11
    mov BX,DX  ;1235h
    
ENDM CHECKEVEN

SMALLEST MACRO X
    LOCAL LABEL13
    LOCAL smalllet3
    MOV AH,1
    INT 21H 

    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL13
    JMP X ;CHECK IF CAPITAL LETTER
    LABEL13:

    CMP AL,78H  ;CHECK IF INRANGE TILL SMALL x  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE smalllet3
    JMP  Error2 ;he eneteed from x ....

    ;small letter 8 bit reg or direct addressing mode
    smalllet3: SUB AL,59H  ;8 9... 1f
    MOV AH,0 
    MOV BX,AX 

ENDM SMALLEST

readnum MACRO params
    LOCAL LABEL74
    LOCAL LABEL75
    LOCAL LABEL76
    LOCAL LABEL77

    mov ah,01h 
    int 21h 

    CMP AL,41H
    JB LABEL74
    CMP AL,46H
    JBE LABEL75
    JMP Error2
    LABEL75:
    SUB AL,37H
    JMP params

    LABEL74:
    CMP AL,30H
    JAE LABEL76
    JMP Error2
    LABEL76:
    CMP AL,39h
    JBE LABEL77
    JMP Error2
    LABEL77:
    SUB AL,30h


ENDM readnum

read2num MACRO 
    LOCAL LABEL82
    LOCAL LABEL83
LOCAL nopower
    LOCAL itsone

    readnum LABEL82

    LABEL82:
    MOV CL,10H
    MUL CL
    MOV NUM2,AL

    readnum LABEL83

    LABEL83:
    ADD NUM2,AL

    cmp power_stuck,1
    jnz nopower
    cmp stuck_val,0
    jnz itsone

    mov al,11111110b
    mov cl,dataline
    rol al,cl
    and num2,al
    jmp nopower ;; to exit only

    itsone:
    mov al,00000001b
    mov cl,dataline
    rol al,cl
    or num2,al

    nopower:   
            
ENDM read2num

read4num MACRO 
    LOCAL LABEL80
    LOCAL LABEL81
    LOCAL LABEL82
    LOCAL LABEL83
            
    readnum LABEL80

    LABEL80:
    MOV CL,10H
    MUL CL
    MOV NUM4,AL

    readnum LABEL81

    LABEL81:
    ADD NUM4,AL

    readnum LABEL82

    LABEL82:
    MOV CL,10H
    MUL CL
    MOV NUM4+1,AL

    readnum LABEL83

    LABEL83:
    ADD NUM4+1,AL
            
ENDM read4num

.CODE

MAIN PROC FAR
    MOV AX,@DATA
    MOV DS,AX

    ;load stack pointer 
   LEA AX,STACK1
   MOV REG[4],AH
   MOV REG[5],AL

    MOV REG[8],12H
    MOV REG[9],34H

    MOV REG[10],23H
    MOV REG[11],45H
    
    MOV REG,00H
    MOV REG[1],01H

    MOV REG[17],21H
    MOV REG[18],12H
    MOV REG[13],4H

    MOV CARRY,1

    mov cx,5

loop1:
    PUSH CX
    ;Reading the insturction 0-f
    MOV AH,1
    INT 21H

    
    ;Find instruction
    CMP AL,30H ; COMPARE CHAR READ WITH 0
    JZ MOVFUNC  ; INSTRUCTION MOVE
    ;///////////////////////////////////////////////////////////////////////////////////////////
    CMP AL,31H
    JNZ CLOSE
    JMP EXIT
    CLOSE:

    CMP AL,32H
    JNZ CLOSE1
    JMP CLCFUNC
    CLOSE1:

    CMP AL,33H
    JNZ CLOSE2
    JMP INCFUNC ;INSTRUCTION INC
    CLOSE2:

    CMP AL,34H
    JNZ CLOSE3
    JMP DECFUNC ;DEC INS
    CLOSE3:

    CMP AL,35H ;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JNZ CLOSE4
    JMP SHLFUNC ;SHL INS
    CLOSE4:

    CMP AL,36H ;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JNZ CLOSE5
    JMP SHRFUNC ;SHR INS
    CLOSE5:


    CMP AL,37H
    JNZ CLOSE6
    JMP PUSHFUNC ;PUSH INS
    CLOSE6:

    CMP AL,38H
    JNZ CLOSE7
    JMP POPFUNC ;POP INS
    CLOSE7:

    CMP AL,39H
    JNZ CLOSE8
    JMP RORFUNC ;ROR INS
    CLOSE8:

    CMP AL,61H
    JNZ CLOSE9
    JMP ROLFUNC ;ROL INS
    CLOSE9:

    CMP AL,62H
    JNZ CLOSE10
    JMP RCRFUNC ;RCR INS
    CLOSE10:

    CMP AL,63H
    JNZ CLOSE11
    JMP RCLFUNC ;RCL INS
    CLOSE11:

    CMP AL,64H
    JNZ CLOSE12
    JMP SARFUNC ;RCL INS
    CLOSE12:

    CMP AL,65H
    JNZ CLOSE13
    JMP MULFUNC ;RCL INS
    CLOSE13:

    CMP AL,66H
    JNZ CLOSE14
    JMP DIVFUNC ;RCL INS
    CLOSE14:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    CLCFUNC:
    MOV CARRY,1
    JMP EXIT

   
    MOVFUNC:
   ;;;;;;;;;;;;;;;read destination
    MOV AH,1
    INT 21H 

    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEl84
    JMP  CHECK16;CHECK IF CAPITAL LETTER mmkn yeb2a 16 bit register aw [si]
    LABEL84:

    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE smalllet4
    JMP  CHECKMEM ;he eneteed from i ....------take care not error hynot ye4of low kanet memory

    ;small letter 8 bit reg or direct addressing mode
    smalllet4: SUB AL,59H  ;8 9... 1f
    MOV AH,0 
    MOV BX,AX 
    ;;;;;;;;;;;;;;;;;read source for 8 bit register
    MOV AH,1
    INT 21h
    CMP AL,41H
    JB LABEL85 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    CMP AL,4FH
    JBE LABEL97
    JMP LABEL87 ;;;;;;;;;;;;;;;;;;;; MAY BE 8BITS REG OR MEMORY
    LABEL97:
    CMP AL,'B'
    JE Regindirect3

    CMP AL,'D'
    JE Regindirect3

    CMP AL,'R'
    JE Regindirect3

    JMP Error2

    Regindirect3:
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX  

    mov DH,REG[SI] 
    mov DL,REG[SI+1] ;take value that si point at
    cmp DX,0fh
    jbe label86
    jmp Error2
    ;DX  0001 ADDRESS
    label86:
    add DX,10H ;==> MOV TO INDEX 17 DEC==> HEX 11
    mov SI,DX  ;1235h
    MOV CL,REG[SI]
    MOV REG[BX],CL
    JMP EXIT
    LABEL85: ;;;;;;Number
    read2num
    MOV AL,NUM2
    MOV REG[BX],AL
    JMP EXIT
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LABEL87: ;;may be 8bit reg or memory
    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL13
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL13:
    CMP AL,78H  ;CHECK IF INRANGE TILL SMALL x  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE LABEL88
    JMP Error2
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LABEL88:
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG[si]
    mov reg[bx],al
    JMP EXIT
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&8BITS DONE&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&77
    CHECK16:;;;;check if 16 bit register or [si] destination

     CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LAbel89
    JMP Error2 ;he eneted not a letter
    LABEL89:
    
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL90
    JMP  Error2 ;above O
    LABEL90:
    ;CHECK IF ODD ==> 16 bit register not [si]
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL91
    JMP MAYEVEN10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;still
    LABEL91:
    POP AX
    ;capital letter 16 bit reg mode 16 BIT REGISTER DESTINATION
    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX
    ;;;;;;;;;;READ SOURCE for 16 bit register
    MOV AH,1
    INT 21H
    
    CMP AL,41H
    JB LABEL92 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    CMP AL,4FH
    JBE LABEL98
    JMP LABEL93 ;;;;;;;;;;;;;;;;;;;; MAY BE  MEMORY
    ;;;;MAY BEY 16 BIT REGISTER OR [SI]
    LABEL98:
    CMP AL,'B'
    JE Regindirect4

    CMP AL,'D'
    JE Regindirect4

    CMP AL,'R'
    JE Regindirect4
;bit16 register
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG[SI]
    MOV CL,REG[SI+1]
    MOV REG[BX],CH
    MOV REG[BX+1],CL
;000000000000000000000000000000000000000000000000
    Regindirect4:
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX
    MOV DH,REG[SI]
    MOV DL,REG[SI+1]
    CMP DX,0fh
    JBE LABEL96
    JMP Error2
    LABEL96:
    ADD DX,10H
    MOV SI,DX
    MOV CH,REG[SI]
    MOV CL,REG[SI+1]
    MOV REG[BX],CH
    MOV REG[BX+1],CL
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LABEL92: ;read 4 numbers
    read4num
    mov AH,NUM4
    MOV AL,NUM4+1
    mov reg[bx],ah
    mov reg[bx+1],al
    jmp exit


    LABEL93: ;;;reading memory
    
    CMP AL,69H ;;check if it starts from i
    JAE LABEL94 
    JMP Error2
    LABEL94:

    CMP AL,78H ;;check if it is in range till x
    JBE LABEL95
    JMP Error2
    LABEL95:


    SUB AL,59H  ;to get memory location 16 decimal
    MOV AH,0
    MOV SI,AX
    MOV AL,REG[SI]
    MOV AH,REG[SI+1]
    MOV REG[BX],AH
    MOV REG[BX+1],AL
    
    JMP EXIT


    MAYEVEN10:
    CHECKEVEN ;;;;;;;;enter in si have destination in bx

    ;;;;;;;;;;;;;;;;;;;;READ SOURCE OF SI;;;;;;;;;;;;;;;;;;;;;
     MOV AH,1
    INT 21h 
    CMP AL,41H  ;;check if 16 bit register
    JAE LABEL99
    JMP MAYNUM ;;;;;;;;;;;;maybe reading number ENTER 0 TO READ NUM
    LABEL99:
    CMP AL,4FH
    JBE LABEL100
    JMP MAY8BIT  ;;;;;;;;;;;MAYBE8BITREGISTER;;;;;;;;;;;
    LABEL100: ;;;;;;;;;;;IF 16 bit register
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL101
    JMP Error2
    LABEL101:
    pop ax
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG[SI]
    MOV CL,REG[SI+1]
    MOV REG[BX],CH
    MOV REG[BX+1],CL
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;16BIT REGISTER DONE
    MAYNUM:
    read2num
    MOV AL,NUM2
    MOV REG[BX],AL
    JMP EXIT

    MAY8BIT:
    CMP AL,61H ;CHECK IF SMALL LETTER FROM A
    JAE LABEL102
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL102:
    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE LABEL103
    JMP Error2

    LABEL103:
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG[si]
    mov reg[bx],al
    JMP EXIT

    CHECKMEM:
    cmp al,78H
    jbe label104
    jmp Error2
    label104:
    sub al,59H
    mov ah,0
    mov bx,AX

    ;SOURCE
    MOV AH,1
    INT 21H
    CMP AL,41H
    JB MAYLEGNUM
    CMP AL,4FH
    JBE LABEL107
    JMP MAYLEGAL8
    LABEL107:
    CMP AL,'B'
    JNZ LABEL108
    JMP Error2
    LABEL108:
    CMP AL,'D'
    JNZ LABEL109
    JMP Error2
    LABEL109:
    CMP AL,'R'
    JNZ LABEL110
    JMP Error2
    LABEL110:
    ; 16 BITS REG 
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG[SI]
    MOV CL,REG[SI+1]
    MOV REG[BX],CH
    MOV REG[BX+1],CL
    JMP EXIT

    MAYLEGNUM:
    read2num
    mov al,NUM2
    MOV REG[BX],al
    JMP EXIT

    MAYLEGAL8:
    
    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL105
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL105:
    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JB LABEL106
    JMP Error2
    LABEL106:
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG[si]
    mov reg[bx],al
    JMP EXIT

    
    
    


    ;INC FUNCTION &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    INCFUNC:
    ;Read Destination a-x or A-C-E-O
   SMALLEST CHECKCAPITAL

   CANBEREGIN10:
    mov CL,REG[BX]
    inc CL
    Mov REG[BX],CL
    JMP EXIT

    CHECKCAPITAL:
     CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL4
    JMP Error2 ;he eneted not a letter
    LABEL4:
    
    
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL5
    JMP  Error2 ;above O
    LABEL5:

    ;CHECK IF ODD
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL6
    JMP MAYEVEN9
    LABEL6:
    POP AX
    ;capital letter 16 bit reg mode
    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX


    ADD REG[BX+1],1
    ADC REG[BX],0
    JMP EXIT
    MAYEVEN9:
    
    CHECKEVEN
    JMP CANBEREGIN10

;//////////////////////////////////////////////7777&&&&&&&&&&&&&&&&&&&&&&&7777
    DECFUNC:
    ;READ DESTINATION
    MOV AH,1
    INT 21H 

    CMP AL,61H ;CHECK IF SMALL LETTER
    JB CHECKCAPITALLETTER2 ;CHECK IF CAPITAL LETTER

    CMP AL,78H  ;CHECK IF INRANGE TILL SMALL i
    JBE smalllet2
    JMP  Error2 ;he eneteed from i ....

    ;small letter 8 bit reg or direct addressing mode
    smalllet2: SUB AL,59H  ;8 9... 1f
    MOV AH,0 
    MOV BX,AX 

    SUB REG[BX],1
    JMP EXIT

    CHECKCAPITALLETTER2:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL7
    JMP Error2 ;he eneted not a letter
    LABEL7:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL8
    JMP  Error2 ;above O
    LABEL8:

    ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL12
    JMP Error2
    LABEL12:
    POP AX
    ;//

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX
    SUB REG[BX+1],1
    SBB REG[BX],0

    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SHL FUNCTION
    SHLFUNC:

    SMALLEST CHECKCAPITALLETTER3

    CANBEREGIN: ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    MOV DL,REG[BX]

   MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL ; JUMP IF NOT 1 IT MAYBE CL
    SHL DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL29
    JMP Error2
    LABEL29:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHL DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER3:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL9
    JMP Error2 ;he eneted not a letter
    LABEL9:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL10
    JMP  Error2 ;above O
    LABEL10:

    ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL11
    JMP MAYEVEN ;////////////////////////////////// NOT ERROR2  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    LABEL11:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL2 ; JUMP IF NOT 1 IT MAYBE CL
    SHL DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT

    MAYCL2:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL15
    JMP Error2
    LABEL15:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHL DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    

    ;;;;;;;;;;;;;;;;;;;;;;;;;&&&&&&&&&&&&&&&&&&&&& SHR
    SHRFUNC:

    SMALLEST CHECKCAPITALLETTER4

    CANBEREGIN2:
    MOV DL,REG[BX]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL3 ; JUMP IF NOT 1 IT MAYBE CL
    SHR DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL3:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL24
    JMP Error2
    LABEL24:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHR DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER4:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL25
    JMP Error2 ;he eneted not a letter
    LABEL25:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL26
    JMP  Error2 ;above O
    LABEL26:

    ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL27
    JMP MAYEVEN1
    LABEL27:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL4 ; JUMP IF NOT 1 IT MAYBE CL
    SHR DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT

    MAYCL4:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL28
    JMP Error2
    LABEL28:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHR DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN1:
    CHECKEVEN
    JMP CANBEREGIN2

    



    ;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& START OF PUSH AND POP

    PUSHFUNC:
     ;READ DESTINATION A ,C,E,..0
    MOV AH,1
    INT 21H 
    CMP AL,'E' ;; CHECK IF STACK POINTER
    JNE LABEL22
    JMP Error2
    LABEL22:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL16
    Jmp Error2 ;he eneted not a letter
    LABEL16:
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL17
    JMP  Error2 ;above O
    LABEL17:
   ;check odd
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL18
    JMP Error2
    LABEL18:
    POP AX

    ;FOR TEST
    ;MOV REG[0],12H
    ;MOV REG[1],34H

    SUB AL,41H 
    MOV AH,0
    ;GETTING SOURCE CX
    MOV BX,AX 
    mov CH,REG[BX]
    mov CL,REG[BX+1]
    
    MOV BH,REG[4]
    MOV BL,REG[5]
    mov [BX],CH ;==>STACH[]==>STCK[SP]
    mov [BX+1],CL ;==>STACH[]==>STCK[SP]

    ;increament SP
    add REG[5],2
    adc REG[4],0
    JMP EXIT


;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  pop
    POPFUNC:
    ;READ DESTINATION A ,C,E,..0
    MOV AH,1
    INT 21H 

    CMP AL,'E' ;; CHECK IF STACK POINTER
    JNE LABEL23
    JMP Error2
    LABEL23:

    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL19
    JMP Error2 ;he eneted not a letter
    LABEL19:
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL20
    JMP  Error2 ;above O
    LABEL20:
   ;check odd
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL21
    JMP Error2
    LABEL21:
    POP AX

    ;FOR TEST
    ;MOV STACK1[0],12H
    ;MOV STACK1[1],34H
    ;ADD REG[5],2
    ;ADC REG[4],0

    SUB AL,41H 
    MOV AH,0

    ;Getting value
    MOV BH,REG[4]
    MOV BL,REG[5]
    mov CH,[BX-2] ;==>STACH[]==>STCK[SP]
    mov CL,[BX-1];==>STACH[]==>STCK[SP]


    ;GETTING DESTINATION CX
    MOV BX,AX 
    mov REG[BX],CH
    mov REG[BX+1],CL
    
   
    ;decreament SP
    SUB REG[5],2
    SBB REG[4],0
    JMP EXIT


;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&RoR
    RORFUNC:
    
    ;destination
    SMALLEST CHECKCAPITALLETTER5

    CANBEREGIN3:
    MOV DL,REG[BX]

    ;Source
     MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL5 ; JUMP IF NOT 1 IT MAYBE CL
    ROR DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL5:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL35
    JMP Error2
    LABEL35:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    ROR DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER5:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL36
    JMP Error2 ;he eneted not a letter
    LABEL36:


    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL37
    JMP  Error2 ;above O
    LABEL37:

     ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL38
    JMP MAYEVEN2
    LABEL38:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL6 ; JUMP IF NOT 1 IT MAYBE CL
    ROR DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT


    MAYCL6:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL39
    JMP Error2
    LABEL39:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    ROR DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN2:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN3

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;rol FUNCTION
    ROLFUNC:
    SMALLEST CHECKCAPITALLETTER6

    CANBEREGIN4:
    MOV DL,REG[BX]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL7 ; JUMP IF NOT 1 IT MAYBE CL
    ROL DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL7:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL41
    JMP Error2
    LABEL41:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    ROL DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER6:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL42
    JMP Error2 ;he eneted not a letter
    LABEL42:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL43
    JMP  Error2 ;above O
    LABEL43:

    ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL44
    JMP MAYEVEN3
    LABEL44:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL8 ; JUMP IF NOT 1 IT MAYBE CL
    ROL DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT

    MAYCL8:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL45
    JMP Error2
    LABEL45:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    ROL DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN3:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN4
 
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& RCR
    RCRFUNC:

    ;destination
    SMALLEST CHECKCAPITALLETTER7

    CANBEREGIN5:
    MOV DL,REG[BX]

    ;Source
    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL9 ; JUMP IF NOT 1 IT MAYBE CL
    SHR CARRY,1
    RCR DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL9:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL47
    JMP Error2
    LABEL47:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHR CARRY,1
    RCR DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER7:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL48
    JMP Error2 ;he eneted not a letter
    LABEL48:


    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL49
    JMP  Error2 ;above O
    LABEL49:

     ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL50
    JMP MAYEVEN4
    LABEL50:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL10 ; JUMP IF NOT 1 IT MAYBE CL
    SHR CARRY,1
    RCR DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT

    MAYCL10:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL51
    JMP Error2
    LABEL51:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHR CARRY,1
    RCR DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN4:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN5

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& RCR
    RCLFUNC:

    ;destination
    SMALLEST CHECKCAPITALLETTER8

    CANBEREGIN6:
    MOV DL,REG[BX]

    ;Source
    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL11 ; JUMP IF NOT 1 IT MAYBE CL
    SHR CARRY,1
    RCL DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL11:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL53
    JMP Error2
    LABEL53:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHR CARRY,1
    RCL DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER8:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL54
    JMP Error2 ;he eneted not a letter
    LABEL54:


    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL55
    JMP  Error2 ;above O
    LABEL55:

     ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL56
    JMP MAYEVEN5
    LABEL56:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL12 ; JUMP IF NOT 1 IT MAYBE CL
    SHR CARRY,1
    RCL DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT

    MAYCL12:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL57
    JMP Error2
    LABEL57:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SHR CARRY,1
    RCL DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN5:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN6

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& SAR

    SARFUNC:

    SMALLEST CHECKCAPITALLETTER9

    CANBEREGIN7:
    MOV DL,REG[BX]

    MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL13 ; JUMP IF NOT 1 IT MAYBE CL
    SAR DL,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    JMP EXIT

    MAYCL13:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL59
    JMP Error2
    LABEL59:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SAR DL,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY

    JMP EXIT

    CHECKCAPITALLETTER9:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL60
    JMP Error2 ;he eneted not a letter
    LABEL60:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL61
    JMP  Error2 ;above O
    LABEL61:

    ;CHECK IF ODD //
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL62
    JMP MAYEVEN6
    LABEL62:
    POP AX
    ;///

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG[BX]
    MOV DL,REG[BX+1]

   MOV AH,1
    INT 21H
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL14 ; JUMP IF NOT 1 IT MAYBE CL
    SAR DX,1
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL
    JMP EXIT

    MAYCL14:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL63
    JMP Error2
    LABEL63:
    MOV CL,REG[13] ;; GET THE VALUE IN CL
    SAR DX,CL
    MOV CARRY,0
    ADC CARRY,0
    MOV REG[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY SET TO NEW CARRY
    MOV REG[BX+1],DL

    JMP EXIT

    MAYEVEN6:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN7

;;;;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& MUL
    MULFUNC:

    SMALLEST CHECKCAPITALLETTER10

    CANBEREGIN8:
    MOV DL, REG[BX]
    MOV AL, REG[9]
    MUL DL
    MOV REG[8],AH
    MOV REG[9],AL
    
    JMP EXIT

    CHECKCAPITALLETTER10:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL68
    JMP Error2 ;he eneted not a letter
    LABEL68:
    
    
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL69
    JMP  Error2 ;above O
    LABEL69:

    ;CHECK IF ODD
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL70
    JMP MAYEVEN7
    LABEL70:
    POP AX

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV CH,REG[BX]
    MOV CL,REG[BX+1]

    MOV AH,REG[8]
    MOV AL,REG[9]

    MUL CX

    MOV REG[8],AH
    MOV REG[9],AL
    MOV REG[14],DH
    MOV REG[15],DL

    JMP EXIT

    MAYEVEN7:
    CHECKEVEN
    JMP CANBEREGIN8

;;;;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& MUL
    DIVFUNC:

    SMALLEST CHECKCAPITALLETTER11

    CANBEREGIN9:
    MOV DL, REG[BX]
    MOV AH, REG[8]
    MOV AL, REG[9]

    DIV DL
    MOV REG[8],AH
    MOV REG[9],AL

    JMP EXIT

    CHECKCAPITALLETTER11:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL71
    JMP Error2 ;he eneted not a letter
    LABEL71:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL72
    JMP  Error2 ;above O
    LABEL72:

    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL73
    JMP MAYEVEN8
    LABEL73:
    POP AX

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV CH,REG[BX]
    MOV CL,REG[BX+1]

    MOV AH,REG[8]
    MOV AL,REG[9]

    DIV CX

    MOV REG[8],AH
    MOV REG[9],AL
    MOV REG[14],DH
    MOV REG[15],DL

    JMP EXIT

    MAYEVEN8:
    CHECKEVEN
    JMP CANBEREGIN9

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


  Error2:   
  EXIT:  
   pop CX
   dec cx
   cmp cx,0
   je EN
   jmp loop1
EN:  
   HLT    
   MAIN ENDP
END MAIN