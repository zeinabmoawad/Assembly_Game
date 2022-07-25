EXTRN REG2:BYTE
EXTRN STACK2:BYTE
EXTRN CARRY2:BYTE
;;;000000000000000000***********************
EXTRN power_stuck:BYTE
EXTRN dataline:BYTE
EXTRN stuck_val:BYTE
;;;000000000000000000***********************
EXTRN points:BYTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;neweman;;;;;;;;;;;;;;;;;;;;;;;
EXTRN forbidendestination:BYTE
EXTRN forbideninstruction:BYTE
EXTRN forbidensource:BYTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
public instruction
.MODEL SMALL
.STACK 64
.DATA
;REG2 DB 32 DUP(00H)
;CARRY2 DB ?
;STACK2 DB 100 DUP(?)
NUM2 DB ?
NUM4 Db 2 DUP(?)

.CODE

CHECKEVEN MACRO 
    LOCAL label67
    LOCAL Regindirect2
    LOCAL myfunction251
    POP AX
    CMP AL,'B'
    JE Regindirect2

    CMP AL,'D'
    JE Regindirect2

    CMP AL,'L'
    JE Regindirect2

    JMP Error2

    Regindirect2:
    ;;===================new==============================;;
    CHECKDESTINATION myfunction251 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction251:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV BX,AX  

    mov Dh,REG2[BX] 
    mov DL,REG2[BX+1]
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
    LOCAL myfunction31

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
    smalllet3: 
    ;===================new==============================;;
    CHECKDESTINATION myfunction31 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction31:
    ;;===================new==============================;;
    SUB AL,59H  ;8 9... 1f
    MOV AH,0 
    MOV BX,AX 

ENDM SMALLEST

readnum MACRO params
    LOCAL LABEL74
    LOCAL LABEL75
    LOCAL LABEL76
    LOCAL LABEL77
    LOCAL myfunction72
    LOCAL myfunction71


    mov ah,01h 
    int 21h 

    CMP AL,41H
    JB LABEL74 ;;;may be number
    CMP AL,46H
    JBE LABEL75
    JMP Error2
    LABEL75:
    CHECKSOURCE myfunction72
    myfunction72:
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
    CHECKSOURCE myfunction71
    myfunction71:
    SUB AL,30h


ENDM readnum

read2num MACRO ;;;000000000000000000***********************
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
    mov power_stuck,0


ENDM read2num

read4num MACRO  ;;;000000000000000000***********************
    LOCAL LABEL80
    LOCAL LABEL81
    LOCAL LABEL82
    LOCAL LABEL83
    LOCAL nopower
    LOCAL itsone
    LOCAL itsone2
    LOCAL higher
            
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

    cmp power_stuck,1
    jnz nopower
    cmp dataline,8
    jae higher 
    ;;; lower part of num4 ==> num4+1
    cmp stuck_val,0
    jnz itsone

    mov al,11111110b
    mov cl,dataline
    rol al,cl
    and num4+1,al
    jmp nopower ;; to exit only

    itsone:
    mov al,00000001b
    mov cl,dataline
    rol al,cl
    or num4+1,al
    jmp nopower
    ;;; higher part of num4 ==> num4+1
    higher:
    cmp stuck_val,0
    jnz itsone2

    mov al,11111110b
    mov cl,dataline
    sub cl,8
    rol al,cl
    and num4,al
    jmp nopower ;; to exit only

    itsone2:
    mov al,00000001b
    mov cl,dataline
    sub cl,8
    rol al,cl
    or num4,al
    jmp nopower

    nopower:  
    mov power_stuck,0 
            
ENDM read4num
;;;;;;;;;;;;;;;;;;;;;;;;;;;neweman;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4
CHECKDESTINATION MACRO Z
    LOCAL myloop1
    LOCAL notequal1

    mov si,offset forbidendestination
    mov cl,24h
    CMP CL,[SI]
    JNE myloop1
    JMP Z ;;;;;;;;;;;;;;;;there is no forbbiden destination
    myloop1:
    cmp [SI],AL
    jne notequal1
    jmp EXIT
    notequal1:
    INC SI
    MOV CL,[SI]
    cmp cl,24h
    jne myloop1

ENDM CHECKDESTINATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;neweman;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4
CHECKSOURCE MACRO W
    LOCAL myloop2
    LOCAL notequal2

    mov si,offset forbidensource
    mov cl,24h
    CMP CL,[SI]
    JNE myloop2
    JMP W ;;;;;;;;;;;;;;;;there is no forbbiden destination
    myloop2:
    cmp [SI],AL
    jne notequal2
    jmp EXIT
    notequal2:
    INC SI
    MOV CL,[SI]
    cmp cl,24h
    jne myloop2

ENDM CHECKSOURCE





instruction PROC FAR
    MOV AX,@DATA
    MOV DS,AX

    ;Reading the insturction 0-f
    MOV AH,1
    INT 21H
    ;;;;;;;;;;;;;;;;;;;;;;;neweman;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov si,offset forbideninstruction
    mov cl,24h
    CMP CL,[SI]
    JNE myloop
    JMP MYFUNCTION ;;;;;;;;;;;;;;;;there is no forbbiden instruction
    myloop:
    cmp [SI],AL
    jne notequal
    jmp EXIT
    notequal:
    INC SI
    MOV CL,[SI]
    cmp cl,24h
    jne myloop

    MYFUNCTION:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;neweman;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;Find instruction
    CMP AL,30H ; COMPARE CHAR READ WITH 0
    JNZ CLOSSET
    JMP MOVFUNC  ; INSTRUCTION MOVE
    CLOSSET:
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

    CMP AL,67H
    JNZ CLOSE15
    JMP IMULFUNC ;IMUL INS
    CLOSE15:

    CMP AL,68H
    JNZ CLOSE16
    JMP IDIVFUNC ;IDIV INS
    CLOSE16:

    CMP AL,69H
    JNZ CLOSE17
    JMP ADDFUNC ;ADD INS
    CLOSE17:

    CMP AL,6AH
    JNZ CLOSE18
    JMP SUBFUNC ;SUB INS
    CLOSE18:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    CLCFUNC:
    MOV CARRY2,0
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
    smalllet4: 
    ;;===================new==============================;;
    CHECKDESTINATION myfunction1 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction1:
    ;;===================new==============================;;
    SUB AL,59H  ;8 9... 1f
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

    CMP AL,'L'
    JE Regindirect3

    JMP Error2

    Regindirect3:
    ;;===================new==============================;;
    CHECKSOURCE myfunction22 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction22:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX  

    mov DH,REG2[SI] 
    mov DL,REG2[SI+1] ;take value that si point at
    cmp DX,0fh
    jbe label86
    jmp Error2
    ;DX  0001 ADDRESS
    label86:
    add DX,10H ;==> MOV TO INDEX 17 DEC==> HEX 11
    mov SI,DX  ;1235h
    MOV CL,REG2[SI]
    MOV REG2[BX],CL
    JMP EXIT
    LABEL85: ;;;;;;Number
    read2num
    MOV AL,NUM2
    MOV REG2[BX],AL
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
    ;;===================new==============================;;
    CHECKSOURCE myfunction23 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction23:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    mov REG2[bx],al
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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction20 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction20:
    ;;===================new==============================;;
    ;CHECK IF ODD ==> 16 bit register not [si]
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL91
    JMP MAYEVEN11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;still
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
    JAE LABEL123
    JMP LABEL92 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    LABEL123:
    CMP AL,4FH
    JBE LABEL98
    JMP LABEL93 ;;;;;;;;;;;;;;;;;;;; MAY BE  MEMORY
    ;;;;MAY BEY 16 BIT REGISTER OR [SI]
    LABEL98:
    CMP AL,'B'
    JE Regindirect4

    CMP AL,'D'
    JE Regindirect4

    CMP AL,'L'
    JE Regindirect4
;bit16 register
    ;;===================new==============================;;
    CHECKSOURCE myfunction24 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction24:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    MOV REG2[BX],CH
    MOV REG2[BX+1],CL
    JMP EXIT
;000000000000000000000000000000000000000000000000
    Regindirect4:
    ;;===================new==============================;;
    CHECKSOURCE myfunction25 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction25:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX
    MOV DH,REG2[SI]
    MOV DL,REG2[SI+1]
    CMP DX,0fh
    JBE LABEL96
    JMP Error2
    LABEL96:
    ADD DX,10H
    MOV SI,DX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    MOV REG2[BX],CH
    MOV REG2[BX+1],CL
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LABEL92: ;read 4 numbers
    read4num
    mov AH,NUM4
    MOV AL,NUM4+1
    mov reg2[bx],ah
    mov reg2[bx+1],al
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
    ;;===================new==============================;;
    CHECKSOURCE myfunction26 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction26:
    ;;===================new==============================;;
    SUB AL,59H  ;to get memory location 16 decimal
    MOV AH,0
    MOV SI,AX
    MOV AL,REG2[SI]
    MOV AH,REG2[SI+1]
    MOV REG2[BX],AH
    MOV REG2[BX+1],AL
    
    JMP EXIT


    MAYEVEN11:
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
    ;;===================new==============================;;
    CHECKSOURCE myfunction27 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction27:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    MOV REG2[BX],CH
    MOV REG2[BX+1],CL
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;16BIT REGISTER DONE
    MAYNUM:
    read2num
    MOV AL,NUM2
    MOV REG2[BX],AL
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
    ;;===================new==============================;;
    CHECKSOURCE myfunction28 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction28:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    mov reg2[bx],al
    JMP EXIT
;;;read destination
    CHECKMEM:
    cmp al,78H
    jbe label104
    jmp Error2
    label104:
    ;;===================new==============================;;
    CHECKDESTINATION myfunction218 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction218:
    ;;===================new==============================;;
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
    CMP AL,'L'
    JNZ LABEL110
    JMP Error2
    LABEL110:
    ; 16 BITS REG 
    ;;===================new==============================;;
    CHECKSOURCE myfunction29 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction29:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    MOV REG2[BX],CH
    MOV REG2[BX+1],CL
    JMP EXIT

    MAYLEGNUM:
    read2num
    mov al,NUM2
    MOV REG2[BX],al
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
    ;;===================new==============================;;
    CHECKSOURCE myfunction30 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction30:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    mov reg2[bx],al
    JMP EXIT
;//////////////////////////////////////////////7777&&&&&&&&&&&&&&&&&&&&&&&7777
    ;INC FUNCTION
    INCFUNC:
    ;Read Destination a-x or A-C-E-O
   SMALLEST CHECKCAPITAL

   CANBEREGIN10:
    mov CL,REG2[BX]
    inc CL
    Mov REG2[BX],CL
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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction32 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction32:
    ;;===================new==============================;;
    ;capital letter 16 bit reg mode
    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX


    ADD REG2[BX+1],1
    ADC REG2[BX],0
    JMP EXIT

    MAYEVEN9:
    
    CHECKEVEN
    JMP CANBEREGIN10

;//////////////////////////////////////////////7777&&&&&&&&&&&&&&&&&&&&&&&7777
    DECFUNC:
    SMALLEST CHECKCAPITALLETTER2

    CANBEREGIN11:
    SUB REG2[BX],1
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
    JMP MAYEVEN10
    LABEL12:
    POP AX
    ;;===================new==============================;;
    CHECKDESTINATION myfunction33 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction33:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX


  MOV DL,REG2[BX+1]
    MOV DH,REG2[BX]

    SUB DL,1
    SBB DH,0
    MOV REG2[BX+1],DL
    MOV REG2[BX],DH

    JMP EXIT

    MAYEVEN10:
    CHECKEVEN
    JMP CANBEREGIN11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SHL FUNCTION
    SHLFUNC:

    SMALLEST CHECKCAPITALLETTER3

    CANBEREGIN: ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    MOV DL,REG2[BX]

   MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction43 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction43:
    ;;===================new==============================;;
    SHL DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL29
    JMP Error2
    LABEL29:
    ;;===================new==============================;;
    CHECKSOURCE myfunction34 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction34:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHL DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction35 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction35:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL2 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction36 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction36:
    ;;===================new==============================;;
    SHL DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT

    MAYCL2:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL15
    JMP Error2
    LABEL15:
    ;;===================new==============================;;
    CHECKSOURCE myfunction37 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction37:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHL DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    

    ;;;;;;;;;;;;;;;;;;;;;;;;;&&&&&&&&&&&&&&&&&&&&& SHR
    SHRFUNC:

    SMALLEST CHECKCAPITALLETTER4

    CANBEREGIN2:
    MOV DL,REG2[BX]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL3 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction38 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction38:
    ;;===================new==============================;;
    SHR DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL3:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL24
    JMP Error2
    LABEL24:
    ;;===================new==============================;;
    CHECKSOURCE myfunction39 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction39:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHR DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction40 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction40:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL4 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction41 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction41:
    ;;===================new==============================;;
    SHR DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT

    MAYCL4:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL28
    JMP Error2
    LABEL28:
    
    ;;===================new==============================;;
    CHECKSOURCE myfunction42 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction42:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHR DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN1:
    CHECKEVEN
    JMP CANBEREGIN2

    



    ;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& START OF PUSH AND POP

    PUSHFUNC:
     ;READ DESTINATION A ,C,E,..0
    MOV AH,1
    INT 21H 

    CMP AL,'E' ;; CHECK IF STACK2 POINTER
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
    ;MOV REG2[0],12H
    ;MOV REG2[1],34H
    ;;===================new==============================;;
    CHECKDESTINATION myfunction74 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction74:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0
    ;GETTING SOURCE CX
    MOV BX,AX 
    mov CH,REG2[BX]
    mov CL,REG2[BX+1]
    
    MOV BH,REG2[4]
    MOV BL,REG2[5]
    mov stack2[BX],CH ;==>STACH[]==>STCK[SP]
    mov stack2[BX+1],CL ;==>STACH[]==>STCK[SP]

    ;increament SP
    add REG2[5],2
    adc REG2[4],0
    JMP EXIT


;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  pop 
    POPFUNC:
    ;READ DESTINATION A ,C,E,..0
    MOV AH,1
    INT 21H 

    CMP AL,'E' ;; CHECK IF STACK2 POINTER
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
    ;ADD REG2[5],2
    ;ADC REG2[4],0
    ;;===================new==============================;;
    CHECKDESTINATION myfunction73 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction73:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0

    ;Getting value
    MOV BH,REG2[4]
    MOV BL,REG2[5]
    mov CH,stack2[BX-2] ;==>STACH[]==>STCK[SP]
    mov CL,stack2[BX-1];==>STACH[]==>STCK[SP]


    ;GETTING DESTINATION CX
    MOV BX,AX 
    mov REG2[BX],CH
    mov REG2[BX+1],CL
    
   
    ;decreament SP
    SUB REG2[5],2
    SBB REG2[4],0
    JMP EXIT


;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&RoR
    RORFUNC:
    
    ;destination
    SMALLEST CHECKCAPITALLETTER5

    CANBEREGIN3:
    MOV DL,REG2[BX]

    ;Source
     MOV AH,1
    INT 21H
    
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL5 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction44 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction44:
    ;;===================new==============================;;
    ROR DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL5:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL35
    JMP Error2
    LABEL35:
    ;;===================new==============================;;
    CHECKSOURCE myfunction45 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction45:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    ROR DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction46 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction46:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL6 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction47 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction47:
    ;;===================new==============================;;
    ROR DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT


    MAYCL6:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL39
    JMP Error2
    LABEL39:
    ;;===================new==============================;;
    CHECKSOURCE myfunction48 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction48:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    ROR DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN2:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN3

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;rol FUNCTION
    ROLFUNC:
    SMALLEST CHECKCAPITALLETTER6

    CANBEREGIN4:
    MOV DL,REG2[BX]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL7 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction49 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction49:
    ;;===================new==============================;;
    ROL DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL7:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL41
    JMP Error2
    LABEL41:
    ;;===================new==============================;;
    CHECKSOURCE myfunction50 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction50:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    ROL DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction51 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction51:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL8 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction52 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction52:
    ;;===================new==============================;;
    ROL DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT

    MAYCL8:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL45
    JMP Error2
    LABEL45:
    ;;===================new==============================;;
    CHECKSOURCE myfunction53 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction53:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    ROL DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN3:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN4
 
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& RCR
    RCRFUNC:

    ;destination
    SMALLEST CHECKCAPITALLETTER7

    CANBEREGIN5:
    MOV DL,REG2[BX]

    ;Source
    MOV AH,1
    INT 21H


    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL9 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction54 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction54:
    ;;===================new==============================;;
    SHR CARRY2,1
    RCR DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL9:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL47
    JMP Error2
    LABEL47:
    ;;===================new==============================;;
    CHECKSOURCE myfunction55 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction55:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHR CARRY2,1
    RCR DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
        ;;===================new==============================;;
    CHECKDESTINATION myfunction56 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction56:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL10 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction57 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction57:
    ;;===================new==============================;;
    SHR CARRY2,1
    RCR DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT

    MAYCL10:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL51
    JMP Error2
    LABEL51:
    ;;===================new==============================;;
    CHECKSOURCE myfunction58 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction58:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHR CARRY2,1
    RCR DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN4:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN5

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& RCR
    RCLFUNC:

    ;destination
    SMALLEST CHECKCAPITALLETTER8

    CANBEREGIN6:
    MOV DL,REG2[BX]

    ;Source
    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL11 ; JUMP IF NOT 1 IT MAYBE CL
        ;;===================new==============================;;
    CHECKSOURCE myfunction59 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction59:
    ;;===================new==============================;;
    SHR CARRY2,1
    RCL DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL11:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL53
    JMP Error2
    LABEL53:
    ;;===================new==============================;;
    CHECKSOURCE myfunction60 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction60:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHR CARRY2,1
    RCL DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction61 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction61:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

    MOV AH,1
    INT 21H
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;================lastthing====================;;;;;;;;;;;;;;;;
    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL12 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction62 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction62:
    ;;===================new==============================;;
    SHR CARRY2,1
    RCL DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT

    MAYCL12:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL57
    JMP Error2
    LABEL57:
    ;;===================new==============================;;
    CHECKSOURCE myfunction63 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction63:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SHR CARRY2,1
    RCL DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN5:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN6

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& SAR

    SARFUNC:

    SMALLEST CHECKCAPITALLETTER9

    CANBEREGIN7:
    MOV DL,REG2[BX]

    MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL13 ; JUMP IF NOT 1 IT MAYBE CL
          ;;===================new==============================;;
    CHECKSOURCE myfunction64 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction64:
    ;;===================new==============================;;
    SAR DL,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    JMP EXIT

    MAYCL13:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL59
    JMP Error2
    LABEL59:
          ;;===================new==============================;;
    CHECKSOURCE myfunction65 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction65:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SAR DL,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DL ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction66 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction66:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV DH,REG2[BX]
    MOV DL,REG2[BX+1]

   MOV AH,1
    INT 21H

    CMP AL,31H ; CHECK IF NUMBER IS 1
    JNZ MAYCL14 ; JUMP IF NOT 1 IT MAYBE CL
    ;;===================new==============================;;
    CHECKSOURCE myfunction67 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction67:
    ;;===================new==============================;;
    SAR DX,1
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL
    JMP EXIT

    MAYCL14:
    CMP AL,'f' ;CHECK IF CL
    JZ LABEL63
    JMP Error2
    LABEL63:
    ;;===================new==============================;;
    CHECKSOURCE myfunction68 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction68:
    ;;===================new==============================;;
    MOV CL,REG2[13] ;; GET THE VALUE IN CL
    SAR DX,CL
    MOV CARRY2,0
    ADC CARRY2,0
    MOV REG2[BX],DH ;;GET BACK AFTER BEING SHIFT AND CARRY2 SET TO NEW CARRY2
    MOV REG2[BX+1],DL

    JMP EXIT

    MAYEVEN6:  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    CHECKEVEN
    JMP CANBEREGIN7

;;;;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& MUL
    MULFUNC:

    SMALLEST CHECKCAPITALLETTER10

    CANBEREGIN8:
    MOV DL, REG2[BX]
    MOV AL, REG2[9]
    MUL DL
    MOV REG2[8],AH
    MOV REG2[9],AL
    
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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction69 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction69:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV CH,REG2[BX]
    MOV CL,REG2[BX+1]

    MOV AH,REG2[8]
    MOV AL,REG2[9]

    MUL CX

    MOV REG2[8],AH
    MOV REG2[9],AL
    MOV REG2[14],DH
    MOV REG2[15],DL

    JMP EXIT

    MAYEVEN7:
    CHECKEVEN
    JMP CANBEREGIN8

;;;;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& MUL
    DIVFUNC:

    SMALLEST CHECKCAPITALLETTER11

    CANBEREGIN9:
    MOV DL, REG2[BX]
    MOV AH, REG2[8]
    MOV AL, REG2[9]

    DIV DL
    MOV REG2[8],AH
    MOV REG2[9],AL

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
    ;;===================new==============================;;
    CHECKDESTINATION myfunction70 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction70:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV CH,REG2[BX]
    MOV CL,REG2[BX+1]

    MOV AH,REG2[8]
    MOV AL,REG2[9]

    mov dh,reg2[14]
    mov dl,reg2[15]
    DIV CX

    MOV REG2[8],AH
    MOV REG2[9],AL
    MOV REG2[14],DH
    MOV REG2[15],DL

    JMP EXIT

    MAYEVEN8:
    CHECKEVEN
    JMP CANBEREGIN9

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

    IMULFUNC:

    SMALLEST CHECKCAPITALLETTER12

    CANBEREGIN12:
    MOV DL, REG2[BX]
    MOV AL, REG2[9]
    IMUL DL
    MOV REG2[8],AH
    MOV REG2[9],AL
    
    JMP EXIT

    CHECKCAPITALLETTER12:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL150
    JMP Error2 ;he eneted not a letter
    LABEL150:
    
    
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL151
    JMP  Error2 ;above O
    LABEL151:

    ;CHECK IF ODD
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL152
    JMP MAYEVEN12
    LABEL152:
    POP AX
 ;;===================new==============================;;
    CHECKDESTINATION myfunction75 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction75:
    ;;===================new==============================;;
    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV CH,REG2[BX]
    MOV CL,REG2[BX+1]

    MOV AH,REG2[8]
    MOV AL,REG2[9]

    IMUL CX

    MOV REG2[8],AH
    MOV REG2[9],AL
    MOV REG2[14],DH
    MOV REG2[15],DL

    JMP EXIT

    MAYEVEN12:
    CHECKEVEN
    JMP CANBEREGIN12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    IDIVFUNC:
        SMALLEST CHECKCAPITALLETTER13

    CANBEREGIN13:
    MOV DL, REG2[BX]
    MOV AH, REG2[8]
    MOV AL, REG2[9]

    IDIV DL
    MOV REG2[8],AH
    MOV REG2[9],AL

    JMP EXIT

    CHECKCAPITALLETTER13:
    CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL153
    JMP Error2 ;he eneted not a letter
    LABEL153:

    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL154
    JMP  Error2 ;above O
    LABEL154:

    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL155
    JMP MAYEVEN13
    LABEL155:
    POP AX

    ;;===================new==============================;;
   CHECKDESTINATION myfunction76 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction76:
    ;;===================new==============================;;

    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX

    MOV CH,REG2[BX]
    MOV CL,REG2[BX+1]

    MOV AH,REG2[8]
    MOV AL,REG2[9]

    mov dh,reg2[14]
    mov dl,reg2[15]
    IDIV CX

    MOV REG2[8],AH
    MOV REG2[9],AL
    MOV REG2[14],DH
    MOV REG2[15],DL

    JMP EXIT

    MAYEVEN13:
    CHECKEVEN
    JMP CANBEREGIN13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ADDING FUNC
    ADDFUNC:
    ;;;;;;;;;;;;;;;read destination
    MOV AH,1
    INT 21H 

    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEl156
    JMP  CHECK162;CHECK IF CAPITAL LETTER mmkn yeb2a 16 bit register aw [si]
    LABEl156:

    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE smalllet6
    JMP  CHECKMEM2 ;he eneteed from i ....------take care not error hynot ye4of low kanet memory

    ;small letter 8 bit reg or direct addressing mode
    smalllet6: 
    ;;===================new==============================;;
    CHECKDESTINATION myfunction90 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction90:
    ;;===================new==============================;;
    SUB AL,59H  ;8 9... 1f
    MOV AH,0 
    MOV BX,AX 
    ;;;;;;;;;;;;;;;;;read source for 8 bit register
    MOV AH,1
    INT 21h

    CMP AL,41H
    JB LABEL157 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    CMP AL,4FH
    JBE LABEL158
    JMP LABEL159 ;;;;;;;;;;;;;;;;;;;; MAY BE 8BITS REG OR MEMORY
    LABEL158:
    CMP AL,'B'
    JE Regindirect7

    CMP AL,'D'
    JE Regindirect7

    CMP AL,'L'
    JE Regindirect7

    JMP Error2

    Regindirect7:
    ;;===================new==============================;;
    CHECKSOURCE myfunction91 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction91:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX  

    mov DH,REG2[SI] 
    mov DL,REG2[SI+1] ;take value that si point at
    cmp DX,0fh
    jbe label200
    jmp Error2
    ;DX  0001 ADDRESS
    label200:
    add DX,10H ;==> MOV TO INDEX 17 DEC==> HEX 11
    mov SI,DX  ;1235h
    MOV CL,REG2[SI]
    ADD REG2[BX],CL
    JMP EXIT
    LABEL157: ;;;;;;Number
    read2num
    MOV AL,NUM2
    ADD REG2[BX],AL
    JMP EXIT
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LABEL159: ;;may be 8bit reg or memory
    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE label201
    JMP Error2 ;CHECK IF CAPITAL LETTER
    label201:
    CMP AL,78H  ;CHECK IF INRANGE TILL SMALL x  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE label202
    JMP Error2
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    label202:
    ;;===================new==============================;;
    CHECKSOURCE myfunction239 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction239:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    ADD REG2[bx],al
    JMP EXIT
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&8BITS DONE&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&77
    CHECK162:;;;;check if 16 bit register or [si] destination

     CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE label203
    JMP Error2 ;he eneted not a letter
    label203:
    
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE label204
    JMP  Error2 ;above O
    label204:
    ;;===================new==============================;;
    CHECKDESTINATION myfunction209 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction209:
    ;;===================new==============================;;
    ;CHECK IF ODD ==> 16 bit register not [si]
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE label205
    JMP MAYEVEN21 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;still
    label205:
    POP AX
    ;capital letter 16 bit reg mode 16 BIT REGISTER DESTINATION
    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX
    ;;;;;;;;;;READ SOURCE for 16 bit register
    MOV AH,1
    INT 21H
    
    CMP AL,41H
    JAE label206
    JMP label207 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    label206:
    CMP AL,4FH
    JBE label208
    JMP label209 ;;;;;;;;;;;;;;;;;;;; MAY BE  MEMORY
    ;;;;MAY BEY 16 BIT REGISTER OR [SI]
    label208:
    CMP AL,'B'
    JE RegindirecT9

    CMP AL,'D'
    JE Regindirect9

    CMP AL,'L'
    JE Regindirect9
;bit16 register
    ;;===================new==============================;;
    CHECKSOURCE myfunction249 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction249:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    ADD REG2[BX+1],CL
    ADC REG2[BX],CH
    JMP EXIT
;000000000000000000000000000000000000000000000000
    Regindirect9:
    ;;===================new==============================;;
    CHECKSOURCE myfunction259 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction259:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX
    MOV DH,REG2[SI]
    MOV DL,REG2[SI+1]
    CMP DX,0fh
    JBE label210
    JMP Error2
    label210:
    ADD DX,10H
    MOV SI,DX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    ADD REG2[BX+1],CL
    ADC REG2[BX],CH
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    label207: ;read 4 numbers
    read4num
    mov AH,NUM4
    MOV AL,NUM4+1
    ADD reg2[bx+1],al
    ADC reg2[bx],ah
    
    jmp exit


    label209: ;;;reading memory
    
    CMP AL,69H ;;check if it starts from i
    JAE label211 
    JMP Error2
    label211:

    CMP AL,78H ;;check if it is in range till x
    JBE label212
    JMP Error2
    label212:
    ;;===================new==============================;;
    CHECKSOURCE myfunction269 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction269:
    ;;===================new==============================;;
    SUB AL,59H  ;to get memory location 16 decimal
    MOV AH,0
    MOV SI,AX
    MOV AL,REG2[SI]
    MOV AH,REG2[SI+1]
    ADD REG2[BX+1],AL
    ADC REG2[BX],AH
    
    JMP EXIT


    MAYEVEN21:
    CHECKEVEN ;;;;;;;;enter in si have destination in bx

    ;;;;;;;;;;;;;;;;;;;;READ SOURCE OF SI;;;;;;;;;;;;;;;;;;;;;
     MOV AH,1
    INT 21h 

    CMP AL,41H  ;;check if 16 bit register
    JAE label213
    JMP MAYNUM2 ;;;;;;;;;;;;maybe reading number ENTER 0 TO READ NUM
    label213:
    CMP AL,4FH
    JBE label214
    JMP MAY8BIT2  ;;;;;;;;;;;MAYBE8BITREGISTER;;;;;;;;;;;
    label214: ;;;;;;;;;;;IF 16 bit register
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE label215
    JMP Error2
    label215:
    pop ax
    ;;===================new==============================;;
    CHECKSOURCE myfunction279 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction279:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    ADD REG2[BX+1],CL
    ADC REG2[BX],CH
    
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;16BIT REGISTER DONE
    MAYNUM2:
    read2num
    MOV AL,NUM2
    ADD REG2[BX],AL
    JMP EXIT

    MAY8BIT2:
    CMP AL,61H ;CHECK IF SMALL LETTER FROM A
    JAE LABEL216
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL216:
    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE LABEL217
    JMP Error2

    LABEL217:
    ;;===================new==============================;;
    CHECKSOURCE myfunction289 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction289:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    ADD reg2[bx],al
    JMP EXIT
;;;read destination
    CHECKMEM2:
    cmp al,78H
    jbe LABEL218
    jmp Error2
    LABEL218:
    ;;===================new==============================;;
    CHECKDESTINATION myfunction219 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction219:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov bx,AX

    ;SOURCE
    MOV AH,1
    INT 21H

    CMP AL,41H
    JB MAYLEGNUM2
    CMP AL,4FH
    JBE LABEL250
    JMP MAYLEGAL82
    LABEL250:
    CMP AL,'B'
    JNZ LABEL221
    JMP Error2
    LABEL221:
    CMP AL,'D'
    JNZ LABEL249
    JMP Error2
    LABEL249:
    CMP AL,'L'
    JNZ LABEL251
    JMP Error2
    LABEL251:
    ; 16 BITS REG 
    ;;===================new==============================;;
    CHECKSOURCE myfunction299 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction299:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    ADD REG2[BX+1],CL
    ADC REG2[BX],CH
    
    JMP EXIT

    MAYLEGNUM2:
    read2num
    mov al,NUM2
    ADD REG2[BX],al
    JMP EXIT

    MAYLEGAL82:
    
    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL252
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL252:
    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JB LABEL225
    JMP Error2
    LABEL225:
    ;;===================new==============================;;
    CHECKSOURCE myfunction309 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction309:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    ADD reg2[bx],al
    JMP EXIT


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
    SUBFUNC:
    MOV AH,1
    INT 21H 

    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL219
    JMP  CHECK163;CHECK IF CAPITAL LETTER mmkn yeb2a 16 bit register aw [si]
    LABEL219:

    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE smalllet9
    JMP  CHECKMEM33 ;he eneteed from i ....------take care not error hynot ye4of low kanet memory

    ;small letter 8 bit reg or direct addressing mode
    smalllet9: 
    ;;===================new==============================;;
    CHECKDESTINATION myfunction590 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction590:
    ;;===================new==============================;;
    SUB AL,59H  ;8 9... 1f
    MOV AH,0 
    MOV BX,AX 
    ;;;;;;;;;;;;;;;;;read source for 8 bit register
    MOV AH,1
    INT 21h

    CMP AL,41H
    JB LABEL220 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    CMP AL,4FH
    JBE LABEL253
    JMP LABEL222 ;;;;;;;;;;;;;;;;;;;; MAY BE 8BITS REG OR MEMORY
    LABEL253:
    CMP AL,'B'
    JE Regindirect10

    CMP AL,'D'
    JE Regindirect10

    CMP AL,'L'
    JE Regindirect10

    JMP Error2

    Regindirect10:
    ;;===================new==============================;;
    CHECKSOURCE myfunction591 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction591:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX  

    mov DH,REG2[SI] 
    mov DL,REG2[SI+1] ;take value that si point at
    cmp DX,0fh
    jbe LABEL223
    jmp Error2
    ;DX  0001 ADDRESS
    LABEL223:
    add DX,10H ;==> MOV TO INDEX 17 DEC==> HEX 11
    mov SI,DX  ;1235h
    MOV CL,REG2[SI]
    SUB REG2[BX],CL
    JMP EXIT
    LABEL220: ;;;;;;Number
    read2num
    MOV AL,NUM2
    SUB REG2[BX],AL
    JMP EXIT
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LABEL222: ;;may be 8bit reg or memory
    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL224
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL224:
    CMP AL,78H  ;CHECK IF INRANGE TILL SMALL x  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE LABEL248
    JMP Error2
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LABEL248:
    ;;===================new==============================;;
    CHECKSOURCE myfunction5239 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5239:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    SUB REG2[bx],al
    JMP EXIT
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&8BITS DONE&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&77
    CHECK163:;;;;check if 16 bit register or [si] destination

     CMP AL,41H ;CHECK IF CAPITAL LETTER
    JAE LABEL226
    JMP Error2 ;he eneted not a letter
    LABEL226:
    
    CMP AL,4FH  ;CHECK IF INRANGE TILL CAPITAL O
    JBE LABEL227
    JMP  Error2 ;above O
    LABEL227:
    ;;===================new==============================;;
    CHECKDESTINATION myfunction5209 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5209:
    ;;===================new==============================;;
    ;CHECK IF ODD ==> 16 bit register not [si]
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL228
    JMP MAYEVEN212 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;still
    LABEL228:
    POP AX
    ;capital letter 16 bit reg mode 16 BIT REGISTER DESTINATION
    SUB AL,41H 
    MOV AH,0 
    MOV BX,AX
    ;;;;;;;;;;READ SOURCE for 16 bit register
    MOV AH,1
    INT 21H
    
    CMP AL,41H
    JAE LABEL229
    JMP LABEL230 ;;;;;;;;;;;;;MAY BE LEGAL NUMBER;;;;to read number enter 0
    LABEL229:
    CMP AL,4FH
    JBE LABEL231
    JMP LABEL232 ;;;;;;;;;;;;;;;;;;;; MAY BE  MEMORY
    ;;;;MAY BEY 16 BIT REGISTER OR [SI]
    LABEL231:
    CMP AL,'B'
    JE RegindirecT12

    CMP AL,'D'
    JE Regindirect12

    CMP AL,'L'
    JE Regindirect12
;bit16 register
    ;;===================new==============================;;
    CHECKSOURCE myfunction5249 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5249:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    SUB REG2[BX+1],CL
    SBB REG2[BX],CH
    JMP EXIT
;000000000000000000000000000000000000000000000000
    Regindirect12:
    ;;===================new==============================;;
    CHECKSOURCE myfunction5259 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5259:
    ;;===================new==============================;;
    sub al,42h   ;0 2 10
    mov ah,00
    MOV SI,AX
    MOV DH,REG2[SI]
    MOV DL,REG2[SI+1]
    CMP DX,0fh
    JBE LABEL233
    JMP Error2
    LABEL233:
    ADD DX,10H
    MOV SI,DX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    SUB REG2[BX+1],CL
    SBB REG2[BX],CH
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LABEL230: ;read 4 numbers
    read4num
    mov AH,NUM4
    MOV AL,NUM4+1
    SUB reg2[bx+1],al
    SBB reg2[bx],ah
    
    jmp exit


    LABEL232: ;;;reading memory
    
    CMP AL,69H ;;check if it starts from i
    JAE LABEL234 
    JMP Error2
    LABEL234:

    CMP AL,78H ;;check if it is in range till x
    JBE LABEL235
    JMP Error2
    LABEL235:
    ;;===================new==============================;;
    CHECKSOURCE myfunction5269 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5269:
    ;;===================new==============================;;
    SUB AL,59H  ;to get memory location 16 decimal
    MOV AH,0
    MOV SI,AX
    MOV AL,REG2[SI]
    MOV AH,REG2[SI+1]
    SUB REG2[BX+1],AL
    SBB REG2[BX],AH
    
    JMP EXIT


    MAYEVEN212:
    CHECKEVEN ;;;;;;;;enter in si have destination in bx

    ;;;;;;;;;;;;;;;;;;;;READ SOURCE OF SI;;;;;;;;;;;;;;;;;;;;;
     MOV AH,1
    INT 21h 

    CMP AL,41H  ;;check if 16 bit register
    JAE LABEL236
    JMP MAYNUM22 ;;;;;;;;;;;;maybe reading number ENTER 0 TO READ NUM
    LABEL236:
    CMP AL,4FH
    JBE LABEL246
    JMP MAY8BIT23  ;;;;;;;;;;;MAYBE8BITREGISTER;;;;;;;;;;;
    LABEL246: ;;;;;;;;;;;IF 16 bit register
    PUSH AX
    MOV AH,0 
    MOV CL,2
    DIV CL
    CMP AH,0
    JNE LABEL237
    JMP Error2
    LABEL237:
    pop ax
    ;;===================new==============================;;
    CHECKSOURCE myfunction5279 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5279:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    SUB REG2[BX+1],CL
    SBB REG2[BX],CH
    
    JMP EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;16BIT REGISTER DONE
    MAYNUM22:
    read2num
    MOV AL,NUM2
    SUB REG2[BX],AL
    JMP EXIT

    MAY8BIT23:
    CMP AL,61H ;CHECK IF SMALL LETTER FROM A
    JAE LABEL238
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL238:
    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JBE LABEL239
    JMP Error2

    LABEL239:
    ;;===================new==============================;;
    CHECKSOURCE myfunction5289 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5289:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    SUB reg2[bx],al
    JMP EXIT
;;;read destination
    CHECKMEM33:
    cmp al,78H
    jbe LABEL240
    jmp Error2
    LABEL240:
    ;;===================new==============================;;
    CHECKDESTINATION myfunction5219 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5219:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov bx,AX

    ;SOURCE
    MOV AH,1
    INT 21H

    CMP AL,41H
    JB MAYLEGNUM23
    CMP AL,4FH
    JBE LABEL247
    JMP MAYLEGAL823
    LABEL247:
    CMP AL,'B'
    JNZ LABEL242
    JMP Error2
    LABEL242:
    CMP AL,'D'
    JNZ LABEL243
    JMP Error2
    LABEL243:
    CMP AL,'L'
    JNZ LABEL244
    JMP Error2
    LABEL244:
    ; 16 BITS REG 
    ;;===================new==============================;;
    CHECKSOURCE myfunction5299 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5299:
    ;;===================new==============================;;
    SUB AL,41H
    MOV AH,0
    MOV SI,AX
    MOV CH,REG2[SI]
    MOV CL,REG2[SI+1]
    SUB REG2[BX+1],CL
    SBB REG2[BX],CH
    
    JMP EXIT

    MAYLEGNUM23:
    read2num
    mov al,NUM2
    SUB REG2[BX],al
    JMP EXIT

    MAYLEGAL823:
    
    CMP AL,61H ;CHECK IF SMALL LETTER
    JAE LABEL245
    JMP Error2 ;CHECK IF CAPITAL LETTER
    LABEL245:
    CMP AL,68H  ;CHECK IF INRANGE TILL SMALL h  ;;;0000000000000000000000000000000&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    JB LABEL241
    JMP Error2
    LABEL241:
    ;;===================new==============================;;
    CHECKSOURCE myfunction5309 ;;check if it is not forbidden after checking if it is not mismatch
    myfunction5309:
    ;;===================new==============================;;
    sub al,59H
    mov ah,0
    mov si,AX
    mov al,REG2[si]
    SUB reg2[bx],al
    JMP EXIT

  Error2:   
        DEC points
  EXIT:  
   ;pop CX
   ;dec cx
   ;cmp cx,0
   ;je EN
  ; jmp loop1
;EN:  
  ; HLT    
  ret
   instruction ENDP
END 