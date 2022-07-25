public play
EXTRN graph:FAR
EXTRN instruction:FAR
EXTRN BALL:FAR ;;;;=======NEWWW ZEINAB NOUR==========;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;neweman;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PUBLIC forbidendestination
PUBLIC forbideninstruction
PUBLIC forbidensource
;;;;;;;;;;;;;;;;
public Forbid1
public Forbid2
public level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
public REG1
PUBLIC REG2
public order
public power1
PUBLIC CARRY1
PUBLIC CARRY2
;PUBLIC STACK1
PUBLIC STACK2
;;;000000000000000000***********************
PUBLIC dataline
PUBLIC stuck_val
PUBLIC power_stuck
;;;000000000000000000***********************
EXTRN nameplayer:BYTE
EXTRN points:BYTE
public nameplayer2
public points2
public FINISH_GAME
PUBLIC WINNER
EXTRN won: byte ;;;;=======NEWWW ZEINAB NOUR==========;;;;
PUBLIC COLORIN2
PUBLIC NUM1 ;;GREAN
PUBLIC NUM2 ;;BLUE
PUBLIC NUM3 ;;GREAN
PUBLIC NUM4 ;;BLUE
PUBLIC NUM5 ;;GREAN
PUBLIC BALLY
PUBLIC BALLX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EXTRN f4:byte
.MODEL SMALL
.STACK 64
.DATA
REG1 DB 32 DUP(00H)
REG2 DB 32 DUP(00H)
REGCOPY DB 32 DUP(00H)
order DB 00H
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
enterlevel db 'Please Enter Level 1 Or 2: ','$'
youchoose db 'Game Level: ','$'
enterforbdin db 'Please Enter Forbidden character 0->9 or A->Z ','$'
enterintialforreg db 'Please Enter Forbidden character 0->9 or A->Z ','$'
readynotify1 db'Waiting for ','$'
readynotify2 db' is Waiting for you','$'
pressf4ready db 'When you are ready press 4f','$'
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
ENTERREG DB 'Enter the initial values of registeres',10,13,'$'
itsSi db 'Si ==> ','$'
itsDi db 10,13,'Di ==> ','$'
itsSp db 10,13,'Sp ==> ','$'
itsBp db 10,13,'Bp ==> ','$'
itsAx db 10,13,'Ax ==> ','$'
itsBx db 10,13,'Bx ==> ','$'
itsCx db 10,13,'Cx ==> ','$'
itsDx db 10,13,'Dx ==> ','$'
readregval db 2 dup(?)
target db 2 dup(?)
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
Waiting1 db ' to choose level','$'
pressenterwhendone db'press Enter when you finsih writing','$'
sc db ' score ','$'
level db ?
Forbid1 db ?
Forbid2 db ?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CARRY1 DB ?
STACK1 DB 16 DUP(?)
val db ?
CARRY1 DB 0
CARRY2 DB 0
CARRYCOPY DB 0
STACK2 DB 16 DUP(?)
var db 0h
inline db 0h
;points db 40  ;;;000000000000000000***********************
power1 db 1
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
power2 db 1
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
dataline db ?
stuck_val db ?
power_stuck db ?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;zeinab and Basma
nameplayer2 db 16 DUP(?)
points2 db ? 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FINISH_GAME DB 0
WINNER DB 0
instt db 0
MYOR DB ?
COUNTERING DB 0 ;;;;=======NEWWW ZEINAB NOUR==========;;;;
COLORIN2 DB ?
NUM1 DB 0
NUM2 DB 0
NUM3 DB 0
NUM4 DB 0
NUM5 DB 0
curse1 DW  150AH
curse2 DW  160AH
deletechat db 30 dup(00h),'$'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;emannew;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
forbideninstruction db 16 DUP(24h)
forbidendestination db 16 DUP(24h)
forbidensource db 16 DUP(24h)

Print_instruction db 'instructions:  Registers:     Memory:'
                  db 10,13,10,13,"0==>mov        AX==>I         [SI]==>B"
                  db 10,13,"1==>nop        BX==>K         [DI]==>D"
		            db 10,13,"2==>clc        CX==>M         [BX]==>L"
                  db 10,13,"3==>inc        DX==>O         [00]==>i"
                  db 10,13,"4==>dec        AH==>a         [01]==>j"
                  db 10,13,"5==>shl        AL==>b         [02]==>k"
                  db 10,13,"6==>shr        BH==>c         [03]==>l"
                  db 10,13,"7==>push       BL==>d         [04]==>m"
                  db 10,13,"8==>pop        CH==>e         [05]==>n"
                  db 10,13,"9==>ror        CL==>f         [06]==>o"
                  db 10,13,"a==>rol        DH==>g         [07]==>p"
                  db 10,13,"b==>rcr        DL==>h         [08]==>q"
                  db 10,13,"c==>rcl        SI==>A         [09]==>r"
                  db 10,13,"d==>sar        DI==>C         [0A]==>s"
                  db 10,13,"e==>mul        SP==>E         [0B]==>t"
                  db 10,13,"f==>div        BP==>G         [0C]==>u"
                  db 10,13,"g==>imul                      [0D]==>v"
                  db 10,13,"h==>idiv                      [0E]==>w"
                  db 10,13,"i==>add                       [0F]==>x"
                  db 10,13,"j==>sub                               ","$"

;;;;=======NEWWW ZEINAB NOUR==========;;;;;;;;=======NEWWW ZEINAB NOUR==========;;;;;;;;=======NEWWW ZEINAB NOUR==========;;;;
Window_Height dw 19h
Window_Width  dw 28h
BALLY Db 0H ;vertical
BALLX Db 0h ;horizontal
MYWORDING DW 0
COLORSS DB 0AH,09H,04H,0EH,0DH

.CODE
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
    jmp wrong
    notequal2:
    INC SI
    MOV CL,[SI]
    cmp cl,24h
    jne myloop2

ENDM CHECKSOURCE
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
readnum MACRO params
   LOCAL LABEL74
   LOCAL LABEL75
   LOCAL LABEL76
   LOCAL LABEL77
   local reread

    mov ah,01h 
    int 21h 
      reread:
    CMP AL,41H
    JB LABEL74
    CMP AL,46H
    JBE LABEL75
    JMP reread
    LABEL75:
    SUB AL,37H
    JMP params

    LABEL74:
    CMP AL,30H
    JAE LABEL76
    JMP reread
    LABEL76:
    CMP AL,39h
    JBE LABEL77
    JMP reread
    LABEL77:
    SUB AL,30h


ENDM readnum

read4num MACRO  ;;;000000000000000000***********************
   LOCAL LABEL80
   LOCAL LABEL81
   LOCAL LABEL82
   LOCAL LABEL83
         
   readnum LABEL80

   LABEL80:
   MOV CL,10H
   MUL CL
   MOV readregval,AL

   readnum LABEL81

   LABEL81:
   ADD readregval,AL

   readnum LABEL82

   LABEL82:
   MOV CL,10H
   MUL CL
   MOV readregval+1,AL

   readnum LABEL83

   LABEL83:
   ADD readregval+1,AL
ENDM read4num

displaynum macro  Level 
    mov dl,Level
    add dl,30h
    mov ah,2h
    INT 21H
endm displaynum

display macro  
    mov bl,10
    mov al, ah
    mov ah,0
    div bl
    mov dl,al
    push ax
    add dl,30h
    mov ah,2h
    INT 21h
    pop ax
    mov dl,ah
    add dl,30h
    mov ah,2h
    INT 21H
endm display

setcursor macro 
mov ah,2
mov bh,0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
int 10h
endm setcursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;description
emptybuffer PROC
empty:
  mov ah,1
  int 16h
JZ back
mov ah,0
int 16h
jmp empty
back:
ret
emptybuffer ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
intial PROC
;Set Divisor Latch Access Bit
mov dx,3fbh 			; Line Control Register
mov al,10000000b		;Set Divisor Latch Access Bit
out dx,al			;Out it
;Set LSB byte of the Baud Rate Divisor Latch register.
mov dx,3f8h			
mov al,0ch			
out dx,al
;Set MSB byte of the Baud Rate Divisor Latch register.
mov dx,3f9h
mov al,00h
out dx,al
;Set port configuration
mov dx,3fbh

mov al,00011011b
out dx,al
ret
intial ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;description
send PROC
    mov dx , 3FDH		; Line Status Register
AGAIN1:  	In al , dx 			;Read Line Status
  		AND al , 00100000b
  		JZ AGAIN1

;If empty put the VALUE in Transmit data register
  		mov dx , 3F8H		; Transmit data register
  		mov  al,val
  		out dx , al
          ret
send ENDP

sendarray PROC

  mov di,0
   loopoverarray:
   mov dl,REG2[di]
   mov val,dl
  call send
     add di,1
     cmp di,33
     jne loopoverarray

mov di,0
   loopoverarray4:
   mov dl,REG1[di]
   mov val,dl
  call send
     add di,1
     cmp di,33
     jne loopoverarraY4
          

  mov dl,dataline
  mov val,dl
  call send

  mov dl,points
  mov val,dl
  call send

  mov dl,stuck_val
  mov val,dl
  call send

  mov dl,power_stuck
  mov val,dl
  call send

    mov dl,FINISH_GAME
  mov val,dl
  call send

   mov dl,WINNER
  mov val,dl
  call send

   mov dl,target
  mov val,dl
  call send

   mov dl,target+1
  mov val,dl
  call send

   ;send carry2
  mov DL,CARRY2
  MOV val,dl
  call send

ret
sendarray ENDP




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;description
recive PROC
;Check that Data Ready
		mov dx , 3FDH		; Line Status Register
	CHK:	in al , dx 
  		AND al , 1
  		JZ CHK

 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		mov val , al
      ret
recive ENDP


recivearray PROC
  mov di,0
   loopoverarray2:
   call  recive
   cmp val ,3eh
   jne e
   mov var ,1
   jmp ex
   e: ;;;;;;;;;;;;;;;تعديييييييييييييييييييييييييييييييل
   cmp val,3bh
   jne labelnot
   mov inline,1
   jmp ex
   labelnot:
   mov dl,val
    mov REG1[di],dl
     add di,1
     cmp di,33
     jne loopoverarray2

mov di,0
   loopoverarray5:
   call  recive
mov dl,val
    mov REG2[di],dl
     add di,1
     cmp di,33
     jne loopoverarray5


  call  recive
  mov dl,val
  mov dataline,dl

  ;;points
  call  recive
  mov dl,val
  mov points2,dl

  call  recive
  mov dl,val
  mov stuck_val,dl

  call  recive
  mov dl,val
  mov power_stuck,dl

   call  recive
  mov dl,val
  mov FINISH_GAME,dl

   call  recive
   mov dl,val
   mov WINNER,dl

   call  recive
   mov dl,val
   mov target,dl

   call  recive
   mov dl,val
   mov target+1,dl

   call  recive
   mov dl,val
   mov carry1,dl

ex:
      ret
recivearray ENDP

inlinechat MACRO
   LOCAL check
   LOCAL label6
   LOCAL reciving
   LOCAL backspace
   LOCAL sendit
   LOCAL label01
   LOCAL exit
   LOCAL label1001
   LOCAL label5
   LOCAL label7
   LOCAL backspace4
   LOCAL label14
   LOCAL label12
   LOCAL labelenter
   LOCAL labelend1
   LOCAL labelend2
   LOCAL enter
   LOCAL enter2
   LOCAL endofline1
   LOCAL endofline2
 mov curse1,150Ah
   mov curse2,160Ah

    ;read key pressed
  check:
 
  ;setcursor curse1
  mov ah,2
  mov bh,0
  mov dx,curse1
  int 10h 


  mov ah,1
  int 16h
  jnz label6
  jmp reciving ;didn't recving
  label6:
  ;send
  ;make buffer empty
  mov ah,0
  int 16h
  ;if backspce
  cmp ah,0Eh ;backspace
  je backspace
  ;if f1
  cmp ah,3bh
  je sendit
  ;enter
  cmp ah,1Ch
  jne labelenter
  mov val,AH
  call send2
  jmp enter;;;;;;;;;;;;;;;;;;;;;wait
  labelenter:
  jmp label01 ;read it

  sendit:;senf f1 end of chat
  mov val,ah
  call send2
  jmp exit

;;backspace
  backspace:
   mov dx,curse1
   cmp dl,0AH ;;begin
   jne label1001
   jmp check
   label1001:

  ; label10:
  dec dl ;x
  push dx
  setcursor dx ;new postion for printing null
    ;print null
    mov dl,00h
   mov ah,2
   int 21h
   pop dx
   ;dec dl
  mov curse1,dx
  setcursor curse1
  mov val,0Eh
  call send2
  jmp check

  label01:
  mov dx,curse1
  cmp dl,38d
  jne labelend1
  push ax
  jmp endofline1;;=========================>>>>
  labelend1:
   setcursor curse1
  mov ah,2
  mov dl,al
  int 21h
  push DX
  ;;get the curesr postion
  mov ah,3h
mov bh,0h
int 10h
mov curse1,Dx

  ;;send the letter to the other
  pop dx
  mov val,dl
  call send2
  jmp check


  endofline1:
   mov ah,2
  mov bh,0
  mov dx,150Ah
  int 10H

  ;;printig null
  mov ah,9
  mov dx,offset deletechat
  int 21H

  ;cleated
  ;seetiiing curser again
  mov curse1,150Ah

  ;setcursor curse1
    mov ah,2
  mov bh,0
  mov dx,curse1
  int 10h 


  pop ax
  mov ah,2
  mov dl,AL
  int 21H

  mov val,dl
  call send2

    mov ah,3h
mov bh,0h
int 10h
mov curse1,Dx
  jmp check

  ;;enetr
  enter:
  mov ah,2
  mov bh,0
  mov dx,150Ah
  int 10H

  ;;printig null
  mov ah,9
  mov dx,offset deletechat
  int 21H

  ;cleated
  ;seetiiing curser again
  mov curse1,150Ah


  jmp check

  reciving:
  call recive2
  jnz label5
  jmp check ;zf=1 didn't recive
  label5:
  ;;zf =0 he recived sth ==>val
  cmp val,0Eh;backspace
  jne label7
  jmp backspace4
  label7:
  ;if waht i recive is f1
  cmp val,3bh;f1
  jne label14
  jmp exit
  label14:
  cmp val,1Ch;enetr
  jne label12
  jmp enter2

  label12:
   ;setcursor curse1
   ;check if end
   mov dx,curse2
  cmp dl,38d
  jne labelend2
  jmp endofline2;;=========================>>>>
  
  labelend2:
  mov ah,2
  mov bh,0
  mov dx,curse2
  int 10h 

  
  ;;print the val
  mov ah,2
  mov dl,val
  int 21h
    ;;get the curesr postion
  mov ah,3h
mov bh,0h
int 10h
mov curse2,Dx
jmp check


backspace4:
 
  ;curser1 posyion previous
  ;dec col
  mov dx,curse2
  ;;special 
  dec dl ;x
  push dx
  setcursor dx ;new postion for printing null
    ;print null
    mov dl,00h
   mov ah,2
   int 21h
   pop dx
   ;dec dl
  mov curse2,dx
  setcursor curse2
  jmp check

  enter2:
  mov ah,2
  mov bh,0
  mov dx,160Ah
  int 10H

   ;;printig null
  mov ah,9
  mov dx,offset deletechat
  int 21H

  ;cleated
  ;seetiiing curser again
  mov curse2,160Ah
  jmp check



  endofline2:
     mov ah,2
  mov bh,0
  mov dx,160Ah
  int 10H

  ;;printig null
  mov ah,9
  mov dx,offset deletechat
  int 21H

  ;cleated
  ;seetiiing curser again
  mov curse2,160Ah

  ;setcursor curse1
    mov ah,2
  mov bh,0
  mov dx,curse2
  int 10h 

  mov ah,2
  mov dl,val
  int 21H


   ;;get the curesr postion
  mov ah,3h
mov bh,0h
int 10h
mov curse2,Dx
  jmp check


  
exit:
   mov ah,2
  mov bh,0
  mov dx,150Ah
  int 10H

  ;;printig null
  mov ah,9
  mov dx,offset deletechat
  int 21H


    mov ah,2
  mov bh,0
  mov dx,160Ah
  int 10H

  ;;printig null
  mov ah,9
  mov dx,offset deletechat
  int 21H


ENDM inlinechat

;;;000000000000000000********* EMAN &&& NOUR****************0000000000000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;macro check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkwinner MACRO params ;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   local label125
   local label126
   cmp points,0
   jnz label125
   MOV FINISH_GAME,1
   call graph
   jmp ENDOFMY
   label125:
   mov cx,8h ;;;;;;;;;;;;;;stop before el memory
   mov si,offset REG2
   checkreg:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
   MOV BX,params
   mov AH,[SI]
   mov AL,[SI+1]
   CMP AX,BX
   JNZ LABEL126
   MOV FINISH_GAME,1
   MOV WINNER,1
   call graph
   JMP ENDOFMY
   LABEL126:
   inc si
   inc si
   dec cx
   jnz checkreg

   cmp bx,105EH
   jz ENDOFMY
   mov target,10H
   mov target+1,5EH

         ENDOFMY:
ENDM checkwinner


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TURNFORBIDN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MACROTOPUTFORBIDENS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TURNFORBIDEN MACRO
   LOCAL LABEL161
   LOCAL LABEL136
   LOCAL LABEL162
   LOCAL LABEL137
   LOCAL LABEL163
   LOCAL LABEL138
   LOCAL LABEL164
   LOCAL LABEL139
   LOCAL LABEL165
   LOCAL LABEL140
   LOCAL LABEL166
   LOCAL label141
   LOCAL LABEL142
   LOCAL LABEL167
   LOCAL LABEL143
   LOCAL LABEL168
   LOCAL LABEL144
   LOCAL LABEL179
   LOCAL LABEL180
   LOCAL LABEL169
   LOCAL LABEL181
   LOCAL LABEL170
   LOCAL LABEL182
   LOCAL LABEL171
   LOCAL LABEL183
   LOCAL LABEL172
   LOCAL LABEL184
   LOCAL LABEL173
   LOCAL LABEL185
   LOCAL LABEL186
   LOCAL LABEL174
   LOCAL LABEL187
   LOCAL LABEL175
   LOCAL LABEL188
   LOCAL LABEL189
   LOCAL LABEL176
   LOCAL LABEL190
   LOCAL LABEL177
   LOCAL LABEL191
   LOCAL LABEL192
   LOCAL LABEL178
   LOCAL LABEL193
   LOCAL label194
   LOCAL LABEL160
   LOCAL label195
   LOCAL LABEL196
   LOCAL LABEL197
   LOCAL LABEL198
   LOCAL LABEL199
   LOCAL LABEL1
   LOCAL LABEL2
   LOCAL LABEL3
   LOCAL LABEL4

  ;;;;;;;;;COMPARE FROM A-> Z TO FILL THREE ARRAYS ;;;;;;;;;;;;;;;;;;;;;;;;
  MOV SI,OFFSET forbideninstruction
  MOV DI,OFFSET forbidendestination
  MOV BX,OFFSET forbidensource

  CMP Forbid2,41H ;;;;;;IF A
  JE LABEL161
  JMP label136
  LABEL161:
  ;;;;;;;;;;;;;;;;;;;forbbiden A
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],64h
   MOV byte ptr [SI+1],69h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],49h
  MOV byte ptr [DI+1],61h
  MOV byte ptr [DI+2],62h
  MOV byte ptr [DI+3],73h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],49h
  MOV byte ptr [BX+1],61h
  MOV byte ptr [BX+2],62h
  MOV byte ptr [BX+3],73h
  MOV byte ptr [BX+4],41h


  JMP ENDMACRO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label136:
  CMP Forbid2,42H ;;;;;;IF B
  JE LABEL162
  jMP label137
  LABEL162:
  ;;;;;;;;;;;;;;;;;;;forbbiden B
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
   MOV byte ptr [SI],6Ah
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],4Bh
  MOV byte ptr [DI+1],63h
  MOV byte ptr [DI+2],64h
  MOV byte ptr [DI+3],4Ch
  MOV byte ptr [DI+4],47h
  MOV byte ptr [DI+5],74h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],4Bh
  MOV byte ptr [BX+1],63h
  MOV byte ptr [BX+2],64h
  MOV byte ptr [BX+3],4Ch
  MOV byte ptr [BX+4],47h
  MOV byte ptr [BX+5],74h
   MOV byte ptr [BX+6],42h
  JMP ENDMACRO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label137:
  CMP Forbid2,43H ;;;;;;IF C
   JE LABEL163
  jMP label138
  LABEL163:
  ;;;;;;;;;;;;;;;;;;;forbbiden C
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],32h
  MOV byte ptr [SI+1],33h
  MOV byte ptr [SI+2],34h
  MOV byte ptr [SI+3],62h
  MOV byte ptr [SI+4],63h
  ;MOV byte ptr [SI+5],75h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],65h
  MOV byte ptr [DI+1],66h
  MOV byte ptr [DI+2],4Dh
  MOV byte ptr [DI+3],75h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],65h
  MOV byte ptr [BX+1],66h
  MOV byte ptr [BX+2],4Dh
  MOV byte ptr [BX+3],75h
   MOV byte ptr [BX+4],43h
  JMP ENDMACRO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label138:
  CMP Forbid2,44H ;;;;;;IF D
    JE LABEL164
  jMP label139
  LABEL164:
  ;;;;;;;;;;;;;;;;;;;forbbiden D
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],34h
  MOV byte ptr [SI+1],66h
   MOV byte ptr [SI+2],69h
   MOV byte ptr [SI+3],68h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],43h
  MOV byte ptr [DI+1],67h
  MOV byte ptr [DI+2],68h
  MOV byte ptr [DI+3],4Fh
  MOV byte ptr [DI+4],44h
  MOV byte ptr [DI+5],76h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
   MOV byte ptr [bx],43h
  MOV byte ptr [bx+1],67h
  MOV byte ptr [bx+2],68h
  MOV byte ptr [bx+3],4Fh
  MOV byte ptr [bx+4],44h
  MOV byte ptr [bx+5],76h
  ; MOV byte ptr [BX+6],44h
  JMP ENDMACRO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label139:
  CMP Forbid2,45H ;;;;;;IF E
  JE LABEL165
  jMP label140
  LABEL165:
  ;;;;;;;;;;;;;;;;;;;forbbiden E
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],34h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],77h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
   MOV byte ptr [bx],77h
    MOV byte ptr [BX+1],45h
  JMP ENDMACRO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label140:
  CMP Forbid2,46H ;;;;;;IF F
  JE LABEL166
  jMP label141
  LABEL166:
  ;;;;;;;;;;;;;;;;;;;forbbiden F
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],78h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [bx],78h
   MOV byte ptr [BX+1],46h
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  label141:
  CMP Forbid2,47H ;;;;;;IF G
  jne label142
  JMP ENDMACRO

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label142:
  CMP Forbid2,48H ;;;;;;IF H
  JE LABEL167
  jMP label143
  LABEL167:
  ;;;;;;;;;;;;;;;;;;;forbbiden H
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],35h
  MOV byte ptr [SI+1],36h
  MOV byte ptr [SI+2],37h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],61h
  MOV byte ptr [DI+1],63h
  MOV byte ptr [DI+2],65h
  MOV byte ptr [DI+3],67h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],61h
  MOV byte ptr [BX+1],63h
  MOV byte ptr [BX+2],65h
  MOV byte ptr [BX+3],67h
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label143:
  CMP Forbid2,49H ;;;;;;IF I
   JE LABEL168
  jMP label144
  LABEL168:
  ;;;;;;;;;;;;;;;;;;;forbbiden I
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
   MOV byte ptr [SI],33h
   MOV byte ptr [SI+1],66h
   MOV byte ptr [SI+2],67h
   MOV byte ptr [SI+3],68h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
   Mov byte ptr[DI],41h
   Mov byte ptr[DI+1],42h
   Mov byte ptr[DI+2],43h
   Mov byte ptr[DI+3],44h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
   Mov byte ptr[BX],41h
   Mov byte ptr[BX+1],42h
   Mov byte ptr[BX+2],43h
   Mov byte ptr[BX+3],44h
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label144:
  CMP Forbid2,4AH ;;;;;;IF J
  jne label179
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label179:
  CMP Forbid2,4BH ;;;;;;IF K
  jne label180
   JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label180:
  CMP Forbid2,4CH ;;;;;;IF L
  je LABEL169
  jMP label181
  LABEL169:
  ;;;;;;;;;;;;;;;;;;;forbbiden L
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],32h
  MOV byte ptr [SI+1],35h
  MOV byte ptr [SI+2],61h
  MOV byte ptr [SI+3],63h
  MOV byte ptr [SI+4],65h
   MOV byte ptr [SI+5],67h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],62h
  MOV byte ptr [DI+1],64h
  MOV byte ptr [DI+2],66h
  MOV byte ptr [DI+3],68h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],62h
  MOV byte ptr [BX+1],64h
  MOV byte ptr [BX+2],66h
  MOV byte ptr [BX+3],68h
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label181:
  CMP Forbid2,4DH ;;;;;;IF M
   JE LABEL170
  jMP label182
  LABEL170:
  ;;;;;;;;;;;;;;;;;;;forbbiden M
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],30h
  MOV byte ptr [SI+1],65h
   MOV byte ptr [SI+2],67h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label182:
  CMP Forbid2,4EH ;;;;;;IF N
   JE LABEL171
  jMP label183
  LABEL171:
  ;;;;;;;;;;;;;;;;;;;forbbiden N
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],31h
  MOV byte ptr [SI+1],33h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label183:
  CMP Forbid2,4FH ;;;;;;IF O
   JE LABEL172
  jMP label184
  LABEL172:
  ;;;;;;;;;;;;;;;;;;;forbbiden O
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],30h
  MOV byte ptr [SI+1],31h
  MOV byte ptr [SI+2],38h
  MOV byte ptr [SI+3],39h
  MOV byte ptr [SI+4],61h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label184:
  CMP Forbid2,50H ;;;;;;IF P
  JE LABEL173
  jMP label185
  LABEL173:
  ;;;;;;;;;;;;;;;;;;;forbbiden P
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],31h
  MOV byte ptr [SI+1],37h
  MOV byte ptr [SI+2],38h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],45h
  MOV byte ptr [DI+1],47h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],45h
  MOV byte ptr [BX+1],47h
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label185:
  CMP Forbid2,51H ;;;;;;IF Q
  jne label186
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label186:
  CMP Forbid2,52H ;;;;;;IF R
   JE LABEL174
  jMP label187
  LABEL174:
  ;;;;;;;;;;;;;;;;;;;forbbiden R
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],36h
  MOV byte ptr [SI+1],39h
  MOV byte ptr [SI+2],62h
  MOV byte ptr [SI+3],64h
  MOV byte ptr [SI+4],61h
  MOV byte ptr [SI+5],63h;;

  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label187:
  CMP Forbid2,53H ;;;;;;IF S
   JE LABEL175
  jMP label188
  LABEL175:
  ;;;;;;;;;;;;;;;;;;;forbbiden S
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
   MOV byte ptr [SI],35h
   MOV byte ptr [SI+1],36h
   MOV byte ptr [SI+2],64h
   MOV byte ptr [SI+3],6Ah
   MOV byte ptr [SI+4],37h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
   MOV byte ptr [DI],41h
   MOV byte ptr [DI+1],42h
   MOV byte ptr [DI+2],45h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
   MOV byte ptr [BX],41h
   MOV byte ptr [BX+1],42h
   MOV byte ptr [BX+2],45h
   JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label188:
  CMP Forbid2,54H ;;;;;;IF T
  jne label189
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label189:
  CMP Forbid2,55H ;;;;;;IF U
   JE LABEL176
  jMP label190
  LABEL176:
  ;;;;;;;;;;;;;;;;;;;forbbiden U
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],37h
  MOV byte ptr [SI+1],65h
   MOV byte ptr [SI+2],6Ah
   MOV byte ptr [SI+3],67h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label190:
  CMP Forbid2,56H ;;;;;;IF V
   JE LABEL177
  jMP label191
  LABEL177:
  ;;;;;;;;;;;;;;;;;;;forbbiden V
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
  MOV byte ptr [SI],30h
  MOV byte ptr [SI+1],66h
   MOV byte ptr [SI+2],68h
  ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label191:
  CMP Forbid2,57H ;;;;;;IF W
  jne label192
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label192:
  CMP Forbid2,58H ;;;;;;IF X
   JE LABEL178
  jMP label193
  LABEL178:
  ;;;;;;;;;;;;;;;;;;;forbbiden X
  ;;;;;;;;;;;;;;;;INSTRUCTION;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;DESTINATION;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],49h
  MOV byte ptr [DI+1],4Bh
  MOV byte ptr [DI+2],4Dh
  MOV byte ptr [DI+3],4Fh
  MOV byte ptr [DI+4],4Ch
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],49h
  MOV byte ptr [BX+1],4Bh
  MOV byte ptr [BX+2],4Dh
  MOV byte ptr [BX+3],4Fh
  MOV byte ptr [BX+4],4Ch
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label193:
  CMP Forbid2,59H ;;;;;;IF Y
  jne label194
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label194:
  CMP Forbid2,5AH ;;;;;;IF Z
  jne label160
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label160:;;;;;;;;;;;;;;;;;number
  CMP Forbid2,30h ;;;;0
  jne label195
  ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],69h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],30H
  MOV byte ptr [BX+1],69h
  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label195:
  CMP Forbid2,31h ;;1
   jne label196
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],6Ah
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],31H
  MOV byte ptr [BX+1],6Ah

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label196:
  CMP Forbid2,32h ;;2
   jne label197
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],6Bh
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],32H
  MOV byte ptr [BX+1],6Bh

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label197:
  CMP Forbid2,33h ;;;3
   jne label198
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],6Ch
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],33H
  MOV byte ptr [BX+1],6Ch

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label198:
  CMP Forbid2,34h ;;;;4
   jne label199
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],6Dh
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],34H
  MOV byte ptr [BX+1],6Dh

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label199:
  CMP Forbid2,35h ;;;5
   jne label1
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],6Eh
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],35H
  MOV byte ptr [BX+1],6Eh

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label1:
  CMP Forbid2,36h  ;;;;6
   jne label2
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],6Fh
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],36H
  MOV byte ptr [BX+1],6Fh

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label2:
  CMP Forbid2,37h  ;;;;7
   jne label3
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],70h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],37H
  MOV byte ptr [BX+1],70h

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label3:
  CMP Forbid2,38h ;;;8
   jne label4
     ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],71h
;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],38H
  MOV byte ptr [BX+1],71h

  JMP ENDMACRO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  label4:
  CMP Forbid2,39h ;;9
  jne ENDMACRO
    ;;;;;;;;;;;;;;Destination;;;;;;;;;;;;;;;;;
  MOV byte ptr [DI],72h
  ;;;;;;;;;;;;;;;SOURCE;;;;;;;;;;;;;;;;;;;;;
  MOV byte ptr [BX],39H
  MOV byte ptr [BX+1],72h

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ENDMACRO:

ENDM TURNFORBIDEN

recive2 PROC
;Check that Data Ready
		mov dx , 3FDH		; Line Status Register
		in al , dx 
  		AND al , 1
  		JZ exit10

 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		mov val , al
     exit10:
      ret
recive2 ENDP

send2 PROC
       mov dx , 3FDH		; Line Status Register
       In al , dx 			;Read Line Status
  	   AND al , 00100000b
  		JZ AGAIN10

;If empty put the VALUE in Transmit data register
  		mov dx , 3F8H		; Transmit data register
  		mov  al,val
  		out dx , al
          AGAIN10:
          ret
send2 ENDP

;1
play PROC FAR
   MOV AX,@DATA
    MOV DS,AX
   call  intial 

   cmp order,1
   jnz nothosttt
   ;load stack2 pointer 
   LEA AX,STACK2
   ;MOV REG2[17],12H
   MOV REG2[5],AL
   ;;;;;;;;;;;;;;;;;
      LEA AX,STACK1
   ;;MOV REG2[17],12H
   MOV REG1[5],AL
   ;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   jmp myhosting

   nothosttt:
      LEA AX,STACK2
   ;MOV REG2[17],12H
   MOV REG1[5],AL
   ;;;;;;;;;;;;;;;;;
      LEA AX,STACK1
   ;;MOV REG2[17],12H
   MOV REG2[5],AL

   myhosting:

   mov target,10H
   mov target+1,5EH


;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;new
;;;;;;;;;;;;;;take level
;clear screen
    mov ax,0600h
    mov bh,07
    mov cx,0
    mov dx,184fh
    int 10h


  cmp order ,1
  jne nothost

   mov dl,0d
   mov dh,24d
   setcursor

    mov ah,9
    mov dx,offset pressenterwhendone
    int 21h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;تعدييييييييييييييييل
   mov dl,0d
   mov dh,0d
   setcursor

    mov ah,9
    mov dx,offset enterlevel
    int 21h


    ;choose level
   takelevel:
   mov ah,0
   int 16h
   cmp al,31H
   je changenum
   cmp al,32H
   je changenum
   jmp takelevel
   ;chang to number
   changenum:
   ;print the number
   mov ah,2
   mov dl,al
   int 21H

   sub al,30h
   mov level,al

;we finish press enter
   enter: mov ah,0
   int 16h
   cmp ah,0Eh;back space
   je back2
   cmp ah,1Ch;enter
   jne enter
   jmp leveldone

   back2:
    ;;get curser
   mov ah,3
   mov bh,0h
   int 10h
   dec dl
   push dx

   ;set curser to the new postion
   mov ah,2
   mov bh,0
   int 10h

   mov dl,00h
   mov ah,2
   int 21h
   dec dl
   ;seeting curser for second read
   pop dx
   mov ah,2
   mov bh,0
   int 10h
   jmp takelevel


;send level
leveldone:
mov al,level
mov val,al
call send
jmp readingforbidden

nothost:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;basma waiting for level
mov dl,0d
mov dh,0d
setcursor

mov ah,9
mov dx,offset readynotify1
int 21h

mov ah,9
mov dx,offset nameplayer2
int 21h

mov ah,9
mov dx,offset Waiting1
int 21h 

call recive
mov al,val
mov level,al

;;;;;;;;;;;;forbidden
readingforbidden:
 ;print
   mov dl,0d
  mov dh,24d
  setcursor

  mov ah,9
  mov dx,offset pressenterwhendone
  int 21h

mov dl,0d
mov dh,2d
setcursor

mov ah,9
mov dx,offset youchoose
int 21h 

  ;mov ah,level
   displaynum level


   mov dl,0d
   mov dh,4d
   setcursor

;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   cmp level,1
   jnz label142
   jmp label141
   label142:
   mov ah,9
   mov dx,offset ENTERREG
   int 21h

   mov si,0
 
   mov ah,9
   mov dx,offset itsSi
   int 21h

   CMP ORDER,1
   JZ HOSTT 
   CALL recivearray
   HOSTT:

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2

   mov ah,9
   mov dx,offset itsDi
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2

   mov ah,9
   mov dx,offset itsSp
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2

   mov ah,9
   mov dx,offset itsBp
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2

   mov ah,9
   mov dx,offset itsAx
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2
   
   mov ah,9
   mov dx,offset itsBx
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2

   mov ah,9
   mov dx,offset itsCx
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2
 
   mov ah,9
   mov dx,offset itsDx
   int 21h

   read4num

   mov dh,readregval
   mov dl,readregval+1
   mov reg1[si],dh
   mov reg1[si+1],dl

   add si,2

   ;load stack2 pointer 
   ;LEA AX,STACK2
   ;MOV REG2[17],12H
   ;MOV REG2[5],AL
   ;;;;;;;;;;;;;;;;;
   LEA AX,STACK1
   ;;MOV REG2[17],12HFF
   MOV REG1[5],AL

   LEA AX,STACK2
   ;MOV REG2[17],12H
   MOV AL,REG1[0]
   MOV REG2[5],AL

   CALL sendarray
   CMP ORDER,1
   JNZ HOSTTT
   CALL recivearray
   HOSTTT:

   mov dl,0d
   mov dh,19d
   setcursor

   label141:
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
    
    mov ah,9
    mov dx,offset enterforbdin
    int 21h
    ;;;read
   readforbidden:
   mov ah,0
   int 16h

   cmp al,30h ;1
   jae checkinno2
   jmp readforbidden

  checkinno2: 
   cmp al,39H
   jbe rightforbidden ;stiil notright
   cmp al,41h ;;A letter
   jae checkinletter2 ;;;;;;;;;;;;;;;;;;else letter
   jmp rightforbidden
   ;else right ;firstdigit
   checkinletter2:
   cmp al,5Ah
   ja readforbidden

  rightforbidden:
   ;print the char;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   mov ah,2
   mov dl,al
   int 21h
   push ax
;sub al,30H
;press enter when done
 enter3: mov ah,0
   int 16h
   cmp ah,0Eh;back space
   je back3
   cmp ah,1Ch;enter
   jne enter3
   jmp forbiddendone

    back3:
    ;;get curser
   mov ah,3
   mov bh,0h
   int 10h
   dec dl
   push dx

   ;set curser to the new postion
   mov ah,2
   mov bh,0
   int 10h

   mov dl,00h
   mov ah,2
   int 21h
   dec dl
   ;seeting curser for second read
   pop dx
   mov ah,2
   mov bh,0
   int 10h
   jmp readforbidden

forbiddendone:
pop ax
mov Forbid1 ,al
;wait fo f4 to start
;print
     mov ah,2h
    mov dx,1700H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset pressf4ready
    int 21h
check:
  mov ah,1
  int 16h
  jz reciving ;didn't recving
  ;send
  ;make buffer empty
  mov ah,0
  int 16h
  cmp ah,3Eh ;F4 scan code
  je sendready
  ;recive
  reciving:
  call recive2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;yah
  jz check ;zf=1 didn't recive

  ;he recived sth
  cmp val,3Eh ;f4
  je acceptready

sendready:
 mov val,3Eh
 call send2
  
    mov ah,2h
    mov dx,1500H
    mov bh,0
    int 10h
;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset readynotify1
    int 21h
    ;;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset nameplayer2
    int 21h
    ;;;;;;;;;;;;;;

waituntillacceptmyinvitation:
call recive2
jz waituntillacceptmyinvitation;recive his name
cmp val,3eh
je start


acceptready:
;;;;;;;;;recive sender name
;;;;;;;;;;;invit bar
;setcurser at 
    mov ah,2h
    mov dx,1500H
    mov bh,0
    int 10h
;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset nameplayer2
    int 21h
    ;;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset readynotify2
    int 21h
;wait key press to start game or exit 
againf4:
mov ah,0
int 16h
cmp ah,3eh ;f4 scan code ==accept
je sendresponse

jmp againf4

sendresponse:
mov val ,3eh
call send2
;;;;;;;;;;;;;;;;start game
start:
    ;send forbiden
    ;host
    ;send
    cmp order,1
    jne otherplayer
    mov al,Forbid1
    mov val,al
    call send
    ;recive
    call recive
    mov al,val
    mov Forbid2,al
jmp game
    ;;nothost
    ;recive
    otherplayer:
    call recive
    mov al,val
    mov Forbid2,al
    ;send
    mov al,Forbid1
    mov val,al
    call send
  game:
    ;;graphics mode
    mov ah,0
    mov al,13h
    int 10h

   ;mov dl,0d
   ;mov dh,24d
   ;setcursor
    ;;;;;;;;;;;;;;
    ;mov ah,9
    ;mov dx,offset youchoose
    ;int 21h
    ;;;;;;;;;;;;print levelnum
    ;mov ah,level
    ;displaynum Level 
;;;000000000000000000********* EMAN &&& NOUR****************0000000000000
;mov cx,5;;;;;;;;;;;;;;;;;;;;;need to change
   TURNFORBIDEN

  recall: 
   
   CMP FINISH_GAME,1
   JNZ LABEL132
   JMP EEEEXIT
   LABEL132:

   ;push cx
   call graph

   Mov ah,2ch ;get the system time
   int 21h ;ch=hour cl=minute dh=secod dl=1/100 seconds
   CMP DH,0h
   JB LABEL11122
   JMP LABEL255
   LABEL11122:

;========================================
   mov ah,0
   mov al,order
   mov bl,2
   div bl
   cmp ah,0 ;even

   JNE LABEL261
   jMP GETBALLY
   LABEL261:

   label11111:
   mov ah,2ch ;get the system time
   int 21h ;ch=hour cl=minute dh=secod dl=1/100 seconds

   PUSH DX
   MOV AL,DH
   MOV AH,0
   MOV DX,0
   MOV BL,5
   DIV BL

   MOV DH,00
   MOV DL,AH
   MOV SI,DX
   MOV BX, OFFSET COLORSS
   MOV DX,[BX][SI]
   MOV COLORIN2,DL
   POP DX

   mov ax, dx   ; DH=seconds, DL=hundredths of second

   MOV MYWORDING,DX

   mov bx,Window_Width
   mov dx,0
   mov ah,0
   div bl
   MOV DH,00
   MOV DL,AH
   add BALLX,dl
   cmp ballx,39d
   ja label11111

   MOV DX,MYWORDING

   mov ax, dx   ; DH=seconds, DL=hundredths of second
   mov bx,Window_Height
   mov dx,0
   mov ah,0
   div bL
   MOV DH,00
   MOV DL,AH

   add BALLy,dl
   cmp BALLY,24d
   ja label11111

   MOV DL,BALLX
   MOV VAL,DL
   CALL SEND

   MOV DL,BALLY
   MOV VAL,DL
   CALL SEND

   MOV DL,COLORIN2
   MOV VAL,DL
   CALL SEND

   JMP MYCONTI

   GETBALLY:
   call recive
   mov dl,val
   MOV BALLX,DL

   call recive
   mov dl,val
   MOV BALLY,DL

   call recive
   mov dl,val
   MOV COLORIN2,DL

   MYCONTI:
   CALL BALL

;; if one arrowed up and two arrowed up is not handled

;;;; check if won = 0 then wait to receive
   CMP WON,0
   JNZ LABEL258

   call  recive
   mov dl,val
   CMP DL,1
;; COMPARE WITH TWO THEN JUMP TO LABEL255
   JE LABEL2RY
   JMP LABEL255 ;;;;;;;;;;;; TO DELETE
   LABEL2RY:
   CMP COLORIN2,0AH
   JNE MAYBLUE
   INC POINTS2
   JMP KMMMEL
   MAYBLUE:
   CMP COLORIN2,09H
   JNE MAYRED
   ADD POINTS2,2
   JMP KMMMEL
   MAYRED:
   CMP COLORIN2,04H
   JNE MAYYEL
   ADD POINTS2,3
   JMP KMMMEL
   MAYYEL:
   CMP COLORIN2,0EH
   JNE MAYMAG
   ADD POINTS2,4
   JMP KMMMEL
   MAYMAG:
   ADD POINTS2,5
   ;MOV VAL,1;;;;;;;===========))))))
   ;CALL SEND2;;;;;;;===========))))))
   KMMMEL:
   JMP LABEL255
   
   
   LABEL258:
;;;; check if something to recieve AND won = 1 ==> won = 0
   call recive2
   MOV DL,VAL
   CMP DL,1
   JNE LABEL259

   CMP COLORIN2,0AH
   JNE MAYBLUE2
   INC POINTS2
   JMP KMMMEL2
   MAYBLUE2:
   CMP COLORIN2,09H
   JNE MAYRED2
   ADD POINTS2,2
   JMP KMMMEL2
   MAYRED2:
   CMP COLORIN2,04H
   JNE MAYYEL2
   ADD POINTS2,3
   JMP KMMMEL2
   MAYYEL2:
   CMP COLORIN2,0EH
   JNE MAYMAG2
   ADD POINTS2,4
   JMP KMMMEL2
   MAYMAG2:
   ADD POINTS2,5
   KMMMEL2:
   ;MOV VAL,1;;;;;;;===========))))))
   ;CALL SEND2;;;;;;;===========))))))
   JMP LABEL255

   LABEL259:

;;;; check if won then send
   MOV VAL,1
   CALL SEND2
   
   CMP COLORIN2,0AH
   JNE MAYBLUE3
   INC NUM1
   INC POINTS
   JMP KMMMEL3
   MAYBLUE3:
   CMP COLORIN2,09H
   JNE MAYRED3
   INC NUM2
   ADD POINTS,2
   JMP KMMMEL3
   MAYRED3:
   CMP COLORIN2,04H
   JNE MAYYEL3
   INC NUM3
   ADD POINTS,3
   JMP KMMMEL3
   MAYYEL3:
   CMP COLORIN2,0EH
   JNE MAYMAG3
   INC NUM4
   ADD POINTS,4
   JMP KMMMEL3
   MAYMAG3:
   INC NUM5
   ADD POINTS,5
   KMMMEL3:
   ;call  recive ;;;;;;;===========))))))

   LABEL255:
   call graph
   MOV WON,0
   ;check it is my order
   mov ah,0
   mov al,order
   mov bl,2
   div bl
   cmp ah,0 ;even

;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   JNE LABEL140
   jMP recivingplay
   LABEL140:
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   call emptybuffer 
   mov ah,0
    int 16h
    cmp al,70h ;;;;;;;;;;enter p small to choose power anything else to call instruction
    jnz label111
    jmp power
    label111:

    cmp al,69h ;;;;;;;;;;enter i small to choose power anything else to call instruction
    jnz label145
    jmp INSST
    label145:

   CONTIN:

   cmp ah,3eh 
   jne lab
   mov val,3Eh
   call send;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;f4 to main page
   jmp l
   lab:

   cmp ah,3bh ;pressed f1
   je label1234
   jmp label0909
   label1234:
   mov val,ah
   call send;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;f4 to main page
   inlinechat
   jmp recall
   label0909:

   ;;;;;;;;;;;;;;;;;my order "send"                     ;if odd 127 to 143
   ;set curser                           ;else 151
   mov ah,2
   mov dl,0
   mov dh,17d
   int 10h
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;empty buffer
    call emptybuffer
   ;;take instruction ;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    mov Di,offset REGCOPY
    put0inreG7:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov DL,[SI]
    mov [DI],DL
    inc si
    INC DI
    dec cx
    jnz put0inreg7
   MOV AH,1
   INT 21H
   MOV MYOR,AL
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   call instruction
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   CMP Level,1
   JZ LEVEL1
      CMP MYOR,31H ;; ENTER ONE BEFOR INSTRUCTION TO MOV IN YOUR REG
      JZ LABEL138
      JMP LEVEL1
      LABEL138:
      mov cx,20h ;;;;;;;;;;;;;;stop before el memory
      mov si,offset REG2
      MOV BX,offset REG1
      mov Di,offset REGCOPY
      put0inreG8:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
      mov DL,[SI]
      mov DH,[DI]
      CMP DH,DL
      JZ LABEL139
      MOV [BX],DL
      LABEL139:
      inc si
      INC BX
      INC DI
      dec cx
      jnz put0inreg8

      mov cx,20h ;;;;;;;;;;;;;;stop before el memory
      mov si,offset REG2
      mov Di,offset REGCOPY
      put0inreg9:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
      mov DL,[DI]
      mov [SI],DL
      inc si
      INC DI
      dec cx
      jnz put0inreg9

   LEVEL1:
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
   mov dh,target
   mov dl,target+1
   checkwinner dx
   ;;clear
   mov ax,0600h
   mov bh,0
   mov ch,17d
   mov cl,0
   mov dh,17d
   mov dl,7
   int 10h

   ;send the
   call sendarray
   inc order ;to make order even for reciving in next path
   jmp again

   recivingplay:
   call recivearray
   cmp var,1
   jne a
   jmp l
   a:  ;;;;;;;;;;;;;;;;;;;;;;;;تعديييييييييييييييييل

   cmp inline,1
   je label2345
   jmp labelinline
   label2345:
   mov inline,0
   inlinechat
   jmp recall
   labelinline:

   CMP FINISH_GAME,0
   JZ LABEL137
      CMP WINNER,0 ; IF WINNER =0
      JZ LABEL134
      MOV WINNER,0
      JMP LABEL135  ; ELSE ==> WINNER =1
      LABEL134: ; IF WINNER =0 
      MOV WINNER,1
      LABEL135:
   LABEL137:
   inc order

   again:
      ;;clear
      mov ax,0600h
   mov bh,0
   mov ch,17d
   mov cl,0
   mov dh,17d
   mov dl,7
   int 10h


   jmp recall


    power:
    mov ah,2
    mov dl,0
    mov dh,17d
    int 10h

    mov ah,0
    int 16h

    mov ah,2
    mov dl,al
    int 21h

    cmp al,31H
    jnz checkpw
    
    ;;;;;;;;;;;;;;;;;;;COMPARE POINTS AND COMPARE POWER1
    cmp power1,0
    jne label113
    INC order
   CALL sendarray
    jmp AGAIN ;;;;;;;used before
    label113:
    cmp points,30d ;;compare if it has points to subtract from
    
    ja label112
    
    INC order
   CALL sendarray
    jmp AGAIN
    
    label112:
    mov dh,power1
    sub dh,1
    mov power1,dh

    mov dh,points
    sub dh,1Eh
    mov points,dh
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;new;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov cx,10h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG1
    put0inreg:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov byte ptr [si],00h
    inc si
    dec cx
    jnz put0inreg

    call sendarray
    ;CALL recivearray
    inc order
    call graph
    jmp again


    checkpw: ;;;000000000000000000***********************
    cmp al,32H
    jnz checkpw2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;************** POWER2
    cmp points,2
    ja label120
    inc order
    jmp AGAIN
    label120:
    mov dh,points
    sub dh,2
    mov points,dh
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;new;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    mov power_stuck,1
    mov ah,1
    int 21h
    SUB AL,30H
    mov dataline,al
    mov ah,1
    int 21h
    SUB AL,30H
    mov stuck_val,al
    CALL sendarray
    inc order
    JMP AGAIN


    checkpw2:
    cmp al,33H
   JZ LABELAITY
    jMP checkpw3
   LABELAITY:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;************** POWER3
    cmp points,5
    ja label121
    inc order
    jmp AGAIN
    label121:
    mov dh,points
    sub dh,5
    mov points,dh
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;new;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    mov Di,offset REGCOPY
    put0inreg2:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov DL,[SI]
    mov [DI],DL
    inc si
    INC DI
    dec cx
    jnz put0inreg2

   MOV DL,CARRY2
   MOV CARRYCOPY,DL

   ;;take instruction
    call instruction

    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    MOV BX,offset REG1
    mov Di,offset REGCOPY
    put0inreg6:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov DL,[SI]
    mov DH,[DI]
    CMP DH,DL
    JZ LABEL122
    MOV [BX],DL
    LABEL122:
    inc si
    INC BX
    INC DI
    dec cx
    jnz put0inreg6

   MOV DL,CARRY2
   MOV DH,CARRYCOPY
   CMP DH,DL
   JZ NOEDITING
   MOV CARRY1,DL
   NOEDITING:


    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    mov Di,offset REGCOPY
    put0inreg3:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov DL,[DI]
    mov [SI],DL
    inc si
    INC DI
    dec cx
    jnz put0inreg3
   MOV DH,CARRYCOPY
   MOV CARRY2,DH

    CALL sendarray
    INC order
    JMP AGAIN


    checkpw3:
    cmp al,34H
    jnz checkpw4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;************** POWER4
    cmp points,3
    ja label123
    inc order
    jmp AGAIN
    label123:
    mov dh,points
    sub dh,3
    mov points,dh
   
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;new;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    mov Di,offset REGCOPY
    put0inreg4:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov DL,[SI]
    mov [DI],DL
    inc si
    INC DI
    dec cx
    jnz put0inreg4

   MOV DL,CARRY2
   MOV CARRYCOPY,DL

   ;;take instruction
    call instruction

    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    MOV BX,offset REG1
    mov Di,offset REGCOPY
    put0inreg5:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov DL,[SI]
    mov DH,[DI]
    CMP DH,DL
    JZ LABEL124
    MOV [BX],DL
    LABEL124:
    inc si
    INC DI
    INC BX
    dec cx
    jnz put0inreg5

   MOV DL,CARRY2
   MOV DH,CARRYCOPY
   CMP DH,DL
   JZ NOEDITING2
   MOV CARRY1,DL
   NOEDITING2:

    CALL sendarray
    INC order
    JMP AGAIN
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
    checkpw4:
    cmp al,35H
    jz label127  
    INC order
    jmp again
    label127:
      cmp level,2
      jz label144
      jmp wrong
      label144:
      cmp power2,0
      jne label143
      INC order
      jmp AGAIN ;;;;;;;used before
      label143:
      read4num 
      mov dh,readregval
      mov dl,readregval+1
      mov cx,20h ;;;;;;;;;;;;;;stop before el memory
      mov si,offset REG2
      put0inreg11:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
      mov bh,[SI]
      mov bl,[si+1]
      cmp bx,dx
      je wrong
      add si,2
      dec cx
      jnz put0inreg11

      mov cx,20h ;;;;;;;;;;;;;;stop before el memory
      mov si,offset REG1
      put0inreg10:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
      mov bh,[SI]
      mov bl,[si+1]
      cmp bx,dx
      je wrong
      add si,2
      dec cx
      jnz put0inreg10

      mov target,dh
      mov target+1,dl

      wrong: 
      CALL sendarray
      INC order
      JMP AGAIN

INSST:

   mov ah,0 
   mov al,13h
   int 10h 

   mov ax,0600h
   mov bh,00
   mov cx,0 
   mov dx,164FH
   int 10h 

   mov dl,0d
   mov dh,0d
   setcursor

   mov ah, 9
   mov dx, offset Print_instruction
   int 21h

   mov ah,0
   int 16h
   cmp al,69h ;;;;;;;;;;enter p small to choose power anything else to call instruction
   jnz label147

   mov ax,0600h
   mov bh,00
   mov cx,0 
   mov dx,164FH
   int 10h 
   CALL graph



   jmp CONTIN

   label147:
   JMP EEEEXIT


l:
    mov var,0
    mov ah,0
    mov al,3
    int 10h
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;left
    ;clearscreen
    mov ax,0600h
    mov bh,07
    mov cx,0
    mov dx,184fh
    int 10h

   ;print word points
    mov ah,2h
    mov dx,0A15H
    mov bh,0
    int 10h

   mov ah,9
   mov dx,offset nameplayer
    int 21h

   mov ah,9
   mov dx,offset sc
    int 21h

   ;print number ofpoints
  ;setcurser at

   mov al,0
   mov ah,Points
   display
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;print word points
;setcurser at 
    mov ah,2h
    mov dx,0c15H
    mov bh,0
    int 10h

   mov ah,9
   mov dx,offset nameplayer2
    int 21h
    ;;;;;;;;;;;;;;;;
   mov ah,9
   mov dx,offset sc
    int 21h

   ;print number ofpoints
   mov al,0
   mov ah,Points2
   display
       ;empty buffer
    mov ah,2ch ;get the system time
    int 21h ;ch=hour cl=minute dh=secod dl=1/100 seconds
    mov bh,dh ;strt

    time:
     mov ah,2ch ;get the system time
    int 21h ;ch=hour cl=minute dh=secod dl=1/100 seconds
    sub dh,bh ;if 5
    cmp dh,5
    jne time
      MOV ORDER,0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;هبللللللللللللللللللللللل
       ;clearscreen
    mov ax,0600h
    mov bh,07
    mov cx,0
    mov dx,184fh
    int 10h
    mov f4,1

    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG1
    put0inreg15:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov byte ptr [si],00h
    inc si
    dec cx
    jnz put0inreg15

    mov cx,20h ;;;;;;;;;;;;;;stop before el memory
    mov si,offset REG2
    put0inreg16:;;;;;;;;;;;;;;;;;PUTALL REGISTER ZERO
    mov byte ptr [si],00h
    inc si
    dec cx
    jnz put0inreg16
;; ONLY FOR LEVEL 2NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNOUUUUUUUUUUURRRR
    
EEEEXIT:
   

   ret
   play ENDP
END 