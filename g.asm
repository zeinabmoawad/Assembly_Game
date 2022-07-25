public graph
public guncol
public gunrow
EXTRN REG1:BYTE
EXTRN REG2:BYTE
EXTRN nameplayer:BYTE
EXTRN nameplayer2:BYTE
EXTRN level:BYTE
EXTRN Forbid1:BYTE
EXTRN Forbid2:BYTE
EXTRN points:BYTE
EXTRN points2:BYTE
EXTRN power1:BYTE
EXTRN NUM1:BYTE
EXTRN NUM2:BYTE
EXTRN NUM3:BYTE
EXTRN NUM4:BYTE
EXTRN NUM5:BYTE

.MODEL SMALL
.STACK 64
.DATA 
guncol db 2d
gunrow db 132d
ASC_TBL DB   '0','1','2','3','4','5','6','7','8','9'
        DB   'A','B','C','D','E','F'
x DW ? ;;;;;;;;;;;;;;;;;;;x1
y DW ? ;;;;;;;;;;;;;;;;;;;;y1
w DW ? ;;;;;;;;;;;;;;;;;;;;x2
z DW ? ;;;;;;;;;;;;;;;;;;;;y2
color Db ? ;;;;;;;;;;;;;;;;;;;;y2
pointdis db 'points','$'
powerdis db 'power','$'
end1 DW ? ;end coordinate of line
endline db 10,13,"$"
notifcationbarline db 40 dup('-'),'$'
chat1 db 'chat1 line','$'
chat2 db 'chat2 line','$'
forbid db 'Forbid','$'
youchoose db 'Game Level: ','$'
.CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

drawlinevertical macro x1,y1,xend,clr
local back
 mov cx,y1 ;Column
    mov dx,x1 ;Row
    mov al,clr ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back: int 10h
    inc dx
    cmp dx,xend
    jnz back
endm drawlinevertical
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

drawlinehorizontal macro x2,y2,yend,clr
local back
 mov cx,y2 ;Column
    mov dx,x2 ;Row
    mov al,clr ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back: int 10h
    inc cx
    cmp cx,yend
    jnz back
endm drawlinehorizontal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printmemorynumbers macro letter
mov ah,2
mov dl,letter
int 21h
endm  printmemorynumbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
setcursor macro 
mov ah,2
MOV BH,0 
int 10h
endm setcursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printrectangle macro x1,x2,y1,y2,clr
 local back
 local back1
 local back2
 local back3
 ;print upper horizontal line y2,y1,x1
   mov cx,y1 ;Column
    mov dx,x1 ;Row
    mov al,clr ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back: int 10h
    inc cx
    cmp cx,y2
    jnz back
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;print lower  horizontal line y2,y1,x2
 mov cx,y1 ;Column x2,y1,y2
     mov dx,x2 ;Row
    mov al,clr ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back1: int 10h
    inc cx
    cmp cx,y2
    jnz back1
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;print left vertical line x1,x2,y1
   mov cx,y1 ;Column
    mov dx,x1 ;Row
    mov al,clr ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back2: int 10h
    inc dx
    cmp dx,x2
    jnz back2
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;print left vertical line x1,x2,y2
   mov cx,y2 ;Column
    mov dx,x1 ;Row
    mov al,clr ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back3: int 10h
    inc dx
    cmp dx,x2
    jnz back3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
endm printrectangle

displaynum macro  Level 
    mov dl,Level
    add dl,30h
    mov ah,2h
    INT 21H
endm displaynum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display number with character
displaycolor macro color 

    add dl,30h;charcter


   

   mov ah, 0eh           ;0eh = 14
   mov al, dl
   xor bx, bx            ;Page number zero
   mov bl, color          ;Color is red
   int 10h


endm displaycolor


graph PROC FAR
    MOV AX,@DATA
    MOV DS,AX
    ;;;;;;;;;;;;;;;;;;;
 ;;half the screen
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    mov x,0
    mov y,160
    mov end1,130
    mov color,0Bh
    drawlinevertical x,y,end1,color
     
     ;set cursor to print memory name
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;right side;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;print MEMORY NAME
mov cl,0
mov ch,0 ;;PPPPPPPPPPPPPPتعدييييييييييييييييييييييييل
mov dh,0
loop1:
   mov dl,36d
   setcursor
   mov al,cl
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
   inc cl
   inc dh
   cmp cl,16d
   jnz loop1
   ;;;;;;;;;;;;;;;;;;;;print secondline at right;
    mov x,0
    mov y,300
    mov end1,130
     mov color,0Bh
    drawlinevertical x,y,end1,color

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;print registers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;first four  register rectangles right left side
    mov y,190
    mov z,225
    mov x,5
    mov w,17
    mov color,0Bh
    demi: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dimensions increment
    printrectangle x,w,y,z,color
    add x,16
    add w,16
    cmp x,133
    jnz demi
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;second four  register rectangles right right side
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;si
    mov dl,21d
   mov dh,1d
    setcursor
   mov al,53h
   printmemorynumbers al
    mov dl,22d
   mov dh,1d
    setcursor
   mov al,49h
   printmemorynumbers al

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;di
    mov dl,21d
   mov dh,3d
    setcursor
   mov al,44h
   printmemorynumbers al
    mov dl,22d
   mov dh,3d
    setcursor
   mov al,49h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;sp
    mov dl,21d
   mov dh,5d
    setcursor
   mov al,53h
   printmemorynumbers al
    mov dl,22d
   mov dh,5d
    setcursor
   mov al,50h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bp
    mov dl,21d
   mov dh,7d
    setcursor
   mov al,42h
   printmemorynumbers al
    mov dl,22d
   mov dh,7d
    setcursor
   mov al,50h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ax
    mov dl,21d
   mov dh,9d
    setcursor
   mov al,41h
   printmemorynumbers al
    mov dl,22d
   mov dh,9d
    setcursor
   mov al,58h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;bx
   mov dl,21d
   mov dh,11d
    setcursor
   mov al,42h
   printmemorynumbers al
    mov dl,22d
   mov dh,11d
    setcursor
   mov al,58h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;cx
    mov dl,21d
   mov dh,13d
    setcursor
   mov al,43h
   printmemorynumbers al
    mov dl,22d
   mov dh,13d
    setcursor
   mov al,58h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dx
    mov dl,21d
   mov dh,15d
    setcursor
   mov al,44h
   printmemorynumbers al
    mov dl,22d
   mov dh,15d
    setcursor
   mov al,58h
   printmemorynumbers al
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Print Reg2 Right;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;setting curser for each reg
    mov dl,24d
   mov dh,1d
   mov di,0
    settingcurser2:
    push DX
  
  
    setcursor ;12
     
    loops2:
    mov ah,0
   mov al,REG2+[di]
  mov cl,4
   shr al,cl;1
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
   ;second didigt
  mov al,REG2[di]
   mov cl,4
   shl al,cl;2
   shr al,cl
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al

 inc di 
 push ax
      mov ax,di 
      mov dl,2
      div dl
   cmp ah,0
   pop ax
   jne loops2

        pop DX
   inc dh
   inc dh
   cmp dh,17d
   jne settingcurser2

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Print Memory 2 RIGHT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;setting curser for each reg
    mov dl,38d
   mov dh,0d
   mov di,16d
    settingcurser4:
    push DX
  
  
    setcursor ;12
     
   
    mov ah,0
   mov al,REG2[di]
  mov cl,4
   shr al,cl;1
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
   ;second didigt
  mov al,REG2[di]
   mov cl,4
   shl al,cl;2
   shr al,cl
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al

  inc di 


   pop DX
   inc dh
   cmp di,32d
   jne settingcurser4

   ;print name
   mov dl,19d
   mov dh,19d
   setcursor
   

    mov ah,9
    mov dx,offset nameplayer2
    int 21h


    ;print word Points   
    mov dl,29d
    mov dh,1d
    setcursor

    mov ah,9
    mov dx,offset pointdis
    int 21h

    ;Print number of points
   mov dl,31d
   mov dh,3d
   setcursor

   mov al,0
   mov ah,Points2
   display
;;;;;;;;;;

cmp level,2
je level2right
   mov dl,29d
   mov dh,10d
   setcursor

   mov ah,9
   mov dx,offset forbid
   int 21h
   ;;;;;
   mov dl,31d
   mov dh,12d
   setcursor

   mov ah,2
   mov dl,Forbid1
   int 21h
level2right:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;left side;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;print MEMORY NAME
    mov cl,0
    mov ch,0
   mov dh,0
loop2:
   mov dl,16d
   setcursor
   mov al,cl
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
   inc cl
   inc dh
   cmp cl,16d
   jnz loop2
   ;;;;;;;;;;;;;;;;;;;;print secondline at left;
    mov x,0
    mov y,140
    mov end1,130
     mov color,0Bh
    drawlinevertical x,y,end1,color

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;print registers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;first four  register rectangles right left side
    mov y,20
    mov z,57
    mov x,5
    mov w,17
    mov color,0Bh
    demi1: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dimensions increment
    printrectangle x,w,y,z,color
    add x,16
    add w,16
    cmp x,133
    jnz demi1
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;si
    mov dl,0d
   mov dh,1d
    setcursor
   mov al,53h
   printmemorynumbers al
    mov dl,1d
   mov dh,1d
    setcursor
   mov al,49h
   printmemorynumbers al

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;di
    mov dl,0d
   mov dh,3d
    setcursor
   mov al,44h
   printmemorynumbers al
    mov dl,1d
   mov dh,3d
    setcursor
   mov al,49h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;sp
    mov dl,0d
   mov dh,5d
    setcursor
   mov al,53h
   printmemorynumbers al
    mov dl,1d
   mov dh,5d
    setcursor
   mov al,50h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bp
    mov dl,0d
   mov dh,7d
    setcursor
   mov al,42h
   printmemorynumbers al
    mov dl,1d
   mov dh,7d
    setcursor
   mov al,50h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ax
    mov dl,0d
   mov dh,9d
    setcursor
   mov al,41h
   printmemorynumbers al
    mov dl,1d
   mov dh,9d
    setcursor
   mov al,58h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;bx
   mov dl,0d
   mov dh,11d
    setcursor
   mov al,42h
   printmemorynumbers al
    mov dl,1d
   mov dh,11d
    setcursor
   mov al,58h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;cx
    mov dl,0d
   mov dh,13d
    setcursor
   mov al,43h
   printmemorynumbers al
    mov dl,1d
   mov dh,13d
    setcursor
   mov al,58h
   printmemorynumbers al
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dx
    mov dl,0d
   mov dh,15d
    setcursor
   mov al,44h
   printmemorynumbers al
    mov dl,1d
   mov dh,15d
    setcursor
   mov al,58h
   printmemorynumbers al

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DRAWING REG VALUES;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Reg1 left;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;setting curser for each reg
    mov dl,3d
   mov dh,1d
   mov di,0
    settingcurser:
    push DX
  
  
    setcursor ;12
     
    loops1:
    mov ah,0
   mov al,REG1+[di]
  mov cl,4
   shr al,cl;1
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
   ;second didigt
  mov al,REG1[di]
   mov cl,4
   shl al,cl;2
   shr al,cl
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al

 inc di 
 push ax
      mov ax,di 
      mov dl,2
      div dl
   cmp ah,0
   pop ax
   jne loops1

        pop DX
   inc dh
   inc dh
   cmp dh,17d
   jne settingcurser

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Print Memory 1 LEFT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;setting curser for each reg
    mov dl,18d
   mov dh,0d
   mov di,16d
    settingcurser3:
    push DX
    setcursor ;12
    mov ah,0
   mov al,REG1[di]
  mov cl,4
   shr al,cl;1
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
   ;second didigt
  mov al,REG1[di]
   mov cl,4
   shl al,cl;2
   shr al,cl
   mov BX,offset ASC_TBL
   XLAT
   printmemorynumbers al
  inc di 
   pop DX
   inc dh
   cmp di,32d
   jne settingcurser3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;instruction
mov x,130
mov y,0
mov z,320
mov color,0Bh
drawlinehorizontal x,y,z,color
;mov dl,0
;mov dh,17d
;setcursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mov x,145
mov y,0
mov z,320
 mov color,0Bh
drawlinehorizontal x,y,z,color

;;print my  name
    mov dl,0d
    mov dh,19d
   setcursor

   mov ah,9
   mov dx,offset nameplayer
    int 21h

   ;print word points
   mov dl,9d
   mov dh,1d
   setcursor

   mov ah,9
    mov dx,offset pointdis
    int 21h

   ;print number ofpoints
   mov dl,11d
   mov dh,3d
   setcursor

   mov al,0
   mov ah,Points
   display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;need string

   mov dl,9d
   mov dh,6d
   setcursor

   mov ah,9
    mov dx,offset powerdis
    int 21h


   ;print Power 0 or 1
   mov dl,11d
   mov dh,8d
   setcursor

   mov al,0
   mov ah,power1
   display

;;;;;;;;;;;;forbidden for player2
   cmp level,2
   je level2left
   mov dl,9d
   mov dh,10d
   setcursor

   mov ah,9
   mov dx,offset forbid
   int 21h
   ;;;;;
   mov dl,11d
   mov dh,12d
   setcursor

   mov ah,2
   mov dl,Forbid2
   int 21h
;;;;;;;;;;;;;
level2left:
    ;chat1
    mov dl,0d
   mov dh,21d
   setcursor

   mov ah,9
    mov dx,offset nameplayer
    int 21h
 
  ;chat2
    mov dl,0d
   mov dh,22d
   setcursor

   mov ah,9
    mov dx,offset nameplayer2
    int 21h
 

   ;notifiaction bar of the game
    mov dl,0d
   mov dh,23d
   setcursor

   mov ah,9
    mov dx,offset notifcationbarline
    int 21h

    ;draw gun
    ;mov ah,2
   ;mov dl,0
   ;mov dh,17d
   ;int 10h

    ;mov al,guncol
    ;mov ah,0
    ;mov y,ax
    ;mov z,ax
    ;add z,18
    ;mov al,gunrow
    ;mov x,ax
    ;mov w,ax
    ;add w,10
    ; mov color,0Bh
 
   ;printrectangle x,w,y,z,color



   

   mov dl,0d
   mov dh,24d
   setcursor
    ;;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset youchoose
    int 21h
    ;;;;;;;;;;;;print levelnum
    ;mov ah,level
    displaynum Level 


    ;;;for nour and zeinab

  mov dl,38d
  mov dh,19d
  setcursor 

;mov al,0
mov dl,num1
displaycolor 0AH  ;;DRAW GREEN COLOR

  
  mov dl,36d
  mov dh,19d
  setcursor 

 ;mov al,0
mov dl,num2
displaycolor 9 ;;;Draw Blue  


 mov dl,34d
  mov dh,19d
  setcursor 

 ;mov al,0
mov dl,num3
displaycolor 04h ;;;Draw YELLOW  

 mov dl,32d
  mov dh,19d
  setcursor 

 ;mov al,0
mov dl,num4
displaycolor 0EH ;;;Draw RED 

 mov dl,30d
  mov dh,19d
  setcursor 

 ;mov al,0
mov dl,num5
displaycolor 0dh ;;;Draw DARK BLUE 
   
    


    
 

    

 ret    
  graph ENDP
  END 