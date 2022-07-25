public namepro
public nameplayer
public points
.MODEL SMALL
.STACK 64
.DATA
nameplayer db 16 dup('$')
points db ?
welcome db'Welcome to Game','$'
str1 db 'Enter Your name: ','$'
str2 db 'Enter Initial Points: ','$'
digit1 db ?
digit2 db ?
rule db 'Press Enter To Continue','$'
rulename db'UserName:Enter only letters no sprcial chracters or spaces max 15 letters','$'
rulenuber db'Intial Points: Enter only number max number 99','$'
.CODE
changetwonumbers proc  
    mov al,digit1                                                     
    sub al,30h 
    mov bl,al 

    mov al,digit2
    sub al,30h 
    mov bh,al

    mov al,bl
    mov bl,10
    mul bl
    add al,bh

    mov points,al
    ret
changetwonumbers ENDP 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
namepro PROC FAR
    MOV AX,@DATA
    MOV DS,AX
    push ax
    push bx
    push cx
    push dx
    ;clear
    mov ax,0600h
    mov bh,07
    mov cx,0
    mov dx,184fh
    int 10h

    ;welcome
    ;setcurser at 
    mov ah,2h
    mov dx,0120H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset welcome
    int 21h
 

    ;;;;;;;;;;;;;;bannerEnter
    ;setcurser at 
    mov ah,2h
    mov dx,1600H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset rule
    int 21h
 

;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;banner Rule of name
    ;setcurser at 
    mov ah,2h
    mov dx,1700H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset rulename
    int 21h


    ;;;Banner rule of points
    ;setcurser at 
    mov ah,2h
    mov dx,1800H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset rulenuber
    int 21h

    
;set curser
    mov ah,2h
    mov dx,0A15H
    mov bh,0
    int 10h
    ;;;;;;;;;;;;
    mov ah,9
    mov dx,offset str1
    int 21h
 
   mov si,0
   readthename:

   mov ah,0
   int 16h

   cmp ah,1Ch;enter
   je nameDone

   cmp ah,0Eh ;backspace
   je backspace

   cmp al,61h ;letter a
   jae checkinsmall
   jmp readthename

   checkinsmall: 
   cmp al,7AH
   ja readthename

  
   mov nameplayer[si],al
   inc si
   ;print the char
   mov ah,2
   mov dl,al
   int 21h
   cmp si,15
   je nameDone
   jmp readthename;still



   backspace:
   ;;setcurser one before
   mov ah,3
   mov bh,0h
   int 10h
   ;dl col
   cmp si,0;check if this is the firstcharacter
   je readthename
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
   
   
   ;set curser to the new postion
   pop dx
   mov ah,2
   mov bh,0
   int 10h
   mov al,'$'
   dec si
   mov nameplayer[si],al
 
   jmp readthename

   ;;;;;;We got the name
   nameDone:
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Initial Points;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;set curser
    mov ah,2h
    mov dx,0C15H
    mov bh,0
    int 10h
    ;;;;;;;;;;;;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset str2
    int 21h
    ;;;;;;;;;;;;;;;;;intiak point
    readpoint:
      mov ah,0
      int 16h
      
   cmp al,30h ;1
   jae checkinno
   jmp readpoint

   checkinno: 
   cmp al,39H
   ja readpoint ;stiil notright

   ;else right ;firstdigit
   ;print the char
   mov ah,2
   mov dl,al
   int 21h

   mov digit1,al

   ;second digit
    readpoint2:
      mov ah,0
      int 16h   
    
   cmp ah,1CH;enetr ==>one digit number
   je onedigit

   cmp ah,0Eh ;backspace
   je backspace2
   
      
   cmp al,30h ;1
   jae checkinno2
   jmp readpoint2

   checkinno2: 
   cmp al,39H
   ja readpoint2 ;stiil notright

   ;else right ;firstdigit
   ;print the char
   mov ah,2
   mov dl,al
   int 21h

   mov digit2,al
   ;edit the 2 digits 
   call changetwonumbers
   jmp pointsDone

   backspace2:
   ;;setcurser one before
   mov ah,3
   mov bh,0h
   int 10h
   ;dl col
   dec dl
   
   ;set curser to the new postion
   mov ah,2
   mov bh,0
   int 10h
   jmp readpoint
   
   
   onedigit:  
    mov al,digit1                                                     
    sub al,30h  
    mov points,al

   pointsDone:
   pop dx
   pop cx
   pop bx
   pop ax
   ret 
   namepro ENDP
   END 