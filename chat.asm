;Me writiing @middle
;Recieving top
public chatproc
EXTRN nameplayer:BYTE
EXTRN nameplayer2:BYTE

.MODEL SMALL
.STACK 64
.DATA 
ToExitChat  db 'To end chat press f3','$'
notifcationbarline db 80 dup('-'),10,13,'$'
val db ?
curser1 dw 0C00h
curser2 dw 0100h
endline db 10,13,"$"

.CODE
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

send PROC
       mov dx , 3FDH		; Line Status Register
       In al , dx 			;Read Line Status
  	   AND al , 00100000b
  		JZ AGAIN

;If empty put the VALUE in Transmit data register
  		mov dx , 3F8H		; Transmit data register
  		mov  al,val
  		out dx , al
          AGAIN:
          ret
send ENDP

recive PROC
;Check that Data Ready
		mov dx , 3FDH		; Line Status Register
		in al , dx 
  		AND al , 1
  		JZ exit1

 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		mov val , al
     exit1:
      ret
recive ENDP

setcursor macro par
mov ah,2
mov dx,par
MOV BH,0 
int 10h
endm setcursor

scrollong macro x
;;check if end of work space
  mov bx,curser1
  cmp bh,22d

  jne x
  ;scroll
    mov ax,0601h
    mov bh,07
   mov cx,0C00h
   mov dx,154fh     
   int 10h
   ;;set cuser 21
   mov bx,curser1
   dec bh
   mov bl,0
   mov curser1,bx
endm scrollong

scrollongtop macro y
;;check if end of work space
  mov bx,curser2
  cmp bh,9h

  jne y
  ;scroll
    mov ax,0601h
    mov bh,07
   mov cx,0100h
   mov dx,084fh     
   int 10h
   ;;set cuser 21
   mov bx,curser2
   dec bh
   mov bl,0
   mov curser2,bx
endm scrollongtop

chatproc PROC FAR
    MOV AX,@DATA
    MOV DS,AX

   mov curser1,0C00h
   mov curser2,0100h
    ;clear screen
   mov ax,0600h
   mov bh,07
   mov cx,0
   mov dx,184FH
   int 10h

      ;notifiaction bar of the chat
    mov dl,0d
   mov dh,23d
   setcursor dx

   mov ah,9
    mov dx,offset notifcationbarline
    int 21h

      mov ah,9h
    mov dx,offset ToExitChat
    int 21h

   ;;;setting curser @ center of screen
   mov dx,0A00h
   setcursor DX
   
;Middle line
mov cx,80d
  kol:
mov ah,2
mov dl,'-'
int 21h
dec cx
jnz kol

;;My name
 mov dx,0B00h
   setcursor DX

   mov ah,9
    mov dx,offset nameplayer
   int 21h

   ;;His
 mov dx,0000h
   setcursor DX

   mov ah,9
    mov dx,offset nameplayer2
   int 21h

;;Chating
call intial
  ;read key pressed
  check:
  setcursor curser1
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
  ;if f3
  cmp ah,3dh
  je sendit
  cmp ah,1Ch
  je enter
  jmp label01

  sendit:
  mov val,ah
  call send
  jmp exit
  ;letter is in al
  ;;print chartetr
  ;set curser tto the new postion curser1
  enter :
  mov val,ah
  call send
  mov bx,curser1
  inc bh
  mov bl,00;@begiing of the row
  mov curser1,bx
  scrollong label15
  label15:
  setcursor curser1
  jmp check

  backspace:
  
  ;curser1 posyion previous
  ;dec col
  mov dx,curser1
  ;;special 
  cmp dl,0 ;;begin
  jne label10
  jmp check
  label10:
  dec dl ;x
  push dx
  setcursor dx ;new postion for printing null
    ;print null
    mov dl,00h
   mov ah,2
   int 21h
   pop dx
   ;dec dl
  mov curser1,dx
  setcursor curser1
  mov val,0Eh
  call send
  jmp check

  

  label01:
  push ax
  scrollong label11
  
    

  label11:
  pop ax
  setcursor curser1
  mov ah,2
  mov dl,al
  int 21h
  push DX
  ;;get the curesr postion
  mov ah,3h
mov bh,0h
int 10h
mov curser1,Dx

  ;;send the letter to the other
  pop dx
  mov val,dl
  call send
  jmp check

  ;recive
  reciving:
  call recive
  jnz label5
  jmp check ;zf=1 didn't recive
  label5:
  ;;zf =0 he recived sth ==>val
  cmp val,0Eh;backspace
  jne label7
  jmp backspace4
  label7:
  ;if waht i recive is f3
  cmp val,3dh;f3
  jne label14
  jmp exit
  label14:
  cmp val,1Ch;enetr
  jne label2
  ;enetr
    mov bx,curser2
  inc bh
  mov bl,00;@begiing of the row
  mov curser2,bx
  scrollongtop label16  ;;;;;;;;;;;;;;rto be checked
  label16:
  setcursor curser2
  jmp check
  ;setting curser
  label2:
  push ax
  scrollongtop z
  z:
  pop ax
  setcursor curser2
  ;;print the val
  mov ah,2
  mov dl,val
  int 21h
    ;;get the curesr postion
  mov ah,3h
mov bh,0h
int 10h
mov curser2,Dx
jmp check

 backspace4:
 
  ;curser1 posyion previous
  ;dec col
  mov dx,curser2
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
  mov curser2,dx
  setcursor curser2
  jmp check

exit:
 ret    
  chatproc ENDP
  END 