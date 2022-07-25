EXTRN namepro:FAR
EXTRN play:FAR
EXTRN chatproc:FAR
EXTRN order:byte
EXTRN nameplayer:BYTE
EXTRN nameplayer2:BYTE
EXTRN points:BYTE
EXTRN points2:BYTE
EXTRN WINNER:BYTE
public f4
.MODEL SMALL
.STACK 64
.DATA
invitation db ?
val db ?
exitstr db 'END Of The Game','$'
WINNING db 'YOU ARE THE WINNER','$'
LOSING db 'YOU LOST','$'
chat db 'TO Start chat Press F1','$'
game  db 'TO Start Game Press F2','$'
escp db 'TO Exit Press Esc','$'
requestnotification db ' sent you game invitation to accept press F2','$'
requestnotification2 db ' sent you chat invitation to accept press F1','$'
waitnotification db'Waiting for ','$'
waitnotification2 db' to accept your inviataion','$'
variable db ?
f4 db 0h 
.CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;description
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
;;;;;;;
recive2 PROC
;Check that Data Ready
		mov dx , 3FDH		; Line Status Register
	CHK2:	in al , dx 
  		AND al , 1
  		JZ CHK2

 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		mov val , al
      ret
recive2 ENDP
;;;;;
recivename PROC
  mov di,0
   loopove:
   call  recive2
    mov dl,val
    mov nameplayer2[di],dl
     add di,1
     cmp di,17
     jne loopove
      ret
recivename ENDP
;;;;;;;;;;;;;;;;
send2 PROC
    mov dx , 3FDH		; Line Status Register
AGAIN2:  	In al , dx 			;Read Line Status
  		AND al , 00100000b
  		JZ AGAIN2

;If empty put the VALUE in Transmit data register
  		mov dx , 3F8H		; Transmit data register
  		mov  al,val
  		out dx , al
          ret
send2 ENDP
;;;;;;;;;;;;;
;send name
sendname PROC
   ; mov val,3cH
    ;call send

  mov di,0
   loopovername:
   mov dl,nameplayer[di]
   mov val,dl
  call send2
     add di,1
     cmp di,17
     jne loopovername
          ret
sendname ENDP
;1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MAIN PROC FAR
    MOV AX,@DATA
    MOV DS,AX
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;name and intial points
    call namepro
    mainmenu:
    mov f4,0h
    ;clear screen
    mov ax,0600h
    mov bh,07
    mov cx,0
    mov dx,184fh
    int 10h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;to print f1
    ;setcurser at 
    mov ah,2h
    mov dx,0A15H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset chat
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;to print f2
    ;setcurser at 
    mov ah,2h
    mov dx,0C15H
    mov bh,0
    int 10h

    mov ah,9
    mov dx,offset game
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;to print escape
    mov ah,2h
    mov dx,0E15H
    mov bh,0
    int 10h

    mov ah,9h
    mov dx,offset escp
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  call intial
  ;read key pressed
  check:
  mov ah,1
  int 16h
  jz reciving ;didn't recving
  ;send
  ;make buffer empty
  mov ah,0
  int 16h
  push ax
  cmp ah,3bh
  je sendinvitation
  cmp ah,3ch ;F2 scan code
  je sendinvitation
  ;;escape
  cmp ah,01h
  jne label09
  jmp exit
  label09:
  ;recive
  reciving:
  call recive
  jz check ;zf=1 didn't recive

  ;check press f2 to accept invitation
  mov ah,val
  mov variable ,ah
  cmp val,3ch
  je acceptinvitation
  cmp val,3bh
  je acceptinvitation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;need cehck
;;;;;;;;;;;;;;;;;;;;;NEW EMAN &BASMA
;send
sendinvitation:
pop ax
 mov val,ah
 call send
 call sendname
 call recivename ;recive name of the one who i sent to him the invetation
 ;setcurser at 
    mov ah,2h
    mov dx,1700H
    mov bh,0
    int 10h
;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset waitnotification
    int 21h
    ;;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset nameplayer2
    int 21h
    ;;;;;;;;;;;;;;
 mov ah,9
    mov dx,offset waitnotification2
   int 21h
    ;;;;;;;;;;;;;;;;;
waituntillacceptmyinvitation:
call recive
jz waituntillacceptmyinvitation;recive his name
cmp val,3ch
jne label009
jmp startgameforfristplayer
label009:
cmp val,3bh ;f1
jmp startchat;;;;;hande if the recievd wasn't f1 or f2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
acceptinvitation:
;;;;;;;;;recive sender name
call recivename
call sendname ;;send my name to the invitor
;;;;;;;;;;;invit bar
;setcurser at 
    mov ah,2h
    mov dx,1700H
    mov bh,0
    int 10h
;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset nameplayer2
    int 21h
    ;;;;;;;;;;;;;;
    cmp variable,3ch 
    jne labelchat
    mov dx,offset requestnotification
    jmp interrupt
    labelchat:
    mov dx,offset requestnotification2
    interrupt:
     mov ah,9
     int 21h
;wait key press to start game or exit 
againf2:
mov ah,0
int 16h
cmp ah,variable
jne againf2
cmp ah,3ch ;f2 scan code ==accept
jne mayf1
jmp sendresponse
mayf1:
cmp ah,3bh ;f1 scan code ==accept
jne againf2
jmp sendresponsechat
;jmp againf2

acceptchat:
;;;;;;;;;recive sender name
call recivename
call sendname ;;send my name to the invitor
;;;;;;;;;;;invit bar
;setcurser at 
    mov ah,2h
    mov dx,1700H
    mov bh,0
    int 10h
;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset nameplayer2
    int 21h
    ;;;;;;;;;;;;;;
    mov ah,9
    mov dx,offset requestnotification2
    int 21h
;wait key press to chat 
againf1:
mov ah,0
int 16h
cmp ah,3bh ;f1 scan code ==accept
je sendresponsechat
jmp againf1

;accept chat inivation 
sendresponsechat:
mov val ,3bh
call send
jmp startchat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;accept invitation
sendresponse:
mov val ,3ch
call send
;;send my points to the host 
mov al,points
mov val,al
call send2;sent my points to the host

call recive2 ;waiting for the points
mov al,val
mov points,al
mov points2,al
call play
jmp esacpe

startgameforfristplayer:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;test;;;;;;;;;;
;recive points from the oppeneir
;recieve point of the player2
call recive2
mov al,val
cmp al,points
jb points2less;points2<points
mov al,points;points2>points
mov points2,al;modify his points
jmp se
;opnenetisgreater:mov al,points;i have less (host has less)
;sending respond
points2less:
mov points,al
jmp se
;mov points2,al ;;setting points and points2 with al(min)
;send the less point to the player2
se:
mov val,al
call send2
mov order,1 
call play
jmp esacpe

;;;;;;;;;;;;;;;;;;start chat 
startchat:
call chatproc
jmp mainmenu

;;;want to start game
esacpe:

    cmp f4,1
    jne l12
    jmp mainmenu
    l12:  
    mov ax,0600h
    mov bh,00
    mov cx,0
    mov dx,184fh
    int 10h

    ;;;;;;;;;;;;;;bannerEnter
    ;setcurser at 
    mov ah,2h
    mov dx,0A0AH
    mov bh,0
    int 10h

    CMP WINNER,1
    JZ LABEL136
    mov ah,9
    mov dx,offset LOSING
    int 21h
    JMP EXITAEA

    LABEL136:
    mov ah,9
    mov dx,offset WINNING
    int 21h
    JMP EXITAEA

exit:
    mov ax,0600h
   mov bh,00
   mov cx,0 
   mov dx,184FH
   int 10h 
EXITAEA:
ret 
   MAIN ENDP
END MAIN