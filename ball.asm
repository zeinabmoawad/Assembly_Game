PUBLIC BALL
public won
EXTRN COLORIN2: BYTE
EXTRN BALLY: BYTE
EXTRN BALLX: BYTE
EXTRN graph:FAR
.model small
.stack 64

.data
;BALLX Db 0h ;horizontal
;BALLY Db 0H ;vertical
ballsize dw 04h ;size of ball (how many pixels does the ball have in width and height ),4 pixels on x and 4 on y
timeaux db 12h ;variable used when checking if the time has changed

Window_Height dw 19h
Window_Width  dw 28h


;uparrow          equ 48h
;downarrow        equ 50h
;leftarrow        equ 4Bh
;rightarrow       equ 4Dh        
;spacebar         equ 39h
x db 0
y db 0

storex db 0
storey db 0

won db 0

.code

drawball macro coloraia
mov  al,03h
mov  bl, coloraia  ;Color is red
mov  bh, 0    ;Display page
mov  ah, 0Eh  ;Teletype
int  10h 
endm drawball

setcursor macro x1,y1
;set cursor
mov ah,2
mov bh,0
mov dh,x1
mov dl,y1
;mov dx,0A0Ah
int 10h
endm setcursor

drawplayer macro
mov ah,2
mov al,1Eh
mov dl,al
int 21h
endm drawplayer

drawshoot macro 
local yousl
local checky
local looping
local looping2
local looping3
local looping4
local winneraea
local finalexit

yousl:
sub x,1
cmp x,0
jbe checky

;mov dh,00H
;mov dl,x
;mov ah,00h
;mov al,BALLx
;cmp dl,al
;jne kamel
;jmp finalexit
;kamel:
setcursor x,y
mov  al,04h
mov  bl, 0ch  ;Color is red
mov  bh, 0    ;Display page
mov  ah, 0Eh  ;Teletype
int  10h ; display character in graphics mode with 10*10 pixel for single character

;mov ah,2
;mov al,04h
;mov dl,al
;int 21h

mov cx,0ffffh
looping:
dec cx
jnz looping
mov cx,0ffffh
looping2:
dec cx
jnz looping2
mov cx,0ffffh
looping3:
dec cx
jnz looping3
mov cx,0ffffh
looping4:
dec cx
jnz looping4

push ax

mov ah,3
mov bh,0h
int 10h
dec dl

mov ah,2
mov bh,0
int 10h

mov dl,00h
mov ah,2
int 21h

CALL graph

pop ax


jmp yousl

checky:
mov dl,y
mov al,BALLy
cmp dl,al
je winneraea
JMP finalexit
winneraea:
setcursor 0,BALLY
mov dl,00h
mov ah,2
int 21h

mov aL,'A'
mov won,1

looping10:
dec cx
jnz looping10
mov cx,0ffffh
looping90:
dec cx
jnz looping90
;jmp EXITAITI2
;;;;; increase my points and remove the flying obj
    
finalexit:

endm drawshoot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;///////////////////////
movplayer macro
local CHECK1911
local closmy1
local exitaea
local closmy
local myjump
local mykl
local label3
local exit2
local exit
local closing

CHECK1911: 
mov ah,1
int 16h
jz CHECK1911

mov ah,0
int 16h 

cmp aL,'A'
jnz closmy1
mov dh,x
mov dl,y
mov storex,dh
mov storey,dl
jmp exitaea
closmy1:

cmp ah,39h
jz closmy
jmp myjump
closmy:
mov dh,x
mov dl,y
mov storex,dh
mov storey,dl

drawshoot

cmp aL,'A'
jnz mykl

jmp exitaea
mykl:

mov dh,storex
mov dl,storey
mov x,dh
mov y,dl
myjump:

push ax

;mov ah,3
;mov bh,0h
;int 10h
;dec dl

mov ah,2
mov bh,0
mov dh,x
mov dl,y
int 10h

mov dl,00h
mov ah,2
int 21h

pop ax


cmp ah,4Bh ;left arrow
jnz label3 
sub y,1
cmp y,0
ja exit2
add y,1
exit2:
jmp exit

label3: 
cmp ah,4Dh ;; right
jnz label304
add y,1
cmp y,39
JB EXIT33
sub y,1
EXIT33:
jMP exit

label304:
cmp ah,48h ;; right
jnz label305
sub X,1
cmp X,1
ja exit22
add X,1
exit22:
jmp exit

label305:
cmp ah,50h ;; right
jnz EXIT
ADD X,1
cmp X,24
jB exit23
SUB X,1
exit23:
jmp exit

exit:

CALL graph

setcursor x,y
drawplayer

;jz closing
jmp CHECK1911
;closing:

exitaea:
;mov ah,0
;int 16h 
endm movplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;///////////////


BALL proc far
MOV AX,@DATA
MOV DS,AX

mov al,BALLX
mov x,al
mov al,BALLy
mov y,al
CMP Y,0
JNE ROOO7
MOV Y,1
MOV BALLY,1
ROOO7:
setcursor 0,y

;; reanomize the color
drawball COLORIN2

mov x,15D
mov y,11D
setcursor x,y
drawplayer
movplayer

EXITAITI22:

    mov cx,0ffffh
    looping5:
    dec cx
    jnz looping5
    mov cx,0ffffh
    looping6:
    dec cx
    jnz looping6

    setcursor 0,BALLY
    mov dl,00h
    mov ah,2
    int 21h

    setcursor storex,storey
    mov dl,00h
    mov ah,2
    int 21h

ret
BALL endp
end 