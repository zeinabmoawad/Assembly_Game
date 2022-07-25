.model small
.stack 64
.data
uparrow          equ 48h
downarrow        equ 50h
leftarrow        equ 4Bh
rightarrow       equ 4Dh        
spacebar         equ 39h
x db 0
y db 0

.code
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

movplayer macro
CHECK: mov ah,1
int 16h

cmp ah,4Bh ;left arrow
jnz label3 
add x,10
jmp exit
label3: 
cmp ah,4Dh ;; right
jnz exit
sub x,10

exit:
setcursor x,y
drawplayer

;mov cx,10
;myloop:
;dec cx
;jnz myloop

mov ah,0
int 16h 
jnz CHECK
endm movplayer


main proc far

mov ah,0
mov al,13h
int 10h

mov x,0Ah
mov y,0Ah
setcursor x,y
drawplayer
movplayer








main endp
end main