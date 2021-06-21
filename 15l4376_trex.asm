[org 0x0100]
jmp start
oldisr: dd 0
oldisr1: dd 0
num :dw '-'
message:db 'SCORE'
num1: dw 0
num2:dw 0
counter1: dw 0
counter: dw 0

		
clrscr:
push cs
pop ds					
pusha


mov ax,0xb800


mov es,ax


mov di,0



nextchar:
mov word[es:di],0x0720


add di,2


cmp di,4000


jne nextchar


popa


ret




starick2:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,13
mov al,80
mul bl
shl ax,1
mov di,ax
mov al,'*'
mov ah,0x07
add di,2
mov word[es:di],ax
popa
pop bp
ret

starick1:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,14
mov al,80
mul bl
shl ax,1
mov di,ax
mov al,'|'
mov ah,0x07
add di,2
mov word[es:di],ax
popa
pop bp
ret

starick3:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,15
mov al,80
mul bl
shl ax,1
mov di,ax
mov al,'\'
mov ah,0x07
add di,4
mov word[es:di],ax
popa
pop bp
ret

starick4:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,15
mov al,80
mul bl
shl ax,1
mov di,ax
mov al,'/'
mov ah,0x07

mov word[es:di],ax
popa
pop bp
ret

starick:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,16
mov al,80
mul bl
shl ax,1
mov di,ax
mov al,[bp+8]
mov ah,0x07
mov cx,80
l1:mov word[es:di],ax
add di,2
loop l1
mov cx,80
popa
pop bp
ret 2


printstring:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,5
mov al,80
mul bl
add ax,50
shl ax,1
mov di,ax
mov ah,0x07
mov si,[bp+6]
mov cx,5
cld
l2:
lodsb
stosw
loop l2
exit:popa
pop bp
ret 4

pillar1:
push cs
pop ds
push bp
mov bp,sp
mov ax, cs
mov ds, ax
add word [counter], 1
cmp word [counter], 10
je skip9
jmp finish
skip9:
mov ax,0xb800
mov es,ax




q9:mov bx,15
mov al,80
mul bl
add ax,79
shl ax,1
cmp di, 2400
jne skip5
mov di,ax
skip5:
mov al,0xDB
mov ah,0x01

mov bx,15
mov al,80
mul bl
add ax,60
shl ax,1
cmp si, 2400
jne skip6
mov si,ax
skip6:
mov al,0xDB
mov ah,0x07

mov bl,0xDB
mov bh,0x07
q10:mov word[es:di],ax
mov word[es:si],ax
t:
inc word[cs:num2]
cmp word[cs:num2],3000
jne t
;push 0xb800
;pop es
cmp word [es:2402], 0x07db
jne skip21
mov word[es:0],0x0741
cmp word [es:2400], 0x072F
jne skip21
jmp start1
skip21:

mov word[es:di],0x0720
mov word[es:si],0x0720
sub si,2
sub di,2
call timer

mov word[counter],0
finish:

;jmp start1
add word [counter1], 1
cmp word [counter1], 120
jne skip11
push 10
call whitespace
pop bp
push 13
call appear
pop bp
mov word [counter1], 0
skip11:
;popa
pop bp
ret
jmp far [cs:oldisr1]
mov al, 0x20
out 0x20, al
iret




printnum:
push cs
pop ds
push bp
mov bp,sp
pusha
mov ax,0xb800
mov es,ax
mov bx,5
mov al,80
mul bl
add ax,56
shl ax,1
mov di,ax
mov ax,[bp+4]
mov bx,10
mov cx,0
l:
mov dx,0
div bx
add dl,0x30
push dx
inc cx
cmp ax,0
jnz l
np:
pop dx
mov dh,0x07
mov [es:di],dx
add di,2
loop np

popa
pop bp
ret 2 

timer:
push cs
pop ds
push ax
mov ax, cs
mov ds, ax
inc word[cs:num1]
push word[cs:num1]


call printnum

pop ax
ret



		
whitespace:
push cs
pop ds	
push bp
mov bp, sp 
sub sp, 2

pusha
mov cx, 3
mov dx, 0x0720
mov bx, [bp+4]
add bx, 2
mov [bp-2], bx
mov bx, [bp+4]

whitespace_loop:	
mov al, 80
mul bl
cmp bx, [bp-2]
je whitespace_legs
whitespace_other:
add ax, 1
shl ax, 1
mov di, ax
mov word[es:di],dx
inc bx
jmp next
													
whitespace_legs:
shl ax, 1
mov di, ax
mov word[es:di],dx
add di, 4
mov word[es:di],dx
													
next:	
loop whitespace_loop
popa
mov sp, bp
pop bp
ret
			
appear:
push cs
pop ds	
push bp
mov bp, sp
pusha
mov bx, [bp+4]
		
mov al, 80
mul bl
add ax, 1
shl ax, 1
mov di, ax
mov al,'*'
mov ah, 0x07			
mov word[es:di], ax
inc bx
		
mov al, 80
mul bl
add ax, 1
shl ax, 1
mov di, ax
mov al,'|'
mov ah, 0x07			
mov word[es:di], ax
inc bx
												
mov al, 80
mul bl
shl ax, 1
mov di, ax
mov al,'/'
mov ah, 0x07			
mov word[es:di], ax
add di, 4
mov al, '\'
mov word[es:di], ax
											
popa
pop bp 
ret
		
jump:
push cs
pop ds
push bp 
mov bp, sp
pusha
mov ax, 0xb800
mov es,ax
mov bx, 13
jump_loop:
push bx
call whitespace
pop bx
sub bx, 3
push bx
call appear
pop bx
push bx
pop bx
add bx, 3
push bx
pop bx
popa
pop bp
ret




kbisr:
push cs
pop ds
push ax
mov ax, cs
mov ds, ax
in al, 0x60 ; read a char from keyboard port
cmp al, 0x48 ; is the up key button
jne nextcmp ; no, try next comparison
call jump
jmp nomatch ; leave interrupt routine
		
nextcmp:
cmp al, 0x39 ; is the space key
jne nomatch ; no, leave interrupt routine
call jump 
		
nomatch:
pop ax
jmp far [cs:oldisr]
mov al, 0x20
out 0x20, al ; send EOI to PIC
pop ax
iret


start:
call clrscr
push word[num]
mov ax,message
push ax
push word[num1]
call printstring
call starick
call starick2
call starick1
call starick3
call starick4

xor ax,ax
mov es,ax
mov ax, [es:9*4]
mov  [oldisr], ax
mov ax, [es:9*4+2]
mov  [oldisr+2], ax

mov ax, [es:8*4]
mov  [oldisr1], ax
mov ax, [es:8*4+2]
mov  [oldisr1+2], ax

mov si, 2520
mov di, 2558

cli
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2
sti


l20:
call pillar1
jmp l20

cli
mov word[es:8*4],pillar1
mov [es:8*4+2],cs
sti
start1:
mov dx,start
add dx,15
mov cl,4
shr dx,cl
mov ax,0x3100
int 0x21