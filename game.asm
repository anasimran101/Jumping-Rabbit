 [org 0x0100]
jmp start
; --- Easy Navigation ---
;
; Move cursor to the required key word and enter ctrl+f and then enter key(one or more times)
;     (this is only for those editors that have shortcut for find option)
;
;   ->  Global_Variables
;   ->  delay 
;   ->	newMoveBrickForward
;   ->	newMoveBrickBackward
;   ->	moveTheBrick
;   ->  moveMountainsBackward 
;   ->  moveCarsForward 
;   ->  interactiveAnimation 
;   ->  printRabit 
;   ->	brick
;   ->	makeBricks
;   ->  chirri 
;   ->  makeBirds 
;   ->  makeOneCar
;   ->  makeCars
;   ->  makeGrassBackground
;   ->  whiteTree 
;   ->  makeMountain 
;   ->  makeMountainRange 
;   ->  makeRoadLane 
;   ->  makeForegroundFloor 
;   ->  makeRoad 
;   ->  makeBackgroundSky 
;   ->  clearScreen 
;   ->	printStr
;   ->	homeScreen
;   ->  printHomeScreen 
;   ->  printPlayingScreen 
;   ->  printAllScreens 
;   ->  Main
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------Global_Variables---------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
message1: db'-----------------------'
message2: db  'Welcome to our game'
length1: dw 23
length2: dw 19
movingBrickCurrLine: dw 10560,8976,7392		;si=6 rabit pos
RabitCurrLine: dw 10296
movingBrickCurrPos: dw 10692,9094,7512           ;si=6 rabit pos
RabitPos: dw 10440		
movingBrickCurrSpeed: dw 0,1,2
variableD1: dw 0,0,0,0          ;4th variable as a rabit variable
variable1: dw 0,0,0,0		;4th variable as a rabit variable
variable2: dw 18
oldisr: dw 0,0
oldTisr: dw 0,0
exitC: db 0
counter: dw 0
exitT: db 0
exitR: db 0
escapebuffer: times 5680 dw  0
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------delay--------------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
delay:
push bp
mov bp,sp
push cx 
push ax
push dx
mov ax, 0
mov dx, [bp+4]
mov cx, [bp+4]
delay2:
	delay1:
	sub dx, 1
	jnz delay1
	loop delay2
pop dx
pop ax
pop cx 
pop bp
ret 2

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------randnum------------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------

randnum:

   push bp
   mov bp,sp
   push ax
   push cx
   push dx
   
   MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      
   mov ax, dx
   xor dx, dx
   mov cx, [bp+4] 
   inc cx   
   div  cx       ; here dx contains the remainder of the division - from 0 to 9
   mov [bp+6], dx
   
   pop dx
   pop cx
   pop ax
   pop bp   
   ret 2


;---------------------------------timer----------------------------------

timerisr:
	
	cmp word[movingBrickCurrSpeed], 3
	jnz timerend
	
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	inc word[counter]
	cmp word[counter], 100
	jbe timerend1
	mov byte[exitT], 1
	timerend1:


	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
	timerend:
	jmp far [cs:oldTisr]
	

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;;;---------------------escape
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
escapescreen:

push ax
push bx
push cx
push dx
push si
push di
push ds
push es

mov cx, 5676
push ds
pop es
mov si, 0
push 0xb800
pop ds
mov di, escapebuffer
rep movsw



call clearScreen


mov ah, 0
int 16h

cmp al, 'n'
jne exitesc

mov cx, 5676
push es
pop ds
mov di, 0
push 0xb800
pop es
mov si, escapebuffer
rep movsw
mov byte[exitC], 0


exitesc:
pop es
pop ds
pop di
pop si
pop dx
pop cx
pop bx
pop ax
ret

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------Kbisr--------------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
kbisr: 
	push ax 
 	push es 
 	
in al, 0x60 
cmp al, 0x48
jne kbisresc

call CheckJump
cmp byte[exitR],1
je kbisrend
call moveRabitUp
call MainScroll


jmp kbisrend

kbisresc:

cmp al, 1
jne kbisrend
mov byte[exitC],1


 
 kbisrend:
 pop es 
 pop ax 
 jmp far [cs:oldisr]  

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------SetVar-------------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
SetVar:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx

	mov bx,[bp+4]
	mov ax,word [bx+2]
	mov word[bx],ax
	mov ax,word [bx+4]
	mov word[bx+2],ax

	pop cx
	pop bx
	pop ax
	pop bp
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------MainScroll---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
MainScroll:
	push ax
	push bx
	push cx
	push dx	
	push es
	push di
	push si

	push 4
	call ScrollDown           ; bottom line gone

	mov ax,variable1
	push ax
	call SetVar
	mov word[variable1+4],0
	mov ax, [variable1]
	mov word[variable1+6],ax


	mov ax,variableD1
	push ax
	call SetVar
	mov word[variableD1+4],0
	mov ax, [variableD1]
	mov word[variableD1+6],ax


	mov ax,movingBrickCurrSpeed
	push ax
	call SetVar

	sub sp,2                                        ;; for making space to place value in register after function
	push 3
	call randnum
	pop si

	mov word[movingBrickCurrSpeed+4],si              

	add word[movingBrickCurrPos+2],1584		;6*264=1584 (Gap between two centre lines)
	add word[movingBrickCurrPos+4],1584
	
	

	mov ax,movingBrickCurrPos
	push ax
	call SetVar

;;;;;;;;;;;;;Adding new line on top

	mov word[movingBrickCurrPos+4],6994  
	sub sp,2
	push 8
	call randnum
	pop si
	test si,1
	jz lo
	add si,1
lo:
	add word[movingBrickCurrPos+4],si	

                                                        ;Make a function for proper colour allotment using cmp
	mov al, 0x20                                    ;0x20 for space
	push word [movingBrickCurrPos+4]
	mov ah, byte[movingBrickCurrSpeed+4]
	add ah, 4                                       ;Colour of brick in ah
	push ax
	call brick
	add word[movingBrickCurrPos+4],	528	        ;Moving it down 2 lines to  show animation and bringing it previous to top brick line

	push 2
	call ScrollDown

	mov word[counter], 0


	pop si
	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
ret

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------CheckJump---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
CheckJump:

push ax
push cx
push bx

mov bx, [RabitPos]
mov ax, 264
mov cx, 5
mul cx       ;check for error mul
sub bx, ax
mov cx, [movingBrickCurrPos+2]
add cx, 2
cmp bx, cx
jb checkended
add cx, 20
cmp bx, cx
ja checkended

jmp simplecheck


checkended:
mov byte[exitR],1


simplecheck:
pop bx
pop cx
pop ax


ret 
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------ScrollDown---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
ScrollDown:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es
push ds 

call moveCarsForward 
call moveMountainsBackward    
mov ax, 132 ; load chars per row in ax 
mul byte [bp+4] ; calculate source position 
push ax ; save position for later use 
shl ax, 1 ; convert to byte offset 
mov si, 11350 ; last location on the screen 
sub si, ax ; load source position in si 
mov cx, 2244 ; number of screen locations
shr ax, 1 
sub cx, ax ; count of words to move 
mov ax, 0xb800 
mov es, ax ; point es to video base 
mov ds, ax ; point ds to video base 
mov di, 11350 ; point di to lower right column 
std ; set auto decrement mode 
rep movsw ; scroll up 
mov ax, 0x3020 ; space in normal attribute 
pop cx ; count of positions to clear 
rep stosw
mov cx, 1000
push 0x0003
call delay
call moveCarsForward 
call moveMountainsBackward    

pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------newMoveBrickForward------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------

newMoveBrickForward:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es
push ds 

;1

mov ax, 0xb800
mov ds, ax 
mov es, ax 
mov si, [bp+4]; 3958
mov cx, 1
cld
moveBrickl1:

lodsw
push ax 
add si, 262 
sub cx, 1 
jnz moveBrickl1

;2

mov bx, [bp+4]; destination
mov dx, [bp+4]
sub dx, 2         ;3956 ;source
mov cx, 1 ; repeat outer loop for lines
moveBrickl2:
std ;set direction flag
mov di, bx  
mov si, dx 
push cx 
mov cx, 131
rep movsw 
pop cx
add bx, 264
add dx, 264
loop moveBrickl2

;3

mov di, [bp+4]
sub di, 262 
mov cx, 1
moveBrickl3:

pop ax 
mov [es:di], ax 
sub di, 264 
sub cx, 1
jnz moveBrickl3





pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret 2

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------newMoveBrickBackward------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------

newMoveBrickBackward:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es
push ds 

mov ax, 0xb800
mov ds, ax 
mov es, ax 
mov si, [bp+4] 
mov cx, 1 
std
lodsw
push ax 

mov bx, [bp+4] ;destination
mov dx, bx 
add dx, 2  ;source
cld ;set direction flag
mov di, bx  
mov si, dx 
mov cx, 131
rep movsw 

mov di, [bp+4]
add di, 262 
pop ax 
mov [es:di], ax 

pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------moveTheBrick-------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
moveTheBrick:
	push bp 
	mov bp, sp 
	push ax
	push bx 
	push cx
	push dx 
	push si 
	push di 
	push es 
	push ds
	cmp word [bp+4],0
	jz rett
	cmp word [bp+4],3
	jz rett

	
		mov si, [bp+8]
		cmp word [bp+4],2
		jz MFL
;;;1
                mov cx,word  [variable1+si]
		mov dx,cx
		mov ax,word [variable2]   
                cmp cx,ax
                jz lB ;------------------Jump for backward

		mov ax, [bp+6]
		add ax, 262
		push ax
		call newMoveBrickForward
		
		add dx,1
                mov word [variable1+si],dx
		add word[movingBrickCurrPos+si],2
		mov bx,0
		mov word [variableD1+si],bx
                jmp lR
;;;2

MFL:
		mov cx,word  [variable1+si]
		mov dx,cx
		mov ax,word [variable2]   
                cmp cx,ax
                jz MFB 
		mov ax, [bp+6]
		add ax, 262
		push ax
		call newMoveBrickForward
		mov ax, [bp+6]
		add ax, 262
		push ax
		call newMoveBrickForward

                add dx,2
                mov word [variable1+si],dx
		add word[movingBrickCurrPos+si],4
		mov bx,0
		mov word [variableD1+si],bx
                jmp lR
rett:
	cmp word [bp+4],0
	jz lR
	cmp word [bp+4],3
	jz lR
lB:          
 		mov bx, word [variableD1+si]
		mov cx, word [variable2]
		cmp bx,cx
                jz sl1
		push word [bp+6]
		call newMoveBrickBackward
		;push word [bp+6]
		;call newMoveBrickBackward

		add bx,1
		mov word[variableD1+si],bx
		sub word[movingBrickCurrPos+si],2
		jmp lR
sl1:
		mov cx,0
		mov word [variable1+si],cx
		jmp lR
MFB:

		mov bx, word [variableD1+si]
		mov cx, word [variable2]
		cmp bx,cx
                jz sl2
		push word [bp+6]
		call newMoveBrickBackward
		push word [bp+6]
		call newMoveBrickBackward

		add bx,2
		mov word[variableD1+si],bx
		sub word[movingBrickCurrPos+si],4
		jmp lR
sl2:
		mov cx,0
		mov word [variable1+si],cx
		jmp lR
      	          
lR:
	pop ds 
	pop es 
	pop di 	
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
 
	pop bp
ret 6
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------moveRabitUp--------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
moveRabitUp:
	push bp 
	mov bp, sp 
	push ax
	push bx 
	push cx
	push dx 
	push si 
	push di 
	push es 
	push ds
	mov cx, 4
	mov ax,[RabitPos]
RabitUpLoop:
	push ax
	call resetScreen
	sub ax,264
	push ax
	call PrintRabit
	push 0x0003
	call delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call moveCarsForward 
	call moveMountainsBackward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	loop RabitUpLoop

	push ax
	call resetScreen
	push 0x0002			;check delaaay
	call delay
	sub ax,528
	push ax
	call PrintRabit

        ;mov word[RabitPos], ax
	pop ds 
	pop es 
	pop di 	
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	pop bp
ret

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------resetScreen--------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
resetScreen:
	push bp 
	mov bp, sp 
	push ax
	push bx 
	push cx
	push dx 
	push si 
	push di 
	push es 
	push ds
	
	mov ax,0xb800
	mov es,ax
	mov ds,ax
	mov di,[bp+4]
	mov ah,0x30
	mov al,' '
	mov [es:di],ax
	add di,2
	mov [es:di],ax

	pop ds 
	pop es 
	pop di 	
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	pop bp
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------moveMountainsBackward----------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
moveMountainsBackward:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es
push ds 

mov ax, 0xb800
mov ds, ax 
mov es, ax 
mov si, 0 
mov cx, 14 
std
moveMountainsl1:
lodsw
push ax 
add si, 266 
sub cx, 1 
jnz moveMountainsl1

mov bx, 0 ;destination
mov dx, 2 ;source
mov cx, 14 ; repeat outer loop for lines
moveMountainsl2:
cld ;set direction flag
mov di, bx  
mov si, dx 
push cx 
mov cx, 131
rep movsw 
pop cx
add bx, 264
add dx, 264
sub cx, 1 
jnz moveMountainsl2 

mov di, 3694 
mov cx, 14 
moveMountainsl3:
pop ax 
mov [es:di], ax 
sub di, 264
sub cx, 1
jnz moveMountainsl3 

pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------moveCarsForward----------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
moveCarsForward:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es
push ds 

;1

mov ax, 0xb800
mov ds, ax 
mov es, ax 
mov si, 3958
mov cx, 12
cld
moveCarsl1:

lodsw
push ax 
add si, 262 
sub cx, 1 
jnz moveCarsl1

;2

mov bx, 3958; destination
mov dx, 3956 ;source
mov cx, 12 ; repeat outer loop for lines
moveCarsl2:
std ;set direction flag
mov di, bx  
mov si, dx 
push cx 
mov cx, 131
rep movsw 
pop cx
add bx, 264
add dx, 264
loop moveCarsl2

;3

mov di, 6600 
mov cx, 12
moveCarsl3:

pop ax 
mov [es:di], ax 
sub di, 264 
sub cx, 1
jnz moveCarsl3

pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------interactiveAnimation-----------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
interactiveAnimation:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es
push ds 
 
interactiveAnimationl1:
call moveCarsForward 
call moveMountainsBackward


;cli

push 0
push word[movingBrickCurrLine]
push word[movingBrickCurrSpeed]
call moveTheBrick



push 2
push word[movingBrickCurrLine+2]
push word[movingBrickCurrSpeed+2]
call moveTheBrick

push 4
push word[movingBrickCurrLine+4]
push word[movingBrickCurrSpeed+4]
call moveTheBrick

push 6
push word[movingBrickCurrLine+6]
push word[movingBrickCurrSpeed]
call moveTheBrick

;push 0xffff
;call delay
push 0x0003
call delay


;sti

cmp byte[exitT],0
jne codefinished

cmp byte[exitR],0
jne codefinished

cmp byte[exitC],0
je tryingexit1
call escapescreen
tryingexit1:
cmp byte[exitC],0
jne codefinished

jmp interactiveAnimationl1




codefinished:
;call clearScreen                          ;call end screen here

 
 
 cli ; disable interrupts 
 
 
 mov ax, [oldisr] ; read old offset in ax 
 mov bx, [oldisr+2] ; read old segment in bx
 mov [es:9*4], ax ; restore old offset from ax 
 mov [es:9*4+2], bx ; restore old segment from bx 
 
 mov ax, [oldTisr] ; read old offset in ax 
 mov bx, [oldTisr+2] ; read old segment in bx
 mov [es:8*4], ax ; restore old offset from ax 
 mov [es:8*4+2], bx ; restore old segment from bx 
 
 
 sti 
pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax
pop bp
ret

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------PrintRabit---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
PrintRabit:
push bp 
mov bp, sp 
push ax
push bx 
push cx 
push dx 
push si 
push di 
push es 

mov ax, 0xb800
mov es, ax 
mov di,[bp+4]
mov ah,0x20
mov al,'R'

mov [es:di],ax
add di,2
mov [es:di],ax

pop es   
pop di 
pop si 
pop dx
pop cx   
pop bx 
pop ax 
pop bp 
ret 2

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------PrintCarrot--------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
PrintCarrot:

push bp
mov bp, sp
push ax
push bx
push cx
push dx
push si
push di
push es

 
mov ax, 0xb800
mov es, ax
mov di,[bp+4]
mov ah,0x20
mov al,'C'
 
mov [es:di],ax
add di,2
mov [es:di],ax
 
pop es  
pop di
pop si
pop dx
pop cx  
pop bx
pop ax
pop bp
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------brick--------------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
brick:
push bp 
mov bp, sp 
push ax
push bx 
push cx 
push dx 
push si 
push di 
push es 

mov ax, 0xb800 
mov es, ax 
mov di, [bp+6] 
mov cx, 12
mov ax, [bp+4] ;; 4020
shl ah, 4
mmbl1:
mov [es:di], ax 
add di, 2 
sub cx, 1 
jnz mmbl1

pop es   
pop di 
pop si 
pop dx
pop cx   
pop bx 
pop ax 
pop bp 
ret 4 ; location was needed
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeBricks---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeBricks:
push ax
push si
push cx

mov cx, 3
xor si, si
makeBloop:
	mov al, 0x20
	push word [movingBrickCurrPos+si]
	mov ah, byte[movingBrickCurrSpeed+si]
	add ah, 4
	push ax
	call brick 
	add si, 2
loop makeBloop
push word [RabitPos]
call PrintRabit
push 8856
call PrintCarrot

pop cx
pop si
pop ax
ret 
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------chirri-------------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
chirri:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es 

mov ax, 0xb800 
mov es, ax 
mov di, [bp+4] 
mov ah, 0xB0  
mov al, '\' 
mov [es:di], ax 
add di, 2
mov al, '/' 
mov [es:di], ax 

pop es
pop di
pop si  
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeBirds----------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeBirds:
push 196
call chirri 
push 166
call chirri 
;push 322
;call chirri 
push 366
call chirri 
push 490
call chirri 
push 560
call chirri 
push 610
call chirri 
push 376
call chirri 
push 470
call chirri 
push 220
call chirri 
push 400
call chirri
push 280
call chirri
ret 
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeOneCar---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeOneCar:       
;give me parameter ->starting index of large rectangle 
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es 

mov si, [bp+4]
mov ax, 0xb800 
mov es, ax 
; mov ah, 0x40
; mov al, 0x20
mov ax, [bp+6]
mov cx, 12 
carbigbox:
mov [es:si], ax ;           stosw
add si, 2
sub cx, 1
jnz carbigbox
mov al, ':'
mov ah, 0x6f
mov [es:si], ax
sub si, 280
mov cx, 6
mov ah, 0x70
mov al, ' '
carlittlebox:
cmp cx, 4
jnz skipwindowoflittleboxcar
mov al, '|'
skipwindowoflittleboxcar:
mov [es:si], ax ;           stosw
mov al, ' '
add si, 2
sub cx, 1
jnz carlittlebox
mov al, 'O'
mov ah, 0x07
add si, 512	
mov [es:si], ax 
add si, 18
mov [es:si], ax
pop es
pop di
pop si  
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret 4
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeCars-----------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeCars: 
push 0x2020
push 4224
call makeOneCar 
push 0x5020
push 4384
call makeOneCar 
push 0x1020
push 5340
call makeOneCar 
push 0x4020
push 5480
call makeOneCar 
push 0x2020
push 6510
call makeOneCar 
push 0x4020
push 6350
call makeOneCar 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeGrassBackground------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeGrassBackground:
push dx 
push di 
push es
push ax 
push cx 

mov ax, 0xb800 
mov es, ax  
mov al, ' '
mov ah, 0x20
mov di, 3432
wbl1:
mov [es:di], ax ;stosw
add di, 2
cmp di, 3696
jnz wbl1
;mov di, 3432
;mov dl, '^'
;mov dh, 0x20
;waveouterloop:
;mov cx, 0
;wavel1:
;mov [es:di], dx
;add di, cx
;add cx, 2
;cmp cx, 10
;jnz wavel1
;add di, 50
;cmp di, 3696
;jna waveouterloop
pop cx 
pop ax 
pop es 
pop di 
pop dx 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------whiteTree----------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
whiteTree:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es 

mov ax, 0xb800 
mov es, ax 
mov ah, 0x70 
mov al, '|'
mov di, [bp+4] 
add di, 4
;sub di, 264
mov [es:di], ax 
mov cx, 3
repeatbushforlines:
push cx
sub di, 266
mov al, ' '
mov cx, 3
repeatbush:
mov [es:di], ax 
add di, 2
sub cx, 1 
cmp cx, 0
jnz repeatbush
pop cx
sub di, 4
mov al, 'V'
mov [es:di], ax
sub cx, 1 
jnz repeatbushforlines

pop es
pop di
pop si  
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret 2
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeMountain-------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeMountain:

push bp
mov bp, sp

 sub sp,2
 push dx
mov dx,0
push es
push ax
push cx
push di

mov word [bp-2],2
mov ax, 0xb800
mov es, ax ; point es to video base
mov di, [bp + 6]     ; point di to top left column
add di, 264
; es:di --> b800:0000\  

mov ax,0
mov cx, [bp+4] ; load length of string in cx
mov bh, 0x30     ; normal attribute fixed in al
mov bl, 0x2F

frontDiagonal:
mov bl, 0x20
mov [es:di], bx
jmp secondDiagonal
l1: add di, 260
;loop frontDiagonal
inc dx
cmp dx,cx
jz return
jmp frontDiagonal

secondDiagonal:
mov ax,dx
mul byte[bp-2]
mul byte[bp-2]
mul byte[bp-2]
add ax,2
mov bl, 0x20
mov si, di
mov word[bp - 4], ax
add word[bp - 4], si

color:
push bx
mov bx, 0x6f20
mov [es:si], bx
add si, 2
cmp si, [bp-4]
jne color
je popping

popping:
pop bx

;add si,ax 
mov [es:si], bx
jmp l1

return: 
sub si, 4      
push si 
call whiteTree
pop di
pop cx
pop ax
pop es
pop dx
mov sp, bp 
pop bp

ret 4
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeMountainRange--------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeMountainRange:
push 304                 ;index of top 
push 11              ;  height of mountain
call makeMountain

push 216
push 12
call makeMountain

push 1428
push 7
call makeMountain

push 2000
push 5
call makeMountain
ret   
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeRoadLane-------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeRoadLane:
push bp 
mov bp, sp
push dx 
push di 
push es
push ax 
push cx 
mov ax, 0xb800 
mov es, ax  
mov dl, '_'
mov dh, 0x07
mov di, [bp+4]
mbl1:
mov cx, 3
add di, 2
mbl2:
mov [es:di], dx
add di, 2
sub cx, 1
cmp cx, 0
jnz mbl2
add di, 2
mov cx, [bp+4] 
add cx, 250
cmp di, cx
jna mbl1

pop cx
pop ax 
pop es 
pop di 
pop dx 
pop bp 
ret 2

;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeForegroundFloor------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeForegroundFloor: 
push dx 
push di 
push es
push ax 

mov ax, 0xb800 
mov es, ax  
mov dl, ' '
mov dh, 0x00
mov di, 3696
bfl1:
mov [es:di], dx
add di, 2
cmp di, 6864
jnz bfl1
mov ax, 0x3020
checking:
stosw
cmp di, 11352
jnz checking
pop ax 
pop es 
pop di 
pop dx 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeRoad-----------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeRoad:
call makeForegroundFloor
push 4488
call makeRoadLane
push 5544
call makeRoadLane
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------makeBackgroundSky--------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
makeBackgroundSky:
push dx 
push di 
push es
push ax 
mov ax, 0xb800 
mov es, ax  
mov dl, ' '
mov dh, 0x30
xor di, di
bsl1:
mov [es:di], dx
add di, 2
cmp di, 3960
jnz bsl1
pop ax 
pop es 
pop di 
pop dx 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------clearScreen--------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
clearScreen:
push ax
push bx 
push di 
push es

mov ax, 0xb800
mov es, ax 
mov di, 0
clrScrnNextChar:
mov word [es:di], 0x0720
add di, 2
cmp di, 11352
jne clrScrnNextChar 

pop es
pop di
pop bx
pop ax 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------printStr-----------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
 printStr:
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 

 mov ax, 0xb800 
 mov es, ax 
 
 mov di, [bp+10]
 mov ah, [bp+8]  
 mov si, [bp+6] 
 mov cx, [bp+4]  

cld  

nextchar: lodsb  
	 stosw 
 loop nextchar 

 pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 8 
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------homeScreen---------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
homeScreen:
push bp 
mov bp, sp 
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es 
;1
push 1366
mov ax, 7 
push ax 
mov ax, message1 
push ax  
push word [length1]
call printStr

push 1630
mov ax, 7 
push ax 
mov ax, message1 
push ax  
push word [length1]
call printStr

push 1894
mov ax, 7 
push ax 
mov ax, message1 
push ax  
push word [length1]
call printStr

push 2426
mov ax, 7 
push ax 
mov ax, message2 
push ax  
push word [length2]
call printStr

push 2950
mov ax, 7 
push ax 
mov ax, message1 
push ax  
push word [length1]
call printStr

push 3214
mov ax, 7 
push ax 
mov ax, message1 
push ax  
push word [length1]
call printStr

push 3478
mov ax, 7 
push ax 
mov ax, message1 
push ax  
push word [length1]
call printStr


mov ah,0
int 0x16

mov ax, 0xb800
mov es,ax;
mov di, 130
mov bl,'*'
mov bh,70
mov [es:di],bx
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp 
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------printHomeScreen----------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
printHomeScreen:
call clearScreen 
call homeScreen 
ret 
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------printPlayingScreen-------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
printPlayingScreen:
call clearScreen 
call makeBackgroundSky
call makeRoad
call makeMountainRange
call makeGrassBackground
call makeCars
call makeBirds
call makeBricks
ret
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------printAllScreens----------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
printAllScreens:
call printHomeScreen 
call printPlayingScreen 
ret 
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
;------------------------------Main Function------------------------------------------
;-------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------
start:
mov ah,0x00
mov al, 0x54
int 0x10

xor ax, ax 
 mov es, ax
 mov ax, [es:9*4] 
 mov [oldisr], ax 
 mov ax, [es:9*4+2] 
 mov [oldisr+2], ax
 
 

mov ax, [es:8*4] 
mov [oldTisr], ax 
mov ax, [es:8*4+2] 
mov [oldTisr+2], ax

cli 

mov word [es:9*4], kbisr 
mov [es:9*4+2], cs 
 
mov word [es:8*4], timerisr 
mov [es:8*4+2], cs 

sti


;makes 45x132 screen
call printAllScreens
;push 3
;call ScrollDown
push word [bp+2]
call interactiveAnimation


mov ax, 0x4c00 
int 0x21
;phase 3 is from lines 27 to 45 inclusive -> 19 lines total
;phase 2 is from lines 15 to 26 inclusive
;phase 1 si from lines 1 to 14 inclusive
;add custom (eventually random) speed to brick movemen
