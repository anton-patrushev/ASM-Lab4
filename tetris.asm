.model small

stack segment
    dw 200h dup('$$')
ends

data segment
    RANDOM_NUMBER db 0

    ITEM_HEIGHT db 0
    ITEM_WIDTH db 0

    ITEM_X db 10
    ITEM_Y db 0

	NEXT_X dw 0
	NEXT_y dw 0         

	BLACK_SYMBOL db 35h,0      ;black character on black background
	BLUE_SYMBOL db 38h,11h    ;blue character on blue background
	GRAY_SYMBOL db 30h,77h     ;gray character in gray background
    
    ELAPSED_TIME dw 0
ends

code segment

init macro
    RIGHT_KEY equ 4Dh
    LEFT_KEY equ 4Bh
    UP_KEY equ 48h
    DOWN_KEY equ 50h

	SCREEN_WIDTH equ 0A0h       ;screen width in bytes (dec: 80 x 2 = 160) 
    SCREEN_HEIGHT equ 19h       ;screen height in characters (dec: 25)
    
    DELAY equ 2
	FACTOR equ 2  
	  
	mov ax, data
  	mov ds, ax
  
	mov ah,00h
  	mov al,3
  	int 10h

  	mov ax,0B800h
  	mov es,ax
endm

random proc ;generate random number from 0 to 2 in store it in RANDOM_NUMBER
	pusha
	mov ah, 00h  ; interrupts to get system time        
	int 1ah      ; CX:DX now hold number of clock ticks since midnight      	
	mov  ax, dx
	xor  dx, dx
	mov  cx, 3    
	div  cx       ; here dx contains the remainder of the division - from 0 to 2
	mov byte ptr[RANDOM_NUMBER], dx	
	popa    
	ret
endp

print_rect proc        ; accepts X and Y - initial coordinates, width, height of the rectangle, char and attribute in one parameter 
  	push bp
  	mov bp, sp

  	; now [bp + 2] = call ret adress
    ; [bp + 4] = symbol
    ; [bp + 6] = height
  	; [bp + 8] = width
  	; [bp + 10] = y
  	; [bp + 12] = x  

  	push ax
  	push bx  
  	push cx
  	push dx
  	push di
  
  	mov ax, [bp + 10] ; y
  	mov bx, [bp + 12] ; x 
  	call convert_to_offset ; ax = `y` & bx = 'x' => dx = calculated offset
  	mov di, dx
    
  	mov ax, [bp + 4] ; ax = ascii char + attribute
  	mov cx, [bp + 6] ; cx = height
    
  	print_rect_loop:  
    	push cx
    	mov cx, [bp + 8] ; cx = width
   
    	push di
    	rep stosw
    	pop di
    
    	add di, SCREEN_WIDTH
    
    	pop cx
  	loop print_rect_loop
    
  	pop di
  	pop dx
  	pop cx
  	pop bx
  	pop ax
  	pop bp
  	ret
endp

call_print_rect macro x, y, width, height, symbol
  	push x                 ;X coordinate
  	push y                 ;Y coordinate
  	push width             ;width of the rectangle
  	push height            ;height of the rectangle
  	push word ptr symbol   ;char with attribute
  
 	  call print_rect

  	pop dx
  	pop dx
  	pop dx
  	pop dx
  	pop dx
endm

; done
convert_to_offset proc       ;accepts Y in ax, X in bx. returns offset in DX 
    push cx
    push bx
    
    mov cl, SCREEN_WIDTH ;ScreenWidth = 80x2=160
    mul cl ; ax = y * ScreenWidth
    mov dx, ax ; dx = `y` * 80x2  
    
    mov ax, bx
    mov bx, FACTOR
    push dx 
    mul bx ; ax = `x` * 2
    pop dx
    add dx, ax ; dx contain offset
    
    pop bx
    pop cx
    ret
convert_to_offset endp

; done
print_layout proc       ; prints the field
    ; call macro-wrappers
  	call_print_rect 1, 0, 19, 1, GRAY_SYMBOL ; top
  	call_print_rect 0, 0, 1, 24, GRAY_SYMBOL ; left
  	call_print_rect 19, 1, 1, 24, GRAY_SYMBOL ; right
  	call_print_rect 0, 24, 19, 1, GRAY_SYMBOL ; bottom

 	ret
endp  

clear_screen macro
    mov ah, 0h
    mov al, 3h
    int 10h
endm 

exit macro
    clear_screen
  	mov ax, 4c00h
  	int 21h
endm

start:
	init
	call print_layout

    ;game_loop: ;endless loop
    ;    call app
    ;jmp game_loop

	exit
end start

app proc
	mov ah, 0 ;get ticks count
    int 1Ah ;cx - greater byte, dx - smaller one
    
    xor cx,cx
    cmp dx, word ptr ds:[ELAPSED_TIME]
	jb skip_app_iteration
	
	add dx, DELAY
    mov word ptr [ELAPSED_TIME], dx
    
    call check_input
    ; call print_score

	skip_app_iteration:
	ret
endp

check_input proc 	;reads pressed key 
    push ax
    
    mov ah, 1 	;check for key pressed
    int 16h 	;keyboard interrupt
    
    jz end_check_input 	;if zf=1 - key not pressed
    
    mov ah, 0 	;get key
    int 16h 	;read key
	
	perfom_action
    
    end_check_input:
    pop ax
	ret
endp

perfom_action macro ;accept scan_code in `ah`
	cmp ah, LEFT ;compare scan code
    je call_move_item
    
    cmp ah,RIGHT
    je call_move_item
    
    cmp ah,UP
    je call_rotate_item
    
    cmp ah,DOWN
	je call_drop_item

	call_rotate_item:
		call rotate_item
		jmp end_perfom_action
	
	call_move_item:
		call move_item
		jmp end_perfom_action
	
	call_drop_item:
		call drop_item
		jmp end_perfom_action

	end_perfom_action:
endm

rotate_item proc ;accept scan_code in `ah`
	;rotate
	ret
endp

move_item proc ;accept scan_code in `ah`
	;move_item
	ret
endp

drop_item proc ;accept scan_code in `ah`
	;move_item
	ret
endp

ends
            