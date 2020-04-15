.model small
.stack 100h

.data
	NEW_ITEM db 1 ;flag for creating new item
	RANDOM_NUMBER db 0
	ITEM_HEIGHT db 0
	ITEM_WIDTH db 0
	ITEM_X db 0
  	ITEM_Y db 0
  	ITEM_ROTATE db 0 ;0 -> 1 -> 2-> 3 -> 4
		
	BLACK_SYMBOL db 35h,0      ;black character on black background
	BLUE_SYMBOL db 38h,11h     ;blue character on blue background
	GRAY_SYMBOL db 30h,77h     ;gray character in gray background, attribute 0111 0111
	ITEM_CHAR db 23h
	RED_ATTRIBUTE db 44h
	TEMP_SYMBOL dw 0    
	ELAPSED_TIME dw 0

	BLOCK_ROTATE db 1

	RED_SYMBOL db 23h, 44h
.code
jmp start

init macro
	RIGHT_KEY equ 4Dh
	LEFT_KEY equ 4Bh
	UP_KEY equ 48h
	DOWN_KEY equ 50h

	_SCREEN_WIDTH_ equ 50h
	SCREEN_WIDTH equ 0A0h       ;screen width in bytes (dec: 80 x 2 = 160) 
	SCREEN_HEIGHT equ 19h       ;screen height in characters (dec: 25)
	
	DELAY equ 2
	FACTOR equ 2 

	LEFT_LIMIT equ 1
	RIGHT_LIMIT equ 21
	DOWN_LIMIT equ 24
	  
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
	mov word ptr[RANDOM_NUMBER], dx	
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

print_current_item proc
	pusha

	xor ax, ax
	xor bx, bx
	xor cx, cx
	xor dx, dx
	
	mov al, byte ptr ds:[ITEM_X]
	mov bl, byte ptr ds:[ITEM_Y]
	mov cl, byte ptr ds:[ITEM_WIDTH]
	mov dl, byte ptr ds:[ITEM_HEIGHT]
	
	call_print_rect ax, bx, cx, dx, TEMP_SYMBOL
	
	popa
	ret
endp

clear_rect proc
	push bp
	mov bp, sp
	
	; [bp + 2] = call ret adress
	; [bp + 4] = height
	  ; [bp + 6] = width
	  ; [bp + 8] = y
	; [bp + 10] = x
	  
	push ax
	push bx  
	push cx
	push dx
	push di
  
mov ax, [bp + 8] ; y
	mov bx, [bp + 10] ; x 
	call convert_to_offset ; ax = `y` & bx = 'x' => dx = calculated offset
	mov di, dx
	
	mov ax, word ptr ds:[BLACK_SYMBOL]; ax = ascii char + attribute
	mov cx, [bp + 4] ; cx = height
	
	clear_rect_loop:  
		push cx
		mov cx, [bp + 6] ; cx = width
   
		push di
		rep stosw
		pop di
	
		add di, SCREEN_WIDTH
	
		pop cx
	loop clear_rect_loop
	
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret
endp

call_clear_rect macro x, y, width, height
	push x                 ;X coordinate
	  push y                 ;Y coordinate
	  push width             ;width of the rectangle
	  push height            ;height of the rectangle
  
	 call clear_rect

	  pop dx
	  pop dx
	  pop dx
	  pop dx
endm

clear_current_item proc
	pusha
	
	xor ax, ax
	xor bx, bx
	xor cx, cx
	xor dx, dx
	
	mov al, byte ptr ds:[ITEM_X]
	mov bl, byte ptr ds:[ITEM_Y]
	mov cl, byte ptr ds:[ITEM_WIDTH]
	mov dl, byte ptr ds:[ITEM_HEIGHT]

	call_clear_rect ax, bx, cx, dx
	
	popa
	ret
endp

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
endp

; done
print_layout proc       ; prints the field
	; call macro-wrappers
	call_print_rect 1, 0, RIGHT_LIMIT, 1, GRAY_SYMBOL ; top
	call_print_rect 0, 0, 1, DOWN_LIMIT, GRAY_SYMBOL ; left
	call_print_rect RIGHT_LIMIT, 1, 1, DOWN_LIMIT, GRAY_SYMBOL ; right
	call_print_rect 0, DOWN_LIMIT, RIGHT_LIMIT, 1, GRAY_SYMBOL ; bottom

	; call_print_rect 4, 23, 17, 1, RED_SYMBOL ; mock
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

app proc
	mov ah, 0 ;get ticks count
	int 1Ah ;cx - higher byte, dx - lower one
	
	xor cx, cx
	cmp dx, word ptr ds:[ELAPSED_TIME]
	jb skip_app_iteration
	
	add dx, DELAY
	mov word ptr ds:[ELAPSED_TIME], dx
	
	cmp byte ptr ds:[NEW_ITEM], 0
	je app_skip_drawing

	call create_item

	app_skip_drawing:
	call check_input

	skip_app_iteration:
	ret
endp

create_item proc
	;generate random number
	;choose item model
	;mocked model is 0 (4 x char line)
	push dx
	xor dx, dx
	
	mov dl, byte ptr ds:[ITEM_CHAR]
	mov dh, byte ptr ds:[RED_ATTRIBUTE]
	mov word ptr ds:[TEMP_SYMBOL], dx
	
	mov byte ptr ds:[ITEM_WIDTH], 3
	mov byte ptr ds:[ITEM_HEIGHT], 1

	mov byte ptr ds:[ITEM_X], 8
	mov byte ptr ds:[ITEM_Y], 1

	pusha

	xor ax, ax
	xor bx, bx
	xor cx, cx
	xor dx, dx
	
	mov al, byte ptr ds:[ITEM_X]
	mov bl, byte ptr ds:[ITEM_Y]
	mov cl, byte ptr ds:[ITEM_WIDTH]
	mov dl, byte ptr ds:[ITEM_HEIGHT]


	call_print_rect ax, bx, cx, dx, TEMP_SYMBOL

	popa

	pop dx
	mov byte ptr ds:[NEW_ITEM], 0 ;disable drawing new item
	ret
endp

check_input proc 	;reads pressed key 
	push ax
	
	mov ah, 1 	;check for key pressed
	int 16h 	;keyboard interrupt
	
	jz end_check_input 	;if zf=1 - key not pressed
	
	mov ah, 0 	;get key
	int 16h 	;read key
	
	end_check_input:

	call perfom_action
	pop ax
	ret
endp

check_completed_rows proc 
	pusha
	
	mov bp, DOWN_LIMIT
	dec bp
	mov al, byte ptr ds:[ITEM_CHAR]
	
	mov cx, bp
	check_completed_rows_loop:
		push cx
		mov cx, RIGHT_LIMIT
		dec cx
		
		push ax
		mov ax, bp
		mov bx, 1
		call convert_to_offset ; ax = `y` & bx = 'x' => dx = calculated offset
		pop ax

		mov bx, dx
		check_completed_rows_row_loop:
			cmp byte ptr es:[bx], al
			jne check_completed_rows_next_row
			add bx, 2
		loop check_completed_rows_row_loop
		; mov bx, dx
		; mov dx, RIGHT_LIMIT
		; add dx, RIGHT_LIMIT
		; sub dx, 2
		; sub bx, dx

		mov bx, dx
		call_clear_rect 1, bp, 20, 1
		
		push ds
		push es
		pop ds

		push bp
		check_completed_rows_shift:
			mov di, bx ; di on cleaning row
			sub bx, SCREEN_WIDTH
			mov si, bx ; si on previous row

			mov cx, 20
			rep movsw ; shift down string
			
			dec bp
			cmp bp, 1
		ja check_completed_rows_shift
		pop bp	
		pop ds

		check_completed_rows_next_row:
		dec bp ; y = y-1
		pop cx
	loop check_completed_rows_loop

	popa
	ret
endp

perfom_action proc ;accept scan_code in `ah`
	cmp ah, LEFT_KEY ;compare scan code
	je call_move_item
	
	cmp ah, RIGHT_KEY
	je call_move_item
	
	cmp ah, UP_KEY
	je call_rotate_item
	
	; cmp ah, DOWN_KEY
	; je call_drop_item
	jne call_move_item

	call_rotate_item:
		call rotate_item
		call print_current_item
	
	call_move_item:
		call move_item
		jmp end_perfom_action
	
	; call_drop_item:
	; 	call drop_item

	call check_completed_rows

	end_perfom_action:
	ret
endp

check_for_borders proc
	push ax
	push bx
	push cx

	xor ax, ax
	xor bx, bx
	xor cx, cx

	mov al, byte ptr ds:[ITEM_Y] ; y
  	mov bl, byte ptr ds:[ITEM_X] ; x 
  	call convert_to_offset ; ax = `y` & bx = 'x' => dx = calculated offset
	
	mov bx, dx
	xor dx, dx
	
	mov cl, byte ptr ds:[ITEM_HEIGHT]
	check_for_borders_height_loop:
		add bx, SCREEN_WIDTH
	loop check_for_borders_height_loop

	mov cl, byte ptr ds:[ITEM_WIDTH]
	check_for_borders_width_loop:
		mov ax, word ptr ds:[GRAY_SYMBOL]
		cmp ax, word ptr es:[bx]
		je check_for_borders_success

		mov al, byte ptr ds:[ITEM_CHAR]
		cmp al, byte ptr es:[bx]
		je check_for_borders_success

		add bx, 2
	loop check_for_borders_width_loop
	jmp end_check_for_borders

	check_for_borders_success:
	mov dx, 1
	
	
	end_check_for_borders:
	pop cx
	pop bx
	pop ax
	ret
endp

check_rotating proc
	push cx
	xor cx, cx

	cmp byte ptr ds:[ITEM_ROTATE], 0
  	je check_rotating_item_initial

  	cmp byte ptr ds:[ITEM_ROTATE], 1
  	je check_rotating_item_first
  
  	cmp byte ptr ds:[ITEM_ROTATE], 2
  	je check_rotating_item_second

  	cmp byte ptr ds:[ITEM_ROTATE], 3
	je check_rotating_item_third
	
	; _____________________0 -> 1_______________________________________
	check_rotating_item_initial:
	mov bl, byte ptr ds:[ITEM_WIDTH] 	;bl = h2
	sub bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = h2 - h1
	mov cl, bl 							;cl = h2 - h1 (iterations count)
	
	mov al, byte ptr ds:[ITEM_Y] 		; y
	mov bl, byte ptr ds:[ITEM_X] 		; x
	call convert_to_offset 				; ax = `y` & bx = 'x' => dx = calculated offset
	mov bx, dx

	mov dl, byte ptr ds:[ITEM_CHAR]
	mov ax, word ptr ds:[GRAY_SYMBOL]

	check_rotating_item_initial_loop:
		sub bx, SCREEN_WIDTH 			; go up
		cmp byte ptr es:[bx], dl
		je block_rotating
		cmp word ptr es:[bx], ax
		je block_rotating
	loop check_rotating_item_initial_loop
	jmp non_block_rotating
	
	; _____________________1 -> 2_______________________________________
	check_rotating_item_first:
	mov bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = w2
	sub bl, byte ptr ds:[ITEM_WIDTH] 	;bl = w2 - w1
	mov cl, bl							;cl = w2 - w1 (iterations count)
	
	mov al, byte ptr ds:[ITEM_Y] 		; y
	mov bl, byte ptr ds:[ITEM_X] 		; x
	call convert_to_offset 				; ax = `y` & bx = 'x' => dx = calculated offset
	mov bx, dx

	push cx
	mov cl, byte ptr ds:[ITEM_HEIGHT]
	dec cl
	_check_rotating_item_first_loop:
		add bx, SCREEN_WIDTH ;go down
	loop _check_rotating_item_first_loop
	pop cx

	mov dl, byte ptr ds:[ITEM_CHAR]
	mov ax, word ptr ds:[GRAY_SYMBOL]

	check_rotating_item_first_loop:
		sub bx, 2 						; go left
		cmp byte ptr es:[bx], dl
		je block_rotating
		cmp word ptr es:[bx], ax
		je block_rotating
	loop check_rotating_item_first_loop
	jmp non_block_rotating
  
	; _____________________2 -> 3_______________________________________
	check_rotating_item_second:
	mov bl, byte ptr ds:[ITEM_WIDTH] 	;bl = h2
	sub bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = h2 - h1
	mov cl, bl							;cl = h2 - h1 (iterations count)

	mov bl, byte ptr ds:[ITEM_X] 		; x
	mov al, byte ptr ds:[ITEM_WIDTH]
	dec al
	mov dl, 2
	mul dl

	add bx, ax
	xor ax, ax
	mov al, byte ptr ds:[ITEM_Y] 		; y
	call convert_to_offset 				; ax = `y` & bx = 'x' => dx = calculated offset
	mov bx, dx

	mov dl, byte ptr ds:[ITEM_CHAR]
	mov ax, word ptr ds:[GRAY_SYMBOL]
	check_rotating_item_second_loop:
		sub bx, SCREEN_WIDTH 			; go down
		cmp byte ptr es:[bx], dl
		je block_rotating
		cmp word ptr es:[bx], ax
		je block_rotating
	loop check_rotating_item_second_loop
	jmp non_block_rotating
  
	; _____________________3 -> 0_______________________________________
	check_rotating_item_third:
	mov bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = w2
	sub bl, byte ptr ds:[ITEM_WIDTH] 	;bl = w2 - w1
	mov cl, bl							;cl = w2 - w1 (iterations count)

	mov al, byte ptr ds:[ITEM_Y] 		; y
	mov bl, byte ptr ds:[ITEM_X] 		; x
	call convert_to_offset 				; ax = `y` & bx = 'x' => dx = calculated offset
	mov bx, dx

	mov dl, byte ptr ds:[ITEM_CHAR]
	mov ax, word ptr ds:[GRAY_SYMBOL]
	check_rotating_item_third_loop:
		add bx, 2 						; go right
		cmp byte ptr es:[bx], dl
		je block_rotating
		cmp word ptr es:[bx], ax
		je block_rotating
	loop check_rotating_item_third_loop
	jmp non_block_rotating

	block_rotating:
	mov byte ptr ds:[BLOCK_ROTATE], 1
	jmp end_check_rotating

	non_block_rotating:
	mov byte ptr ds:[BLOCK_ROTATE], 0

	end_check_rotating:
	pop cx
	ret
endp

rotate_item proc ;accept scan_code in `ah`
	push ax
	push bx
	push dx

	xor ax, ax
	xor bx, bx
	xor dx, dx
	
	call check_rotating
	cmp byte ptr ds:[BLOCK_ROTATE], 1
	je end_rotate_item
  	call clear_current_item
	  
	cmp byte ptr ds:[ITEM_ROTATE], 0
  	je rotate_item_initial

  	cmp byte ptr ds:[ITEM_ROTATE], 1
  	je rotate_item_first
  
  	cmp byte ptr ds:[ITEM_ROTATE], 2
  	je rotate_item_second

  	cmp byte ptr ds:[ITEM_ROTATE], 3
  	je rotate_item_third
  
  	rotate_item_initial:
	mov bl, byte ptr ds:[ITEM_WIDTH] 	;bl = h2
	sub bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = h2 - h1
	sub byte ptr ds:[ITEM_Y], bl		;bl = y2 = y1 + (h2 - h1)
	
	; swap width and height
	mov al, byte ptr ds:[ITEM_WIDTH]
	xchg al, byte ptr ds:[ITEM_HEIGHT]
	mov byte ptr ds:[ITEM_WIDTH], al
	
	;set current rotate state
	inc byte ptr ds:[ITEM_ROTATE]		;set 1
  	jmp end_rotate_item

	rotate_item_first:
	mov bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = w2
	sub bl, byte ptr ds:[ITEM_WIDTH] 	;bl = w2 - w1
	sub byte ptr ds:[ITEM_X], bl 		;x2 = x1 - (w2 - w1)

	mov bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = h1
	sub bl, byte ptr ds:[ITEM_WIDTH] 	;bl = h1 - h2
	add byte ptr ds:[ITEM_Y], bl		;y2 = y1 - (h1 - h2)
	
	; swap width and height
	mov al, byte ptr ds:[ITEM_WIDTH]
	xchg al, byte ptr ds:[ITEM_HEIGHT]
	mov byte ptr ds:[ITEM_WIDTH], al
	
	;set current rotate state
	inc byte ptr ds:[ITEM_ROTATE]		;set 2
  	jmp end_rotate_item
  
	rotate_item_second:
	mov bl, byte ptr ds:[ITEM_WIDTH] 	;bl = w1
	sub bl, byte ptr ds:[ITEM_HEIGHT] 	;bl = w1 - w2
	add byte ptr ds:[ITEM_X], bl 		;x2 = x1 + (w1 - w2)

	; swap width and height
	mov al, byte ptr ds:[ITEM_WIDTH]
	xchg al, byte ptr ds:[ITEM_HEIGHT]
	mov byte ptr ds:[ITEM_WIDTH], al
	
	;set current rotate state
	inc byte ptr ds:[ITEM_ROTATE]		;set 3
  	jmp end_rotate_item
  
	rotate_item_third:
	; swap width and height 
	mov al, byte ptr ds:[ITEM_WIDTH]
	xchg al, byte ptr ds:[ITEM_HEIGHT]
	mov byte ptr ds:[ITEM_WIDTH], al

	;set current rotate state
	mov byte ptr ds:[ITEM_ROTATE], 0	;set 0
	
	end_rotate_item:
	
	pop dx
	pop bx
	pop ax
	ret
endp

check_side proc
	push ax
	push bx
	push cx
	xor ax, ax
	xor bx, bx
	xor cx, cx

	mov al, byte ptr ds:[ITEM_Y] ; y
	mov bl, byte ptr ds:[ITEM_X] ; x
	call convert_to_offset ; ax = `y` & bx = 'x' => dx = calculated offset
	mov bx, dx
	sub bx, 2

	mov al, byte ptr ds:[ITEM_WIDTH]
	mov cl, 2
	mul cl
	add ax, 2

	mov cl, byte ptr ds:[ITEM_HEIGHT]
	mov dl, byte ptr ds:[ITEM_CHAR]
	check_side_height_loop:
		cmp byte ptr es:[bx], dl
		je disable_side_move

		add bx, ax

		cmp byte ptr es:[bx], dl
		je disable_side_move

		add bx, SCREEN_WIDTH
		sub bx, ax
	loop check_side_height_loop
	mov dx, 0
	jmp end_check_side

	disable_side_move:
	mov dx, 1

	end_check_side:
	pop cx
	pop bx
	pop ax
	ret
endp

move_item proc ;accept scan_code in `ah`
	pusha
	call check_for_borders
	cmp dx, 1
	je create_new_item

	call clear_current_item

	cmp ah, LEFT_KEY
	je move_item_left
	
	cmp ah, RIGHT_KEY
	je move_item_right

	jmp move_item_down

	move_item_left:
		cmp byte ptr ds:[ITEM_X], LEFT_LIMIT
		jbe move_item_down

		call check_side
		cmp dx, 1
		je move_item_down

		sub byte ptr ds:[ITEM_X], 1
		jmp move_item_down

	move_item_right:
		xor dx, dx
		xor bx, bx

		mov dl, byte ptr ds:[ITEM_X]
		mov bl, byte ptr ds:[ITEM_WIDTH]
		add dx, bx

		cmp dx, RIGHT_LIMIT
		jae move_item_down

		call check_side
		cmp dx, 1
		je move_item_down

		add byte ptr ds:[ITEM_X], 1
		jmp move_item_down		

  	move_item_down:
		add byte ptr ds:[ITEM_Y], 1
		call print_current_item
		popa
		ret

	create_new_item:
	pop dx
	pop bx
	pop ax
	
	mov byte ptr ds:[ITEM_ROTATE], 0
	mov byte ptr ds:[NEW_ITEM], 1
	call check_completed_rows

	popa
	ret
endp

; drop_item proc ;accept scan_code in `ah`
; 	call check_for_borders
; 	cmp dx, 1
; 	je create_new_item_drop

; 	call clear_current_item

; 	push ax
; 	push bx
; 	push cx

; 	xor ax, ax
; 	xor bx, bx
; 	xor cx, cx

; 	mov al, ds:[ITEM_Y] ; y
; 	mov bl, ds:[ITEM_X] ; x
; 	call convert_to_offset ; ax = `y` & bx = 'x' => dx = calculated offset
	
; 	mov ax, dx
; 	mov 

; 	loop 

; 	mov cl, ds:[ITEM_HEIGHT]

; 	drop_item_loop:
; 		push cx
; 		add bx, SCREEN_WIDTH
; 		mov cl, ds:[ITEM_WIDTH]

; 		push bx
; 		drop_item_inner_loop:
; 			mov ax, word ptr ds:[GRAY_SYMBOL]
; 			cmp ax, word ptr es:[bx]
; 			je drop_item_start

; 			mov al, byte ptr ds:[ITEM_CHAR]
; 			cmp al, byte ptr es:[bx]
; 			je drop_item_start

; 			add bx, 2
; 		loop drop_item_inner_loop

; 		pop bx
; 		pop cx
; 	loop drop_item_loop
	
; 	drop_item_start:
; 	create_new_item_drop:

; 	pop cx
; 	pop bx
; 	pop ax

; 	mov byte ptr ds:[NEW_ITEM], 1
; 	ret
; endp
			
start proc
	init
	call print_layout
	game_loop: ;endless loop
	   call app
	jmp game_loop

	exit
	ret
endp
