int08  EQU  08    ;8253 T0 定时器中断号
int09  EQU  09    ;Keyboard 键盘中断号
irq_mask8_15  equ 0FFH	;中断掩码
irq0_mask equ 11111110B
irq1_mask equ 11111101B


;Scancode of PC key
;Scancode of PC key
KRShiftScan  equ 36H;Shift(右)
KLShiftScan  equ 2aH;Shift(左)
KESCUP equ 81H
KESCDOWN EQU 01H
K1UP EQU 82H
K1DOWN EQU 02H
K2UP EQU 83H
K2DOWN EQU 03H
K3UP EQU 84H
K3DOWN EQU 04H
K4UP EQU 85H
K4DOWN EQU 05H
K5UP EQU 86H
K5DOWN EQU 06H
K6UP EQU 87H
K6DOWN EQU 07H
K7UP EQU 88H
K7DOWN EQU 08H
K8UP EQU 89H
K8DOWN EQU 09H
K9UP EQU 8aH
K9DOWN EQU 0aH
KQUP EQU 90H
KQDOWN EQU 10H
KWUP EQU 91H
KWDOWN EQU 11H
KEUP EQU 92H
KEDOWN EQU 12H
KRUP EQU 93H
KRDOWN EQU 13H
KTUP EQU 94H
KTDOWN EQU 14H
KYUP EQU 95H
KYDOWN EQU 15H


CNTdispP EQU 160*0+44	;窗口中显示点阵的起始位置
PKeyDispP EQU 160*20+30	;琴键在窗口中的位置
ScreenDispP EQU 160*12+30
ScreenDispP1 EQU 160*16+30





data  segment
;Interrupt vectors 4 INT08 & INT09
;频率值
freq   DW 262,294,330,349,392,440,494,523,587,659,698,784,880,988,1047;
ptr0	DW ? ;指针
Vflag   DW 0  ;是否允许输出
cs08  DW 0
ip08  DW 0
cs09  DW 0
ip09  DW 0
mini1 DB 0h
mini2 DB 0h
sec1 DB 0h
sec2 DB 0h
sec3 DB 0h
sec4 DB 0h
t0  dw 0      ;for basic counting
kbflag db 0        ;Quit flag
headP_DI DW 0      ;bedining position for VRAM (ES:) DI
firstkey db 0h

s1$ db 0dH,0aH,0dh,0aH
    db '-------------------ELECTRONIC KEYBOARD-------------------',0dH,0AH,0DH,0AH
    db '		 08118123 Tianhao Zhang			',0DH,0AH
	db '		 08118116 Jiajie  Wu			',0DH,0AH,0DH,0AH
	db '	   	 [1]Press any key to start 	',0DH,0AH,0DH,0AH
	db '	   	 [2]Press ESC to EXIT				',0DH,0AH,0DH,0AH,24H

s2$ db 0dH,0aH
    db ' VGA:Hit any key to start or go on, ESC to quit ',0dh,0ah,24H


STRING DB 0DH,0AH,'Press A to Replay; Press Other Keys To Exit', 0DH,0AH,'$'

sBF0$ db ' Steinway&Sons PIANO '

SCLR  DB  0,0  ;clear a char with black color

;7*9 dotmatrix for '0'-'9'
Ndotmatrix db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H   ;'0'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,0FEH,0FEH
           db 0FEH,00H,00H,00H,0FEH,00H,0FEH
           db 0FEH,00H,00H,0FEH,00H,00H,0FEH
           db 0FEH,00H,0FEH,00H,00H,00H,0FEH
           db 0FEH,0FEH,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
Nm1        db 00H,00H,00H,0FEH,00H,00H,00H    ;'1'
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,00H,0FEH,0FEH,00H,00H,00H
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
Nm2        db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H   ;'2'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,0FEH,00H
           db 00H,00H,00H,0FEH,0FEH,00H,00H
           db 00H,00H,0FEH,00H,00H,00H,00H
           db 00H,0FEH,00H,00H,00H,00H,00H
           db 0FEH,00H,00H,00H,00H,00H,00H
           db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH
Nm3        db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H   ;'3'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,0FEH,0FEH,0FEH,0FEH,00H
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
Nm4        db 00H,00H,00H,00H,00H,0FEH,00H    ;'4'
           db 00H,00H,00H,00H,0FEH,0FEH,00H
           db 00H,00H,00H,0FEH,00H,0FEH,00H
           db 00H,00H,0FEH,00H,00H,0FEH,00H
           db 00H,0FEH,00H,00H,00H,0FEH,00H
           db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH
           db 00H,00H,00H,00H,00H,0FEH,00H
           db 00H,00H,00H,00H,00H,0FEH,00H
           db 00H,00H,00H,00H,00H,0FEH,00H
Nm5        db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH   ;'5'
           db 0FEH,00H,00H,00H,00H,00H,00H
           db 0FEH,00H,00H,00H,00H,00H,00H
           db 0FEH,00H,00H,00H,00H,00H,00H
           db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
Nm6        db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H   ;'6'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,00H
           db 0FEH,00H,00H,00H,00H,00H,00H
           db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
Nm7        db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH   ;'7'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,0FEH,00H
           db 00H,00H,00H,00H,0FEH,00H,00H
           db 00H,00H,00H,0FEH,00H,00H,00H
           db 00H,00H,0FEH,00H,00H,00H,00H
           db 00H,00H,0FEH,00H,00H,00H,00H
           db 00H,00H,0FEH,00H,00H,00H,00H
           db 00H,00H,0FEH,00H,00H,00H,00H
Nm8        db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H   ;'8'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
Nm9        db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH   ;'9'
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 00H,00H,00H,00H,00H,00H,0FEH
           db 0FEH,00H,00H,00H,00H,00H,0FEH
           db 00H,0FEH,0FEH,0FEH,0FEH,0FEH,00H
      

Colon		db 00H,00H,00H,00H,00H		;':'
			db 00H,00H,00H,00H,00H
			db 00H,00H,0FEH,00H,00H
			db 00H,00H,00H,00H,00H
			db 00H,00H,00H,00H,00H
			db 00H,00H,00H,00H,00H
			db 00H,00H,0FEH,00H,00H
			db 00H,00H,00H,00H,00H
			db 00H,00H,00H,00H,00H

PKey		db 00h,00h
			db 00h,00h
			db 00h,00h

PKey0		db 00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			
			

PKey1		db 0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H

PKey2		db 0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H

PKey3		db 0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H

PKey4		db 0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H

PKey5		db 0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H,00H

PKey6		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H,00H

PKey7		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H,00H

PKey8		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H,00H

PKey9		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H,00H

PKeQ		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H,00H

PKeyW		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H,00H

PKeyE		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H,00H

PKeyR		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H,00H

PKeyT		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,00H

PKeyY		db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH
			db 0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH,0FEH
data ends

stacks segment stack
       db 256 dup (?)
stacks ends

code segment
       assume cs:code,ds:data,ss:stacks
main proc far
start:  mov ax,data
        mov ds,ax
        mov ax,stacks
        mov ss,ax
        mov sp,256
		
P0:     mov dx,offset s1$
        mov ah,9
        int 21H
        mov ah,1
        int 21H
        cmp al,1BH
        jnz VRAM0
        MOV AX,4C00H
        INT 21H    ;exit
		
		;Filling buffers in VRAM B800:0-FFFH(the Page#0)
VRAM0:  mov ah,0
        mov al,03H
        int 10H  ;设置为彩色文本80*25模式
		
		
		
		mov ax,0B800H
        mov es,ax
        mov ah,1
        int 21H
        cmp al,1bH
        jnz VRAM1
        JMP VRAMEXIT      ;EXIT
		
VRAM1:  
        mov al,0
		mov mini1,al
		mov mini2,al
        mov sec1,al     ;count initialization values
        mov sec2,al
        mov sec3,al
        mov sec4,al
        mov kbflag,al
		
		call tcntdisp   		;disp counter	
		call showkey
        call showScreen
		call showScreenP
		call KBInt0    		;IRQ1 & INT09Hfor KB interrupt Initialization
			
		call T0Int0		;8253 T0 /IRQ0/INT08H 初始化

        
		;在原版中下面几行是用来显示短横的，但这两条指令要留着
        mov ax,seg s2$
        mov ds,ax


		
VRAM3:  call tcntdisp           	;refresh cnt 
		call PressKey
		call genVoice
		call ScreenC
        call delay              	;waiting next time for display refresh
        cmp kbflag,0ffh        	;Quit？
        jnz VRAM3               	;routine tasks

        call INT08_RST         	;Restore old INT 08 interrupt Vector
        call INT09_RST        	;Restore old INT 09 interrupt Vector
        mov ah,0
        int 16H      ;any key
		
VRAMEXIT:mov ah,6    ;clear screen or page scrolling up (to next page)
        mov al,0    ;scroll up lines
        mov bh,7    ;widows bottom lines attr. 
        mov cx,0    ;windows up-left conner position: CH=Row,CL=Column No
        mov dx,184FH;right bottom conner
        int 10H
		mov AX,4C00H
        int 21H
        JMP p0
	
;display reverse counter
tcntdisp  proc
         push es
         push di
         push cx

;显示点阵
;display the timer 1 rev.count
         mov ax,0B800H
         mov es,ax
         mov al,mini1		;第1个点阵显示的内容,分钟十位
         mov ah,7*9
         mul ah			
         mov bx,offset Ndotmatrix
         add bx,ax    		;point to 0-9 matrix
         mov di,CNTdispP    	;CntdispPosition, es:di对应位置, ds:bx对应字符点阵
         mov ah,04H         	;attr.byte
         call dispdotmatrix   	;disp the dot matrix

         mov ax,0B800H
         mov es,ax
         mov al,mini2		;第2个点阵显示的内容,分钟个位
         mov ah,7*9
         mul ah
         mov bx,offset Ndotmatrix
         add bx,ax    		
         mov di,CNTdispP+16 	
         mov ah,04H          	
         call dispdotmatrix   	
        
		 mov ax,0B800H
         mov es,ax
         mov al,10		;显示点阵冒号
         mov ah,7*9
         mul ah
         mov bx,offset Ndotmatrix
         add bx,ax    		
         mov di,CNTdispP+30 	
         mov ah,04H          	
         call dispdotmatrix_colon 
		
         mov ax,0B800H
         mov es,ax
         mov al,sec1		;第3个点阵显示的内容,秒十位
         mov ah,7*9
         mul ah
         mov bx,offset Ndotmatrix
         add bx,ax    		
         mov di,CNTdispP+42     
         mov ah,04H          	
         call dispdotmatrix 
		 
		 mov ax,0B800H
         mov es,ax
         mov al,sec2		;第4个点阵显示的内容,秒个位
         mov ah,7*9
         mul ah
         mov bx,offset Ndotmatrix
         add bx,ax    		
         mov di,CNTdispP+58    
         mov ah,04H          	
         call dispdotmatrix 
		 
		 
 pop cx
         pop di
         pop es
         ret
tcntdisp endp	
	

;产生声音
genVoice proc near
		push si
		push di 
		push ax
		push dx
		mov si, ptr0
		mov di, freq[si]

		mov al,0b6h    ;设置8253通道2方式3二进制计数
        out 43h,al
        mov dx,12h
        mov ax,12928   ; DX AX=896*533h=18*65536+12928=1.19M
        div di
        out 42h,al     ;8253通道2口设置初值
        mov al,ah
        out 42h,al
        in al,61h       ;读8255PB口
        mov ah,al
		cmp Vflag,0
		jz NotAllow
        or al,3
        out 61h,al
		jmp back
NotAllow:and al, 0fdh
		out 61h,al

back:	pop dx
		pop ax
		pop di
		pop si
		ret
genVoice endp

;显示琴键
showKey proc near
		push si
		push di 
		push ax
		push cx
		push dx
		mov ax, 0B800H
		mov es,ax
		mov si,0
nextKey:mov ax,6
		mul	si	
		mov bx,offset PKey
		add ax,PKeyDispP
		mov di,ax
		mov ah,077h
;显示
		mov cl,3 ;lines
showKey1:mov ch,2
          push di
showKey2:mov al,[bx]
          mov es:[di],ax       ;one dot character
          add di,2
          inc bx
          dec ch
          jnz showKey2
          pop di
          add di,160           ;to next line
          dec cl
          jnz showKey1
		inc si
		cmp si,14
		jbe nextKey
		pop dx
		pop cx
		pop ax
		pop di
		pop si
		ret
showKey endp



;按压琴键
pressKey proc near
		push si
		push di 
		push ax
		push cx
		push dx
		mov ax, 0B800H
		mov es,ax
		mov si,0
npKey:mov ax,6
		mul	si	
		mov bx,offset PKey
		add ax,PKeyDispP
		mov di,ax
		mov ax,ptr0
		shr ax,1
		cmp si,ax
		jnz notchange
		cmp Vflag,0
		jz  notchange
		mov ah,044h
		jmp change
notchange:mov ah,077h
;显示
change:	mov cl,3 ;lines
pressKey1:mov ch,2
          push di
pressKey2:mov al,[bx]
          mov es:[di],ax       ;one dot character
          add di,2
          inc bx
          dec ch
          jnz pressKey2
          pop di
          add di,160           ;to next line
          dec cl
          jnz pressKey1
		inc si
		cmp si,14
		jbe npKey
		pop dx
		pop cx
		pop ax
		pop di
		pop si
		ret
pressKey endp


;显示音阶
showScreen proc near
		push si
		push di 
		push ax
		push cx
		push dx
		mov ax, 0B800H
		mov es,ax
		mov di,ScreenDispP
		mov ah,77h
		
;显示
		mov cl,2 ;lines
showScreen1:mov ch,15
          push di
showScreen2:mov al,0FEh
          mov es:[di],ax       ;one dot character
          add di,6
          inc bx
          dec ch
          jnz showScreen2
          pop di
          add di,160          ;to next line
          dec cl
          jnz showScreen1
		pop dx
		pop cx
		pop ax
		pop di
		pop si
		ret
showScreen endp

showScreenP proc near
		push si
		push di 
		push ax
		push cx
		push dx
		mov ax, 0B800H
		mov es,ax
		mov di,ScreenDispP1
		mov ah,44h
		
;显示
		mov cl,2 ;lines
showScreenP1:mov ch,15
          push di
showScreenP2:mov al,0FEh
          mov es:[di],ax       ;one dot character
          add di,6
          inc bx
          dec ch
          jnz showScreenP2
          pop di
          add di,160          ;to next line
          dec cl
		jnz showScreenP1
		pop dx
		pop cx
		pop ax
		pop di
		pop si
		ret
showScreenP endp

ScreenC proc near
		mov ax,0B800H
         mov es,ax

         mov ax,ptr0		
		 shr ax,1
		 inc al
         mov ah,2*15
         mul ah			
         mov bx,offset Pkey0
		 cmp Vflag,0
		 jz notchanges
		 add bx,ax    		
notchanges:mov di,ScreenDispP    	
        
		 mov cl,2   ;lines
SCREEN1:mov ch,15
          push di
SCREEN2:mov al,[bx]
		  cmp al,0FEH
		  jnz  notchangeC
		  mov ah,044h
		  jmp beginP
notchangeC:mov ah,077h
beginP:   mov es:[di],ax       ;one dot character
          add di,6
          inc bx
          dec ch
          jnz SCREEN2
          pop di
          add di,160           ;to next line
          dec cl
          jnz SCREEN1
		  ret
ScreenC endp

;display a 7*9 dotmatrix for a number 0-9;
;ah=disp attribute byte
;es:di=VRAM position
;ds:bx=number's dot matrix
;used none 
dispdotmatrix proc
         push cx
         push di
         push bx
         push ax
         mov cl,9   ;lines
dispdotm1:mov ch,7
          push di
dispdotm2:mov al,[bx]
          mov es:[di],ax       ;one dot character
          add di,2
          inc bx
          dec ch
          jnz dispdotm2
          pop di
          add di,160           ;to next line
          dec cl
          jnz dispdotm1
          pop ax
          pop bx
          pop di
          pop cx
          ret
dispdotmatrix endp

;显示冒号
dispdotmatrix_colon proc
         push cx
         push di
         push bx
         push ax
         mov cl,9   ;lines
dispdotm_colon1:mov ch,5
          push di
dispdotm_colon2:mov al,[bx]
          mov es:[di],ax       ;one dot character
          add di,2
          inc bx
          dec ch
          jnz dispdotm_colon2
          pop di
          add di,160           ;to next line
          dec cl
          jnz dispdotm_colon1
          pop ax
          pop bx
          pop di
          pop cx
          ret
dispdotmatrix_colon endp
	
;T0 中断服务程序：计数器+1
INT08_proc proc far
       sti
       push ax
       push ds
       mov ax,data
       mov ds,ax

cmp firstkey,1h
jz next0
mov t0,0

next0:
mov ax,t0
cmp ax,10;0.1s
jnc INT08P_1;到0.1s则跳转
inc t0
jmp EXIT

INT08P_1:
mov t0,0
cmp sec3,9
jnz INT08P_B;sec3之前计数不到9则直接继续计数
mov sec3,0
jmp INT08P_2

INT08P_B:
inc sec3
jmp EXIT

INT08P_2:
cmp sec2,9
jnz INT08P_C
mov sec2,0
jmp INT08P_3

INT08P_C:
inc sec2
jmp EXIT

INT08P_3:
cmp sec1,5
jnz INT08P_D
mov sec1,0
jmp INT08P_4

INT08P_D:
inc sec1
jmp EXIT

INT08P_4:
cmp mini2,9
jnz INT08P_E
mov mini2,0
jmp INT08P_5

INT08P_E:
inc mini2
jmp EXIT

INT08P_5:
cmp mini1,5
jnz INT08P_F
mov mini1,0
jmp EXIT

INT08P_F:
inc mini1

EXIT:
		mov al,20h ;Send EOI
        out 0a0h,al
        out 20h,al
        pop ds
        pop ax
        iret
INT08_proc endp	   
	
;按键中断服务程序
INT09_proc proc far
       sti
       push ax
       push ds
       push di
	   push si
       mov ax,data
       mov ds,ax
		mov di,headP_DI   	;current VRAM address
		in al,60H         		;get key scancode
		
		cmp al,K1DOWN
		jb outscale
		cmp al,K9DOWN
		jbe INT09_inscale
		cmp al,KQDOWN
		jb outscale
		cmp al,KYDOWN
		jbe INT09_inscale1
outscale:cmp al,KESCDOWN   	;ESC键按下
		jnz INT09_ELSE		;其他情况
		mov al,0ffh
		mov kbflag,al      	;退出标志
        jmp INT09_ELSE
INT09_inscale1:
		sub al,KQDOWN
		add al,09h
		jmp ensureptr
INT09_inscale:
		sub al,K1DOWN
ensureptr:mov ah,0
		shl ax,1
		mov ptr0,ax
		jmp INT09_END
INT09_ELSE:mov Vflag,0
			nop
			jmp INT09_END1
INT09_END:mov Vflag,1
		  mov firstkey,1h
INT09_END1:	mov al,20h ;Send EOI
        out 0a0h,al
        out 20h,al
		pop si
        pop di
        pop ds
        pop ax
        iret
INT09_proc endp	
;;  T0--IRQ0-INT 08H 系统定时器8253（地址40-43H）初始化
;；方式3，40/20ms,中断服务程序INT08_PROC
T0INT0  proc
       push es
       ;初始化8253
       cli
       mov al,36H
       out 43h,al
       mov AX,11900   ;1.19*10000=11900,10ms,100Hz;
       out 40H,al
       mov al,ah
       out 40H,al  

       ;手动保护n=08H对应的中断向量
       mov ax,0       
       mov es,ax
       mov al,int08	;保存原中断向量
       mov ah,4
       mul ah        	;中断矢量偏移量地址
       mov si,ax
       mov ax,es:[si+2] 
       mov cs08,ax	
       mov bx,es:[si]   ;0000:0020H
       mov ip08,bx

       ;设置新中断向量
       mov ax,cs 
       mov es:[si+2],ax
       mov dx,offset int08_proc
       mov es:[si],dx
       in al, 21h ;设置中断掩码
       and al, irq0_mask
       out 21h, al
       in al, 0a1h
       and al, irq_mask8_15
       out 0a1h, al
       pop es
       sti
       ret
T0int0  endp

;；键盘按键/抬起变化中断--IRQ1-INT 09H，初始化
;；扫描码端口：60H,中断服务程序INT09_PROC

KBINT0  proc
       push es
       cli

       mov al,int09;保存原中断向量
       mov ah,35h
       int 21h
       mov ax,es
       mov cs09,ax
       mov ip09,bx
       mov ax,cs ;设置新中断向量
       mov ds,ax
       mov dx,offset int09_proc
       mov al,int09
       mov ah,25h
       int 21h

       in al, 21h ;设置中断掩码
       and al,irq1_mask
       out 21h, al
       in al, 0a1h
       and al, irq_mask8_15
       out 0a1h, al
       pop es
       sti
       ret
KBint0  endp
	
	;;restore Int08 原来的中断矢量
INT08_Rst PROC 
         cli
         mov bl, irq0_mask ;恢复中断掩码
         not bl
         in al, 21h
         or al, bl
         out 21h, al
         mov bl, irq_mask8_15
         not bl
         in al, 0a1h
         or al, bl
         mov ax,0
         mov es,ax
         mov si,20H
         mov dx,ip08 ;恢复原中断向量
         mov es:[si],dx      ;V offset restore
         mov ax,cs08
         mov es:[si+2],ax    ;V seg restore
         sti
         RET
INT08_RST ENDP

;;恢复Int09 原来的中断矢量
INT09_Rst PROC 
         cli
         mov bl, irq1_mask ;恢复中断掩码
         not bl
         in al, 21h
         or al, bl
         mov bl, irq_mask8_15
         not bl
         in al, 0a1h
         or al, bl
         out 0a1h, al
         mov dx,ip09 ;恢复原中断向量
         mov ax,cs09
         mov ds,ax
         mov ah,25h
         mov al,int09
         int 21h
         sti
         RET
INT09_RST ENDP


delay proc
        push bx
        mov bx,500            ;延时
delay1:  dec bx
        jnz delay1
        pop bx
        ret
delay   endp
	
main endp
code ends
end start	