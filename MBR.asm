;-----------------------------------------------------------------------------------------------------------------------
;
;	File:		mbr.asm
;
;   Tools and Information Which has used:
;   http://en.wikipedia.org/wiki/Master_boot_record
;   https://codereview.stackexchange.com/questions/94220/the-loaderless-bootloader
;   http://www.hex-rays.com/idapro/
;   https://github.com/qb40/hexit?files=1
;   http://www.sevyn.com/pro_os_001.html
;   https://en.wikibooks.org/wiki/First_steps_towards_system_programming_under_MS-DOS_7/Appendix
;   https://countuponsecurity.com/2017/07/02/analysis-of-a-master-boot-record-eternalpetya/
;   https://wiki.osdev.org/Memory_Map_(x86)
;   https://www.gonwan.com/2013/10/17/bios-boot-sequence/
;   http://samuelkerr.com/?p=262
;
;	Description:	This program is QNX 2.21 BootLoader.
;
;			a valid boot sector is defined with code to load the operating system kernel
;			program from the disk image.
;
;			This source can assemble into a single boot-sector or an entire floppy disk image file. To
;			output the disk image file, include the line "%define BUILDDISK" at the top of the file or
;			include -DBUILDDISK in the NASM command line.
;
;	Revised:	February 05, 2020
;
;	Assembly:	nasm mbr.asm -f bin -o mbr.flp -l mbr.lst -DBUILDDISK
;
;			Copyright (C) 2020 Ehsan Ghasemlou. All Rights Reserved.
;
;-----------------------------------------------------------------------------------------------------------------------
;0000h - 01B7h       Code Area (440 bytes)
;01B8h - 01BBh       Disk Signature (4 bytes)
;01BCh - 01BDh       Generally Zeroed out (2 bytes)
;01BEh - 01FDh       List of Partition Records (4 * 16-byte structures)
;01FEh - 01FFh       MBR Signature (2 bytes - Must be AA55h)
;-----------------------------------------------------------------------------------------------------------------------
;
;	Equates
;
;	The equate (equ) statements define symbolic names for fixed values so that these values can be defined and
;	verified once and then used throughout the code. Equate names are always capitalized and are the only symbolic
;	names that begin with the letter E.
;
;-----------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------
;
;	Loader Constants
;
;-----------------------------------------------------------------------------------------------------------------------
    EMAXTRIES		equ	0x5					    	;max read retries
    EFATBUFFER		equ	0x400						;fat i/o address
;-----------------------------------------------------------------------------------------------------------------------
;
;	BIOS Interrupts								    EBIOS...
;
;-----------------------------------------------------------------------------------------------------------------------
    EBIOSINT16	    	equ	0x16					;BIOS video services interrupt
    EBIOSTTYOUTPUTFN	equ	0x14					;BIOS video TTY output function
    EBIOSKEYBOARDINT	equ	0x22					;BIOS keyboard services interrupt
    EBIOSWAITFORKEYFN	equ	0x0						;BIOS keyboard wait for key function
;-----------------------------------------------------------------------------------------------------------------------
;
;	8042 Keyboard Controller						EKEY...
;
;	The 8042 Keyboard Controller (8042) is a programmable controller that accepts input signals from the keyboard
;	device and signals a hardware interrupt to the CPU.
;
;-----------------------------------------------------------------------------------------------------------------------
    EKEYPORTSTAT		equ	0x064				    ;8042 status port
;-----------------------------------------------------------------------------------------------------------------------
;
;	Boot Sector Code							@disk: 000000	@mem: 007C00
;
;	The first sector of the disk is the boot sector. The BIOS will load the boot sector into memory and pass
;	control to the code at the start of the sector. The boot sector code is responsible for loading the operating
;	system into memory. Our boot sector assumes only that the operating system code is referenced by a directory
;	entry for a program called OS.COM. Our boot sector code will search the directory and read the file allocation
;	table to locate all sectors containing OS.COM code.
;
;-----------------------------------------------------------------------------------------------------------------------                  
;cpu	8086						                ;assume minimum CPU
;section			boot	vstart=0100h	    	;emulate .COM (CS,DS,ES=PSP)
;bits	16						                    ;16-bit code at power-up
;Boot			jmp	word Boot.10			        ;jump over parameter table
;-----------------------------------------------------------------------------------------------------------------------
;	Disk Parameter Table
;-----------------------------------------------------------------------------------------------------------------------
;                   db	'QNX'    	    		;disk parameters label
;SectorBytes		dw	512						;bytes per sector
;ClusterSectors		db	1						;sectors per cluster
;ReservedSectors	dw	1						;reserved sectors
;FatCount		    db	2						;file allocation table copies
;DirEntries		    dw	224						;max directory entries
;DiskSectors		dw	2880					;sectors per disk
;DiskType		    db	0f0h					;1.44MB
;FatSectors		    dw	9						;sectors per FAT copy
;TrackSectors		dw	18						;sectors per track
;DiskSides		    dw	2						;sides per disk
;SpecialSectors		dw	0						;special sectors
;-----------------------------------------------------------------------------------------------------------------------
;	BIOS will load this sector at absolute address 7c00. But we don't assume the contents of CS:IP because BIOS
;	code varies - could be 0:7c00, 700:c00, 7c0:0, etc. First get the absolute addr in BX.
;-----------------------------------------------------------------------------------------------------------------------
[org 0x7c00]                ; Our BootLoader will be load on 0x7c00 - 0x7e00
cli                         ; Clear interrupt flag, interrupts disabled when interrupt flag cleared.
xor ax,ax                   ; Clear ax, we could use mov ax,0 but it has more bytes after assembeling but the same effect
mov ss,ax                   ; Clear ss, there is no direct way to initialaize Stack Segment register
mov sp,0x7c00               ; Set Stack Pointer to 07c00h, there is no guard for stack, although it is growing in different direction
mov si, sp
mov es,ax                   ; Clear Extra Segment
;push ax
;pop es

mov ds,ax                   ; Clear Data Segment
;push ax
;pop ds
sti                         ; Set interrupt flag; external, maskable interrupts enabled at the end of the next instruction.

cld                         ; Clear DF flag.
mov di,0x600                ; DI register is Destination Pointer but why 0x600 ; This is the destination for the copy                                   
mov cx,0x100                ; Why 100 byte? maybe it is copying from bios Data? ; We want to copy 200h bytes, which is the length of half sector, i.e. the whole MBR.
                            ; mov     si, 7C00h       ; Source for the copy





repne                       ; Repeats a string instruction the number of times specified in the count register ((E)CX) or until the indicated condition of the ZF flag is no longer met. (repeat while not equal),
movsw                       ; Move word at address DS:SI to address ES:DI

;to fix
;jmp 0x0:0x601+ $
jmp 0x0:0x61D             ; we will run from 0x0:0x61D which is next line 
                            ; push    61Dh
                            ; retf
mov si,0x7BE                ; This is the offset to the first partition record. In the MBR structure, the first partition record is at base+1BEh, so it is 600h + 1BEh = 7BEh
mov bl,0x4                  ; This used for the loop. We only want to examine 4 partition records

NextPartition:
cmp byte [si+0x0],0x80    ; Compares the status byte to 080h
jz FoundBootableEntry                    ;
cmp byte [si+0x0],0x0       ;
jnz  PrintInvalidPartitionTable              ;
add si,0x10                 ;
dec bl                      ;
jnz NextPartition              ;

;to fix
int 0x18                    ;ROM-BASIC (only available on
                                          ; some IBM machines!) Many BIOS
                                          ; simply display "PRESS A
                                          ; KEY TO REBOOT" when an
                                          ; Interrupt 18h is executed

FoundBootableEntry:
mov dx,[si+0x0]             ; Drive  -> DL /   Head -> DH
                                          ; For the standard MBR code,
          ; DL will always be 80h; which means ONLY the first drive can
          ; be bootable! [ This part of the code is often changed by MBR
          ; replacements to boot from another (second, etc.) drive! ]
mov cx,[si+0x2]             ;
mov bp,si                   ;
IfNotMoreThanOneNoneZeroTestNext:
add si,0x10                 ;
dec bl                      ;
jz OneActiveandThreeNonActiveRead               ;
cmp byte [si+0x0],0x0       ;
jz IfNotMoreThanOneNoneZeroTestNext               ;
PrintInvalidPartitionTable:
mov si,INVALID                ;
PrintSiHalt:
call PrintSi                  ;
jmp $              ;

PrintSi:
push cx                     ;
xor ch,ch                   ;
lodsb                       ;Load byte at address DS:(E)SI into AL
mov cl,al                   ;
NextChar:
lodsb                       ;Load byte at address DS:(E)SI into AL
call OutCharAl                  ;
loop NextChar                   ;
pop cx                      ;
ret                         ;


;--------b-100E--CXABCD-----------------------
;INT 10 - V20-XT-BIOS - TELETYPE OUTPUT WITH ATTRIBUTE
;	AH = 0Eh
;	CX = ABCDh
;	BP = ABCDh
;	AL = character to write
;	BH = page number
;	BL = foreground color (text modes as well as graphics modes)
;Return: nothing
;Program: V20-XT-BIOS is a ROM BIOS replacement with extensions by Peter
;	  Koehlmann / c't magazine
;Desc:	display a character on the screen, advancing the cursor and scrolling
;	  the screen as necessary
;Notes:	characters 07h (BEL), 08h (BS), 0Ah (LF), and 0Dh (CR) are interpreted
;	  and do the expected things
;SeeAlso: INT 15/AH=84h"V20-XT-BIOS"
;--------V-100F-------------------------------

OutCharAl:
push ax
push bx
push cx
push si
push bp
mov bx,0x07                  ; BH = Page Number, BL = Color (only in graphic mode)
mov ah,0x0E                  ; Teletype output AH=0Eh
int 0x010                    ; Write Character in TTY Mode -  AL = Character, 
pop bp
pop si
pop cx
pop bx
pop ax
ret    


OneActiveandThreeNonActiveRead:
;fix 7EE to 7BE
;mov si,0x7BE                ; second partition address 0x600+0x1EE
mov si,0x7EE
;fix si,bp to bp,si
;sub bp,si                   ; bp is 0x7BE so 0x7EE - 0x7BE = 0x30
sub si,bp
;change all si to bp
;shr bp,0x1                  ;
;shr bp,0x1                  ;
;shr bp,0x1                  ;
;shr bp,0x1                  ; can be shr si, 0x4
shr si,0x1                  ;
shr si,0x1                  ;
shr si,0x1                  ;
shr si,0x1                  ; can be shr si, 0x4
mov ax,0x31                 ; it is the ascii code of 1 
;add ax,bp                   ; after four times of shift right si should be 1 and one plus one is two
add ax,si                   ; after four times of shift right si should be 1 and one plus one is two
mov [PARNUM],al             ;
mov si,LOADER                ;
call PrintSi                  ;

;--------B-1600-------------------------------
;INT 16 - KEYBOARD - GET KEYSTROKE
;	AH = 00h
;Return: AH = BIOS scan code
;	AL = ASCII character
;Notes:	on extended keyboards, this function discards any extended keystrokes,
;	  returning only when a non-extended keystroke is available
;	the BIOS scan code is usually, but not always, the same as the hardware
;	  scan code processed by INT 09.  It is the same for ASCII keystrokes
;	  and most unshifted special keys (F-keys, arrow keys, etc.), but
;	  differs for shifted special keys
;	some (older) clone BIOSes do not discard extended keystrokes and manage
;	  function AH=00h and AH=10h the same
;	the K3PLUS v6.00+ INT 16 BIOS replacement doesn't discard extended
;	  keystrokes (same as with functions 10h and 20h), but will always
;	  translate prefix E0h to 00h. This allows old programs to use extended
;	  keystrokes and should not cause compatibility problems
;SeeAlso: AH=01h,AH=05h,AH=10h,AH=20h,AX=AF4Dh"K3PLUS",INT 18/AH=00h
;SeeAlso: INT 09,INT 15/AH=4Fh

mov cx,0xAFC8               ; why is that number
CheckTimeOut:
mov ah,0x1                  ;
push bp                     ;
push cx                     ;
int EBIOSINT16                    ;
pop cx                      ;
pop bp                      ;
jnz KeyPressed              ;
loop CheckTimeOut                   ;
jmp Dfault              ; if after 0xAFC8 times the key was not released 

;--------B-1601-------------------------------
;INT 16 - KEYBOARD - CHECK FOR KEYSTROKE
;	AH = 01h
;Return: ZF set if no keystroke available
;	ZF clear if keystroke available
;	    AH = BIOS scan code
;	    AL = ASCII character
;Note:	if a keystroke is present, it is not removed from the keyboard buffer;
;	  however, any extended keystrokes which are not compatible with 83/84-
;	  key keyboards are removed by IBM and most fully-compatible BIOSes in
;	  the process of checking whether a non-extended keystroke is available
;	some (older) clone BIOSes do not discard extended keystrokes and manage
;	  function AH=00h and AH=10h the same
;	the K3PLUS v6.00+ INT 16 BIOS replacement doesn't discard extended
;	  keystrokes (same as with functions 10h and 20h), but will always
;	  translate prefix E0h to 00h. This allows old programs to use extended
;	  keystrokes and should not cause compatibility problems
;SeeAlso: AH=00h,AH=11h,AH=21h,INT 18/AH=01h,INT 09,INT 15/AH=4Fh

KeyPressed:
mov ah,0x0                  ;
int EBIOSINT16                    ;
cmp al,0x31                 ;
jb Dfault               ;
cmp al,0x34                 ;
ja Dfault               ;
call OutCharAl                  ;
sub al,0x31                 ;
mov ah,0x0                  ;
shl ax,0x1                  ;
shl ax,0x1                  ;
shl ax,0x1                  ;
shl ax,0x1                  ;
;change from 0x7EE to 0x7BE
;mov bp,0x7BE                ; 600 + 1BE  = ; 1CE ; 1DE ; 1EE
mov bp,0x7EE                ; 600 + 1BE  = ; 1CE ; 1DE ; 1EE
;change from sub to add
;add bp,ax                   ;
sub bp,ax
Dfault:

;mov bp,0x7BE

;instead of this line i added two byte at the end of loader
;mov al,0xD                  ; this line look unessesarry
;call OutCharAl                  ;
;mov al,0xA                  ;
;call OutCharAl                  ;

mov di,0x5                  ; what is the 5 ?
mov si,bp                   ;
mov dx,[si+0x0]             ;

mov dl,0x80                 ;why by default is 80; DL = drive number (bit 7 set for hard disk)
mov cx,[si+0x2]             ;


;--------B-1302-------------------------------
;INT 13 - DISK - READ SECTOR(S) INTO MEMORY
;	AH = 02h
;	AL = number of sectors to read (must be nonzero)
;	CH = low eight bits of cylinder number
;	CL = sector number 1-63 (bits 0-5)
;	     high two bits of cylinder (bits 6-7, hard disk only)
;	DH = head number
;	DL = drive number (bit 7 set for hard disk)
;	ES:BX -> data buffer
;Return: CF set on error
;	    if AH = 11h (corrected ECC error), AL = burst length
;	CF clear if successful
;	AH = status (see #00234)
;	AL = number of sectors transferred (only valid if CF set for some
;	      BIOSes)
;Notes:	errors on a floppy may be due to the motor failing to spin up quickly
;	  enough; the read should be retried at least three times, resetting
;	  the disk with AH=00h between attempts
;	most BIOSes support "multitrack" reads, where the value in AL
;	  exceeds the number of sectors remaining on the track, in which
;	  case any additional sectors are read beginning at sector 1 on
;	  the following head in the same cylinder; the MSDOS CONFIG.SYS command
;	  MULTITRACK (or the Novell DOS DEBLOCK=) can be used to force DOS to
;	  split disk accesses which would wrap across a track boundary into two
;	  separate calls
;	the IBM AT BIOS and many other BIOSes use only the low four bits of
;	  DH (head number) since the WD-1003 controller which is the standard
;	  AT controller (and the controller that IDE emulates) only supports
;	  16 heads
;	AWARD AT BIOS and AMI 386sx BIOS have been extended to handle more
;	  than 1024 cylinders by placing bits 10 and 11 of the cylinder number
;	  into bits 6 and 7 of DH
;	under Windows95, a volume must be locked (see INT 21/AX=440Dh/CX=084Bh)
;	  in order to perform direct accesses such as INT 13h reads and writes
;	all versions of MS-DOS (including MS-DOS 7 [Windows 95]) have a bug
;	  which prevents booting on hard disks with 256 heads (FFh), so many
;	  modern BIOSes provide mappings with at most 255 (FEh) heads
;	some cache drivers flush their buffers when detecting that DOS is
;	  bypassed by directly issuing INT 13h from applications.  A dummy
;	  read can be used as one of several methods to force cache
;	  flushing for unknown caches (e.g. before rebooting).
;BUGS:	When reading from floppies, some AMI BIOSes (around 1990-1991) trash
;	  the byte following the data buffer, if it is not arranged to an even
;	  memory boundary.  A workaround is to either make the buffer word
;	  aligned (which may also help to speed up things), or to add a dummy
;	  byte after the buffer.
;	MS-DOS may leave interrupts disabled on return from this function.
;	Apparently some BIOSes or intercepting resident software have bugs
;	  that may destroy DX on return or not properly set the Carry flag.
;	  At least some Microsoft software frames calls to this function with
;	  PUSH DX, STC, INT 13h, STI, POP DX.
;	on the original IBM AT BIOS (1984/01/10) this function does not disable
;	  interrupts for harddisks (DL >= 80h).	 On these machines the MS-DOS/
;	  PC DOS IO.SYS/IBMBIO.COM installs a special filter to bypass the
;	  buggy code in the ROM (see CALL F000h:211Eh)
;SeeAlso: AH=03h,AH=0Ah,AH=06h"V10DISK.SYS",AH=21h"PS/1",AH=42h"IBM"
;SeeAlso: INT 21/AX=440Dh/CX=084Bh,INT 4D/AH=02h

RepeatRead:
mov bx,0x7C00               ;
mov ax,0x201                ; ah=2 al=1
push di                     ;
int 0x13                    ;
pop di                      ;
jae ReadDone              ; cf=0 or zf=1

;--------B-1300-------------------------------
;INT 13 - DISK - RESET DISK SYSTEM
;	AH = 00h
;	DL = drive (if bit 7 is set both hard disks and floppy disks reset)
;Return: AH = status (see #00234)
;	CF clear if successful (returned AH=00h)
;	CF set on error
;Note:	forces controller to recalibrate drive heads (seek to track 0)
;	for PS/2 35SX, 35LS, 40SX and L40SX, as well as many other systems,
;	  both the master drive and the slave drive respond to the Reset
;	  function that is issued to either drive
;SeeAlso: AH=0Dh,AH=11h,INT 21/AH=0Dh,INT 4D/AH=00h"TI Professional"
;SeeAlso: INT 56"Tandy 2000",MEM 0040h:003Eh

xor ax,ax                   ;
int 0x13                    ;
dec di                      ;
jnz RepeatRead              ;
mov si,ERROR                ;
jmp PrintSiHalt                    ;

ReadDone:
mov si,MISSING                ;
cmp word [0x7DFE],0xAA55    ;
jz RunVBR               ;
jmp PrintSiHalt                   ;

RunVBR:
mov si,bp                   ;
jmp 0x0:0x7C00              ;







; ; prints the value of DX as hex. 
; print_hex:
; ; TODO: manipulate chars at HEX_OUT to reflect DX
; pusha
; mov si,HEX_OUT
; add si,0x6
; mov cx,0x0
; looph:
; 		cmp cl,0x4
; 		je endh
; 				mov ax,dx
; 				and ax,0x000f
; 				cmp al,0x9
; 				jle number
; 						add al,0x57
; 						jmp afteradd
; number:
; 						add al,0x30
; afteradd:
; 				mov [si],al
; 				sub si,1
; 				shr dx,0x4
; 				add cl,0x1
; 				jmp looph
; endh:

; mov si, HEX_OUT       ; print the string pointed to 
; call PrintSi; by BX ret

; popa
; ret
; ; Data 
; HEX_OUT:
;  db 0x6, '0x0000'



INVALID: db 0x17,"Invalid Partition Table"
ERROR: db 0x1E,"Error Loading Operation System"
MISSING: db 0x18, "Missing Operation System"
LOADER: db 0x20, "Ehsan Loader", 0x0D, 0x0A, "Boot Partition "
PARNUM: db 0x30
db 0x0D, 0x0A
;INVALID: db 0x1, "I"
;ERROR: db 0x1, "E"
;MISSING: db 0x1, "M"
;LOADER: db 0x6, "Q", 0x0D, 0x0A, "B "
;PARNUM: db 0x30

times 0x1B8-($-$$) db 0
;times 0x1BE-($-$$) db 0
disksignature db  0x00, 0x00, 0x00, 0x00
mostlyzeroout db  0x0, 0x0

;0000h - 0000h    Status byte (80h for bootable, 00h for non-bootable, others are invalid) (1 byte)
;0001h - 0003h    CHS address of first absolute sector (3 bytes)
;0004h - 0004h    Partition type (1 byte)
;0005h - 0007h    CHS address of last absolute sector (3 bytes)
;0008h - 000Bh    LBA of first absolute sector (4 bytes)
;000Ch - 000Fh    Number of sectors in partition (4 bytes)

times 0x3 db 0x00, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
db 0x80, 0x01, 0x01, 0x0, 0x7, 0xfe, 0x3f, 0x22, 0x3f, 0x0, 0x0, 0x0, 0x24, 0x94, 0x08, 0x0

dw 0xaa55 