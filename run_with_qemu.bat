set PATH=%PATH%;C:\Program Files\qemu
qemu-system-x86_64 -display sdl -drive file=mbr.bin,format=raw,index=0,media=disk 
pause