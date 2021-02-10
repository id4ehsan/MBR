set PATH=%PATH%;C:\Program Files\NASM\
nasm %~dp0MBR.asm -i %~dp0 -f bin -o %~dp0MBR.bin -l %~dp0mbr.lst -DBUILDDISK
pause