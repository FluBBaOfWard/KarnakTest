set romname=KarnakTest

del %romname%.bfb

..\bin\nasm -f bin -o %romname%.bfb %romname%.asm

pause
