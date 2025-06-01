#!/bin/sh
romname=KarnakTest

nasm -f bin -o $romname.bfb $romname.asm -l $romname.lst
