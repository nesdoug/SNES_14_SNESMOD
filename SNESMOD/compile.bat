@echo off

set name="snesmod_driver"

set path=%path%;..\..\bin\

set CC65_HOME=..\

ca65 main.s -g

ld65 -C spc700.cfg -o %name%.bin main.o -Ln labels.txt

del *.o

pause

