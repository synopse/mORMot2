@echo off

del *.o
copy ..\quickjs2.o quickjs2.o

objconv -nr:round:r0und -nr:trunc:trunk -nr:sqrt:sq4t -nr:exit:ex1t -nr:cos:c0s -nr:sin:s1n -nr:exp:e4p quickjs2.o quickjs.o

del quickjs2.o
copy quickjs.o d:\dev\lib2\static\delphi

@rem pause