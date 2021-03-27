@echo off

del *.o
copy ..\cutils.o cutils2.o
copy ..\libbf.o libbf2.o
copy ..\libregexp.o libregexp2.o
copy ..\libunicode.o libunicode2.o
copy ..\quickjs.o quickjs2.o

objconv -felf64 cutils2.o cutils.o
objconv -felf64 libbf2.o libbf.o
objconv -felf64 libregexp2.o libregexp.o
objconv -felf64 libunicode2.o libunicode.o

objconv -felf64 -nr:lre_realloc:lrerealloc2 -nr:round:r0und -nr:trunc:trunk -nr:sqrt:sq4t -nr:exit:ex1t -nr:cos:c0s -nr:sin:s1n -nr:exp:e4p quickjs2.o quickjs.o

del *2.o
copy *.o d:\dev\lib2\static\delphi

@rem 
pause