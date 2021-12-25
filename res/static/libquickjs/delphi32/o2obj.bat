@echo off

copy ..\*.o .

coff2omf cutils.o
omf2d cutils.o cutils.obj

coff2omf libbf.o
omf2d libbf.o libbf.obj

coff2omf libregexp.o
omf2d libregexp.o libregexp.obj

coff2omf libunicode.o
omf2d libunicode.o libunicode.obj

objconv -nr:_lre_realloc:_lrerealloc2 -nr:_round:r0und -nr:_trunc:trunk -nr:_sqrt:sq4t -nr:_exit:ex1t -nr:_cos:c0s -nr:_sin:s1n -nr:_exp:e4p quickjs.o quickjs2.o
coff2omf quickjs2.o
omf2d quickjs2.o quickjs.obj

copy *.obj d:\dev\lib2\static\delphi

@rem pause