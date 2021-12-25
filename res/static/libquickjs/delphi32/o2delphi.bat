@echo off

copy ..\quickjs2.o quickjs.o

objconv -nr:_round:r0und -nr:_trunc:trunk -nr:_sqrt:sq4t -nr:_exit:ex1t -nr:_cos:c0s -nr:_sin:s1n -nr:_exp:e4p quickjs.o quickjs2.o
coff2omf quickjs2.o
omf2d quickjs2.o quickjs.obj

copy quickjs.obj d:\dev\lib2\static\delphi

@rem pause