set dest=..\src\

brcc32 mormot.win.default.manifest.rc -fo%dest%mormot.win.default.manifest.res
brcc32 mormot.win.admin.manifest.rc -fo%dest%mormot.win.admin.manifest.res
brcc32 mormot.tz.rc -fo%dest%mormot.tz.res

pause