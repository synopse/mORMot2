/// Command Line .mab Files Generation Tool
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program mab;

{
  *****************************************************************************

  Command-Line Tool to Generate .mab files from existing .map or .dbg files
  - with Delphi, enable .map file by setting "Detailed" debug in Project Options
  - with FPC, will use DWARF debugging information instead of the `.map` file -
  a good idea is to generate -Xg external .dbg info to keep the executable small
  - if some .map/.dbg file name is specified (you can use wild chars), it will
  process all those .map files, then create the corresponding .mab files
  - if some .exe/.dll file name is specified (you can use wild chars), will
  process all matching .exe/.dll files with associated debug information, and
  will create the .mab files, then embedd the .mab content to the .exe/.dll

  *****************************************************************************
}

{$I ..\..\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I ..\..\mormot.uses.inc}
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log;

procedure Process(const FileName: TFileName; Options: TDebugFileScope);
var
  deb: TDebugFile;
  SR: TSearchRec;
  Path, FN: TFileName;
  Ext, Count: integer;
  AllOk: boolean;
begin
  AllOk := true;
  Ext := GetFileNameExtIndex(FileName, 'map,dbg,exe,dll,ocx,bpl,');
  if (Ext >= 0) and
     (FindFirst(FileName, faAnyFile, SR) = 0) then
  try
    Path := ExtractFilePath(FileName);
    repeat
      if SearchRecValidFile(SR) then
      try
        // setup the debug source file name
        FN := Path + SR.Name;
        // force (re)generate the mab content, maybe into the executable itself
        Count := 0;
        deb := TDebugFile.Create(FN,
                 Options + [dfsNoMabExternalCheck, dfsNoMabInternalCheck]);
        try
          Count := deb.LinesCount;
          if deb.DebugInfo = diNone then
          begin
            ConsoleWrite('Hint: No original Debug Info found for %', [FN]);
            AllOk := false;
          end
          else if Ext > 1 then // has debug info and is not a map/dbg
            if deb.ExeFile <> Executable.InstanceFileName then // self is busy
              deb.SaveToExe(FN); // embedd into the executable
        finally
          deb.Free;
        end;
        // ensure the (embedded) mab content is actually readable
        deb := TDebugFile.Create(FN, Options + [dfsNoMabSaveAtCreate]);
        try
          if (Count > 0) and
             (Count <> deb.LinesCount) then // paranoid
            ESynLogException.RaiseUtf8('Invalid % content (%<>%)',
              [deb.MabFile, Count, deb.LinesCount]);
          ConsoleObject(deb);
        finally
          deb.Free;
        end;
      except
        on E: Exception do
        begin
          // ignore any problem here: just print it and process next file
          ConsoleWrite('Error: % %', [E, E.Message]);
          AllOk := false;
        end;
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end
  else
  begin
    ConsoleWrite('Error: cant find any file to process matching: %', [FileName]);
    ExitCode := 2;
  end;
  if not AllOk then
    ExitCode := 3;
end;

var
  c: TExecutableCommandLine;
  fn: TFileName;
  opt: TDebugFileScope;
begin
  c := Executable.Command;
  fn := c.ArgFile(0, 'exe or ' + DEBUG_EXT + ' #source filename or mask', {optional=}false);
  opt := [];
  if c.Option('no&symbol', 'include only line info for production') then
    include(opt, dfsNoSymbols);
  if c.Option('no&mab', 'only embed to exe, no external .mab file') then
    include(opt, dfsNoMabSaveAtCreate);
  if c.ConsoleHelpFailed('mORMot ' + SYNOPSE_FRAMEWORK_VERSION +
                         ' .mab file generator') then
    ExitCode := 1
  else
    Process(fn, opt);
end.

