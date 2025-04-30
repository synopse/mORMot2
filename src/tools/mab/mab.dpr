/// Command Line .mab Files Generation Tool
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program mab;

{
  *****************************************************************************

  Command-Line Tool to Generate .mab files from existing .map or .dbg files
  - if some .map file name is specified (you can use wild chars), it will
  process all those .map files, then create the corresponding .mab files
  - if some .exe/.dll file name is specified (you can use wild chars), will
  process all matching .exe/.dll files with an associated .map file, and will
  create the .mab files, then embedd the .mab content to the .exe/.dll
  - if no file name is specified, will process *.map into *.mab from the
  current directory
  - with FPC, will use DWARF debugging information instead of the `.map` file

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
  mormot.core.base         in '..\..\core\mormot.core.base.pas',
  mormot.core.os           in '..\..\core\mormot.core.os.pas',
  mormot.core.text         in '..\..\core\mormot.core.text.pas',
  mormot.core.log          in '..\..\core\mormot.core.log.pas';

procedure Process(const FileName: TFileName);
var
  SR: TSearchRec;
  Path, Map, FN: TFileName;
  Ext, Count: integer;
  AllOk: boolean;
begin
  AllOk := true;
  Ext := GetFileNameExtIndex(FileName, 'map,dbg,exe,dll,ocx,bpl');
  if (Ext >= 0) and
     (FindFirst(FileName, faAnyFile, SR) = 0) then
  try
    Path := ExtractFilePath(FileName);
    repeat
      if SearchRecValidFile(SR) then
      try
        // setup the debug source file name
        FN := Path + SR.Name;
        {$ifdef ISDELPHI}
        if Ext = 2 then // search a valid .map newer than the .exe
        begin
          Map := ChangeFileExt(FN, '.map');
          if FileAgeToUnixTimeUtc(Map) < FileAgeToUnixTimeUtc(FN) then
            Map := FN;
        end
        else
        {$endif ISDELPHI}
          Map := FN;
        // generate the mab content, maybe into the executable itself
        with TDebugFile.Create(Map, {MabCreate=}true) do
        try
          Count := length(Symbols);
          if not HasDebugInfo then
          begin
            ConsoleWrite('Error: no Debug Info found on %', [FN]);
            AllOk := false;
          end
          else if Ext > 1 then // has debug info and is not a map/dbg
            SaveToExe(FN);     // embedd into the executable
        finally
          Free;
        end;
        // ensure the generated mab content is actually readable
        with TDebugFile.Create(FN, {MabCreate=}false) do
        try
          if Count <> length(Symbols) then
            raise ESynLogException.Create('Invalid .mab content');
          ConsoleWrite('Found % symbols in %', [Count, SR.Name]);
        finally
          Free;
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
    ExitCode := 1;
end;

begin
  if ParamCount > 0 then
    Process(ParamStr(1))
  else
    Process('*.map');
end.

