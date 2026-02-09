{:
———————————————————————————————————————————————— (C) martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgConst.pas

  Last modified
    Date : 07.02.2026
    Author : Martin Doyle
    Email : martin-doyle@online.de

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to
    deal in the Software without restriction, including without limitation the
    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
    sell copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.
————————————————————————————————————————————————————————————————————————————
}
unit rgConst;

interface

{$I mormot.defines.inc}

type
  TRunMode = (rmLocal, rmService);

const
  ApplicationTitle = 'Rechnung';
  DatabaseFile = 'Project10.db';
  RunMode: TRunMode = rmService;
  HttpHost = 'localhost';
  HttpPort = '11111';
  ConfigFileName = 'rechnung.config';
  { Path }
  IniDataPath = 'Data';

resourcestring
  { Messages }
  mErrorNoDatabase = 'Database not found. Please call the hotline.';
  mErrorEmptyStartDate = 'The start date field must be filled in.';
  mErrorStartDateFormat = 'Please enter a valid start date';
  mErrorEmptyEndDate = 'The end date field must be filled in.';
  mErrorEndDateFormat = 'Please enter a valid end date.';

var
  DataPath: string;
  DataFile: string;
  ApplicationPath: string;

implementation

uses
  SysUtils,
  {$ifdef OSPOSIX}
  fileinfo,
  {$ifdef OSDARWIN}
  machoreader,
  {$else}
  elfreader,
  {$endif OSDARWIN}
  {$endif OSPOSIX}
  mormot.core.base,
  mormot.core.log,
  mormot.core.os;

{$ifdef OSPOSIX}
procedure InitVersionFromResources;
var
  Info: TVersionInfo;
begin
  Info := TVersionInfo.Create;
  try
    try
      Info.Load(HInstance);
      SetExecutableVersion(
        Info.FixedInfo.FileVersion[0],
        Info.FixedInfo.FileVersion[1],
        Info.FixedInfo.FileVersion[2],
        Info.FixedInfo.FileVersion[3]);
    except
      // no version resource embedded
    end;
  finally
    Info.Free;
  end;
end;
{$endif OSPOSIX}

begin
  {$ifdef OSWINDOWS}
  GetExecutableVersion;
  {$else}
  InitVersionFromResources;
  {$endif OSWINDOWS}
  ApplicationPath := Executable.ProgramFilePath;
  DataPath := ExpandFileName(IncludeTrailingPathDelimiter(ApplicationPath) +
    '..\' + IniDataPath + '\');
  TSynLog.Family.DestinationPath:='log';
  TSynLog.Family.ArchivePath:='log';
  TSynLog.Family.RotateFileDailyAtHour:=0;
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  TSynLog.Family.OnArchive := EventArchiveSynLZ;
  TSynLog.Family.ArchiveAfterDays := 1;
  TSynLog.Family.EchoToConsole:= LOG_NFO;
  DataFile := DataPath + DatabaseFile;
  TSynLog.Add.Log(sllInfo, 'ApplicationPath: ' + ApplicationPath);
  TSynLog.Add.Log(sllInfo, 'DataPath: ' + DataPath);
  if not FileExists(DataFile) then
  begin
    if not DirectoryExists(DataPath) then
      ForceDirectories(DataPath);
    TSynLog.Add.Log(sllInfo, 'DataFile: ' + DataFile +
      ' not found. A new database will be created.');
  end;
end.
