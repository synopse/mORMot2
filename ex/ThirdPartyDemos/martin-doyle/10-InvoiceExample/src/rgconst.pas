{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
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


const
  ApplicationTitle = 'Rechnung';
  AppVersionMajor = 2;
  AppVersionMinor = 0;
  AppVersionBuild = 489;
  AppBuildDate = '26.12.2025';
  DatabaseFile = 'Project10.db';
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
  mormot.core.base,
  mormot.core.log,
  mormot.core.os;

begin
  ApplicationPath := Executable.ProgramFilePath;
  DataPath := ExpandFileName(IncludeTrailingPathDelimiter(ApplicationPath) +
    '..\' + IniDataPath + '\');
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.OnArchive := EventArchiveSynLZ;
  TSynLog.Family.ArchiveAfterDays := 1;
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
