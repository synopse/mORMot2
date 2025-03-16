// MVC sample web application, publishing a simple BLOG
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program MVCServer;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\src\mormot.win.default.manifest.res}  // from mORMot 2 /src folder
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}

  MVCModel     in 'MVCModel.pas',
  MVCViewModel in 'MVCViewModel.pas',

  sysutils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.os,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.orm.core,
  mormot.rest.http.server,
  mormot.rest.sqlite3,
  mormot.rest.server,
  mormot.core.log;


var
  aModel: TOrmModel;
  aServer: TRestServerDB;
  aApplication: TBlogApplication;
  aHTTPServer: TRestHttpServer;
  aTemplatesFolder: TFileName;
  LogFamily: TSynLogFamily;

begin
  LogFamily := TSynLog.Family;
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOnFile;
  LogFamily.AutoFlushTimeOut := 5;
  LogFamily.HighResolutionTimestamp := true;
  //LogFamily.EchoToConsole := LOG_VERBOSE;
  aModel := CreateModel;
  try
    aServer := TRestServerDB.Create(
      aModel, ChangeFileExt(Executable.ProgramFileName, '.db'));
    try
      aServer.DB.Synchronous := smNormal;
      aServer.DB.LockingMode := lmExclusive;
      aServer.Server.CreateMissingTables;
      aApplication := TBlogApplication.Create;
      try
        if not DirectoryExistsMake(
            [Executable.ProgramFilePath, 'Views'], @aTemplatesFolder) then
          // circumvent if was not compiled into 'exe' sub-folder
          DirectoryExistsMake(
            [Executable.ProgramFilePath, 'exe', 'Views'], @aTemplatesFolder);
        aApplication.Start(aServer, aTemplatesFolder);
        aHTTPServer := TRestHttpServer.Create('8092', aServer, '+',
          HTTP_DEFAULT_MODE, nil, 16, secNone, '', '', HTTPSERVER_DEBUG_OPTIONS);
        try
          aHTTPServer.RootRedirectToURI('blog/default'); // redirect / to blog/default
          aServer.RootRedirectGet := 'blog/default';     // redirect blog to blog/default
          ConsoleWrite(['"MVC Blog Server" launched on port 8092 using ',
            aHttpServer.HttpServer], ccLightCyan);
          ConsoleWrite(#10'You can point to http://localhost:8092 to access the web app.');
          ConsoleWrite('Or check http://localhost:8092/blog/mvc-info for information.');
          ConsoleWrite(#10'Press [Enter] or ^C to close the server.'#10, ccCyan);
          ConsoleWaitForEnterKey;
          ConsoleWrite('HTTP server shutdown...', ccMagenta);
        finally
          aHTTPServer.Free;
        end;
      finally
        aApplication.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
  ConsoleWrite('HTTP server finalized. Bye!');
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

