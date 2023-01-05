// MVC sample web application, publishing a simple BLOG
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program MVCServer;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}

  MVCModel     in 'MVCModel.pas',
  MVCViewModel in 'MVCViewModel.pas',

  sysutils,
  mormot.core.base,
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
        aApplication.Start(aServer);
        aHTTPServer := TRestHttpServer.Create('8092', aServer, '+',
          HTTP_DEFAULT_MODE, nil, 16, secNone, '', '', HTTPSERVER_DEBUG_OPTIONS);
        try
          aHTTPServer.RootRedirectToURI('blog/default'); // redirect / to blog/default
          aServer.RootRedirectGet := 'blog/default';     // redirect blog to blog/default
          writeln('"MVC Blog Server" launched on port 8092 using ',
            aHttpServer.HttpServer.ClassName);
          writeln(#10'You can check http://localhost:8092/blog/mvc-info for information');
          writeln('or point to http://localhost:8092 to access the web app.');
          writeln(#10'Press [Enter] or ^C to close the server.'#10);
          ConsoleWaitForEnterKey;
          writeln('HTTP server shutdown...');
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
  writeln('HTTP server finalized. Bye!');
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

